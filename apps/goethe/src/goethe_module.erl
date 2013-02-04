-module(goethe_module).
-author('Tim Heys twheys@gmail.com').
-behavior(gen_server).

-export([start/4,start_link/4]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% goethe_module exports
-export([internal/2,inbound/4,notify/3,get_api/3,check_deps/2]).

-record(state, {
name,mod,deps=[],nstate
}).


%%==========================================================================
%
%  API
%
%%==========================================================================
-type role() :: 'plain' | 'pencrypt' | 'fencrypt' | 'web' | 'auth' | 'admin' | 'cloud'.
-type session() :: term().

-callback init(Args :: term()) ->
    {ok, State :: term()} | 
    {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | 
    ignore.
-callback handle_internal(
        Request :: term(),
        State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    %{reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {ok, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback handle_inbound(
        Action :: atom(),
        Role:: role(),
        Data:: term(),
        Session :: session(),
        State :: term()) ->
    {ack, NewState :: term()} |
    {ack, Reply :: list(), NewState :: term()} |
    {nack, Code, NewState :: term()} |
    {nack, {Code, Reply :: list()}, NewState :: term()} |
    {ok, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback handle_event(
        Event :: atom(),
        Data:: term(),
        State :: term()) ->
    {ok, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback get_api(Role:: role()) ->
    {ok, Api :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


%%==========================================================================
%
%  exported functions
%
%%==========================================================================
% @spec inbound(
%       Name :: string(),
%       Request :: {atom(), term()}) -> ok.
% @doc 
internal(Name, Request) ->
    gen_server:call(Name, {internal, {Request}}).

-spec inbound(
    Name :: string() | atom(),
    Role :: role(),
    {Action :: atom(),
        Data :: term()},
    Session :: session()) -> ok.
% @doc 
inbound(Name, Role, {Action, Data}, Session) ->
    gen_server:cast(Name, {inbound, Role, {Action, Data}, Session}).

% @spec notify(
%       Name :: string(),
%       Event :: atom(),
%       Data :: term()) -> ok.
% @doc 
notify(Name, Event, Data) ->
    gen_server:cast(Name, {event, {Event, Data}}).

-spec get_api(
    Name :: string() | atom(),
    Role :: role(),
    Session :: session()) -> ok.
% @doc 
get_api(Name, Role, Session) ->
    gen_server:call(Name, {get_api, Role, Session}).

% @spec check_deps(
%       Name :: string(),
%       Available :: list()) -> ok.
% @doc 
check_deps(Name, Available) ->
    gen_server:call(Name, {check_deps, {Available}}).


%%==========================================================================
%
%  gen_server functions
%
%%==========================================================================
start(Name, Mod, Args, Deps) -> gen_server:start({local, Name}, ?MODULE, [Name, Mod, Args, Deps], []).
start_link(Name, Mod, Args, Deps) -> gen_server:start_link({local, Name}, ?MODULE, [Name, Mod, Args, Deps], []).


init([Name, Mod, Args, Deps]) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
        {ok, NewState} ->
            goethe:register_module(Name),
            {ok, #state{name=Name,mod=Mod,nstate=NewState}};
        {ok, NewState, hibernate} ->
            goethe:register_module(Name),
            {ok, #state{name=Name,mod=Mod,deps=Deps,nstate=NewState}, hibernate};
        Other -> Other
    end.


handle_call({internal, Request}, _, #state{mod=Mod,nstate=NState} = State) ->
    case Mod:handle_internal(Request, NState) of
    {reply, Reply, NewNState} -> {reply, Reply, State#state{nstate=NewNState}};
    {ok, NewNState} -> {reply, ok, State#state{nstate=NewNState}};
    {stop, Reason, Reply, NewNState} -> {stop, Reason, Reply, State#state{nstate=NewNState}};
    {stop, Reason, NewNState} -> {stop, Reason, State#state{nstate=NewNState}};
    no_match -> {reply, no_match, State}
    end;

handle_call({get_api, Role, Session}, _, #state{mod=Mod} = State) ->
    {ok, Api} = Mod:get_api(Role),
    Session:send_msg(Api),
    {reply, ok, State};

handle_call({check_deps, {_Available}}, _, #state{deps=_Deps} = State) ->
    % TODO add logic to check deps
    case ok of
    ok -> {reply, ok, State};
    {error, Missing} -> {reply, {error, Missing}, State}
    end;

handle_call(Request, _From, State) ->
    logger:info("Unexpected call in goethe_module: ~p", [Request]),
    {reply, inv_call, State}.


handle_cast({inbound, Role, {Action, Data}, Session}, #state{mod=Mod,nstate=NState} = State) ->
    catch case Mod:handle_inbound(Role, Action, Data, Session, NState) of
    {ack, NewNState} ->
        goethe:ack(Session, Action),
        {noreply, State#state{nstate=NewNState}};
    {ack, [Reply], NewNState} ->
        goethe:ack(Session, Action, [Reply]),
        {noreply, State#state{nstate=NewNState}};
    {ack, Reply, NewNState} ->
        goethe:ack(Session, Action, [Reply]),
        {noreply, State#state{nstate=NewNState}};

    {nack, {Code, [Reply]}, NewNState} ->
        goethe:nack(Session, Action, Code, [Reply]),
        {noreply, State#state{nstate=NewNState}};
    {nack, {Code, Reply}, NewNState} ->
        goethe:nack(Session, Action, Code, [Reply]),
        {noreply, State#state{nstate=NewNState}};
    {nack, Code, NewNState} ->
        goethe:nack(Session, Action, Code),
        {noreply, State#state{nstate=NewNState}};

    {ok, NewNState} -> {noreply, State#state{nstate=NewNState}};

    {stop, Reason, NewNState} -> {stop, Reason, State#state{nstate=NewNState}};
    no_match ->
        logger:info("Unexpected inbound message in Module: ~p Action: [~p]~p:~p", [Mod, Role, Action, Data]),
        goethe:nack(Session, Action, <<"invalid.action">>),
        {noreply, State};
    Other -> goethe:nack(Session, Action, <<"server.error">>)
    end;

handle_cast({event, {Event, Data}}, #state{mod=Mod,nstate=NState} = State) ->
    case Mod:handle_event(Event, Data, NState) of
    {ok, NewNState} -> {noreply, State#state{nstate=NewNState}};
    {stop, Reason, Reply, NewNState} -> {stop, Reason, Reply, State#state{nstate=NewNState}};
    {stop, Reason, NewNState} -> {stop, Reason, State#state{nstate=NewNState}};
    no_match -> {noreply, State}
    end;

handle_cast(Request, State) ->
    logger:info("Unexpected cast in goethe module: ~p", [Request]),
    {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(Reason, #state{mod=Mod,nstate=NState}) -> Mod:terminate(Reason, NState).
code_change(OldVsn, #state{mod=Mod,nstate=NState} = State, Extra) -> 
    {ok, NewNState} = Mod:code_change(OldVsn, NState, Extra),
    {ok, State#state{nstate=NewNState}}.
