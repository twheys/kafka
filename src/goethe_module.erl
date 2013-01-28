-module(goethe_module).
-behavior(gen_server).

-export([start/4,start_link/4]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% goethe_module exports
-export([internal/2,inbound/4,notify/3,get_api/3,check_deps/2]).

-record(state, {
name,mod,deps=[],nstate
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  API
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback init(Args :: term()) ->
    {ok, State :: term()} | 
    {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | 
    ignore.
-callback handle_internal(
        Request :: term(),
        State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {ok, NewState :: term()} |
    {ok, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback handle_inbound(
        Action :: atom(),
        Role:: atom(),
        Data:: term(),
        Session :: {goethe_session,pid(),Tag :: term()},
        State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback handle_event(
        Event :: atom(),
        Data:: term(),
        State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()} |
    no_match.
-callback get_api(Role:: atom()) ->
    {ok, Api :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  exported functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @spec internal(Name :: Action :: atom(), Data :: term(), Session :: {goethe_session,pid(),Tag :: term()})
% @doc 
internal(Name, Request) -> gen_server:call(Name, {internal, {Request}}).

% @spec inbound(Action :: atom(), Data :: term(), Session :: {goethe_session,pid(),Tag :: term()})
% @doc 
inbound(Name, Role, {Action, Data}, Session) -> gen_server:cast(Name, {inbound, Role, {Action, Data}, Session}).

% @spec event(Event :: {atom(), term()}, Session :: {goethe_session,pid(),Tag :: term()})
% @doc 
notify(Name, Event, Data) -> gen_server:cast(Name, {event, {Event, Data}}).

% @spec get_api(Event :: {atom(), term()}, Session :: {goethe_session,pid(),Tag :: term()})
% @doc 
get_api(Name, Role, Session) -> gen_server:call(Name, {get_api, Role, Session}).

% @spec get_api(Event :: {atom(), term()}, Session :: {goethe_session,pid(),Tag :: term()})
% @doc 
check_deps(Name, Available) -> gen_server:call(Name, {check_deps, {Available}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    {reply, Reply, NewNState, hibernate} -> {reply, Reply, State#state{nstate=NewNState}, hibernate};
    {ok, NewNState} -> {reply, ok, State#state{nstate=NewNState}, hibernate};
    {ok, NewNState, hibernate} -> {reply, ok, State#state{nstate=NewNState}, hibernate, hibernate};
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
    case Mod:handle_inbound(Role, Action, Data, Session, NState) of
    {ok, NewNState} -> {noreply, State#state{nstate=NewNState}, hibernate};
    {ok, NewNState, hibernate} -> {noreply, State#state{nstate=NewNState}, hibernate, hibernate};
    {stop, Reason, NewNState} -> {stop, Reason, State#state{nstate=NewNState}};
    no_match -> {noreply, State}
    end;

handle_cast({event, {Event, Data}}, #state{mod=Mod,nstate=NState} = State) ->
    case Mod:handle_event(Event, Data, NState) of
    {ok, NewNState} -> {noreply, State#state{nstate=NewNState}, hibernate};
    {ok, NewNState, hibernate} -> {noreply, State#state{nstate=NewNState}, hibernate, hibernate};
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
