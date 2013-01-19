-module(game).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([register_module/0,action/2]).
-export([greet/1,send_error_code/3,pong/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, []}.

register_module() ->
	gen_server:call(?MODULE, register_module, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Public API - JSON
%     Incoming
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
action(SecurityLevel, [
	FirstAction | Rest
	]) ->
	action(SecurityLevel, FirstAction),
	action(SecurityLevel, Rest);
action(plain,
    {[{<<"action">>,<<"client.get_apple">>}]}
    ) ->
    gen_server:call(?MODULE, {get_apple, {}});
action(plain,
    {[{<<"action">>,<<"client.ping">>}]}
    ) ->
    logger:debug("ping function matched"),
	gen_server:call(?MODULE, {ping, {}});
action(plain,
    {[{<<"action">>,<<"client.encrypt">>}]}
    ) ->
    logger:debug("encrypt function matched"),
    gen_server:call(?MODULE, {encrypt, {}});
action(crypto,
    {[{<<"client.authenticate">>,
        [
        {[{<<"username">>,Username}]},
        {[{<<"password">>,Password}]}
        ]
    }]}
    ) -> 
	gen_server:call(?MODULE, {authenticate, {Username, Password}});
action(crypto,
    {[{<<"action">>,<<"client.get_rooms">>}]}
    ) -> 
    gen_server:call(?MODULE, {get_rooms, {}});
action(crypto,
    {[{<<"client.join_room">>,
        [
        {[{<<"room">>,RoomName}]}
        ]
    }]}
    ) -> 
    gen_server:call(?MODULE, {join_room, {RoomName}});
action(crypto,
    {[{<<"action">>,
        [
        {[{<<"name">>,ActionName}]} |
        Params
        ]
    }]}
    ) ->
	gen_server:call(?MODULE, {send_to_module, {ActionName, Params}});
action(_SecurityLevel, JSON) -> gen_server:call(?MODULE, {invalid_action, {JSON}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Public API - JSON
%     Outgoing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
greet(Pid) -> gen_server:cast(?MODULE, {greet, {Pid}}).
send_error_code(Pid, Blame, Code) -> gen_server:cast(?MODULE, {send_error_code, {Pid, Blame, Code}}).
pong(Pid) -> gen_server:cast(?MODULE, {pong, {Pid}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Handle Calls
%     Incoming
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({get_apple, {}}, {From, _}, State) ->
    logger:info("Received get_apple from client"),
    greet(From),
    {reply, ok, State};

handle_call({ping, {}}, {From, _}, State) ->
    logger:info("Received ping from client"),
    pong(From),
    {reply, ok, State};

handle_call({encrypt, {}}, {From, _}, State) ->
    goethe_server:encrypt(From, full),
    {reply, ok, State};

handle_call({invalid_action, _}, {From, _}, State) ->
    logger:warn("Invalid client action!"),
    game:send_error_code(From, client, invalid_action),
    {reply, ok, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Handle Calls
%     Outgoing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(_Call, _From, State) -> {noreply, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Handle Casts
%     Incoming
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Handle Casts
%     Outgoing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({greet, {Pid}}, State) ->
    goethe_server:send_msg(Pid,
        {[{<<"action">>,
            [
            {[{<<"name">>,<<"server.welcome">>}]},
            {[{<<"msg">>,<<"Welcome to Goethe! Please authenticate...">>}]}
            ]
        }]}
    ),
    {noreply, State};
handle_cast({send_error_code, {Pid, Blame, Code}}, State) ->
    BlameToken = "server.error."++atom_to_list(Blame)++"_side",
    logger:trace("Sending error code to Pid ~p with blame ~p and code ~p", [Pid, BlameToken, Code]),
    goethe_server:send_msg(Pid,
        {[{
            <<"action">>,
            [
                {[{<<"name">>,list_to_binary(BlameToken)}]},
                {[{<<"code">>,atom_to_binary(Code, utf8)}]}
            ]
        }]}
    ),
    {noreply, State};
handle_cast({pong, {Pid}}, State) ->
    goethe_server:send_msg(Pid,
        {[{
            <<"action">>,
            [
                {[{<<"name">>,<<"server.pong">>}]}
            ]
        }]}
    ),
    {noreply, State};


handle_cast(_Cast, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) -> {normal, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
