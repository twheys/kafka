-module(game).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([register_module/0,action/3]).

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

action(SecurityLevel, [
	FirstAction | Rest
	], Connection) ->
	action(SecurityLevel, FirstAction, Connection),
	action(SecurityLevel, Rest, Connection);
action(unencypted, {
    {[{<<"action">>,
        [
        {[{<<"name">>,<<"client.get_apple">>}]},
        {[{<<"note">>,<<"Welcome to Goethe! Please authenticate...">>}]}
        ]
    }]}
    }, Connection) ->
	gen_server:call(?MODULE, {get_apple, {Connection}});
action(unencypted, {
    {[{<<"action">>,
        [
        {[{<<"name">>,<<"client.ping">>}]}
        ]
    }]}
    }, Connection) ->
	gen_server:call(?MODULE, {ping, {Connection}});
action(encrypted, {
    {[{<<"action">>,
        [
        {[{<<"name">>,<<"client.authenticate">>}]},
        {[{<<"username">>,Username}]},
        {[{<<"password">>,Password}]}
        ]
    }]}
    }, Connection) -> 
	gen_server:call(?MODULE, {authenticate, {Connection, Username, Password}});
action(encrypted, {
    {[{<<"action">>,
        [
        {[{<<"name">>,ActionName}]} |
        Params
        ]
    }]}
    }, Connection) ->
	gen_server:call(?MODULE, {send_to_module, {ActionName, Params, Connection}});
action(_SecurityLevel, JSON, Connection) -> gen_server:call(?MODULE, {invalid_action, {Connection, JSON}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({ping, {Connection}}, _From, State) ->
    logger:info("Received ping from client"),
	Connection ! {write, {
	    [{
	        <<"action">>,
            {
	            <<"name">>,
                <<"server.pong">>
            }
        }]
    }},
    {reply, ok, State};

handle_call({invalid_action, {_Connection, JSON}}, _From, State) ->
    logger:error("Invalid client action! Msg: ~p", JSON),
    {reply, ok, State};
handle_call(_Call, _From, State) -> {noreply, State}.
handle_cast(_Cast, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) -> {normal, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
