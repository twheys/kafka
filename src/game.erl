-module(game).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([action/2]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, []}.

action([{action, [{name, "authenticate"} | Params]}] , Connection) -> 
	gen_server:call(?MODULE, authenticate, [Connection, Params]);
action(_JSON, Connection) -> gen_server:call(?MODULE, invalid_action, [Connection]).

handle_call(_Call, _From, State) -> {noreply, State}.
handle_cast(_Cast, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) -> {normal, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.