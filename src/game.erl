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
action(unencypted, [
	{action, [
		{name, "apple"}
	]}], Connection) ->
	gen_server:call(?MODULE, get_apple, [Connection]);
action(encrypted, [
	{action, [
		{name, "authenticate"}, 
		{username, Username},
		{password, Password}
	]}], Connection) -> 
	gen_server:call(?MODULE, authenticate, [Connection, Username, Password]);
action(encrypted, [
	{action, [
		{name, ActionName} |
		Params
	]}], Connection) ->
	gen_server:call(?MODULE, send_to_module, [ActionName, Params, Connection]);
action(_SecurityLevel, _JSON, Connection) -> gen_server:call(?MODULE, invalid_action, [Connection]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(_Call, _From, State) -> {noreply, State}.
handle_cast(_Cast, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) -> {normal, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.