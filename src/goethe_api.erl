-module(goethe_api).
-author("twheys@gmail.com").

-export([start/0,start_link/0]).

-define(PREFIX, server).
-define(API_NAME, goethe).

-record(state, {
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  constructors
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> {ok, spawn(fun() -> init() end)}.
start_link() -> {ok, spawn_link(fun() -> init() end)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init() ->
    logger:info("Loading Core API Module."),
    goethe:register_module(
        game_api,
        self(),
        ?PREFIX,
        goethe,
        [plain,fcrypto,cloud]
    ),
    logger:info("Core Module successfully loaded."),
    %goethe:add_module(?PREFIX, ?API_NAME, [plain,fcrypto,cloud]),
    loop(#state{}).

loop(State) ->
    receive
   	% outbound messages
	{greet, {To}} -> greet(To);
	{send_error_code, {To, Blame, Code}} -> send_error_code(To, Blame, Code);
    {pong, {To}} -> pong(To);

	% inbound messages
    {From, get_apple} -> get_apple(From);
	{From, ping} -> ping(From);
	{From, encrypt} -> encrypt(From);
	{From, invalid_action} -> invalid_action(From);
	{From, {invalid_action, {Msg}}} -> invalid_action(From, Msg);
	{From, Other} -> game_util:invalid_action(From, Other);
	Other -> game_util:invalid_action(Other)
    end,
    loop(State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  outbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
greet(Pid) ->
    goethe:send_msg(Pid,
        {[{<<"server.welcome">>,
            [
            {[{<<"msg">>,<<"Welcome to Goethe! Please authenticate...">>}]}
            ]
        }]}
    ),
    ok.

send_error_code(To, Blame, Code) ->
    BlameToken = "server.error."++atom_to_list(Blame)++"_side",
    logger:trace("Sending error code to Pid ~p with blame ~p and code ~p", 
    	[To, BlameToken, Code]),
    goethe:send_msg(To,
        {[{
            list_to_binary(BlameToken),
            [
                {[{<<"code">>,atom_to_binary(Code, utf8)}]}
            ]
        }]}
    ),
    ok.
pong(To) ->
    goethe:send_msg(To,
   		{[{<<"action">>,<<"server.pong">>}]}
    ),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_apple(From) ->
    logger:info("Received get_apple from client"),
    goethe:greet(From),
    game_util:reply_to(From).

ping(From) ->
    logger:info("Received ping from client"),
    spawn(goethe, pong, [From]),
    game_util:reply_to(From).

encrypt(From) ->
    spawn(goethe, encrypt, [From,full]),
    game_util:reply_to(From).

invalid_action(From) ->
    logger:warn("Invalid client action!"),
    spawn(goethe, send_error_code, [From, client, invalid_action]),
    game_util:reply_to(From).

invalid_action(From, Msg) ->
    logger:warn("Invalid client action! ~p", Msg),
    spawn(goethe, send_error_code, [From, client, invalid_action]),
    game_util:reply_to(From).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  util functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%