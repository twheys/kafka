-module(goethe_core).
-author("twheys@gmail.com").
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([greet/1,send_error_code/3,pong/1]).
-export([get_apple/1,ping/1,encrypt/1]).

-define(NAME, server).


start() ->  gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(_Args) ->
    logger:info("Loading Core API Module."),
    goethe:register_module(
    	{?NAME, {?MODULE, [
    		{get_apple, {get_apple, 1, [plain,fcrypto,cloud]}},
    		{ping, {ping, 1, [plain,fcrypto,cloud]}},
    		{encrypt, {encrypt, 1, [plain,fcrypto,cloud]}},
    		{get_nodes, {get_nodes, 1, [plain,fcrypto,cloud]}},
    		{join_node, {join_node, 2, [plain,fcrypto,cloud]}}
    	]}}
    ),
    timer:sleep(infinity).


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
    greet(From),
    ok.

ping(From) ->
    logger:info("Received ping from client"),
    pong(From),
    ok.

encrypt(From) -> 
    logger:info("Encrypting connection"),
    goethe:encrypt(From,full),
    ok.




handle_call(_Req, _From, State) -> {noreply, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
