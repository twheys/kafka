-module(goethe_core).
-author('twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% Module outbound exports
-export([greet/1,send_error_code/3,pong/1]).
% Module inbound exports
-export([get_apple/1,ping/1,encrypt/2]).
% Module application exports
-export([register_node/3]).

% Module namespace
-define(NAME, server).


start() ->  gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  outbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
greet(Session) ->
    Session:send_msg(
        {[{<<"server.welcome">>,
            [
            {[{<<"msg">>,<<"Welcome to Goethe! Please authenticate...">>}]}
            ]
        }]}
    ),
    ok.

send_error_code(Session, Blame, Code) ->
    BlameToken = goethe_util:output("server.error.~p_side", [Blame]),
    logger:trace("Sending error code with blame ~p and code ~p", 
    	[BlameToken, Code]),
    Session:send_msg(
        {[{
            list_to_binary(BlameToken),
            [
                {[{<<"code">>,atom_to_binary(Code, utf8)}]}
            ]
        }]}
    ),
    ok.
pong(Session) ->
    Session:send_msg(
   		{[{<<"action">>,<<"server.pong">>}]}
    ),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_apple(Session) ->
    logger:info("Received get_apple from client"),
    greet(Session),
    ok.

ping(Session) ->
    logger:info("Received ping from client"),
    pong(Session),
    ok.

encrypt(Session, Key) -> 
    logger:info("Encrypting connection"),
    Session:encrypt(Key),
    ok.

register_node(Session, Name, Port) -> 
    logger:info("Encrypting connection"),
    {ok, {Address, _}} = inet:peername(Session:get(host)),
    case goethe:register_node(Name, Address, Port) of
    ok -> ok; % Send Node network status
    Other -> logger:info("Register Node response ~p", [Other])
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(_Args) ->
    logger:info("Loading Core API Module."),
    goethe:register_module(
    	{?NAME, {?MODULE, [
    		{get_apple, {get_apple, 1, [plain]}},
    		{ping, {ping, 1, [all]}},
    		{encrypt, {encrypt, 2, [fcrypto,cloud]}},
    		{get_node, {get_node, 1, [fcrypto,cloud]}},
    		{join_node, {join_node, 2, [fcrypto,cloud]}},
            {register_node, {register_node, 3, [cloud]}}
    	]}}
    ),
    timer:sleep(infinity).

handle_call(_Req, _From, State) -> {noreply, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.