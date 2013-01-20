%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Goethe Game Server
%%  
%%    This API has three processes, which are merged together to provide
%%    a core system.  The behavior here should be mimicked in each module,
%%    but in a separated manner.
%%    
%%    The three processes performed are described below.
%%       
%%       Application (Erlang Behavior)
%%         When the application is started from the command line,  this
%%         API is called from initialize the supervisor, which initializes
%%         the socket server, modules, and other components.
%%       
%%       Game Server
%%         The central component of the application.  Controls the socket
%%         server and maintains application state, as well as interaction
%%         between other nodes.
%%       
%%       Socket Server
%%         The 
%%       
%%       Game Core Module
%%         The core application is designed to handle modularization, 
%%         which it itself is also a Module.  The module provides the 
%%         key API components for the client, such as initializing the
%%         pidection, parsing JSON messages, encryption, and other
%%         common functions.
%%    
%%    In the current state, these are the three processes of this core API.
%%    In the future it is expected that this API inherits more
%%    functionality.  Each role is implemented separately, with their
%%    respective constructors in this API.  The Game Server has multiple
%%    implementations for each set of server logic used.
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe).
-author("twheys@gmail.com").
-behaviour(application).

% Application exports
-export([start/2, stop/1, startapp/0]).

% constuctors
-export([new_singleton/0,new_singleton_link/0]).
-export([new_api/0,new_api_link/0]).
-export([new_socket/1,new_socket_link/1]).

% public game server functions
-export([register_module/5,get_module/2,get_modules/1,increment_procs/0,decrement_procs/0]).

% public socket server functions
-export([send_msg/2,encrypt/2,cloud/1,shutdown/1,timeout/1]).

% public api functions
-export([greet/1,send_error_code/3,pong/1]).
-export([handle/1,handle/2]).

-define(GAME_SERVER_NAME, game_server).
-define(GAME_API_NAME, game_api).
-define(DEFAULT_CALL_TIMEOUT, 1000).


startapp() -> application:start(goethe).

start(_Type, StartArgs) -> goethe_sup:start_link(StartArgs).

stop(_State) -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  factories
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_singleton() ->
    {ok, Pid} = goethe_singleton:start(),
    global:register(?GAME_SERVER_NAME, Pid),
    {ok, Pid}.
new_singleton_link() ->
    {ok, Pid} = goethe_singleton:start_link(),
    global:register_name(?GAME_SERVER_NAME, Pid),
    {ok, Pid}.

new_api() -> goethe_api:start().
new_api_link() -> goethe_api:start_link().

new_socket(Port) -> goethe_socket:start(Port).
new_socket_link(Port) -> goethe_socket:start_link(Port).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cast(Module, Msg) ->
    logger:trace("Cast ~p : ~p", [Module, Msg]),
    global:send(Module, Msg),
    ok.
call(Module, Msg) ->
    call(Module, Msg, ?DEFAULT_CALL_TIMEOUT).
call(Module, Msg, Timeout) ->
    logger:trace("Call ~p : ~p with timeout ~p", [Module, {self(), Msg}, Timeout]),
    Pid = global:whereis_name(Module),
    Pid ! {self(), Msg},
    receive
        {Pid, Reply} -> 
            logger:trace("Receiving response ~p", [Reply]),
            Reply
    after Timeout ->
        {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public game server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_module(Identifier, Pid, Prefix, Module, Roles) ->
    global:register_name(Identifier, Pid),
    global:send(?GAME_SERVER_NAME, {add_module, {Prefix, Module, Roles}}).
get_module(Prefix, Role) -> call(?GAME_SERVER_NAME, {get_module, {Prefix, Role}}).
get_modules(Role) -> call(?GAME_SERVER_NAME, {get_modules, {Role}}).
increment_procs() -> cast(?GAME_SERVER_NAME, increment_procs).
decrement_procs() -> cast(?GAME_SERVER_NAME, decrement_procs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public socket server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Pid, Msg) -> 
    Pid ! {write, Msg},
    ok.
encrypt(Pid, partial) -> 
    Pid ! partially_encrypted,
    ok;
encrypt(Pid, full) -> 
    Pid ! fully_encrypted,
    ok.
cloud(Pid) -> 
    Pid ! cloud,
    ok.
shutdown(Pid) ->
    Pid ! shutdown,
    ok.
timeout(Pid) ->
    Pid ! timeout,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
greet(To) -> cast(?GAME_API_NAME, {greet, {To}}).
send_error_code(To, Blame, Code) -> cast(?GAME_API_NAME, {send_error_code, {To, Blame, Code}}).
pong(To) -> cast(?GAME_API_NAME, {pong, {To}}).

% Inbound
handle(Msg) -> call(?GAME_API_NAME, Msg).
% handle(ping) -> call(?GAME_API_NAME, ping);
% handle(encrypt) -> call(?GAME_API_NAME, encrypt);
% handle(invalid_action) -> call(?GAME_API_NAME, invalid_action);
% handle(_) -> {error, action_mismatch}.

handle(Msg, Params) -> call(?GAME_API_NAME, {Msg, Params}).