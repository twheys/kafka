-module(goethe_session, [Listener,Properties]).
-author('twheys@gmail.com').

% public socket server functions
-export([send_msg/1,pencrypt/1,fencrypt/1,authenticate/1,cloud/0,close/0,timeout/0]).
-export([new/1,get/1,set/2]).

-record(prop, {
principle
}).

new(Listener) -> {goethe_session,Listener,#prop{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public socket server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Msg) ->
    logger:debug("~p ! ~p", [Listener, Msg]),
    Listener ! {write, Msg},
    ok.
pencrypt(PrivKey) -> 
    Listener ! {partially_encrypted, {PrivKey}},
    ok.
fencrypt(Key) -> 
    Listener ! {fully_encrypted, {Key}},
    ok.
authenticate(Principle) -> 
    Listener ! {auth, {Principle}},
    ok.
cloud() -> 
    Listener ! cloud,
    ok.
close() ->
    Listener ! close,
    ok.
timeout() ->
	Listener ! timeout,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public accessor functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(principle) ->
	#prop{principle=Value} = Properties,
    {ok, Value}.

set(principle, Value) ->
    Properties#prop{principle=Value},
    ok.
