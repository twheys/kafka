-module(goethe_session, [Listener,Principle]).
-author('twheys@gmail.com').

% public socket server functions
-export([send_msg/1,pencrypt/1,fencrypt/1,authenticate/1,cloud/0,close/0,timeout/0]).
-export([new/1,get/1,set/2]).


new(NewListener) -> {goethe_session,NewListener,{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public socket server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Msg) ->
    Listener ! {write, Msg},
    ok.
pencrypt(PrivKey) -> 
    Listener ! {partially_encrypted, {PrivKey}},
    ok.
fencrypt(Key) -> 
    Listener ! {fully_encrypted, {Key}},
    ok.
authenticate(NewPrinciple) -> 
    Listener ! {auth, {NewPrinciple}},
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
get(principle) -> {ok, Principle}.

set(principle, NewPrinciple) ->
    {goethe_session,Listener,NewPrinciple}.
