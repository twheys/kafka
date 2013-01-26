-module(goethe_session, [State,Listener,Properties]).
-author('twheys@gmail.com').

% public socket server functions
-export([send_msg/1,encrypt/0,encrypt/1,authenticate/1,cloud/0,close/0,timeout/0]).
-export([get/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public socket server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Msg) -> 
    Listener ! {write, Msg},
    ok.
encrypt() -> 
    Listener ! partially_encrypted,
    ok.
encrypt(Key) -> 
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
get(state) -> {ok, State};
get(Key) when is_atom(Key) ->
	case proplists:lookup(Key) of
	{Key, Value} -> {ok, Value};
	none -> {error, {inv_attr, Key}}
	end.