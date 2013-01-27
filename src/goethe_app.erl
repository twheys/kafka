-module(goethe_app).

-behaviour(application).

%% Application callbacks
-export([startapp/0,stopapp/0,restartapp/0,start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

startapp() ->
    application:load(couchbeam),
    application:start(goethe).
stopapp() ->
	application:stop(goethe).
restartapp() ->
	stopapp(),
	startapp().

start(_StartType, _StartArgs) -> goethe_sup:start_link().

stop(_State) -> ok.
