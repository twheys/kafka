-module(goethe).

-behaviour(application).
-export([start/2, stop/1, startapp/0]).

startapp() -> application:start(goethe).

start(_Type, StartArgs) -> goethe_sup:start_link(StartArgs).

stop(_State) -> ok.
