%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Goethe Game Server
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe_app).
-author("twheys@gmail.com").
-behaviour(application).

-export([startapp/0,start/2,stop/1]).

startapp() -> application:start(goethe).

start(_Type, StartArgs) -> goethe_sup:start_link(StartArgs).

stop(_State) -> goethe_sup:terminate().
