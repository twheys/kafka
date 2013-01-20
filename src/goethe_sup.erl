-module(goethe_sup).
-behaviour(supervisor).
-author("twheys@gmail.com").

-export([start_link/1, init/1]).

-define(PORT, 34986).
-define(LOGFILE, "logs/goethe.log").
-define(LOGLEVEL, trace).


start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
	
init(_Args) ->
    crypto:start(),
	{ok, {{one_for_one, 3, 10},
	[
        {logger, {logger, start_link, [?LOGFILE,?LOGLEVEL]}, 
            permanent, 5000, worker, [logger]},
        {game_server, {goethe, new_singleton_link, []}, 
            permanent, 5000, worker, [goethe]},
        {socket_server, {goethe, new_socket_link, [?PORT]},
            permanent, 10000, worker, [goethe]},
        {game_api, {goethe, new_api_link, []},
            permanent, 5000, worker, [goethe]}
	]}}.
