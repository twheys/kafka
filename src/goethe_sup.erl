-module(goethe_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
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
	    {goethe_server, {goethe_server, start_link, [?PORT, ?TCP_OPTIONS]},
	        permanent, 5000, worker, [goethe_server]},
        {game, {game, start_link, []}, 
            permanent, 5000, worker, [game]}
	]}}.
