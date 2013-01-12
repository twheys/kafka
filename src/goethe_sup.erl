-module(goethe_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).


start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
	
init([]) ->
    crypto:start(),
	{ok, {{one_for_one, 3, 10},
	[
	{goethe_web, 
		{goethe_web, start_link, []}, permanent, 5000, worker, [goethe_web]},
    {goethe_messagerouter, 
    	{goethe_messagerouter, start_link, []}, permanent, 5000, worker, [goethe_messagerouter]},
    {game, 
    	{game, start_link, []}, permanent, 5000, worker, [game]}
	]}}.