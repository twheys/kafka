-module(goethe_sup).
-author('Tim Heys twheys@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(LOGFILE, "logs/goethe.log").
-define(LOGLEVEL, trace).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    Logger = ?CHILD(logger, worker, []),
    Goethe = ?CHILD(goethe, worker, []),
    CouchUUIDS = ?CHILD(couchbeam_uuids, worker, []),
    {ok, Modules} = application:get_env(modules),
    {ok, Children} = include(Modules),
	{ok, {{one_for_one, 3, 10},
	[
        Logger,
        Goethe,
        CouchUUIDS
        | Children
	]}}.

include(Modules) ->
    include([], Modules).
include(Acc, []) ->
    {ok, lists:reverse(Acc)};
include(Acc, [{Module, Args} | Rest]) ->
    include([?CHILD(Module, worker, Args) | Acc], Rest);
include(Acc, [Module | Rest]) ->
    include([?CHILD(Module, worker, []) | Acc], Rest).