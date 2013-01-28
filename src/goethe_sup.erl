
-module(goethe_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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
    crypto:start(),
    %ibrowse:start(),
    %couchbeam:start(),
    Logger = ?CHILD(logger, worker),
    Goethe = ?CHILD(goethe, worker),
    CoreModule = ?CHILD(goethe_core, worker),
    AuthModule = ?CHILD(goethe_auth, worker),
    ChatModule = ?CHILD(goethe_chat, worker),
	{ok, {{one_for_one, 3, 10},
	[
        Logger,
        Goethe,
        CoreModule,
        AuthModule,
        ChatModule
	]}}.

