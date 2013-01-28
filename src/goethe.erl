%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Goethe Game Server
%%  
%%    This API has three processes, which are merged together to provide
%%    a core system.  The behavior here should be mimicked in each module,
%%    but in a separated manner.
%%    
%%    The three processes performed are described below.
%%       
%%       Application (Erlang Behavior)
%%         When the application is started from the command line,  this
%%         API is called from initialize the supervisor, which initializes
%%         the socket server, modules, and other components.
%%       
%%       Game Server
%%         The central component of the application.  Controls the socket
%%         server and maintains application state, as well as interaction
%%         between other nodes.
%%       
%%       Socket Server
%%         The 
%%       
%%       Game Core Module
%%         The core application is designed to handle modularization, 
%%         which it itself is also a Module.  The module provides the 
%%         key API components for the client, such as initializing the
%%         pidection, parsing JSON messages, encryption, and other
%%         common functions.
%%    
%%    In the current state, these are the three processes of this core API.
%%    In the future it is expected that this API inherits more
%%    functionality.  Each role is implemented separately, with their
%%    respective constructors in this API.  The Game Server has multiple
%%    implementations for each set of server logic used.
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe).
-author('twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% public game server functions
-export([register_module/1,register_connection/1,release_connection/1]).
-export([recv/3,notify/2,get_modules/0]).

% public db functions
-export([db_find/1,db_view/2,db_update/2,db_add/1]).

-define(GAME_SERVER_NAME, game_server).
-define(SOCKET_SERVER_NAME, socket_server).
-define(GAME_API_NAME, game_api).

-include("goethe.hrl").

-record(state, {
procs=0,listener,db,modules=[],connections=[],nodes=[]
}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  game server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_module(Module) when is_atom(Module) ->
    gen_server:cast(?MODULE, {register_module, {Module}}).
register_connection(Connection) -> gen_server:cast(?MODULE, {register_connection, {Connection}}).
release_connection(Connection) -> gen_server:cast(?MODULE, {release_connection, {Connection}}).
recv(_, _, []) -> ok;
recv(Role, Session, [{Module, Action, Data} | Rest]) -> 
    gen_server:call(?MODULE, {recv, {Role, {Module, Action, Data}, Session}}),
    recv(Role, Session, Rest).
notify(EventName, Data) -> gen_server:cast(?MODULE, {notify, {EventName, Data}}).
get_modules() -> gen_server:call(?MODULE, get_modules).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  database functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
db_find(Id) -> gen_server:call(?MODULE, {db_get, {Id}}).
db_view(Design, View) -> gen_server:call(?MODULE, {db_view, {Design, View}}).
db_update(Id, Doc) -> gen_server:call(?MODULE, {db_update, {Id, Doc}}).
db_add(Doc) -> gen_server:call(?MODULE, {db_add, {Doc}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) -> 
    process_flag(trap_exit, true),
    {ok, Port} = {ok, 12345}, %application:get_env(app_port),
    {ok, Listener} = start_socket(Port),
    %{ok, Db} = start_db(),
    {ok, #state{listener=Listener}}.
    
start_socket(Port) ->
    Listener = goethe_socket:start_link(Port),
	logger:info("Socket Server on port ~p successfully initialized!", [Port]),
    {ok, Listener}.
%start_db() ->
%    {ok, Host} = application:get_env(db_address),
%    {ok, Port} = application:get_env(db_port),
%    {ok, Prefix} = application:get_env(db_prefix),
%    {ok, {UserName, Password} = application:get_env(db_auth),
%    Auth = [{basic_auth, {UserName, Password}}],
%    S = couchbeam:server_connection(Host, Port, Prefix, Auth),
%    {ok, DbName} = application:get_env(db_name),
%    {ok, DbOptions} = application:get_env(db_options),
%    {ok, Db} = couchbeam:open_db(S, DbName, DbOptions),
%    {ok, Version} = couchbeam:server_info(S),
%	logger:info("Database connection successfully initialized! Version: ~p", [Version]),
%    {ok, Db}.


handle_call(get_modules, _From, #state{modules=Modules} = State) ->
    logger:trace("Get modules request"),
    {reply, {ok, Modules}, State};

handle_call({recv, {Role, {Module, Action, Data}, Session}}, _From, #state{modules=_Modules} = State) ->
    % TODO check if Module is loaded.
    logger:debug("Goethe Recv from socket: ~p:~p[~p]", [Module, Action, Data]),
    spawn(goethe_module, inbound, [Module, Role, {Action, Data}, Session]),
    {reply, ok, State};    

handle_call({db_get, {Id}}, _From, #state{db=Db} = State) ->
    logger:trace("DB get ID#~p", [Id]),
    {ok, Doc} = couchbeam:open_doc(Db, "test"),
    {reply, {ok, mochijson2:decode(Doc)}, State};

handle_call({db_view, {Design, View}}, _From, #state{db=Db} = State) ->
    logger:trace("DB view Design: ~p View: ~p", [Design, View]),
    {ok, ViewResults} = couchbeam_view:fetch(Db, {Design, View}, []),
    {reply, {ok, ViewResults}, State};

%handle_call({db_update, {Id, Doc}}, _From, #state{db=Db} = State) ->
%    {reply, {ok, mochijson2:decode(NewDoc)}, State};

handle_call({db_add, {Doc}}, _From, #state{db=Db} = State) ->
    logger:trace("DB add Value: ~p", [Doc]),
    {ok, Doc1} = couchbeam:save_doc(Db, Doc),
    {NewDoc} = couchbeam_doc:set_value(<<"id">>, couchbeam_doc:get_id(Doc1), Doc1),
    {reply, {ok, NewDoc}, State};

handle_call(Request, _From, State) ->
    logger:info("Unexpected function call in goethe: ~p", [Request]),
    {reply, inv_call, State}.


handle_cast({register_module, {Module}}, #state{modules=Modules} = State) ->
    error_logger:info_msg("Loading Module [~p].", [Module]),
    FilteredModules = lists:filter(fun(X) -> X =/= Module end, Modules), 
    error_logger:info_msg("Module [~p] successfully loaded!", [Module]),
    {noreply, State#state{modules=[Module | FilteredModules]}};

handle_cast({register_connection, {Connection}}, #state{connections=Connections,procs=Procs} = State) ->
    NewProcs = Procs+1,
    logger:info("Client connected! Currently connected: ", [NewProcs]),
    {noreply, State#state{procs=NewProcs,connections=[Connection | Connections]}};

handle_cast({release_connection, {Connection}}, #state{connections=Connections,procs=Procs} = State) ->
    NewProcs = Procs-1,
    logger:info("Client disconnected! Currently connected: ", [NewProcs]),
    FilteredConnections = lists:filter(fun(X) -> X =/= Connection end, Connections),
    {noreply, State#state{procs=NewProcs,connections=FilteredConnections}};
    
handle_cast({notify, {EventName, Data}}, #state{modules=Modules} = State) ->
    F = fun(Module) -> goethe_module:notify(Module, EventName, Data) end,
    spawn(lists, foreach, [F, Modules]),
    {noreply, State};

handle_cast(Request, State) ->
    logger:info("Unexpected function cast in goethe: ~p", [Request]),
    {noreply, State}.

    
handle_info(_Info, State) -> {noreply, State}.
terminate(Reason, #state{listener=Listener,db=_Db}) ->
    logger:info("Received shut down hook. Reason: ~p", [Reason]),
    logger:info("Stopping socket server."),
    Listener ! stop,
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
