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
-export([register_module/1,register_connection/1,register_node/3,release_connection/1]).
-export([recv/3,get_modules/0,get_nodes/0]).

% public db functions
-export([db_find/1,db_view/2,db_update/2,db_add/1]).

-define(GAME_SERVER_NAME, game_server).
-define(SOCKET_SERVER_NAME, socket_server).
-define(GAME_API_NAME, game_api).

-record(state, {
procs=0,listener,db,modules=[],connections=[],nodes=[]
}).
-record(module, {
name,emod,actions=[]
}).
-record(action, {
name,efun,arity,roles
}).
-record(node, {
name,address,port,is_parent,next_parent
}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  game server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_module({Name, {EMod, Actions}}) ->
    gen_server:cast(?MODULE, {register_module, {Name, {EMod, Actions}}}).
register_node(Name, Address, Port) ->
    gen_server:call(?MODULE, {register_node, {Name, Address, Port}}).
register_connection(Connection) -> gen_server:cast(?MODULE, {register_connection, {Connection}}).
release_connection(Connection) -> gen_server:cast(?MODULE, {release_connection, {Connection}}).
recv(_, _, []) -> ok;
recv(Status, Session, [Action | Rest]) -> 
    gen_server:call(?MODULE, {recv, {Status, Session, Action}}),
    recv(Status, Session, Rest).
get_modules() -> gen_server:call(?MODULE, get_modules).
get_nodes() -> gen_server:call(?MODULE, get_nodes).


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
    {ok, Listener} = start_socket(),
    %{ok, Db} = start_db(),
    {ok, #state{listener=Listener,db={}}}.
    
start_socket() ->
    {ok, Port} = application:get_env(app_port),
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

handle_call({get_node, {Name}}, _From, #state{nodes=Nodes} = State) ->
    logger:trace("Get nodes request"),
    {reply, lookup_node(Name, Nodes), State};

handle_call(get_nodes, _From, #state{nodes=Nodes} = State) ->
    logger:trace("Get nodes request"),
    {reply, {ok, Nodes}, State};

handle_call({register_node, {Name, Address, Port}}, _From, #state{nodes=Nodes} = State) ->
    logger:info("Registering new node ~p", [Name]),
    FilteredNodes = lists:filter(fun(X) -> X#node.name =/= Name end, Name),
    NewNode = #node{name=Name,address=Address,port=Port},
    {reply, {ok, Nodes}, State#state{nodes=[NewNode | FilteredNodes]}}; 

handle_call({recv, {Status, Session, {ModuleName, ActionName, Params}}}, {From, _Ref}, #state{modules=Modules} = State) ->
    logger:debug("Goethe Recv from socket: ~p:~p", [ModuleName, ActionName]),
    case lookup_module(ModuleName, ActionName, length(Params)+1, Status, Modules) of
    {ok, {M, F}} ->
        logger:debug("Spawning worker ~p:~p(~p)", [M, F, [From | Params]]),
        spawn(M, F, [Session | Params]),
        {reply, ok, State};
    {error, Reason} -> 
        goethe_core:send_error_code(self(), client, Reason),
        {reply, {error, Reason}, State}
    end;

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
    {noreply, State}.


handle_cast({register_module, {Name, {EMod, Actions}}}, #state{modules=Modules} = State) ->
    logger:info("Loading Module ~p", [Name]),
    FilteredModules = lists:filter(fun(X) -> X#module.name =/= Name end, Modules),
    logger:debug("Filtered list ~p", [FilteredModules]),
    case create_module(Name, EMod, Actions) of
    {ok, NewModule} -> 
        logger:info("Module [~p] successfully loaded! ~p", [Name, NewModule]),
        {noreply, State#state{modules=[NewModule | FilteredModules]}};
    Other -> 
        logger:warn("Failed to add module. Reason: ~p", [Other]),
        {noreply, {error, Other}}
    end;

handle_cast({register_connection, {Connection}}, #state{connections=Connections,procs=Procs} = State) ->
    NewProcs = Procs+1,
    logger:info("New connection! Currently connected: ", [NewProcs]),
    {noreply, State#state{procs=NewProcs,connections=[Connection | Connections]}};

handle_cast({release_connection, {Client}}, #state{connections=Connections,procs=Procs} = State) ->
    NewProcs = Procs-1,
    logger:info("Connection ended! Currently connected: ", [NewProcs]),
    FilteredConnections = lists:filter(fun(X) -> X =/= Client end, Connections),
    {noreply, State#state{procs=NewProcs,connections=FilteredConnections}};

handle_cast(Request, State) ->
    logger:info("Unexpected function cast in goethe: ~p", [Request]),
    {noreply, State}.

    
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


create_module(Name, EMod, Actions) ->
    logger:debug("Constructing Module ~p", [Name]),
    create_module(Name, EMod, Actions, []).
create_module(Name, EMod, [], MappedActions) ->
    logger:debug("Completing Module ~p", [Name]),
    {ok, #module{emod=EMod,actions=MappedActions,name=Name}};
create_module(Name, EMod, [{FName, {EFun, Arity, Roles}} | Actions], MappedActions) ->
    logger:debug("Constructing Action ~p in Module ~p", [FName, Name]),
    create_module(Name, EMod, Actions, [
            #action{efun=EFun, name=FName,arity=Arity,roles=Roles} | MappedActions
    ]).

lookup_module(_ModuleName, _ActionName, _Arity, _Role, []) ->
    {error, module_not_found};
lookup_module(ModuleName, ActionName, Arity, Role, [Candidate | Rest]) ->
    logger:debug("Looking up Module ~p for ~p/~p[~p]", [ModuleName, ActionName, Arity, Role]),
    logger:debug("Checking ~p", [Candidate#module.name]),
    case ModuleName == Candidate#module.name of
    true ->
        case lookup_action(ActionName, Arity, Role, Candidate#module.actions) of
        {ok, F} -> {ok, {Candidate#module.emod, F}};
        notfound -> lookup_module(ModuleName, ActionName, Arity, Role, Rest)
        end;
    false -> lookup_module(ModuleName, ActionName, Arity, Role, Rest)
    end.

lookup_node(NodeName, []) ->
    {error, {node_not_found, {NodeName}}};
lookup_node(NodeName, [Candidate | Rest]) ->
    case NodeName == Candidate#node.name of
    true -> {ok, Candidate};
    false -> lookup_node(NodeName, Rest)
    end.

lookup_action(_ActionName, _Arity, _Role, []) ->
    {error, action_not_found};
lookup_action(ActionName, Arity, Role, [Candidate | Rest]) ->
    logger:debug("Searching for Function ~p/~p", [ActionName, Arity]),
    logger:debug("Checking ~p", [Candidate#action.name]),
    case ActionName == Candidate#action.name
        andalso Arity == Candidate#action.arity
        andalso [Role] == [X || X <- Candidate#action.roles, X==Role]
        orelse [all] == Candidate#action.roles of
    true -> {ok, Candidate#action.efun};
    false -> lookup_action(ActionName, Arity, Role, Rest)
    end.
