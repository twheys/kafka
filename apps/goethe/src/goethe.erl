%%==========================================================================
%
%  Game Server
%    The central component of the application.  Controls the socket
%    server and maintains application state, as well as interaction
%     between other nodes.
%         
%
%%==========================================================================
-module(goethe).
-author('Tim Heys twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([recv/3,register_module/1,register_connection/1,release_connection/1,get_modules/0]).
% public game server functions
-export([ack/2,ack/3,nack/3,nack/4,notify/2,notify/3,get_session_by_username/1]).

% public db functions
-export([db/0,get/3,save/1]).

-define(GAME_SERVER_NAME, game_server).
-define(SOCKET_SERVER_NAME, socket_server).
-define(GAME_API_NAME, game_api).

-record(state, {
procs=0,listener,db,modules=[],connections=[],nodes=[]
}).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%==========================================================================
%
%  game server functions
%
%%==========================================================================
register_module(Module) when is_atom(Module) ->
    gen_server:cast(?MODULE, {register_module, {Module}}).

register_connection(Connection) when is_pid(Connection) ->
    gen_server:cast(?MODULE, {register_connection, {Connection}}).
release_connection(Connection) when is_pid(Connection) ->
    gen_server:cast(?MODULE, {release_connection, {Connection}}).

recv(_, _, []) -> ok;
recv(Role, Session, [{Module, Action, Data} | Rest]) -> 
    gen_server:call(?MODULE, {recv, {Role, {Module, Action, Data}, Session}}),
    recv(Role, Session, Rest).

notify(EventName, Data) ->
    gen_server:cast(?MODULE, {notify, {EventName, Data}}).
%FIXME need a way of handling default events (events that trigger configurable actions)
notify(default, EventName, Data) ->
    gen_server:cast(?MODULE, {notify, {EventName, Data}}).

get_modules() ->
    gen_server:call(?MODULE, get_modules).

ack(Session, Action) ->
    gen_server:cast(?MODULE, {ack, {Session, Action, []}}).
ack(Session, Action, Info) ->
    gen_server:cast(?MODULE, {ack, {Session, Action, Info}}).
nack(Session, Action, Code) ->
    gen_server:cast(?MODULE, {nack, {Session, Action, Code, []}}).
nack(Session, Action, Code, Info) ->
    gen_server:cast(?MODULE, {nack, {Session, Action, Code, Info}}).

get(View, Key, As) ->
    gen_server:call(?MODULE, {get, {View, Key, As}}).
save(Doc) ->
    gen_server:call(?MODULE, {save, {Doc}}).

get_session_by_username(UserName) ->
    gen_server:call(?MODULE, {get, {{"session", "by_username"}, UserName, goethe_session}}).


%%==========================================================================
%
%  database functions
%
%%==========================================================================
db() -> gen_server:call(?MODULE, db).


%%==========================================================================
%
%  gen_server functions
%
%%==========================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, Listener} = start_socket(),
    {ok, Db} = start_db(),
    {ok, #state{listener=Listener,db=Db}}.
    
start_socket() ->
    {ok, {Port}} = application:get_env(app_port),
    logger:info("Initializing Socket Server on Port: ~p.", [Port]),
    Listener = goethe_socket:start_link(Port),
	logger:info("Socket Server on port ~p successfully initialized!", [Port]),
    {ok, Listener}.
start_db() ->
    {ok, {Host, Port, Prefix, Options}} = application:get_env(dbs),
    S = couchbeam:server_connection(Host, Port, Prefix, Options),
    {ok, {DbName, DbOptions}} = application:get_env(db),
    {ok, Db} = couchbeam:open_db(S, DbName, DbOptions),
    {ok, {[{<<"couchdb">>,<<"Welcome">>},{<<"version">>,Version}]}} = couchbeam:server_info(S),
	logger:info("Database connection successfully initialized! Version: ~p", [binary_to_list(Version)]),
    {ok, Db}.


handle_call(get_modules, _From, #state{modules=Modules} = State) ->
    logger:trace("Get modules request"),
    {reply, {ok, Modules}, State};

handle_call({recv, {Role, {Module, Action, Data}, Session}}, _From, #state{modules=_Modules} = State) ->
    % TODO check if Module is loaded.
    logger:debug("[~p]Action: ~p.~p:~p", [Role, Module, Action, Data]),
    spawn(goethe_module, inbound, [Module, Role, {Action, Data}, Session]),
    {reply, ok, State};

handle_call(db, _From, #state{db=Db} = State) ->
    {reply, {ok, Db}, State};

handle_call({get, {View, Key, As}}, _From, #state{db=Db} = State) ->
    Result = case couchbeam_view:fetch(Db, View, [{key, Key}]) of
        {ok, [{[{<<"id">>,_},
               {<<"key">>,_},
               {<<"value">>,
                Value}]}]} ->
            Entity = As:new(Value),
            {ok, Entity};
        Other ->
            logger:debug("Check get..: ~p", [Other]),
            {error, notfound}
    end,
    {reply, Result, State};

handle_call({save, {
    {[{<<"_id">>,nil} | Doc]}
        }}, _From, #state{db=Db} = State) ->
    logger:debug("Persisting new Entity ~p", [Doc]),
    Result = case couchbeam:save_doc(Db, {
            [{<<"created">>,<<"now">>} |
            [{<<"updated">>,<<"now">>} |
            Doc
        ]]}) of
        {ok, New} -> {ok, New};
        Reason -> {error, Reason}
    end,
    {reply, Result, State};

handle_call({save, {{Doc}}}, _From, #state{db=Db} = State) ->
    logger:debug("Persisting existing Entity ~p", [Doc]),
    Result = case couchbeam:save_doc(Db, {[
            {<<"updated">>,<<"now">>} |
            Doc
        ]}) of
        {ok, New} -> {ok, New};
        Reason -> {error, Reason}
    end,
    {reply, Result, State};

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
    
handle_cast({notify, {Event, Data}}, #state{modules=Modules} = State) ->
    logger:info("Event: ~p:~p", [Event, Data]),
    F = fun(Module) -> goethe_module:notify(Module, Event, Data) end,
    spawn(lists, foreach, [F, Modules]),
    {noreply, State};

handle_cast({ack, {Session, Action, Info}}, State) ->
    Session:send_msg(
        {[{<<"ack">>,
            {
                [{<<"action">>,Action} |
                Info
            ]}
        }]}
    ),
    {noreply, State};
handle_cast({nack, {Session, Action, Code, Info}}, State) ->
    Session:send_msg(
        {[{<<"nack">>,
            {
                [{<<"action">>,Action} |
                [{<<"code">>,Code} |
                Info
            ]]}
        }]}
    ),
    {noreply, State};

handle_cast(Request, State) ->
    logger:info("Unexpected function cast in goethe: ~p", [Request]),
    {noreply, State}.

    
handle_info(_Info, State) -> {noreply, State}.
terminate(Reason, #state{listener=Listener,db=_Db}) ->
    logger:info("Received shut down hook. Reason: ~p. Stopping socket server.", [Reason]),
    Listener ! stop,
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
