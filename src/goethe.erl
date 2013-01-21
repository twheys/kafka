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
-author("twheys@gmail.com").
-behaviour(gen_server).

-export([start/1,start_link/1]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% public game server functions
-export([register_module/1,register_client/1,release_client/1,recv/2]).

% public socket server functions
-export([send_msg/2,encrypt/2,cloud/1,close/1,timeout/1]).
-export([get_modules/0]).

-define(GAME_SERVER_NAME, game_server).
-define(SOCKET_SERVER_NAME, socket_server).
-define(GAME_API_NAME, game_api).

-record(state, {
procs=0,modules=[],clients=[],listener
}).
-record(module, {
name,emod,actions=[]
}).
-record(action, {
name,efun,arity,roles
}).

start(Port) ->  gen_server:start({local, ?MODULE}, ?MODULE, [Port], []).
start_link(Port) ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  game server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register_module({Name, {EMod, Actions}}) ->
    gen_server:cast(?MODULE, {add_module, {Name, {EMod, Actions}}}).
register_client(Client) -> gen_server:cast(?MODULE, {register_client, {Client}}).
release_client(Client) -> gen_server:cast(?MODULE, {release_client, {Client}}).
recv(_, []) -> ok;
recv(Role, [Action | Rest]) -> 
    gen_server:call(?MODULE, {recv, {Role, Action}}),
    recv(Role, Rest).
get_modules() -> gen_server:call(?MODULE, get_modules).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public socket server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Pid, Msg) -> 
    Pid ! {write, Msg},
    ok.
encrypt(Pid, partial) -> 
    Pid ! partially_encrypted,
    ok;
encrypt(Pid, full) -> 
    Pid ! fully_encrypted,
    ok.
cloud(Pid) -> 
    Pid ! cloud,
    ok.
close(Pid) ->
    Pid ! close,
    ok.
timeout(Pid) ->
    Pid ! timeout,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Port]) -> 
    process_flag(trap_exit, true),
    Listener = goethe_socket:start_link(Port),
    {ok, #state{listener=Listener}}.


handle_call(get_modules, _From, #state{modules=Modules} = State) ->
    {reply, {ok, Modules}, State};

%handle_call({recv, {Role, {ModuleName, ActionName, []}}}, From, #state{modules=Modules} = State) ->
%    logger:debug("Goethe Recv from socket: ~p:~p", [ModuleName, ActionName]),
%    {ok, {M, F}} = lookup_module(ModuleName, ActionName, 1, Role, Modules),
%    logger:debug("Spawning worker ~p:~p(~p)", [M, F, [From]]),
%    spawn(M, F, [From]),
%    {reply, ok, State};

handle_call({recv, {Role, {ModuleName, ActionName, Params}}}, {From, _Ref}, #state{modules=Modules} = State) ->
    logger:debug("Goethe Recv from socket: ~p:~p", [ModuleName, ActionName]),
    case lookup_module(ModuleName, ActionName, length(Params)+1, Role, Modules) of
    {ok, {M, F}} ->
        logger:debug("Spawning worker ~p:~p(~p)", [M, F, [From | Params]]),
        spawn(M, F, [From | Params]),
        {reply, ok, State};
    {error, Reason} -> 
        goethe_core:send_error_code(self(), client, Reason),
        {reply, {error, Reason}, State}
    end;

handle_call(Request, _From, State) ->
    logger:info("Unexpected function call in goethe: ~p", [Request]),
    {noreply, State}.


handle_cast({add_module, {Name, {EMod, Actions}}}, #state{modules=Modules} = State) ->
    logger:info("Loading Module ~p", [Name]),
    FilteredModules = lists:filter(fun(#module{name=X}) -> X =/= Name end, Modules),
    logger:debug("Filtered list ~p", [FilteredModules]),
    case create_module(Name, EMod, Actions) of
    {ok, NewModule} -> 
        logger:info("Module [~p] successfully loaded! ~p", [Name, NewModule]),
        {noreply, State#state{modules=[NewModule | FilteredModules]}};
    Other -> 
        logger:warn("Failed to add module. Reason: ~p", [Other]),
        {noreply, {error, Other}}
    end;

handle_cast({register_client, {Client}}, #state{clients=Clients,procs=Procs} = State) ->
    NewProcs = Procs+1,
    logger:info("New connection! Currently connected: ", [NewProcs]),
    {noreply, State#state{procs=NewProcs,clients=[Client | Clients]}};

handle_cast({release_client, {Client}}, #state{clients=Clients,procs=Procs} = State) ->
    NewProcs = Procs-1,
    logger:info("Connection ended! Currently connected: ", [NewProcs]),
    FilteredClients = lists:filter(fun(X) -> X =/= Client end, Clients),
    {noreply, State#state{procs=NewProcs,clients=FilteredClients}};

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

lookup_action(_ActionName, _Arity, _Role, []) ->
    {error, action_not_found};
lookup_action(ActionName, Arity, Role, [Candidate | Rest]) ->
    logger:debug("Searching for Function ~p/~p", [ActionName, Arity]),
    logger:debug("Checking ~p", [Candidate#action.name]),
    case ActionName == Candidate#action.name
        andalso Arity == Candidate#action.arity
        andalso [Role] == [X || X <- Candidate#action.roles, X==Role] of
    true -> {ok, Candidate#action.efun};
    false -> lookup_action(ActionName, Arity, Role, Rest)
    end.
