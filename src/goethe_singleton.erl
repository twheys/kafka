-module(goethe_singleton).
-author("twheys@gmail.com").

-export([start/0,start_link/0]).

-record(state, {
procs=0,modules=[]
}).
-record(module, {
prefix,module,roles=[]
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  constructors
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> {ok, spawn(fun() -> init() end)}.
start_link() -> {ok, spawn_link(fun() -> init() end)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init() -> 
    logger:info("Hello! Welcome to Goethe. Starting server."),
    logger:info("Initializing Core Game Server."),
    loop(#state{}).

loop(#state{modules=Modules,procs=Procs} = State) ->
    receive 
    {add_module, {Prefix, Module, Roles}} -> 
        logger:info("Loading Module ~p", [Module]),
        case add_module(Prefix, Module, Roles, State) of
            {ok, NewState} -> 
                logger:warn("Added Module [~p] with ID: ~p and Roles: ~w",
                    [Module, Prefix, Roles]),
                loop(NewState);
            {error, Reason} -> 
                logger:warn("Failed to add module. Reason: ~p", [Reason]),
                loop(State)
        end;
    {From, {get_modules, {Role}}} ->
        From ! {self(), get_modules(Role, Modules)},
        loop(State);
    {From, {get_module, {Prefix, Role}}} ->
        logger:debug("Locating Module with Prefix: ~p", [Prefix]),
        From ! {self(), get_module(Prefix, Role, Modules)},
        loop(State);
    increment_procs -> 
        NewProcs = Procs+1,
        logger:info("New connection! Currently connected: ", [NewProcs]),
        loop(State#state{procs=NewProcs});
    decrement_procs -> 
        NewProcs = Procs-1,
        logger:info("Connection ended! Currently connected: ", [NewProcs]),
        loop(State#state{procs=NewProcs});
    {From, Action} ->
        logger:warn("Received unexpected message in singeleton game loop: ~p", [Action]),
        loop(State#state{procs=Procs-1});
    Other ->
        logger:warn("Received unexpected message in singeleton game loop: ~p", [Other]),
        loop(State)
    end.

add_module(Prefix, Module, Roles, #state{modules=Modules} = State) ->
    FilteredList = lists:filter(fun(#module{prefix=X}) -> X == Prefix end, Modules),
    NewModule = #module{prefix=Prefix,module=Module,roles=Roles},
    {ok, State#state{modules=[NewModule | FilteredList]}}.

get_modules(Role, [Candidate | Rest]) ->
    case [Role] == [X || X <- Candidate#module.roles, X==Role] of
        true-> {ok, Candidate};
        false -> get_modules(Role, Rest)
    end;
get_modules(_Role, []) ->
    {error, module_not_found}.

get_module(Prefix, Role, [Candidate | Rest]) ->
    case [Role] == [X || X <- Candidate#module.roles, X==Role]
        andalso Candidate#module.prefix == Prefix of
        true-> {ok, Candidate#module.module};
        false -> get_modules(Role, Rest)
    end;
get_module(_Prefix, _Role, []) ->
    {error, module_not_found}.