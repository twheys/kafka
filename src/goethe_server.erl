-module(goethe_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([push/2,increment_procs/0,decrement_procs/0,report_error/1,report_error/2,get_procs/0]).

% Exported for hibernate
-export([accept_loop/1]).

-record(state, {
connections=[],procs,lasterr,top
}).

start_link(Port, TcpOptions) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    bootstrap(Port, TcpOptions),
    {ok, self()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
push(Sock, Msg) when is_binary(Msg) -> gen_server:call(?MODULE, {push, {Sock, Msg}}).
increment_procs() -> gen_server:call(?MODULE, {increment_procs, {}}).
decrement_procs() -> gen_server:call(?MODULE, {decrement_procs, {}}).
report_error(Reason) -> gen_server:call(?MODULE, {report_error, {Reason}}).
report_error(Reason, Params) -> gen_server:call(?MODULE, {report_error, {Reason, Params}}).
get_procs() -> gen_server:call(?MODULE, {get_procs, {}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bootstrap(Port, TcpOptions) -> gen_server:cast(?MODULE, {bootstrap, {Port, TcpOptions}}).
add_connection(Connection) -> gen_server:call(?MODULE, {add_connection, {Connection}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
accept_loop(Sock) ->
    logger:info("Waiting for connections..."),
    case gen_tcp:accept(Sock) of
	{ok, LSock} ->
        logger:info("Received a new connection from ~p", [LSock]),
        Connection = connection:create_connection(LSock),
        add_connection(Connection),
        gen_tcp:controlling_process(Sock, Connection),
        greet(Connection),
	    proc_lib:hibernate(?MODULE, accept_loop, [Sock]);
	{error, Reason} ->
	    report_error("ERROR!!!!!! accepting connection! Reason: ~p", [Reason]),
	    proc_lib:hibernate(?MODULE, accept_loop, [Sock])
	end,
	{stop, shutdown}.
	
greet(Connection) ->
	Connection ! {write, 
	    {[{<<"action">>,
	        [
	        {[{<<"name">>,<<"server.welcome">>}]},
            {[{<<"note">>,<<"Welcome to Goethe! Please authenticate...">>}]}
            ]
        }]}
    }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{connections=[],procs=0}}.

	
handle_call({increment_procs, {}}, _, #state{procs=Procs} = State) ->
    NewProcs = Procs+1,
    logger:debug("There are now ~p clients connected.", [NewProcs]),
    {reply, ok, State#state{procs=NewProcs}};
	
handle_call({decrement_procs, {}}, _, #state{procs=Procs} = State) ->
    NewProcs = Procs-1,
    logger:debug("There are now ~p clients connected.", [NewProcs]),
    {reply, ok, State#state{procs=NewProcs}};
	
handle_call({add_connection, {Connection}}, _, #state{connections=Connections} = State) ->
    {reply, ok, State#state{connections=[Connection | Connections]}};  
      
handle_call({report_error, Reason}, _, #state{lasterr=LastError} = State) ->
    logger:fatal("~p", Reason),
    {reply, ok, State#state{lasterr=LastError}}; 
      
handle_call({report_error, {Reason, Params}}, _, #state{lasterr=LastError} = State) ->
    logger:fatal("~p", Reason, Params),
    {reply, ok, State#state{lasterr=LastError}};

handle_call({get_procs}, _, #state{procs=Procs} = State) ->
    logger:info("Number of connections: ~p", [Procs]),
    {reply, Procs, State};

handle_call({push, {Sock, Msg}}, _, State) ->
	case gen_tcp:send(Sock, Msg) of
		ok -> {reply, ok, State};
	    {error, closed} ->  
	        logger:warn("Failed to send a message because the connection was closed on the client side!"),
	        {reply, error, State};
	    {error, Reason} ->
	        logger:fatal("Error accepting connection! Reason: ~p", [Reason]),
	        {reply, error, State}
	end;

handle_call(_Call, _From, State) -> {noreply, State}.


handle_cast({bootstrap, {Port, TcpOptions}}, State) ->
    logger:info("Hello! Welcome to Goethe. Server is now initializing."),
    logger:info("Bootstrapping server. Using Port ~p", [Port]),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Sock} ->
	    logger:info("Server successfully bootstrapped!"),
	    ConnectionListener = spawn_link(fun()-> accept_loop(Sock) end),
	    {noreply, State#state{top=ConnectionListener}};
	Other ->
	    Other,
	    {noreply, State}
    end;

handle_cast(_Cast, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, State) -> {normal, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.
