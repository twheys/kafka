-module(goethe_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([broadcast/2, send_chat/2]).

-record(state, {
sessions,procs,lasterr
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
broadcast(Msg, Except) -> gen_server:call(?MODULE, {broadcast, {Msg, Except}}).
send_chat(Msg, To) -> gen_server:call(?MODULE, {send_chat, {Msg, To}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bootstrap(Port, TcpOptions) -> gen_server:cast(?MODULE, {bootstrap, {Port, TcpOptions}}). 
wait_for_connection(Conn) -> gen_server:cast(?MODULE, {wait_for_connection, {Conn}}).
listen(Conn) -> gen_server:cast(?MODULE, {listen, {Conn}}).
send_welcome_message(SessionID) -> gen_server:cast(?MODULE, {send_welcome_message, {SessionID}}).
%send_apple(SessionID) -> gen_server:cast(?MODULE, {send_apple, {SessionID}}).
receive_cmd(JSON) -> gen_server:cast(?MODULE, {receive_cmd, {JSON}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_unique_session_id(#state{sessions=Sessions} = State) ->
	X = game_util:generate_hash(),
	case lists:filter(fun({SessionID, _}) -> SessionID == X end, Sessions) of 
		[] -> X;
		_ -> get_unique_session_id(State)
	end.

get_connection(SessionID, #state{sessions=Sessions}) ->
	case lists:filter(fun({ID, _}) -> ID == SessionID end, Sessions) of
		[] -> {error, not_found};
		[{_,Conn}|_] -> {ok, Conn}
	end.

create_session(Conn, #state{sessions=Sessions} = State) ->
    SessionID = get_unique_session_id(State),
    {SessionID, State#state{sessions=[{SessionID, Conn} | Sessions]}}.
    
read_pipe(State, Conn, Ack) ->
	logger:trace("Buffering client input: " + Ack),
    case gen_tcp:recv(Conn, 0) of
	{ok, Binary} when size(Ack) == 0 ->
	    process_inc(State, Conn, Binary);
	{ok, Binary}  ->
	    process_inc(State, Conn, erlang:list_to_binary([Binary, Ack]));
	{error, Reason} ->
	    report_error(State, {badread, Reason})
    end.

process_inc(State, Conn, Binary) ->
    case read_json(Binary) of
	done ->
	    gen_tcp:close(Conn),
	    exit(normal);
	{more, Ack} ->
	    proc_lib:hibernate(?MODULE, read_pipe, {State, Conn, Ack});
	continue ->
	    proc_lib:hibernate(?MODULE, read_pipe, {State, Conn, list_to_binary([])});
	{error, Reason} ->
	    report_error(State, {user_error, Reason});
	Other ->
	    report_error(State, {bad_ret_from_fun, Other})
    end.

read_json(Binary) ->
    try 
        {struct, JSON} = mochijson2:decode(Binary),
        receive_cmd(JSON),
        continue
    catch 
        error:_ -> {more, Binary}
    end.
    
report_error(Reason) ->
    gen_event:notify(error_man, Reason).

report_error(#state{lasterr=LastError} = State, Reason) ->
    gen_event:notify(error_man, Reason),
    State#state{lasterr=LastError}.

push(Conn, Msg) ->
	try
		JSON = mochijson2:encode(Msg),
		gen_tcp:send(Conn, JSON)
	catch
		error:_ ->
			report_error({jsondecode, Msg}),
			exit(error_man)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{sessions=[],procs=0}}.

handle_call({send_welcome_message, {SessionID}}, _, State) ->
	Conn = get_connection(SessionID, State),
	push(Conn, [{message, "Welcome to Goethe! Please authenticate..."}, {sessionID, SessionID}]);

handle_call({broadcast, {Msg, Except}}, _, State) ->
	[send_chat(Id, Msg) || {Id, _} <- State, lists:member(Id, Except) == false],
    {noreply, State};

handle_call({send_chat, {{NickFrom, Visibility, Text}}, To}, _, State) ->
	Conn = get_connection(To, State),
	push(Conn, [
		{action, "chat_message"},
		{from, NickFrom},
		{visibility, Visibility},
		{text, Text}
		]),
    {noreply, State}.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hidden gen_server methods (Used internally)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({bootstrap, {Port, TcpOptions}}, State) ->
    logger:info("Hello! Welcome to Goethe. Server is now initializing."),
    logger:info("Bootstrapping server. Using Port " ++ integer_to_list(Port)),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Conn} ->
	    logger:info("Server successfully bootstrapped!  Now waiting for connections..."),
	    wait_for_connection(Conn),
	    {noreply, State};
	Other ->
	    Other,
	    {noreply, State}
    end;
    
handle_cast({listen, {Conn}}, #state{procs=Procs} = State) ->
	logger:debug("Listening to client..."),
    read_pipe(State, Conn, list_to_binary([])),
	logger:debug("Client disconnected."),
    {noreply, State#state{procs=Procs-1}};
    
handle_cast({wait_for_connection, {Conn}}, #state{procs=Procs} = State) ->
    logger:info("There are now " ++ integer_to_list(Procs) ++ " clients connected"),
    case gen_tcp:accept(Conn) of
	{ok, Listener} ->
        logger:info("Received a new connection from " ++ Listener),
        listen(Listener),
        logger:trace("Waiting for next connection..."),
	    wait_for_connection(Conn),
        logger:trace("Creating new session..."),
        {SessionID, NewState} = create_session(Listener, State),
        logger:info("Created a new session with Session ID " ++ SessionID),
        logger:trace("Greeting client..."),
        send_welcome_message(SessionID),
        logger:trace("Client greeted."),
        {noreply, connected, NewState#state{procs=Procs+1}};
	{error, Reason} ->
	    wait_for_connection(Conn),
	    log:error("Error in connection " ++ Reason),
	    {noreply, {error, Reason}, State}
	end;

handle_cast({receive_cmd, {Command}}, State) ->
	{noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> {normal, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
