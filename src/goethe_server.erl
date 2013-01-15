-module(goethe_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([broadcast/2, send_chat/2]).

-define(PORT, 34986).

-record(state, {
sessions,procs,lasterr
}).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    bootstrap(?PORT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
broadcast(Msg, Except) -> gen_server:cast(?MODULE, {broadcast, {Msg, Except}}).
send_chat(Msg, To) -> gen_server:cast(?MODULE, {send_chat, {Msg, To}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bootstrap(Port) -> gen_server:cast(?MODULE, {bootstrap, {Port}}). 
listen_for_connections(Conn) -> gen_server:call(?MODULE, {listen_for_connections, {Conn}}).
accept_connection(Conn) -> gen_server:call(?MODULE, {accept_connection, {Conn}}).
read_connection(Conn) -> gen_server:call(?MODULE, {read_connection, {Conn}}).
send_welcome_message(SessionID) -> gen_server:cast(?MODULE, {send_welcome_message, {SessionID}}).
send_apple(SessionID) -> gen_server:cast(?MODULE, {send_apple, {SessionID}}).
handle_request(JSON) -> gen_server:cast(?MODULE, {handle_request, {JSON}}).


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
        handle_request(JSON),
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


init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{sessions=[]}}.
	
	
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({bootstrap, {Port}}, _State) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, 
			       {reuseaddr, true},
			       {active, false}]) of
	{ok, Conn} ->
	    listen_for_connections(Conn),
	    ok;
	Other ->
	    Other,
	    no_reply
    end;

handle_cast({send_welcome_message, {SessionID}}, State) ->
	Conn = get_connection(SessionID, State),
	push(Conn, [{message, <<"Welcome to Goethe! Please authenticate...">>}, {sessionID, SessionID}]);

handle_cast({broadcast, {Msg, Except}}, State) ->
	[send_chat(Id, Msg) || {Id, _} <- State, lists:member(Id, Except) == false],
    {noreply, State};

handle_cast({send_chat, {{NickFrom, Visibility, Text}}, To}, State) ->
	Conn = get_connection(To, State),
	push(Conn, [
		{action, <<"chat_message">>},
		{from, NickFrom},
		{visibility, Visibility},
		{text, Text}
		]),
    {noreply, State};

handle_cast({send_handle_request, {
	[{action, <<"get_salt">>},
	{sessionID, SessionID}
	]}}, _) ->
	send_apple(SessionID).

handle_call({listen_for_connections, Conn}, From, #state{procs=Procs,lasterr=LastError} = State) ->
    receive
	{_, connected} ->
	    accept_connection(Conn),
	    proc_lib:hibernate(?MODULE, listen_for_connections, {Conn}),
	    {ok, State#state{procs=Procs+1}};
	{'EXIT', _, {error, Reason}} ->
		report_error(State, Reason),
	    accept_connection(Conn),
	    proc_lib:hibernate(?MODULE, listen_for_connections, {Conn}),
	    {ok, State#state{procs=Procs+1}};
	{'EXIT', _, _} ->
	    proc_lib:hibernate(?MODULE, listen_for_connections, {Conn}),
	    {no_reply, State#state{procs=Procs-1}};
	{From, status} ->
	    From ! {self(), {ok, [{procs, Procs}, {lasterr, LastError}]}},
	    proc_lib:hibernate(?MODULE, listen_for_connections, {Conn}),
	    normal;
	{_From, stop} ->
	    gen_event:stop(stop), %% propagate EXIT to all Pids
	    normal
    end;
    
handle_call({accept_connection, Conn}, From, State) ->
    case gen_tcp:accept(Conn) of
	{ok, Socket} ->
	    From ! {self(), connected},
        read_connection(Socket),
        {SessionID, NewState} = create_session(Socket, State),
        send_welcome_message(SessionID),
        {ok, NewState};
	{error, Reason} ->
	    From ! {'EXIT', self(), Reason},
	    {no_reply, State}
	end;

handle_call({read_connection, Conn}, _, State) ->
    read_pipe(State, Conn, list_to_binary([])).



handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.