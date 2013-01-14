-module(goethe_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([createSession/1]).

-define(PORT, 34986).

-record(state, {
sessions,procs
}).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    bootstrap(?GAMEPORT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%reply(SessionID) -> gen_server:call(?MODULE, {reply, {SessionID}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bootstrap(Port) -> gen_server:cast(?MODULE, {bootstrap, {Port}}. 
listen_for_connections(Conn) -> gen_server:cast(?MODULE, {listen_for_connections, {Conn}}.
accept_connection(Conn) -> gen_server:cast(?MODULE, {accept_connection, {Conn}}.
read_connection(Conn) -> gen_server:cast(?MODULE, {read_connection, {Conn}}.
send_connection_id(SessionID) -> gen_server:call(?MODULE, {send_connection_id, {SessionID}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_unique_connection_id(#state{sessions=Sessions} = State) ->
	X = game_util:generate_hash(),
	case lists:filter(fun({SessionID, _}) -> SessionID == X end, Sessions) of 
		[] -> X;
		_ -> get_unique_sessions_id(State)
	end.
	
get_connection(SessionID, #state{sessions=Sessions}) when is_list(SessionID)->
	case lists:filter(fun(ID, Session) -> ID == SessionID end, Sessions) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end.
	
get_session_id(Conn, #state{sessions=Sessions}) when is_list(SessionID)->
	case lists:filter(fun(ID, Connection) -> Connection == Conn end, Sessions) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end.

register_connection(Conn, #state{sessions=Sessions} = State) ->
    NewSessionID = get_unique_connection_id(State),
    send_session_id(NewSessionID),
    State#(sessions=[{NewSessionID, Conn} | Sessions]}.
    
read_pipe(Conn, Ack) ->
    case gen_tcp:recv(S, 0) of
	{ok, Binary} when size(Ack) == 0 ->
	    process_inc(S, Binary);
	{ok, Binary}  ->
	    process_inc(S, erlang:concat_binary([B, Ack]));
	{error, Reason} ->
	    lasterror({badread, Reason})
    end.

process_inc(Conn, Binary) ->
    case read_json(Binary) of
	done ->
	    gen_tcp:close(S),
	    exit(normal);
	{more, Ack} ->
	    read_pipe(S, Fun, R, Ack);
	continue ->
	    read_pipe(S, Fun, R, list_to_binary([]));
	{error, Reason} ->
	    lasterror({user_error, Reason});
	Other ->
	    lasterror({bad_ret_from_fun, Other})
    end.

read_json(Binary) ->
    try 
        JSON -> mochijson2:decode(Binary),
        continue
    catch 
        error:_ -> {more, Binary}
    end.
    
error(Reason) ->
    gen_event:notify(error_man, Reason),
    exit(error).



init(Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{sessions=[]}}.
	
	
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(bootstrap, {Port}, _, _) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, 
			       {reuseaddr, true},
			       {active, false}]) of
	{ok, Conn} ->
	    listen_for_connections(Conn);
	Other ->
	    Other
    end;

handle_cast(listen_for_connections, {Conn}, From, #state{procs=Procs}) ->
    receive
	{P, connected} ->
	    accept_connection(Conn),
	    listen_for_connections(Conn),
	    State#state{procs=Procs+1};
	{'EXIT', P, {lasterror, Reason}} ->
	    accept_connection(Conn),
	    listen_for_connections(Conn),
	    State#state{procs=Procs+1};
	{'EXIT', _, {lasterror, Reason}} ->
	    gen_event:notify(error_man, Reason),
	    listen_for_connections(Conn),
	    State#state{procs=Procs-1};
	{'EXIT', _, _} ->
	    listen_for_connections(Conn),
	    State#state{procs=Procs-1};
	{From, status} ->
	    From ! {self(), {ok, [{procs, Procs}, {lasterr, LastError}]}},
	    listen_for_connections(Conn);
	{_From, stop} ->
	    gen_event:stop(stop) %% propagate EXIT to all Pids
    end;
    
handle_cast(accept_connection, {Conn}, From, State) ->
    case gen_tcp:accept(Conn) of
	{ok, Socket} ->
	    From ! {self(), connected},
        read_socket(Conn),
        register_connection(Conn, State);
	{error, Reason} ->
	    From ! {'EXIT', self(), Reason}
	    State
	end;

handle_cast(read_connection, {Conn}, _, _) ->
    read_pipe(Conn, list_to_binary([]));
    
handle_call(send_session_id, {}, _, State) ->
    reply(Conn, <<"SessionID">>);
