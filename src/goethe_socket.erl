-module(goethe_socket).
-author('twheys@gmail.com').

-export([start/1,start_link/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(DEFAULT_TIMEOUT, 10 * 60 * 10000).

-record(state, {
sock,status,filters=[],session
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  constructors
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Port) -> {ok, spawn(fun() -> init(Port, ?TCP_OPTIONS) end)}.
start_link(Port) -> {ok, spawn_link(fun() -> init(Port, ?TCP_OPTIONS) end)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(Port, TcpOptions) ->
    logger:info("Initializing Socket Server on Port: ~p.", [Port]),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Sock} ->
	    Top = self(),
		spawn(fun()-> accept(Sock, Top) end),
        receive
        	stop -> 
        		gen_tcp:close(Sock),
        		{ok, closed};
        	closed -> 
        		{ok, closed};
        	{error, Reason} -> 
        		gen_tcp:close(Sock),
        		{error, Reason}
        end;
	Other ->
	    {error, Other}
    end.


accept(LSock, Top) ->
    logger:info("Waiting for connections..."),
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
        logger:info("Received a new connection from ~p", [Sock]),
		spawn(fun()-> accept(LSock, Top) end),
		goethe:register_client(self()),
		Session = goethe_session:new(self(), []),
        plain(Sock, Session);
	{error, closed} ->
	    logger:fatal("Socket closed!"),
		Top ! closed;
	{error, Reason} ->
	    logger:warn("Error accepting connection!"),
		Top ! {error, Reason}
	end.


listen(#state{sock=Sock, status=Status, session=Session} = State) ->
	logger:trace("Listening for read/write orders!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
		{tcp, Sock, Bin} -> read({Status, {Sock, Session}}, Bin, [json]);	
    % Send tcp/ip messages
		{write, Tuple} -> write({Status, {Sock, Session}}, Tuple, [json]);
    % End connection
		{stop, Reason} -> Reason;
	
	% State Handlers
		Message -> common(Status, State, Message),
			listen(State)
	after ?DEFAULT_TIMEOUT ->
		timeout(Session),
		listen(State)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  state functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plain(State, partially_encrypted) -> 
	listen(State#state{
		status=partially_encrypted,
		filters=[json,pcrypto]
	});
% REMOVEME Stubbed in to skip encrytion
plain(State, {auth, {Principle}}) -> 
	listen(State#state{
		status=authenticated,
		session=goethe_session:new(
		self(), 
		[
			{principle, Principle}
		])
	});
plain(State, Other) ->
	common(Other),
	listen(State).


partially_encrypted(State, {fully_encrypted, {Key}}) -> 
	listen(State#state{
		status=fully_encrypted,
		filters=[json,{fcrypto, {Key}}]
	});
partially_encrypted(State, Other) ->
	common(Other),
	listen(State).


fully_encrypted(State, {auth, {Principle}}) -> 
	listen(State#state{
		status=authenticated,
		session=goethe_session:new(
		self(), 
		[
			{principle, Principle}
		])
	});
fully_encrypted(State, Other) ->
	common(Other),
	listen(State).


authenticated(State, admin) ->
	listen(State#state{
		status=admin
	});

authenticated(State, Other) ->
	common(Other),
	listen(State).


admin(State, Other) ->
	common(Other),
	listen(State).


cloud(State, Other) ->
	common(Other),
	listen(State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  util functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write({FLoop, {Sock, Session}}, Tuple, Filters) ->
	{ok, Outbound} = write_filter(Tuple, Filters),
	logger:trace("Outbound: ~p", [Outbound]),
	gen_tcp:send(Sock, Outbound),
	FLoop(Sock, Session).


write_filter(Msg, [json | Rest]) ->
	case goethe_util:json_encode(Msg) of
			{ok, JSON} ->
				write_filter(JSON, Rest);
			{error, Reason} -> 
			    logger:debug("Failed Outbound: ~p", [Msg]),
				error(Reason)
	end;
write_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown write filter: " ++ atom_to_list(Unknown)),
	write_filter(Msg, Rest);
write_filter(Msg, []) ->
	{ok, Msg}.


read(#state{status=Status,session=Session} = State, Bin, Filters) ->
    logger:debug("Inbound: ~p", [Bin]),
    case read_filter(Bin, Filters) of
	{ok, Actions} ->
    	logger:debug("Actions: ~p", [Actions]),
    	goethe:recv(Status, Session, Actions),
    	logger:debug("Send to game engine, back to listening!");
	{error, Reason} -> goethe_core:send_error_code(self(), client, Reason)
	end,
	listen(State).

	
read_filter(Msg, []) ->
	goethe_util:bind_api(Msg);
read_filter(Msg, [json | Rest]) ->
	case goethe_util:json_decode(Msg) of
		{ok, JSON} -> read_filter(JSON, Rest);
		{error, _} -> {error, json_decode}
	end;
read_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown read filter: ~p", [Unknown]),
	read_filter(Msg, Rest).

common(plain, State, Message) ->
	plain(State, Message);
common(partially_encrypted, State, Message) ->
	partially_encrypted(State, Message);
common(fully_encrypted, State, Message) ->
	fully_encrypted(State, Message);
common(authenticated, State, Message) ->
	authenticated(State, Message);
common(admin, State, Message) ->
	admin(State, Message);
common(cloud, State, Message) ->
	cloud(State, Message);
common(_, State, Message) ->
	common(Message),
	listen(State).


common({tcp_closed, _}) ->
		logger:debug("Client disconnected."),
	    goethe:release_client(self()),
        self() ! {stop, tcp_closed},
        ok;
common(close) -> 
		logger:debug("Server killed connection."),
		self() ! {stop, killed_by_server},
        ok;
common(timeout) -> 
		self() ! {stop, timeout},
        ok;
common(Other) -> logger:warn("~p", [{inv_sock_action, Other}]).


timeout(Session) -> 
    spawn_link(fun() ->
	    goethe_core:send_error_code(Session, client, timeout),
    	goethe:timeout(Session)
    end),
    ok.