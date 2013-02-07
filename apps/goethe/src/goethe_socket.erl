-module(goethe_socket).
-author('Tim Heys twheys@gmail.com').

-export([start/1,start_link/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(DEFAULT_TIMEOUT, 10 * 60 * 1000).

-record(state, {
sock,session,status,filters=[]
}).


%%==========================================================================
%
%  constructors
%
%%==========================================================================
start(Port) -> spawn(fun() -> init(Port, ?TCP_OPTIONS) end).
start_link(Port) -> spawn_link(fun() -> init(Port, ?TCP_OPTIONS) end).


%%==========================================================================
%
%  work functions
%
%%==========================================================================	
init(Port, TcpOptions) ->
    goethe:clean({"session", "all"}),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Sock} ->
		logger:info("Socket Server on port ~p successfully initialized!", [Port]),
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
    {error,eaddrinuse} ->
   		logger:info("Socket on port ~p in use.  Waiting to retry.", [Port]),
   		timer:sleep(5000),
   		init(Port, TcpOptions);
	Other ->
   		logger:info("Error starting socket server. ~p", [Other]),
	    {error, Other}
    end.


accept(LSock, Top) ->
    logger:info("Waiting for connections..."),
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
        logger:info("Received a new connection from ~p", [Sock]),
		spawn(fun()-> accept(LSock, Top) end),
		goethe:register_connection(self()),
		Session = goethe_session:new(self()),
		listen(#state{
			sock=Sock,
			session=Session,
			status=plain,
			filters=[json,pencrypt]
		});
	{error, closed} ->
	    logger:fatal("Socket closed!"),
		Top ! closed;
	{error, Reason} ->
	    logger:warn("Error accepting connection!"),
		Top ! {error, Reason}
	end.


listen(#state{sock=Sock, status=Status, filters=Filters, session=Session} = State) ->
	logger:trace("Listening for [~p] read/write orders!", [Status]),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
		{tcp, Sock, Bin} -> read(Bin, Status, Session, Filters);	
    % Send tcp/ip messages
		{write, Tuple} -> write(Tuple, Sock, Filters);
    % End connection
		{stop, Reason} ->
			goethe_core:send_logout_notification(Session),
			Session:delete(),
			exit({stop, Reason});
	
	% State Handlers
		Message -> common(Status, State, Message)
	after ?DEFAULT_TIMEOUT ->
		goethe:notify('client.timeout', {Session})
	end,
	listen(State).


%%==========================================================================
%
%  state functions
%
%%==========================================================================
plain(State, {pencrypt, {PrivKey}}) ->
	listen(State#state{
		status=pencrypt,
		filters=[json,{pencrypt, {PrivKey}}]
	});
plain(_, Other) ->
	common(Other).


pencrypt(State, {fencrypt, {Key}}) ->
	listen(State#state{
		status=fencrypt,
		filters=[json,{fencrypt, {Key}}]
	});
pencrypt(_, Other) ->
	common(Other).

fencrypt(#state{session=Session} = State, {ready, {Principle}}) ->
	NewSession = build_session(Session, Principle),
	logger:debug("Persisted Session: ~p", [NewSession]),
    goethe_core:send_login_notification(Session),
	listen(State#state{
		status=ready,
		session=NewSession
	});
fencrypt(_, Other) ->
	common(Other).


ready(_, Other) ->
	common(Other).


%%==========================================================================
%
%  util functions
%
%%==========================================================================
write(Tuple, Sock, Filters) ->
	logger:trace("Outbound Tuple: ~p", [Tuple]),
	{ok, Outbound} = write_filter(Tuple, Filters),
	logger:trace("Outbound: ~p", [Outbound]),
	gen_tcp:send(Sock, Outbound).


write_filter(Msg, [json | Rest]) ->
	case goethe_util:json_encode(Msg) of
			{ok, JSON} ->
				write_filter(JSON, Rest);
			{error, Reason} -> 
			    logger:debug("Failed to encode JSON: ~p", [Msg]),
				error(Reason)
	end;
write_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown write filter: ~p", [Unknown]),
	write_filter(Msg, Rest);
write_filter(Msg, []) ->
	{ok, Msg}.


read(Bin, Status, Session, Filters) ->
    logger:debug("Inbound [~p]: ~p", [Status, Bin]),
    case read_filter(Bin, Filters) of
	{ok, Actions} ->
    	goethe:recv(Status, Session, Actions);
	{error, Reason} -> 
    	logger:debug("Bad request from client! ~p", [Reason]),
        goethe:nack(Session, <<"unknown">>, <<"bad_request">>)
	end.

	
read_filter(Msg, []) ->
	goethe_util:bind_api(Msg);
read_filter(Msg, [json | Rest]) ->
	case goethe_util:json_decode(Msg) of
		{ok, JSON} ->
			read_filter(JSON, Rest);
		{error, _} -> {error, json_decode}
	end;
read_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown read filter: ~p", [Unknown]),
	read_filter(Msg, Rest).

common(plain, State, Message) ->
	plain(State, Message);
common(pencrypt, State, Message) ->
	pencrypt(State, Message);
common(fencrypt, State, Message) ->
	fencrypt(State, Message);
common(ready, State, Message) ->
	ready(State, Message);
common(_, _, Message) ->
	common(Message).


common({tcp_closed, _}) ->
	logger:debug("Client disconnected."),
    goethe:release_connection(self()),
    self() ! {stop, tcp_closed},
    ok;
common(close) -> 
	logger:debug("Server killed connection."),
	self() ! {stop, killed_by_server},
    ok;
common(timeout) -> 
	self() ! {stop, timeout},
    ok;
common(Other) -> 
	logger:warn("~p", [{inv_sock_action, Other}]),
	ok.

build_session(Session, Principle) ->
	{ok, UserName} = Principle:get(name),
	{ok, Email} = Principle:get(email),
	{ok, Role} = Principle:get(role),
	NSession = Session:build(UserName, Email, Role),
	NSession:save().