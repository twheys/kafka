-module(goethe_socket_impl).
-author("twheys@gmail.com").

-export([start/2,start_link/2]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(DEFAULT_PLAIN_TIMEOUT, 100 * 10 * 1000).
-define(DEFAULT_FULLY_ENCRYPTED_TIMEOUT, 5 * 60 * 1000).

-record(state, {
sock,key,game
}).

start(Port, GameCallback) -> 
    Pid = spawn(fun() -> init(Port, GameCallback, ?TCP_OPTIONS) end),
    {ok, Pid}.

start_link(Port, GameCallback) -> 
    Pid = spawn_link(fun() -> init(Port, GameCallback, ?TCP_OPTIONS) end),
    {ok, Pid}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(Port, GameCallback, TcpOptions) ->
    logger:info("Login Server initializing on port ~p", [Port]),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Sock} ->
	    logger:info("Login Server successfully initialized!"),
		spawn(fun()-> accept(Sock, GameCallback) end),
        timer:sleep(infinity);
	Other ->
	    Other
    end.


accept(Sock, GameCallback) ->
    logger:info("Waiting for connections..."),
    case gen_tcp:accept(Sock) of
		{ok, LSock} ->
	        logger:info("Received a new connection from ~p", [LSock]),
			spawn(fun()-> accept(Sock, GameCallback) end),
	        goethe:greet(GameCallback, self()),
	        plain(#state{sock=LSock,game=GameCallback});
		{error, Reason} ->
		    logger:warn("Error accepting connection! Reason: ~p", [Reason]),
			accept(Sock, GameCallback)
	end.


plain(#state{sock=Sock,game=GameCallback} = State) ->
	logger:trace("Listening for read/write orders in plain mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [json]) of
		{ok, Inbound} -> process_msg(plain, Inbound, GameCallback);
		{error, Reason} -> goethe:send_error_code(self(), client, Reason)
		end,
		plain(State);
		
    % Send tcp/ip messages
	{write, Tuple} ->
		{ok, Outbound} = write_filter(Tuple, [json]),
    	logger:trace("Outbound: ~p", [Outbound]),
		gen_tcp:send(Sock, Outbound),
		plain(State);
		
    % Change state
	fully_encrypted -> fully_encrypted(State);
	cloud -> cloud(State);
	
	% Common Handlers
	Other ->
		case common(Other, State) of
			{ok, NewState} -> plain(NewState);
			{stop, Reason} -> Reason
		end
	after ?DEFAULT_PLAIN_TIMEOUT ->
		timeout(),
		plain(Sock)
	end.


fully_encrypted(#state{sock=Sock,key=_Key,game=GameCallback} = State) ->
	logger:trace("Listening for read/write orders in fully_encrypted mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [json]) of
		{ok, Inbound} -> process_msg(fcrypto, Inbound, GameCallback);
		{error, Reason} -> goethe:send_error_code(self(), client, Reason)
		end,
		fully_encrypted(State);
		
    % Send tcp/ip messages
	{write, Tuple} ->
		{ok, Outbound} = write_filter(Tuple, [json]),
    	logger:trace("Outbound: ~p", [Outbound]),
		gen_tcp:send(Sock, Outbound),
		fully_encrypted(State);

	% Common Handlers
	Other ->
		case common(Other, State) of
			{ok, NewState} -> fully_encrypted(NewState);
			{stop, Reason} -> Reason
		end
	after ?DEFAULT_FULLY_ENCRYPTED_TIMEOUT ->
		timeout(),
		fully_encrypted(Sock)
	end.


cloud(#state{sock=Sock,key=_Key,game=GameCallback} = State) ->
	logger:trace("Listening for read/write orders in cloud mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [json]) of
		{ok, Inbound} -> process_msg(cloud, Inbound, GameCallback);
		{error, Reason} -> goethe:send_error_code(self(), client, Reason)
		end,
		cloud(State);
		
    % Send tcp/ip messages
	{write, Tuple} ->
		{ok, Outbound} = write_filter(Tuple, [json]),
    	logger:trace("Outbound: ~p", [Outbound]),
		gen_tcp:send(Sock, Outbound),
		cloud(State);

	% Common Handlers
	Other ->
		case common(Other, State) of
			{ok, NewState} -> fully_encrypted(NewState);
			{stop, Reason} -> Reason
		end
	end.

write_filter(Msg, [json | Rest]) ->
	case game_util:json_encode(Msg) of
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


read_filter(Msg, [json | Rest]) ->
	case game_util:json_decode(Msg) of
		{ok, JSON} -> read_filter(JSON, Rest);
		{error, _} -> {error, json_decode}
	end;
read_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown read filter: " ++ atom_to_list(Unknown)),
	read_filter(Msg, Rest);
read_filter(Msg, []) ->
	{ok, Msg}.


common({tcp_closed, _, closed}, _State) ->
		logger:debug("Client disconnected."),
	    goethe_server:decrement_procs(),
        {stop, tcp_closed};
common(shutdown, _State) -> {stop, shutdown};
common(timeout, _State) -> {stop, timeout};
common(Other, _State) -> logger:warn("~p", {except_in_lsnr, {cloud, Other}}).

timeout() -> 
	goethe:send_error_code(self(), client, timeout),
    timer:sleep(500),
	goethe_server:timeout(self()).

process_msg(Role, Msg, GameCallback) ->
	{{ModuleId, F}, Params} = game_util:parse_msg(Msg),
	M = goethe:get_module(GameCallback, ModuleId, Role),
	spawn(M, F, [self(), Params]).