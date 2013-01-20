-module(goethe_socket).
-author("twheys@gmail.com").

-export([start/1,start_link/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(DEFAULT_PLAIN_TIMEOUT, 100 * 10 * 1000).
-define(DEFAULT_FULLY_ENCRYPTED_TIMEOUT, 5 * 60 * 1000).

-record(state, {
sock,key,game
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
	    logger:info("Login Server successfully initialized!"),
	    Top = self(),
		spawn(fun()-> accept(Sock, Top) end),
        receive
        	closed -> {ok, closed};
        	Other -> {error, Other}
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
        goethe:greet(self()),
        plain(#state{sock=Sock});
	{error, closed} ->
	    logger:fatal("Socket closed!"),
		Top ! closed;
	{error, Reason} ->
	    logger:warn("Error accepting connection!"),
		Top ! {error, Reason}
	end.


plain(#state{sock=Sock} = State) ->
	logger:trace("Listening for read/write orders in plain mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [json, binder]) of
		{ok, Actions} -> game_util:call_api(plain, Actions);
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


fully_encrypted(#state{sock=Sock,key=_Key} = State) ->
	logger:trace("Listening for read/write orders in fully_encrypted mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [fcrypto,json,binder]) of
		{ok, Actions} -> game_util:call_api(fcrypto, Actions);
		{error, Reason} -> goethe:send_error_code(self(), client, Reason)
		end,
		fully_encrypted(State);
		
    % Send tcp/ip messages
	{write, Tuple} ->
		{ok, Outbound} = write_filter(Tuple, [json,fcrypto]),
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


cloud(#state{sock=Sock,key=_Key} = State) ->
	logger:trace("Listening for read/write orders in cloud mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
	    case read_filter(Bin, [fcrypto,json,binder]) of
		{ok, Actions} -> game_util:call_api(cloud, Actions);
		{error, Reason} -> goethe:send_error_code(self(), client, Reason)
		end,
		cloud(State);
		
    % Send tcp/ip messages
	{write, Tuple} ->
		{ok, Outbound} = write_filter(Tuple, [json,fcrypto]),
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
read_filter(Msg, [binder | Rest]) ->
	case game_util:bind_json_to_api(Msg) of
	{ok, Actions} -> read_filter(Actions, Rest);
	{error, Reason} -> {error, {binder_decode, Reason}}
	end;
read_filter(Msg, [Unknown | Rest]) ->
	logger:warn("Unknown read filter: " ++ atom_to_list(Unknown)),
	read_filter(Msg, Rest);
read_filter(Msg, []) ->
	{ok, Msg}.


common({tcp_closed, _}, _State) ->
		logger:debug("Client disconnected."),
	    goethe:decrement_procs(),
        {stop, tcp_closed};
common(shutdown, _State) -> {stop, shutdown};
common(timeout, _State) -> {stop, timeout};
common(Other, _State) -> logger:warn("~p", [{except_in_lsnr, Other}]).

timeout() -> 
	goethe:send_error_code(self(), client, timeout),
    timer:sleep(500),
	goethe:timeout(self()).