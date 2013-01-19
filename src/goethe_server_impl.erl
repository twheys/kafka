-module(goethe_server_impl).

-export([start/2,start_link/2]).

-define(PLAIN_TEXT_TIMEOUT, 100 * 10 * 1000).
-define(FULLY_ENCRYPTED_TIMEOUT, 5 * 60 * 1000).

start(Port, TcpOptions) -> 
    Pid = spawn(fun() -> bootstrap(Port, TcpOptions) end),
    {ok, Pid}.

start_link(Port, TcpOptions) -> 
    Pid = spawn_link(fun() -> bootstrap(Port, TcpOptions) end),
    {ok, Pid}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
bootstrap(Port, TcpOptions) ->
    logger:info("Hello! Welcome to Goethe. Server is now plain_textizing."),
    logger:info("Bootstrapping server. Using Port ~p", [Port]),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, Sock} ->
	    logger:info("Server successfully bootstrapped!"),
		spawn(fun()-> accept(Sock) end),
        timer:sleep(infinity);
	Other ->
	    Other
    end.


accept(Sock) ->
    logger:info("Waiting for connections..."),
    case gen_tcp:accept(Sock) of
		{ok, LSock} ->
	        logger:info("Received a new connection from ~p", [LSock]),
			spawn(fun()-> accept(Sock) end),
	        game:greet(self()),
	        plain_text(LSock);
		{error, Reason} ->
		    logger:warn("Error accepting connection! Reason: ~p", [Reason]),
			accept(Sock)
	end.


plain_text(Sock) ->
	logger:trace("Listening for read/write orders in plain_text mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
		case game_util:json_decode(Bin) of
			{ok, Tuple} -> game:action(plain, Tuple);
			{error, _} -> game:send_error_code(self(), client, json_encode)
		end,
		plain_text(Sock);
    {tcp_closed, Sock, closed} ->
		logger:debug("Client disconnected."),
	    goethe_server:decrement_procs(),
        {ok, tcp_closed};
		
    % Send tcp/ip messages
	{write, Tuple} -> 
		case game_util:json_encode(Tuple) of
			{ok, Bin} -> 
			    logger:trace("Outbound: ~p", [Bin]),
				gen_tcp:send(Sock, Bin);
			{error, Reason} -> 
			    logger:trace("Failed Outbound: ~p", [Tuple]),
				error(Reason)
		end,
		plain_text(Sock);
		
    % Change state
	fully_encrypted -> 
		fully_encrypted(Sock);
	shutdown ->
        {ok, shutdown};
	timeout ->
        {ok, timeout};
		
    % Unexpected logic flow
	Other -> 
		logger:warn("~p", {except_in_lsnr, {plain_text, Other}})
	after ?PLAIN_TEXT_TIMEOUT ->
		game:send_error_code(self(), client, timeout),
        timer:sleep(500),
		goethe_server:timeout(self()),
		plain_text(Sock)
	end.


fully_encrypted(Sock) ->
	logger:trace("Listening for read/write orders in fully_encrypted mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
    % Recv tcp/ip messages
	{tcp, Sock, Bin} -> 
	    logger:trace("Inbound: ~p", [Bin]),
		case game_util:json_decode(Bin) of
			{ok, Tuple} -> game:action(crypto, Tuple);
			{error, _} -> game:send_error_code(self(), client, json_encode)
		end,
		fully_encrypted(Sock);
    {tcp_closed, Sock, closed} ->
		logger:debug("Client disconnected."),
	    goethe_server:decrement_procs(),
        {ok, tcp_closed};

    % Send tcp/ip messages
	{write, Tuple} ->
		case game_util:json_encode(Tuple) of
			{ok, Bin} -> 
			    logger:trace("Outbound: ~p", [Bin]),
				gen_tcp:send(Sock, Bin);
			{error, Reason} -> 
			    logger:trace("Failed Outbound: ~p", [Tuple]),
				error(Reason)
		end,
		fully_encrypted(Sock);

    % Change state
	shutdown ->
        {ok, shutdown};
	timeout ->
        {ok, timeout};

    % Unexpected logic flow
	Other -> 
		logger:warn("~p", {except_in_lsnr, {inital, Other}})
	after ?FULLY_ENCRYPTED_TIMEOUT ->
		game:send_error_code(self(), client, timeout),
        timer:sleep(500),
		goethe_server:timeout(self()),
		fully_encrypted(Sock)
	end.