-module(connection).

-export([create_connection/1]).

% Events
-export([reinitialize/1,activate/1]).


-record(data, {
sock,listener
}).

create_connection(Sock) -> spawn_link(fun()-> start(Sock) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reinitialize(Connection) -> Connection ! initial.
activate(Connection) -> Connection ! active.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Sock) ->
	goethe_server:increment_procs(),
	initial(#data{sock=Sock}).
	
close() ->
	logger:debug("Client disconnected."),
    goethe_server:decrement_procs(),
    exit(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  States
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial(#data{sock=Sock} = Data) ->
	logger:trace("Listening for read/write orders in initial mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
	    % Recv tcp/ip messages
		{tcp, Sock, Bin} -> 
		    logger:trace("Inbound: ~p", Bin),
			JSON = ejson:decode(Bin),
			game:action(unencrypted, JSON, self()),
			proc_lib:hibernate(?MODULE, initial, [Data]);
	    {tcp_closed, Sock, closed} ->
	        close();
			
	    % Send tcp/ip messages
		{write, Tuple} -> 
			Bin = ejson:encode(Tuple),
		    logger:trace("Outbound: ~p", [Bin]),
			goethe_server:push(Sock, Bin),
			proc_lib:hibernate(?MODULE, initial, [Data]);
			
	    % Change state to active
		active -> 
			active(Data);
			
	    % Unexpected logic flow
		Other -> 
			goethe_server:report_error({except_in_lsnr, {inital, Other}})
	end.

active(#data{sock=Sock} = Data) ->
	logger:trace("Listening for read/write orders in initial mode!"),
    inet:setopts(Sock, [{active, once}]),
	receive
	    % Recv tcp/ip messages
		{tcp, Sock, Bin} -> 
			JSON = ejson:decode(Bin),
			game:action(unencrypted, JSON, self()),
			proc_lib:hibernate(?MODULE, active, [Data]);
	    {tcp_closed, Sock, closed} ->
	        close();
			
	    % Send tcp/ip messages
		{write, Tuple} -> 
		    logger:trace("Outbound -- Tuple ~p", [Tuple]),
			Bin = ejson:encode(Tuple),
		    logger:trace("Outbound -- BIN ~p", [Bin]),
			goethe_server:push(Sock, Bin),
			proc_lib:hibernate(?MODULE, active, [Data]);
			
	    % Change state to initial
		initial -> 
			initial(Data);
			
	    % Unexpected logic flow
		Other -> 
			goethe_server:report_error({except_in_lsnr, {active, Other}})
	end.
