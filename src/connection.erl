-module(connection).

-export([create_connection/1]).

% Events
-export([reinitialize/1,activate/1]).

% Internal Exports
-export([start/1,listen/2]).

-record(data, {
sock,listener
}).

create_connection(Sock) ->
	Connection = spawn_link(?MODULE, start, [Sock]),
	spawn_link(?MODULE, listen, [Connection, Sock]),
	Connection.

start(Sock) ->
	initial(#data{sock=Sock}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reinitialize(Connection) -> Connection ! initial.
activate(Connection) -> Connection ! active.

%send_welcome_message(Connection)-> Connection ! {send_welcome_message}.

%send_chat(Connection, {From, Visibility, Text}) -> Connection ! {send_chat, {From, Visibility, Text}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen(Connection, Sock) ->
	goethe_server:increment_procs(),
	logger:debug("Listening to client..."),
    read_pipe(Connection, Sock),
	logger:debug("Client disconnected."),
	goethe_server:decrement_procs().
    
read_pipe(Connection, Sock) ->
    case gen_tcp:recv(Sock, 0) of
	    {ok, Binary}  -> Connection ! {read, Binary};
	    {error, Reason} -> logger:fatal("Bad read? ")
    end,
    proc_lib:hibernate(?MODULE, read_pipe, [Connection, Sock]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  States
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial(#data{sock=Sock} = Data) ->
	logger:trace("Greeting new connection..."),
	self() ! {write, [
	    {message, "Welcome to Goethe! Please authenticate..."}
	]},
	receive
		{read, Raw} -> 
			String = binary_to_list(Raw),
			{struct, JSON} = mochijson2:decode(String),
			game:action(JSON, self());
		{write, JSON} -> 
			String = mochijson2:encode({struct, JSON}),
			Raw = list_to_binary(String),
			goethe_server:push(Sock, Raw);
		active -> 
			active(Data);
		Other -> 
			goethe_server:report_error({unexpected_event, {active, Other}})
	end,
	proc_lib:hibernate(?MODULE, initial, [Data]).

active(#data{sock=Sock} = Data) ->
	receive
		{read, Raw} -> 
			String = binary_to_list(Raw),
			{struct, JSON} = mochijson2:decode(String),
			game:action(JSON, self());
		{write, JSON} -> 
			String = mochijson2:encode({struct, JSON}),
			Raw = list_to_binary(String),
			goethe_server:push(Sock, Raw);
		initial -> 
			initial(Data);
		Other -> 
			goethe_server:report_error({unexpected_event, {active, Other}})
	end,
	proc_lib:hibernate(?MODULE, active, [Data]).



%active({send_chat, [From, Visibility, Text]}, #data{listener=Conn} = Data) ->
%	goethe_server:push(Conn, [
%		{action, "chat_message"},
%		{from, From},
%		{visibility, Visibility},
%		{text, Text}
%	]),
 %   {next_state, active, Data, hibernate};
%active(Event, Data) ->
%	goethe_server:report_error({unexpected_event, {active, Event}}),
%	{next_state, active, Data, hibernate}.
%
%
%handle_event({send_welcome_message, []}, _StateName, #data{listener=Conn} = Data) ->
 %   ,
 %   {next_state, initial, Data, hibernate}.