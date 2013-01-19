-module(test_client).

-export([start/0]).
-export([client/1]).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(ADDRESS, "localhost").
-define(PORT, 34986).

start() ->
    io:format("CLIENT connecting~n"),
    {ok, Sock} = gen_tcp:connect(?ADDRESS, ?PORT, ?TCP_OPTIONS),
    gen_tcp:controlling_process(Sock, self()),
    spawn_link(?MODULE, client, [Sock]).

client(Sock) ->
	io:format("Listening for read/write orders in client mode!"),
    inet:setopts(Sock, [{active, once}]),
    receive
	    {tcp, Sock, Bin}  ->
            io:format("CLIENT receiving~n"),
            JSON = ejson:decode(Bin),
            receive_cmd(JSON),
            proc_lib:hibernate(?MODULE, client, [Sock]);
	    {tcp_closed, Sock, closed} -> io:format("Connection closed!");
        Other -> io:format("Socket error! Reason: ~p", Other)
    end.
    
receive_cmd(Command) -> io:format("CLIENT RECEIVING COMMAND: ~p~n", [Command]).
