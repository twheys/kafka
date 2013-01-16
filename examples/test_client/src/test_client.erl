-module(test_client).

-export([start/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}]).
-define(ADDRESS, "localhost").
-define(PORT, 34986).

start() ->
    io:format("CLIENT connecting~n"),
    {ok, Conn} = gen_tcp:connect(?ADDRESS, ?PORT, ?TCP_OPTIONS),
    read_pipe(Conn, list_to_binary([])).

read_pipe(Conn, Ack) ->
    io:format("CLIENT reading pipe~n"),
    case gen_tcp:recv(Conn, 0) of
	{ok, Binary} when size(Ack) == 0 ->
	    io:format("Receiving data ~p~n",  Ack),
	    process_inc(Conn, Binary);
	{ok, Binary}  ->
	    process_inc(Conn, erlang:list_to_binary([Binary, Ack]));
	{error, Reason} ->
	    exit({badread, Reason})
    end.

process_inc(Conn, Binary) ->
    case read_json(Binary) of
	done ->
	    gen_tcp:close(Conn),
	    exit(normal);
	{more, Ack} -> proc_lib:hibernate(?MODULE, read_pipe, {Conn, Ack});
	continue -> proc_lib:hibernate(?MODULE, read_pipe, {Conn, list_to_binary([])});
	{error, Reason} -> exit({user_error, Reason});
	Other -> exit({bad_ret_from_fun, Other})
    end.
    
read_json(Binary) ->
    try 
        {struct, JSON} = mochijson2:decode(Binary),
        receive_cmd(JSON),
        continue
    catch 
        error:_ -> {more, Binary}
    end.
    
receive_cmd(Command) -> io:format("CLIENT RECEIVING COMMAND: ~p~n", lists:flatten(Command)).
