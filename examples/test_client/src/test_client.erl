-module(test_client).

-compile(export_all).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(ADDRESS, "localhost").
-define(PORT, 34986).

start() ->
    io:format("CLIENT connecting~n"),
    spawn_link(?MODULE, bootstrap, [?ADDRESS, ?PORT]).

bootstrap(Address, Port) ->
    {ok, Sock} = gen_tcp:connect(Address, Port, ?TCP_OPTIONS),
    client(Sock).

client(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	    {tcp, Sock, Bin}  ->
            io:format("CLIENT RECV: ~s~n", [binary_to_list(Bin)]),
            client(Sock);
	    {tcp_closed, Sock} -> io:format("Connection closed!~n");

        {send, Msg} -> 
            io:format("CLIENT SEND: ~s~n", [binary_to_list(Msg)]),
            gen_tcp:send(Sock, Msg),
            client(Sock);

        disconnect -> 
            gen_tcp:close(Sock),
            ok;

        {reconnect, {Address, Port}} -> 
            gen_tcp:close(Sock),
            bootstrap(Address, Port);

        reconnect -> 
            gen_tcp:close(Sock),
            bootstrap(?ADDRESS, ?PORT);

        reload -> 
            test_client:client(Sock);


        Other -> io:format("Socket error! Reason: ~W~n", Other)
    end.


disconnect(Pid) ->
    Pid ! disconnect.

reconnect(Pid) ->
    Pid ! reconnect.

reconnect(Pid, Address, Port) ->
    Pid ! {reconnect, {Address, Port}}.

reload(Pid) ->
    Pid ! reload.

send_junk(Pid) ->
    Pid ! {send, <<"}{Q#RQ{#Q}ASCa]sc[s]fva [f#}RAFVS{}Fser]w3[rfd">>}.

ping(Pid) ->
    Pid ! {send, <<"{\"action\":\"client.ping\"}]}">>}.

get_apple(Pid) ->
    Pid ! {send, <<"{\"action\":\"client.get_apple\"}]}">>}.

encrypt(Pid) ->
    Pid ! {send, <<"{\"action\":\"client.encrypt\"}]}">>}.

authenticate(Pid, UserName, Password) ->
    Pid ! {send, list_to_binary([
            <<"{\"client.authenticate\":[">>,
            <<"{\"username\":\"">>, 
            UserName, 
            <<"\"},">>,
            <<"{\"password\":\"">>,
            Password,
            <<"\"}]}">>
        ])}.

get_rooms(Pid) ->
    Pid ! {send, <<"{\"action\":\"client.get_rooms\"}]}">>}.

get_rooms(Pid, RoomName) ->
    Pid ! {send, list_to_binary([
            <<"{\"client.join_room\":[">>,
            <<"{\"room\":\"">>, 
            RoomName, 
            <<"\"}]}">>
        ])}.