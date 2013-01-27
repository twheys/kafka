-module(test_client).
-author("twheys@gmail.com").

-compile(export_all).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(ADDRESS, "localhost").
-define(PORT, 12345).

start() ->
    io:format("CLIENT connecting~n"),
    spawn_link(?MODULE, bootstrap, [?ADDRESS, ?PORT]).
start(Port) ->
    io:format("CLIENT connecting~n"),
    spawn_link(?MODULE, bootstrap, [?ADDRESS, Port]).

bootstrap(Address, Port) ->
    {ok, Sock} = gen_tcp:connect(Address, Port, ?TCP_OPTIONS),
    client(Sock).

client(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	    {tcp, Sock, Bin}  ->
            {ok, Incoming} = read_filter(Bin, [binary]),
            io:format("CLIENT RECV: ~s~n", [Incoming]),
            client(Sock);
	    {tcp_closed, Sock} -> io:format("Connection closed!~n");

        {send, Msg} -> 
            {ok, Outgoing} = write_filter(Msg, []),
            io:format("CLIENT SEND: ~s~n", [binary_to_list(Outgoing)]),
            gen_tcp:send(Sock, Outgoing),
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


write_filter(Msg, [Unknown | Rest]) ->
    logger:warn("Unknown write filter: " ++ atom_to_list(Unknown)),
    write_filter(Msg, Rest);
write_filter(Msg, []) ->
    {ok, Msg}.


read_filter(Msg, [binary | Rest]) ->
    read_filter(binary_to_list(Msg), Rest);
read_filter(Msg, [Unknown | Rest]) ->
    logger:warn("Unknown read filter: " ++ atom_to_list(Unknown)),
    read_filter(Msg, Rest);
read_filter(Msg, []) ->
    {ok, Msg}.

quick() ->
    Pid = test_client:start(),
    test_client:ping(Pid),
    timer:sleep(100),
    test_client:get_apple(Pid),
    timer:sleep(100),
    test_client:encrypt(Pid),
    timer:sleep(100),
    test_client:authenticate(Pid, <<"timmy">>, <<"p@ss">>),
    ok.

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
    Pid ! {send, <<"{\"action\":\"server.ping\"}">>}.

get_apple(Pid) ->
    Pid ! {send, <<"{\"action\":\"server.get_apple\"}">>}.

encrypt(Pid) ->
    Pid ! {send, <<"{\"action\":\"server.encrypt\"}">>}.

authenticate(Pid, UserName, Password) ->
    Pid ! {send, list_to_binary([
            <<"{\"user.authenticate\":[">>,
            <<"{\"username\":\"">>, 
            UserName, 
            <<"\"},">>,
            <<"{\"password\":\"">>,
            Password,
            <<"\"}]}">>
        ])}.

get_nodes(Pid) ->
    Pid ! {send, <<"{\"action\":\"server.get_nodes\"}">>}.

join_node(Pid, NodeName) ->
    Pid ! {send, list_to_binary([
            <<"{\"nodes.join_node\":[">>,
            <<"{\"name\":\"">>, 
            NodeName, 
            <<"\"}]}">>
        ])}.