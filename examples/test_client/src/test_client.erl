-module(test_client).
-author('Tim Heys twheys@gmail.com').

-compile(export_all).

-define(TCP_OPTIONS, [binary, {active, false}]).
-define(ADDRESS, "localhost").
-define(PORT, 31863).

start() ->
    io:format("CLIENT connecting~n"),
    spawn_link(?MODULE, bootstrap, [?ADDRESS, ?PORT]).
start(Port) ->
    io:format("CLIENT connecting~n"),
    spawn_link(?MODULE, bootstrap, [?ADDRESS, Port]).

bootstrap(Address, Port) ->
    case gen_tcp:connect(Address, Port, ?TCP_OPTIONS) of 
        {ok, Sock} -> client(Sock);
        {error,econnrefused} ->
            timer:sleep(5000),
            bootstrap(Address, Port)
    end.

client(Sock) ->
    inet:setopts(Sock, [{active, once}]),
    receive
	    {tcp, Sock, Bin}  ->
            {ok, Incoming} = read_filter(Bin, [binary]),
            io:format("~pCLIENT RECV: ~s~n", [self(),Incoming]),
            client(Sock);
	    {tcp_closed, Sock} -> io:format("Connection closed!~n");

        {send, Msg} -> 
            {ok, Outgoing} = write_filter(Msg, []),
            io:format("~pCLIENT SEND: ~s~n", [self(),Outgoing]),
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
    quick(<<"timmy">>, <<"test">>).

quick(UserName, Password) ->
    Pid = test_client:start(),
    test_client:send_junk(Pid),
    timer:sleep(100),
    test_client:ping(Pid),
    timer:sleep(100),
    test_client:greet(Pid),
    timer:sleep(100),
    test_client:encrypt(Pid, <<"1234567890">>),
    timer:sleep(100),
    test_client:login(Pid, UserName, Password),
    Pid.

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

greet(Pid) ->
    Pid ! {send, <<"{\"action\":\"server.hello\"}">>}.

encrypt(Pid, Key) ->
    Pid ! {send, list_to_binary([
            <<"{\"server.encrypt\":{">>,
            <<"\"key\":\"">>, 
            Key,
            <<"\"}}">>
        ])}.

register(Pid, Email, UserName, Password) ->
    Pid ! {send, list_to_binary([
            <<"{\"auth.register\":{">>,
            <<"\"email\":\"">>, 
            Email, 
            <<"\",">>,
            <<"\"username\":\"">>, 
            UserName, 
            <<"\",">>,
            <<"\"password\":\"">>,
            Password,
            <<"\"}}">>
        ])}.

login(Pid, UserName, Password) ->
    Pid ! {send, list_to_binary([
            <<"{\"auth.login\":{">>,
            <<"\"username\":\"">>, 
            UserName, 
            <<"\",">>,
            <<"\"password\":\"">>,
            Password,
            <<"\"}}">>
        ])}.

send_junk_chat(Pid) ->
    Pid ! {send, list_to_binary([
            <<"{\"chat.segfwger\":{">>,
            <<"\"dvrv\":\"jkhscdnnkzjhmd\"}}">>
        ])}.

chat(Pid, Msg) ->
    Pid ! {send, list_to_binary([
            <<"{\"chat.chat\":{">>,
            <<"\"msg\":\"">>, 
            Msg,
            <<"\"}}">>
        ])}.

pchat(Pid, UserName, Msg) ->
    chat(Pid, "/w " ++ UserName ++ " " ++ Msg).

get_rooms(Pid) ->
    Pid ! {send, <<"{\"action\":\"rooms.list\"}">>}.

join_room(Pid, Room) ->
    Pid ! {send, list_to_binary([
            <<"{\"rooms.join\":{">>,
            <<"\"name\":\"">>, 
            Room,
            <<"\"}}">>
        ])}.