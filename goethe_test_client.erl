-module (goethe_test_client).
-author('twheys@gmail.com').

-export ([test_connection/0, test_connection/1]).
-export ([get_messages/0, send_hello/1, send_hello_encrypted/1]).

% Internal Exports
-export ([start_client/2]).

-define(LOCALHOST, "localhost").
-define(PORTNO, 12986).
-define(KEY, nil).
-define(IV, {a, 1, 2, 3, 4, b, c, d}).

test_connection() ->
	test_connection(?PORTNO).
test_connection(PortNo) ->
    spawn(?MODULE, start_client, [self(), PortNo]).

get_messages() -> 
	receive
		Response -> io:format(Response)
	end.

send_hello(Pid) ->
	Pid ! {action, self(), [{action, <<"hello_world">>}]},
	receive
		Response -> io:format(Response)
	end.

send_hello_encrypted(Pid) ->
	Pid ! {action, self(), [{action, <<"hello_world">>}]},
	receive
		Response -> io:format(Response)
	end.

start_client(Top, PortNo) ->
	case gen_tcp:connect(?LOCALHOST, PortNo,
						[binary, {packet, 0}]) of
	{ok, LSock} ->
		gen_tcp:send(LSock, ""),
	    hloop(Top, LSock, list_to_binary([]));
	Other ->
	    Other
    end.

hloop(Top, S, Ack) ->
    case gen_tcp:recv(S, 0) of
	{ok, B} when size(Ack) == 0 ->
	    {Pid, Response} = process_data(Top, B),
	    gen_tcp:send(S, Response),
	    hloop(Pid, S, Ack);
	{ok, B}  ->
	    {Pid, Response} = process_data(Top, erlang:list_to_binary([B, Ack])),
	    gen_tcp:send(S, Response),
	    hloop(Pid, S, Ack);
	{error, Reason} ->
	    lasterror({badread, Reason})
    end.

lasterror(R) ->
    exit({lasterror, R}).

process_data(Top, Data) ->
	try mochijson2:decode(Data) of
		{struct, JSON} -> {struct, handle(Top, JSON)}
	catch
		error:_ -> read_decrypted(Top, Data)
	end.

read_decrypted(Top, Data) ->
	DecryptedData = decrypt(Data),
	try mochijson2:decode(DecryptedData) of
		{struct, JSON} -> handle_decrypted(Top, JSON)
	catch
		error:_ -> {more, Data}
	end.

% FIXME remove to library
decrypt(Data) ->
	crypto:aes_cfb_128_decrypt(?KEY, ?IV, Data).
encrypt(Data) ->
	crypto:aes_cfb_128_encrypt(?KEY, ?IV, Data).


handle(Top, Data) ->
	Top ! {response, self(), Data},
	receive
		{action, Pid, Request} -> {Pid, Request}
	end.
handle_decrypted(Top, Data) ->
	Response = handle_decrypted0(Top, Data),
	encrypt(Response).

handle_decrypted0(Top, Data) ->
	Top ! {response, self(), Data},
	receive
		{xaction, Pid, Request} -> {Pid, Request}
	end.