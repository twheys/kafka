-module (goethe).
-author('twheys@gmail.com').

-export([start_link/0, start_link/1]).
% Actions
-export([hello_world/1, hello_world_encrypted/1]).
% Internal exports
-export([listen/1]).

-import(klib, [with_socket/2]).
-import(mochijson2, [encode/1, decode/1]).

-define(PORTNO, 12986).
-define(PRIVATE_KEY, "MIIEowIBAAKCAQEApvbAaZVuZ6taCRjiVtKtX3ddXSUxyvzEXMYL2B4S02FlJ8Aw4RNOia9XA5xDgKlKcodDCJP7cevRdR+X40WWIvpdn3ceg4Dxc8AORry7HAtYY+tes3yNGv9PtMyZGQI7t6RTIKmZD1SSpwHQauo5XFfBi8nOJsJ5Krzd7QD9A1WYM78HbsG5YiZUplKDq2rSkIlpYR+6BGl8N1FWBxhyIF+YkP/2tZFPpzkI62noz/yjIFkteREI+40ujrqLO5bWXXaeDmntLqCbSsqtOz4WN1ClimDUpoAkbnnZsEdvuLId+2C9EAhuRPDVf9QTgNNvQsYb1NY0O0Id+FBB1atebwIDAQABAoIBAEbBWOclsWTzg7NJPVnn7BsB3aoEgiZwmdJrB0ft7rJvmsuOseggI29LxAc+qIiUtXz1f13aQHuVoO0Ol3Jz4d0u1pAmjeLSuQABistFXdHTqQCnFTng9XdUbPXEorirbp0zYRr9ZGTPxVpK9YTpf6d3dQkFW3yne2u2ICI5Af/wDdaXA9CsibFaNqV9oFgViw4QXWmFQUtPiNv962mispLcNnkQzp8ipSqTtNJ3u2OeqqCuSEmY+u1Zh074f/vl/efJRJ8l2mDH09WXCphUjOUKWZniiIdSwdJTZoR5ydAyxlDcLcEgMq7/19G6hMJLSfMWZdcZrTmVl3lKIaiyF7ECgYEAzx7g/opd9gxmkNQWoz0LydFZbi+GdNr+NbrikZ/14VS4UAD8lq8wk5eVIamXa7ySwnyjB1+9yWZ4YPujAJTzmwAwH5KqIBnKTM3SaZXiHflezDwFyIZ0xRx9prk5aq52wk8pUeED7rMHbMzOp0icB5aDa+aMqm2TVFK6LMKootcCgYEAzl3RD9X6t06Y24W5tRx0oTM5RwJqvdLw9NdPlu/Ghq2+jgYRxp3+11DBI0omWfsyzzJWBzPjRwupStGK+U3rKJS6DRpvTux2U9JAhOmH+0mcp210XUAedevHTQGBMzFKYyITwmgoZw7/riq+ywiNRhSeaX6hP/V+xdKXFAshxikCgYA2aKYyN6b3O8q1wQExYJf4LEaR62omyGB/PNjH6H1u9BJ28ctoyhy4qNlEPojIhIEJFjS9eykf/wsgbvoi7P4RebsqkByg0iWSh37+vaDovtloTVw3kRqbrSBU69Npcus/oAv6tmGSEvWgXyzqROi4FZHwhl/tZVuOiyzdFUW3gQKBgQCovAj+28VeMIPHqrXHonayf8GrkAxbHakraQmIrpEy+ck/MzNBpVoED+F7y1tEY6KNy+iPhxuksaJArMjnQ5Ct472kjSLJ84pkyxs6EgC/tNbEBzkILi852Aljq4FKrFjYabqiUKwIaDki3YyXUmzBCobkV0fdlguMEL67").
-define(PUBLIC_KEY, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCm9sBplW5nq1oJGOJW0q1fd11dJTHK/MRcxgvYHhLTYWUnwDDhE06Jr1cDnEOAqUpyh0MIk/tx69F1H5fjRZYi+l2fdx6DgPFzwA5GvLscC1hj616zfI0a/0+0zJkZAju3pFMgqZkPVJKnAdBq6jlcV8GLyc4mwnkqvN3tAP0DVZgzvwduwbliJlSmUoOratKQiWlhH7oEaXw3UVYHGHIgX5iQ//a1kU+nOQjraejP/KMgWS15EQj7jS6Ouos7ltZddp4Oae0uoJtKyq07PhY3UKWKYNSmgCRuedmwR2+4sh37YL0QCG5E8NV/1BOA029CxhvU1jQ7Qh34UEHVq15v timo@timo-laptop").
-define(IV, {a, 1, 2, 3, 4, b, c, d}).

% Bootstrap the service
start_link() ->
	start_link(?PORTNO).
start_link(Port) ->
	klib:with_socket(Port, listen).

% Server logic to handle an accepted connection
listen(Incoming) ->
	io:format(Incoming),
	case read(Incoming) of
	ok -> get_messages();
	Result -> Result
	end.

get_messages() ->
	receive
	{send_message, Outgoing} -> 
		{continue, Outgoing}
	end.

read(Data) ->
	try mochijson2:decode(Data) of
		{struct, JSON} -> handle(JSON)
	catch
		error:_ -> read_decrypted(Data)
	end.
read_decrypted(Data) ->
	DecryptedData = decrypt(Data),
	try mochijson2:decode(DecryptedData) of
		{struct, JSON} -> handle_decrypted(JSON)
	catch
		error:_ -> {more, Data}
	end.

write(Data) ->
	Json = mochijson2:encode(Data),
	self() ! {send_message, Json}.
write_encrypted(Data) ->
	Json =  mochijson2:encode(Data),
	Cipher = encrypt(Json),
	self() ! {send_message, Cipher}.

% FIXME remove to library
decrypt(Data) ->
	crypto:aes_cfb_128_decrypt(?PRIVATE_KEY, ?IV, Data).
encrypt(Data) ->
	crypto:aes_cfb_128_encrypt(?PRIVATE_KEY, ?IV, Data).

handle([{action, Action} |
			Data
			]) ->
	spawn(?MODULE, Action, Data),
	ok.

handle_decrypted([{action, Action} |
			Data
			]) ->
	spawn(?MODULE, Action, Data),
	ok.

hello_world(_) ->
	write(
		[{message, <<"Hello World!">>}, 
		{action, <<"hello_world">>},
		{pubkey, ?PUBLIC_KEY}]
	).

hello_world_encrypted(_) ->
	write_encrypted(
		[{message, <<"Hello World!">>}, 
		{action, <<"hello_world">>},
		{pubkey, ?PUBLIC_KEY}]
	).