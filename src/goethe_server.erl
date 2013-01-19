-module(goethe_server).

-export([new/2,new_link/2]).

-export([send_msg/2,encrypt/2,shutdown/1,timeout/1]).

-define(PLAIN_TEXT_TIMEOUT, 100 * 10 * 1000).
-define(FULLY_ENCRYPTED_TIMEOUT, 5 * 60 * 1000).

new(Port, TcpOptions) -> goethe_server_impl:start(Port, TcpOptions).

new_link(Port, TcpOptions) -> goethe_server_impl:start_link(Port, TcpOptions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_msg(Pid, Msg) -> 
	Pid ! {write, Msg},
	ok.
encrypt(Pid, partial) -> 
	Pid ! partially_encrypted,
	ok;
encrypt(Pid, full) -> 
	Pid ! fully_encrypted,
	ok.
shutdown(Pid) ->
	Pid ! shutdown,
	ok.
timeout(Pid) ->
	Pid ! timeout,
	ok.