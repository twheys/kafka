%%==========================================================================
%
%  Game Core Module
%    The core application is designed to handle modularization, 
%    which it itself is also a Module.  The module provides the 
%    key API components for the client, such as initializing the
%    pidection, parsing JSON messages, encryption, and other
%    common functions.
%
%%==========================================================================
-module(goethe_core).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/4,handle_event/3,terminate/2,code_change/3]).

% Module application exports
-export([is_admin/1]).

% Module namespace - Must be an atom.
-define(NAME, server).

% Module state
-record(state, {
privkey,pubkey,auth
}).


%%==========================================================================
%
%  module startup functions
%    Functions required for starting and stopping the application.
%    They do not need to be modified but can be if necessary.
%
%%==========================================================================
start() -> goethe_module:start(?NAME, ?MODULE, [], []).
start_link() -> goethe_module:start_link(?NAME, ?MODULE, [], []).


%%==========================================================================
%
%  init
%    Handles creation of a Module record and other necessary
%  terminate
%    Called when the application shuts down.
%  code change
%    Called when a code update occurs
%
%%==========================================================================
init(_Args) ->
    PubKey = <<"abcdefg">>,
    PrivKey = <<"1234567">>,
    {ok, AuthImpl} = application:get_env(auth_impl),
    logger:debug("Goethe Core Module initializing.~p Using PubKey ~p~n Using PrivKey ~p~n Auth Impl ~p", [PubKey, PrivKey, AuthImpl]),
    {ok, #state{pubkey=PubKey,privkey=PrivKey,auth=AuthImpl}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%==========================================================================
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%==========================================================================
is_admin(Role) -> goethe_module:internal(?NAME, {is_admin, {Role}}).


%%==========================================================================
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%==========================================================================
handle_internal({is_admin, {Role}}, #state{auth=Auth} = State) ->
    {reply, Auth:is_admin(Role), State};

handle_internal(_Request, _State) -> no_match.


%%==========================================================================
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%==========================================================================
handle_inbound(hello, {}, Session, #state{privkey=PrivKey,pubkey=PubKey} = State) ->
	case validate(Session) of
		ok ->
		    logger:info("Received greeting from client"),
		    goethe:ack(Session, <<"server.hello">>, [
		        {<<"msg">>,<<"Welcome to Goethe! Please encrypt and authenticate...">>},
		        {<<"pubkey">>,PubKey}
		        ]),
		    Session:pencrypt(PrivKey),
		    {ok, State};
		already_authenticated -> {nack, <<"already_authenticated">>, State}
	end;

handle_inbound(ping, {}, _Session, State) ->
    logger:info("Received ping from client"),
    {ack, [
        {<<"reply">>,<<"pong!">>}
        ], State};

handle_inbound(encrypt, {[
	        {<<"key">>,Key}
	    ]}, Session, State) ->
	case validate(Session) of
		ok ->
		    logger:info("Encrypting connection"),
		    goethe:ack(Session, <<"server.encrypt">>),
		    Session:fencrypt(Key), 			
    		{ok, State};
		already_authenticated -> {nack, <<"already_authenticated">>, State}
	end;

handle_inbound(register, {[
	        {<<"email">>,Email},
	        {<<"username">>,UserName},
	        {<<"password">>,Password}
	    ]}, Session, #state{auth=Auth} = State) ->
	case validate(Session) of
		ok -> do_register(Auth, Email, UserName, Password, State);
		already_authenticated -> {nack, <<"already_authenticated">>, State}
	end;

handle_inbound(login, {[
	        {<<"username">>,UserName},
	        {<<"password">>,Password}
	    ]}, Session, #state{auth=Auth} = State) ->
	case validate(Session) of
		ok -> do_auth(Auth, name, UserName, Password, Session, State);
		already_authenticated -> {nack, <<"already_authenticated">>, State}
	end;

handle_inbound(login, {[
	        {<<"email">>,Email},
	        {<<"password">>,Password}
	    ]}, Session, #state{auth=Auth} = State) ->
	case validate(Session) of
		ok -> do_auth(Auth, email, Email, Password, Session, State);
		already_authenticated -> {nack, <<"already_authenticated">>, State}
	end;

handle_inbound(unauthorized, {}, _, State) -> {nack, <<"unauthorized">>, State};

handle_inbound(_Action, _Data, _Session, _State) -> no_match.


%%==========================================================================
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%==========================================================================
handle_event('server.client_error', {Session, Blame, Code}, State) ->
    BlameToken = goethe_util:output("server.error.~p_side", [Blame]),
    logger:trace("Sending error code with blame ~p and code ~p", 
        [BlameToken, Code]),
    Session:send_msg(
        {[{
            BlameToken,
            {[
                {<<"code">>,Code}
            ]}
        }]}
    ),
    {ok, State};

handle_event(client_timeout, {Session}, State) ->
    Session:send_msg(
        {[{
            <<"client.error">>,
            {[
                {<<"code">>,<<"timeout">>}
            ]}
        }]}
    ),
    Session:timeout(),
    {ok, State};

handle_event(_Event, _Data, _State) -> no_match.


%%==========================================================================
%
%  util functions
%
%%==========================================================================
do_register(Auth, Email, UserName, Password, State) ->
	case Auth:register(Email, UserName, Password) of
		{reply, {ok, Principle}, State} ->
			{ok, UserName} = Principle:get(name),
			{ack, {[
					<<"welcome">>, UserName, 
					<<"message">>, <<"Registration success! Please login.">>
				]}, State};
		{validation, ValidationFailure} -> {nack, ValidationFailure, State};
		{error, Reason} -> {nack, Reason, State}
	end.


do_auth(Auth, IdType, IdToken, Password, Session, State) ->
	case Auth:login(IdType, {IdToken, Password}) of
		 {ok, Principle} ->
			{ok, Role} = Principle:get(role),
            case Auth:is_admin(Role) of
                true -> goethe:notify(helper_login, {Session});
                _ -> goethe:notify(login, {Session})
            end,
			{ok, UserName} = Principle:get(name),
			goethe:ack(Session, <<"server.login">>, [{<<"welcome">>, UserName}]),
            Session:ready(Principle),
			{ok, State};
		 {error, badcreds} -> {nack, <<"bad_credentials">>, State}
	end.

validate(Session) ->
	case Session:get(role) of
		{ok, nil} -> ok;
		_ -> already_authenticated
	end.