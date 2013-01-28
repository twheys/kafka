-module(goethe_core).
-author('twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/5,handle_event/3,get_api/1,terminate/2,code_change/3]).

% Module application exports
-export([]).

% Module namespace - Must be an atom.
-define(NAME, server).

% Module state
-record(state, {
privkey,pubkey
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  module startup functions
%    Functions required for starting and stopping the application.
%    They do not need to be modified but can be if necessary.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> goethe_module:start(?NAME, ?MODULE, [], []).
start_link() -> goethe_module:start_link(?NAME, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  init
%    Handles creation of a Module record and other necessary
%  terminate
%    Called when the application shuts down.
%  code change
%    Called when a code update occurs
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> {ok, #state{pubkey="abcdefg",privkey="1234567"}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_internal(_Request, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_inbound(plain, hello, [], Session, #state{privkey=PrivKey} = State) ->
    logger:info("Received greeting from client"),
    goethe:notify('server.client_hello', {Session}),
    Session:pencrypt(PrivKey),
    {ok, State};
    
handle_inbound(_, ping, [], Session, State) ->
    logger:info("Received ping from client"),
    goethe:notify('server.ping', {Session}),
    {ok, State};
    
handle_inbound(pencrypt, encrypt, [
            {[{<<"key">>, Key}]}
        ], Session, State) ->
    logger:info("Encrypting connection"),
    Session:fencrypt(Key),
    {ok, State};

handle_inbound(_Role, _Action, _Data, _Session, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event('server.client_hello', {Session}, #state{pubkey=PubKey} = State) -> 
    Session:send_msg(
        {[{<<"server.welcome">>,
            [
            {[{<<"msg">>,<<"Welcome to Goethe! Please encrypt...">>}]},
            {[{<<"pubkey">>,PubKey}]}
            ]
        }]}
    ),
    {ok, State};
    
handle_event('server.client_error', {Session, Blame, Code}, State) ->
    BlameToken = goethe_util:output("server.error.~p_side", [Blame]),
    logger:trace("Sending error code with blame ~p and code ~p", 
        [BlameToken, Code]),
    Session:send_msg(
        {[{
            BlameToken,
            [
                {[{<<"code">>,atom_to_binary(Code, utf8)}]}
            ]
        }]}
    ),
    {ok, State};


handle_event('server.ping', {Session}, State) -> 
    logger:debug("Sending pong to ~p", [Session]),
    Session:send_msg(
        {[{<<"action">>,<<"server.pong">>}]}
    ),
    {ok, State};

handle_event(_Event, _Data, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  get api
%    Sends a description of the JSON API of this module available to the
%    client for the given role.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_api(_Role) -> {ok, {}}.
