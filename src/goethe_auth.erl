-module(goethe_auth).
-author('twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/5,handle_event/3,get_api/1,terminate/2,code_change/3]).

% Module application exports
-export([get_user/1]).

% Module namespace - Must be an atom.
-define(NAME, auth).

% Module state
-record(state, {
cookies=[]
}).
-record(principle, {
id,user,session
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
init(_Args) -> {ok, #state{}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_user(UserName) -> goethe_module:internal(?NAME, {get_user, {UserName}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_internal({get_user, {UserName}}, State) ->
    {reply, {UserName}, State};

handle_internal(_Request, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_inbound(fencrypt, register, [
            {[{<<"email">>,Email}]},
            {[{<<"username">>,UserName}]},
            {[{<<"password">>,_Password}]}
        ], Session, State) ->
    Account = {Email, UserName},
    goethe:notify('auth.registration', {Session, Account}),
    {ok, State};

handle_inbound(fencrypt, login, [
            {[{<<"username">>,UserName}]},
            {[{<<"password">>,_Password}]}
        ], Session, #state{cookies=Cookies} = State) ->
    SessionId = goethe_util:generate_hash(),
    Principle = {UserName},
    NewCookie = #principle{id=SessionId, user=Principle},
    Session:authenticate({SessionId, Principle}),
    goethe:notify('auth.login', {Session, Principle}),
    {ok, State#state{cookies=[NewCookie | Cookies]}};

handle_inbound(fencrypt, login, [
            {[{<<"email">>,Email}]},
            {[{<<"password">>,_Password}]}
        ], Session, #state{cookies=Cookies} = State) ->
    SessionId = goethe_util:generate_hash(),
    Principle = {Email},
    NewCookie = #principle{id=SessionId, user=Principle},
    Session:authenticate({SessionId, Principle}),
    goethe:notify('auth.login', {Session, Principle}),
    {ok, State#state{cookies=[NewCookie | Cookies]}};

handle_inbound(_Role, _Action, _Data, _Session, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event('auth.registration', {Session, Account}, State) ->
    % TODO Confirm registration
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
