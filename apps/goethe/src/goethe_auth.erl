-module(goethe_auth).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/5,handle_event/3,get_api/1,terminate/2,code_change/3]).

% Module application exports
-export([get_auth_by_name/1,
         get_auth_by_email/1]).

% Module namespace - Must be an atom.
-define(NAME, auth).

% Module state
-record(state, {
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
init(_Args) -> {ok, #state{}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%==========================================================================
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%==========================================================================
get_auth_by_name(UserName) ->
    goethe:get({"auth", "by_name"}, UserName, goethe_principle).

get_auth_by_email(Email) ->
    goethe:get({"auth", "by_email"}, Email, goethe_principle).


%%==========================================================================
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%==========================================================================
handle_internal(_Request, _State) -> no_match.


%%==========================================================================
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%==========================================================================
handle_inbound(fencrypt, register, {[
            {<<"email">>, _Email},
            {<<"username">>, _UserName},
            {<<"password">>, _Password}
        ]}, _Session, State) ->
    logger:debug("Received register from client"),
    % TODO register
    {ok, State};

handle_inbound(fencrypt, login, {[
            {<<"username">>, UserName},
            {<<"password">>, Password}
        ]}, Session, State) ->
    logger:debug("Received username login from client"),
    {ok, Principle} = get_auth_by_name(UserName),
    case auth(Principle, Password) of
        {ok, Principle} ->
            case Principle:get(is_admin) of
                true -> Session:admin(Principle);
                _ -> Session:auth(Principle)
            end,
            goethe:notify('auth.login', {Session, Principle});
        {error, badcreds} ->
            goethe:notify('auth.badcreds', {Session})
    end,
    {ok, State};

handle_inbound(fencrypt, login, {[
            {<<"email">>, Email},
            {<<"password">>, Password}
        ]}, Session, State) ->
    logger:debug("Received email from client"),
    {ok, Principle} = goethe_principle:get_by_email(Email),
    case auth(Principle, Password) of
        {ok, Principle} ->
            case Principle:get(is_admin) of
                true -> Session:admin(Principle);
                _ -> Session:auth(Principle)
            end,
            UserName = Principle:get(username),
            goethe:ack(Session, <<"auth.login">>, [{<<"welcome">>,UserName}]),
            goethe:notify('auth.login', {Session, Principle});
        {error, badcreds} ->
            goethe:nack(Session, <<"auth.login">>, <<"bad_credentials">>)
    end,
    {ok, State};

handle_inbound(_Role, _Action, _Data, _Session, _State) -> no_match.


%%==========================================================================
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%==========================================================================
handle_event(_Event, _Data, _State) -> no_match.


%%==========================================================================
%
%  get api
%    Sends a description of the JSON API of this module available to the
%    client for the given role.
%
%%==========================================================================
get_api(_Role) -> {ok, {}}.


auth(Principle, Password) ->
    {ok, StoredPassword} = Principle:get(password),
    case Password of
        StoredPassword -> {ok, Principle};
        _ -> {error, badcreds}
    end.