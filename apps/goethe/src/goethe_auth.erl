-module(goethe_auth).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/4,handle_event/3,terminate/2,code_change/3]).

% Module application exports
-export([register/3,
		 login/2,
		 get_auth_by_name/1,
         get_auth_by_email/1,
		 is_admin/1]).

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
init(_Args) ->
    {ok, #state{}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%==========================================================================
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%==========================================================================
register(Email, UserName, Password) -> goethe_module:internal(?NAME, {register, {Email, UserName, Password}}).
login(name, {UserName, Password}) -> goethe_module:internal(?NAME, {login_name, {UserName, Password}});
login(email, {Email, Password}) -> goethe_module:internal(?NAME, {login_email, {Email, Password}}).

get_auth_by_name(UserName) ->
    goethe:get({"auth", "by_name"}, UserName, goethe_principle).
get_auth_by_email(Email) ->
    goethe:get({"auth", "by_email"}, Email, goethe_principle).

is_admin(Role) ->
	case Role of
        <<"helper">> -> true;
        <<"admin">> -> true;
        <<"dev">> -> true;
        _ -> false
    end.


%%==========================================================================
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%==========================================================================
handle_internal({register, {Email, UserName, Password}}, State) ->
    logger:debug("Received register from client"),
    Principle = goethe_principle:new(Email, UserName, Password),
	case Principle:save() of
		ok -> {reply, {ok, Principle}, State};
		{validation, ValidationFailure} -> {validation, ValidationFailure};
		{error, Reason} -> {error, Reason}
	end;

handle_internal({login_name, {UserName, Password}}, State) ->
    logger:debug("Received email from client"),
    {ok, Principle} = get_auth_by_name(UserName),
    case auth(Principle, Password) of
		ok ->{reply, {ok, Principle}, State};
        {error, badcreds} -> {nack, <<"bad_credentials">>, State}
    end;

handle_internal({login_email, {Email, Password}}, State) ->
    logger:debug("Received email from client"),
    {ok, Principle} = get_auth_by_email(Email),
    case auth(Principle, Password) of
		ok -> {reply, {ok, Principle}, State};
        {error, badcreds} -> {nack, <<"bad_credentials">>, State}
    end;

handle_internal(Request, _State) ->
    logger:debug("Unmatched request in Auth ~p.", [Request]),
    no_match.


%%==========================================================================
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%==========================================================================
handle_inbound(_Action, _Data, _Session, _State) -> no_match.


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
%  util functions
%
%%==========================================================================
auth(Principle, Password) ->
    {ok, StoredPassword} = Principle:get(password),
    case Password of
        StoredPassword -> ok;
        _ -> {error, badcreds}
    end.