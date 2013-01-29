%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Goethe Chat Module
%%  
%%    This API is used for online chat.  Messages are delivered immediately
%%    and can only be sent to online users.
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe_chat).
-author('twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/5,handle_event/3,get_api/1,terminate/2,code_change/3]).

% Module application exports
-export([system_msg/2]).

% Module namespace - Must be an atom.
-define(NAME, chat).

% Module state
-record(state, {
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
system_msg(Target, Msg) -> goethe_module:handle_internal(?NAME, {system_msg, {Target, Msg}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_internal({system_msg, {Target, Msg}}, State) ->
    {ok, ToString} = Target:get(principle),
    To = list_to_binary(ToString),
    From = <<"System">>,
    Type = <<"system">>,
    goethe:notify('chat.deliver_msg', {Target, From, To, Type, Msg}),
    {reply, ok, State};

handle_internal(_Request, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_inbound(Role, chat, [
            {<<"msg">>,Msg}
        ], Session, State) when auth == Role; admin == Role ->
	{ok, {_, From}} = Session:get(principle),
    case parse_chat(Msg) of
		{private, {Target, To, Msg}} -> goethe:notify('chat.deliver_msg', {Target, From, To, <<"private">>, Msg});
		{room, {Msg}} -> goethe:notify('chat.room_chat', {Session, From, Msg});
		{region, {Msg}} -> goethe:notify('chat.region_chat', {Session, From, Msg});
		{broadcast, {Msg}} -> goethe:notify('chat.broadcast', {Session, From, Msg})
	end,
    {ok, State};

handle_inbound(_Role, _Action, _Data, _Session, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event('chat.deliver_msg', {Session, From, To, Type, Msg}, State) ->
    {_, _, IsAdmin} = Session:get(principle),
	Session:send_msg(
        {[{<<"chat.deliver">>,
            [
                {<<"from">>,list_to_binary(From)},
                {<<"to">>,list_to_binary(To)},
                {<<"type">>,atom_to_binary(Type, utf8)},
                {<<"msg">>,list_to_binary(Msg)}
                | case IsAdmin of
                    true -> [{<<"msg">>,list_to_binary(Msg)}];
                    _ -> []
                  end
            ]
        }]}
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  util functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_chat(_Msg) ->
	% TODO implement me
	ok.
