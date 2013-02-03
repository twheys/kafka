%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% @type session() = term().
% @type role() = 'plain' | 'pencrypt' | 'fencrypt' | 'auth' | 'admin' | 'cloud'.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe_rooms).
-author('twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/5,handle_event/3,get_api/1,terminate/2,code_change/3]).

% Module application exports
-export([get_room/1,get_current_room/1,get_room_list/0,get_avail_room_names/0]).

% Module namespace - Must be an atom.
-define(NAME, rooms).

% Module state
-record(state, {
roomnames,config
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
init(_Args) ->
    goethe:clean({"rooms", "all"}),
    {ok, RoomNames} = get_avail_room_names(),
    Goethe = goethe_room:new(<<"Goethe">>),
    Goethe:save(),
	{ok, #state{roomnames=RoomNames}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_room(RoomName) -> goethe:get({"rooms", "by_name"}, RoomName, goethe_room).
get_current_room(UserName) -> goethe:get({"rooms", "by_user"}, UserName, goethe_room).
get_room_list() -> goethe:get_enum({"rooms", "by_name"}).
get_avail_room_names() -> goethe:get_enum({"room_names", "by_name"}).


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
handle_inbound(Role, list, {}, _Session, State)
		when auth == Role; admin == Role ->
    logger:debug("Received room.list from client"),
    {ok, RoomList} = get_room_list(),
    {ack, {<<"list">>, RoomList}, State};

handle_inbound(Role, join, {[
            {<<"name">>, RoomName}
        ]}, Session, State)
		when auth == Role; admin == Role ->
    logger:debug("Received room.join from client"),
    {ok, Principle} = Session:get(principle),
    case get_room(RoomName) of
	{ok, Room} ->
	    case validate_room_join(Principle, Room) of
    	ok ->
    		{ok, RoomList} = join_room(Principle, Room),
    		{ack, {<<"users">>, RoomList}, State};
    	{error, Reason} ->
    		{nack, {Reason, {<<"room">>, RoomName}}, State}
		end;
    {error, not_found} ->
        {nack, {<<"no_such_room">>, {<<"room">>, RoomName}}, State}
	end;

handle_inbound(_Role, _Action, _Data, _Session, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_event(_Event, _Data, _State) -> no_match.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  get api
%    Sends a description of the JSON API of this module available to the
%    client for the given role.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_api(_Role) -> {ok, {}}.


%%==========================================================================
%
%  util functions
%
%%==========================================================================
validate_room_join(Principle, Room) ->
	{ok, Users} = Room:get(users),
	{ok, MaxUsers} = Room:get(max_users),
	{ok, UserName} = Principle:get(name),
	{ok, IsAdmin} = Principle:get(is_admin),
	CurrentUsers = length(Users),
	UserInRoom = 0 == lists:filter(fun(X) -> X == UserName end, Users),
	if
		UserInRoom -> already_in_room;
		IsAdmin -> ok;
		MaxUsers < CurrentUsers -> room_full;
		true -> ok
	end.


join_room(Principle, Room) ->
	{ok, UserName} = Principle:get(name),
	leave_current_room(UserName),
	NRoom = Room:add(users, UserName),
	NRoom:save(),
	goethe:notify(user_joined_room, {NRoom, UserName}),
	NRoom:get(users).

leave_current_room(UserName) ->
	case get_current_room(UserName) of
		{ok, OldRoom} ->
			NOldRoom = OldRoom:remove(UserName),
			NOldRoom:save(),
			goethe:notify(user_left_room, {NOldRoom, UserName});
		_ -> ok
	end.