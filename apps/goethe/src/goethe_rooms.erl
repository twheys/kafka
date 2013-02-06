%===========================================================================
%
%
% @type session() = term().
% @type role() = 'plain' | 'pencrypt' | 'fencrypt' | 'auth' | 'admin' | 'cloud'.
%
%===========================================================================
-module(goethe_rooms).
-author('twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/4,handle_event/3,terminate/2,code_change/3]).

% Module application exports
-export([get_room/1,get_current_room/1,get_room_list/0,get_avail_room_names/0]).

% Module namespace - Must be an atom.
-define(NAME, rooms).

% Module state
-record(state, {
roomnames,config
}).


%===========================================================================
%
%  module startup functions
%    Functions required for starting and stopping the application.
%    They do not need to be modified but can be if necessary.
%
%===========================================================================
start() -> goethe_module:start(?NAME, ?MODULE, [], []).
start_link() -> goethe_module:start_link(?NAME, ?MODULE, [], []).


%===========================================================================
%
%  init
%    Handles creation of a Module record and other necessary
%  terminate
%    Called when the application shuts down.
%  code change
%    Called when a code update occurs
%
%===========================================================================
init(_Args) ->
    {ok, RoomNames} = get_avail_room_names(),
    {ok, Rooms} = get_room_list(),
    if 0 == length(Rooms) ->
    		Goethe = goethe_room:new(<<"Goethe">>),
    		Goethe:save();
		true -> ok
	end,
	{ok, #state{roomnames=RoomNames}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%===========================================================================
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%===========================================================================
get_room(RoomName) -> goethe:get({"rooms", "by_name"}, RoomName, goethe_room).
get_current_room(UserName) -> goethe:get({"rooms", "by_user"}, UserName, goethe_room).
get_room_list() -> goethe:get_enum({"rooms", "by_name"}).
get_avail_room_names() -> goethe:get_enum({"room_names", "by_name"}).


%===========================================================================
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%===========================================================================
handle_internal(_Request, _State) -> no_match.


%===========================================================================
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%===========================================================================
handle_inbound(list, {}, _Session, State) ->
    logger:debug("Received room.list from client"),
    {ok, RoomList} = get_room_list(),
    {ack, {<<"list">>, RoomList}, State};

handle_inbound(members, {}, Session, State) ->
    logger:debug("Received room.members from client"),
	{ok, UserName} = Session:get(name),
	case get_current_room(UserName) of
		{ok, Room} ->
			{ok, UserList} = get_user_list(Room),
    		{ack, {<<"list">>, UserList}, State};
		_ -> {nack, <<"not_in_room">>, State}
	end;

handle_inbound(join, {[
            {<<"name">>, RoomName}
        ]}, Session, State) ->
    logger:debug("Received room.join from client"),
    case get_room(RoomName) of
	{ok, Room} ->
	    case validate_room_join(Session, Room) of
    	ok ->
    		{ok, UserList} = join_room(Session, Room),
    		{ack, {<<"users">>, UserList}, State};
    	{error, Reason} ->
    		{nack, {Reason, {<<"room">>, RoomName}}, State}
		end;
    {error, not_found} ->
        {nack, {<<"room_not_found">>, {<<"room">>, RoomName}}, State}
	end;

handle_inbound(leave, {}, Session, State) ->
	case leave_current_room(Session) of
		ok -> {ack, State};
		{error, Reason} ->  {nack, {Reason}, State}
	end;

handle_inbound(_Action, _Data, _Session, _State) -> no_match.


%%==========================================================================
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%===========================================================================
handle_event(module_ready, {goethe_chat}, State) ->
    logger:info("Loading room chat."),
	RoomChat = fun("/r " ++ Body, From, Role) when 0 < length(Body) ->
			send_room_chat(Body, From, Role)
		end,
	goethe_chat:add_handler(room_chat, RoomChat),
    {ok, State};

handle_event(_Event, _Data, _State) -> no_match.


%%==========================================================================
%
%  util functions
%
%%==========================================================================
validate_room_join(Session, Room) ->
	{ok, UserSessions} = Room:get(user_sessions),
	{ok, MaxUsers} = Room:get(max_users),
	{ok, UserName} = Session:get(name),
	{ok, Role} = Session:get(role),
    IsAdmin = goethe_core:is_admin(Role),
	CurrentUsers = length(UserSessions),
	UserInRoom = [] =/= lists:filter(fun(X) -> {ok, UserName} == X:get(name) end, UserSessions),
	IsFull = MaxUsers < CurrentUsers,
	logger:debug("Validate room join ~p", [Session]),
	if
		UserInRoom -> {error, already_in_room};
		IsAdmin -> ok;
		IsFull -> {error, room_full};
		true -> ok
	end.


join_room(Session, Room) ->
	{ok, UserName} = Session:get(name),
	leave_current_room(Session),
	NRoom = Room:add(user_sessions, Session),
	NRoom:save(),
	goethe:notify(user_joined_room, {NRoom, UserName}),
	{ok, UserList} = NRoom:get(user_sessions),
	{ok, proplists:get_keys(UserList)}.

leave_current_room(Session) ->
	{ok, UserName} = Session:get(name),
	case get_current_room(UserName) of
		{ok, OldRoom} ->
			NOldRoom = OldRoom:remove(user_sessions, Session),
			NOldRoom:save(),
			goethe:notify(user_left_room, {NOldRoom, UserName}),
			ok;
		_ -> {error, not_in_room}
	end.

get_user_list(Room) ->
	{ok, UserSessions} = Room:get(user_sessions),
	{ok, lists:map(fun(Session) -> Session:get(name) end, UserSessions)}.

send_room_chat(Body, From, Role) ->
	case get_current_room(From) of
		{ok, Room} ->
			    {ok, Sessions} = Room:get(user_sessions),
			    [goethe_chat:send_chat(Target, From, room, Body, Role) || 
					Target <- Sessions];
	    {error, not_found} -> logger:warn("User is trying to chat in a room but not currently in one.")
	end.