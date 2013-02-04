-module(goethe_room, [Id,Rev,Name,UserSessions,MaxUsers]).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_entity).

% default exports
-export([new/1,get/1,set/2,add/2,remove/2,save/0,delete/0,json/0]).

% public constructor
-export([]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(TYPE, <<"room">>).
-define(NAME, <<"name">>).
-define(USERS_SESSIONS, <<"users">>).
-define(MAX_USERS, <<"max_users">>).

new({Json}) when is_list(Json) ->
    ?TYPE = proplists:get_value(<<"g_type">>, Json),
	Id = proplists:get_value(?ID, Json),
	Rev = proplists:get_value(?REV, Json),
	Name = proplists:get_value(?NAME, Json),
	UserSessions = goethe_entity:clfjson(goethe_session,
						proplists:get_value(?USERS_SESSIONS, Json)),
	MaxUsers = proplists:get_value(?MAX_USERS, Json),
    new(Id, Rev, Name, UserSessions, MaxUsers);
new(Name) -> new(Name, 50).
new(Name, MaxUsers) -> new(nil, nil, Name, [], MaxUsers).
new(Id, Rev, Name, UserSessions, MaxUsers) -> {?MODULE, Id, Rev, Name, UserSessions, MaxUsers}.


save() ->
    {ok, WithId} = goethe:save(json()),
    new(WithId).


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(name) -> {ok, Name};
get(user_sessions) -> {ok, UserSessions};
get(max_users) -> {ok, MaxUsers};
get(_) -> {error, unknown_value}.

set(name, NewName) ->
    new(Id,Rev,NewName,UserSessions,MaxUsers);
set(user_sessions, NewUserSessions) ->
    new(Id,Rev,Name,NewUserSessions,MaxUsers);
set(max_users, NewMaxUsers) ->
    new(Id,Rev,Name,UserSessions,NewMaxUsers);
set(_, _) -> {error, unknown_value}.

add(user_sessions, NewUserSession) ->
    new(Id,Rev,Name,[NewUserSession|UserSessions],MaxUsers);
add(_, _) -> {error, unknown_value}.

remove(user_sessions, UserSession) ->
    set(user_sessions, goethe_entity:rchild(UserSession, UserSessions));
remove(_, _) -> {error, unknown_value}.


delete() ->
    ok.

json() ->
    {[
	    {?ID,Id},
	    {?REV,Rev},
		{?NAME,Name},
		{?USERS_SESSIONS,goethe_entity:cltjson(UserSessions)},
		{?MAX_USERS,MaxUsers},
        {<<"g_type">>, ?TYPE}
	]}.