-module(goethe_room, [Id,Rev,Name,Users,MaxUsers]).
-author('Tim Heys twheys@gmail.com').

-export([new/1,new/2,get/1,set/2,add/2,remove/2,save/0,delete/0]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(NAME, <<"name">>).
-define(USERS, <<"users">>).
-define(MAX_USERS, <<"max_users">>).

% [{<<"_id">>,Id},
%		{<<"_rev">>,_},
%		{<<"name">>,UserName},
%		{<<"users">>,Users},
%		{<<"created">>,_},
%		{<<"updated">>,_},
%		{<<"g_type">>,<<"room">>}]
new({Json}) when is_list(Json) ->
	Id = proplists:get_value(?ID, Json),
	Rev = proplists:get_value(?REV, Json),
	Name = proplists:get_value(?NAME, Json),
	Users = proplists:get_value(?USERS, Json),
	MaxUsers = proplists:get_value(?MAX_USERS, Json),
    new(Id, Rev, Name, Users, MaxUsers);
new(Name) -> new(Name, 50).
new(Name, MaxUsers) -> new(nil, nil, Name, [], MaxUsers).
new(Id, Rev, Name, Users, MaxUsers) -> {?MODULE, Id, Rev, Name, Users, MaxUsers}.


save() ->
    {ok, NewId} = goethe:save({[
	        {?ID,Id},
	        {?REV,Rev},
			{?NAME,Name},
			{?USERS,Users},
			{?MAX_USERS,MaxUsers},
			{<<"g_type">>,<<"room">>}]}),
	new(NewId, Rev, Name, Users, MaxUsers).


delete() ->
    ok.


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(name) -> {ok, Name};
get(users) -> {ok, Users};
get(max_users) -> {ok, MaxUsers};
get(_) -> {error, unknown_value}.

set(name, NewName) ->
    new(Id,Rev,NewName,Users,MaxUsers);
set(users, NewUsers) ->
    new(Id,Rev,Name,NewUsers,MaxUsers);
set(max_users, NewMaxUsers) ->
    new(Id,Rev,Name,Users,NewMaxUsers);
set(_, _) -> {error, unknown_value}.

add(users, NewUser) ->
    new(Id,Rev,Name,[NewUser|Users],MaxUsers);
add(_, _) -> {error, unknown_value}.

remove(users, OldUser) ->
    NewUsers = lists:filter(fun(X) -> X =/= OldUser end, Users),
    set(users, NewUsers);
remove(_, _) -> {error, unknown_value}.
