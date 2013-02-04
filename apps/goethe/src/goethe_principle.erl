-module(goethe_principle, [Id,Rev,Name,Email,Password,IsAdmin]).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_entity).

% default exports
-export([new/1,get/1,set/2,add/2,remove/2,save/0,delete/0,json/0]).

% public constructor
-export([new/3]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(TYPE, <<"auth">>).
-define(NAME, <<"name">>).
-define(EMAIL, <<"email">>).
-define(PASSWORD, <<"password">>).
-define(IS_ADMIN, <<"is_admin">>).

new({Json}) when is_list(Json) ->
    ?TYPE = proplists:get_value(<<"g_type">>, Json),
	Id = proplists:get_value(?ID, Json),
	Rev = proplists:get_value(?REV, Json),
	Name = proplists:get_value(?NAME, Json),
	Email = proplists:get_value(?EMAIL, Json),
	Password = proplists:get_value(?PASSWORD, Json),
	IsAdmin = proplists:get_value(?IS_ADMIN, Json),
    new(Id, Rev, Name, Email, Password, IsAdmin).
new(Email,Name,Password) -> new(nil, nil, Name, Email, Password, false).
new(Id,Rev,Name,Email,Password,IsAdmin) -> {?MODULE,Id,Rev,Name,Email,Password,IsAdmin}.


save() ->
    {ok, WithId} = goethe:save(json()),
    new(WithId).


delete() ->
    ok.


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(email) -> {ok, Email};
get(name) -> {ok, Name};
get(password) -> {ok, Password};
get(is_admin) -> {ok, IsAdmin};
get(_) -> {error, unknown_value}.

set(email, NewEmail) ->
	new(Id, Rev, NewEmail, Email, Password, IsAdmin);
set(name, NewName) ->
	new(Id, Rev, NewName, Email, Password, IsAdmin);
set(password, NewPassword) ->
	new(Id, Rev, Name, Email, NewPassword, IsAdmin);
set(is_admin, NewIsAdmin) ->
	new(Id, Rev, Name, Email, Password, NewIsAdmin);
set(_, _) -> {error, unknown_value}.

add(_, _) -> {error, unknown_value}.
remove(_, _) -> {error, unknown_value}.

json() ->
    {[
        {?ID,Id},
        {?REV,Rev},
		{?NAME,Name},
		{?EMAIL,Email},
		{?PASSWORD,Password},
		{?IS_ADMIN,IsAdmin},
		{<<"g_type">>, ?TYPE}
	]}.