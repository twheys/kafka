-module(goethe_principle, [Id,Rev,Name,Email,Password,IsAdmin]).
-author('Tim Heys twheys@gmail.com').

-export([new/1,new/3,get/1,set/2,add/2,remove/2,save/0,delete/0]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(NAME, <<"name">>).
-define(EMAIL, <<"email">>).
-define(PASSWORD, <<"password">>).
-define(IS_ADMIN, <<"is_admin">>).

% [{<<"_id">>,Id},
%		{<<"_rev">>,_},
%		{<<"name">>,Name},
%		{<<"email">>,Email},
%		{<<"password">>,Password},
%		{<<"is_admin">>,IsAdmin},
%		{<<"created">>,_},
%		{<<"updated">>,_},
%		{<<"g_type">>,<<"auth">>}]
new({Json}) when is_list(Json) ->
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
    {ok, NewId} = goethe:save({[
	        {?ID,Id},
	        {?REV,Rev},
			{?NAME,Name},
			{?EMAIL,Email},
			{?PASSWORD,Password},
			{?IS_ADMIN,IsAdmin},
			{<<"g_type">>,<<"auth">>}]}),
	new(NewId, Rev, Name, Email, Password, IsAdmin).


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
    {?MODULE,Id,Rev,Name,NewEmail,Password,IsAdmin};
set(name, NewName) ->
    {?MODULE,Id,Rev,Name,Email,NewName,Password,IsAdmin};
set(password, NewPassword) ->
    {?MODULE,Id,Rev,Name,Email,Name,NewPassword,IsAdmin};
set(is_admin, NewIsAdmin) ->
    {?MODULE,Id,Rev,Name,Email,Name,Password,NewIsAdmin};
set(_, _) -> {error, unknown_value}.

add(_, _) -> {error, unknown_value}.
remove(_, _) -> {error, unknown_value}.