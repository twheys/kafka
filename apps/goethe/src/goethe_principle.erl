-module(goethe_principle, [Id,Email,UserName,Password,IsAdmin]).
-author('Tim Heys twheys@gmail.com').

-export([new/1,new/3,new/5,get/1,set/2,save/0,delete/0]).

% [{<<"_id">>,Id},
%		{<<"_rev">>,_},
%		{<<"name">>,UserName},
%		{<<"email">>,Email},
%		{<<"password">>,Password},
%		{<<"is_admin">>,IsAdmin},
%		{<<"created">>,_},
%		{<<"updated">>,_},
%		{<<"g_type">>,<<"auth">>}]
new({Json}) when is_list(Json) ->
	Id = proplists:get_value(<<"_id">>, Json),
	Email = proplists:get_value(<<"email">>, Json),
	UserName = proplists:get_value(<<"name">>, Json),
	Password = proplists:get_value(<<"password">>, Json),
	IsAdmin = proplists:get_value(<<"is_admin">>, Json),
    new(Id, Email, UserName, Password, IsAdmin).
new(Email,UserName,Password) -> new(nil,Email,UserName,Password,false).
new(Id,Email,UserName,Password,IsAdmin) -> {goethe_principle,Id,Email,UserName,Password,IsAdmin}.


save() ->
    {ok, New} = goethe:save({[
	        {<<"_id">>,Id},
			{<<"name">>,UserName},
			{<<"email">>,Email},
			{<<"password">>,Password},
			{<<"is_admin">>,IsAdmin},
			{<<"g_type">>,<<"auth">>}]}),
	new(New).


delete() ->
    ok.


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(email) -> {ok, Email};
get(username) -> {ok, UserName};
get(password) -> {ok, Password};
get(is_admin) -> {ok, IsAdmin};
get(_) -> {error, unknown_value}.

set(email, NewEmail) ->
    {goethe_principle,Id,NewEmail,UserName,Password,IsAdmin};
set(username, NewUserName) ->
    {goethe_principle,Id,Email,NewUserName,Password,IsAdmin};
set(password, NewPassword) ->
    {goethe_principle,Id,Email,UserName,NewPassword,IsAdmin};
set(is_admin, NewIsAdmin) ->
    {goethe_principle,Id,Email,UserName,Password,NewIsAdmin};
set(_, _) -> {error, unknown_value}.