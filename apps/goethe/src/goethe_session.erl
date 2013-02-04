-module(goethe_session, [Id,Rev,Listener,Name,Email,IsAdmin]).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_entity).

% default exports
-export([new/1,get/1,set/2,add/2,remove/2,save/0,delete/0,json/0]).

% public constructor
-export([build/1]).

% public socket server functions
-export([send_msg/1,pencrypt/1,fencrypt/1,auth/1,admin/1,cloud/0,close/0,timeout/0]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(TYPE, <<"session">>).
-define(PROC, <<"proc">>).
-define(NAME, <<"name">>).
-define(EMAIL, <<"email">>).
-define(IS_ADMIN, <<"is_admin">>).

new({Json}) when is_list(Json) ->
    ?TYPE = proplists:get_value(<<"g_type">>, Json),
    Id = proplists:get_value(?ID, Json),
    Rev = proplists:get_value(?REV,Json),
    Pid = proplists:get_value(?PROC,Json),
    Name = proplists:get_value(?NAME, Json),
    Email = proplists:get_value(?EMAIL, Json),
    IsAdmin = proplists:get_value(?IS_ADMIN, Json),
    new(Id, Rev, list_to_pid(binary_to_list(Pid)), Name, Email, IsAdmin);
new(Listener) when is_pid(Listener) -> new(nil,nil,Listener,nil,nil,nil).
new(Id,Rev,Listener,Name,Email,IsAdmin) -> {goethe_session,Id,Rev,Listener,Name,Email,IsAdmin}.

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
get(name) -> {ok, Name};
get(email) -> {ok, Email};
get(is_admin) -> {ok, IsAdmin};
get(_) -> {error, unknown_value}.

set(name, NewName) ->
    new(Id, Rev, Listener, NewName, Email, IsAdmin);
set(email, NewEmail) ->
    new(Id, Rev, Listener, Name, NewEmail, IsAdmin);
set(is_admin, NewIsAdmin) ->
    new(Id, Rev, Listener, Name, Email, NewIsAdmin);
set(_, _) -> {error, unknown_value}.

add(_, _) -> {error, unknown_value}.
remove(_, _) -> {error, unknown_value}.

json() ->
    {[
        {?ID, Id},
        {?REV, Rev},
        {?PROC, list_to_binary(pid_to_list(Listener))},
        {?NAME,Name},
        {?EMAIL,Email},
        {?IS_ADMIN,IsAdmin},
        {<<"g_type">>, ?TYPE}
    ]}.

build(Principle) ->
    {ok, NewName} = Principle:get(name),
    {ok, NewEmail} = Principle:get(email),
    {ok, NewIsAdmin} = Principle:get(is_admin),
    new(Id, Rev, Listener, NewName, NewEmail, NewIsAdmin).


%%==========================================================================
%
%  public socket server functions
%
%%==========================================================================
send_msg(Msg) ->
    Listener ! {write, Msg},
    ok.
pencrypt(PrivKey) -> 
    Listener ! {pencrypt, {PrivKey}},
    ok.
fencrypt(Key) -> 
    Listener ! {fencrypt, {Key}},
    ok.
auth(NewPrinciple) -> 
    Listener ! {auth, {NewPrinciple}},
    ok.
admin(NewPrinciple) -> 
    Listener ! {admin, {NewPrinciple}},
    ok.
cloud() -> 
    Listener ! cloud,
    ok.
close() ->
    Listener ! close,
    ok.
timeout() ->
    Listener ! timeout,
    ok.