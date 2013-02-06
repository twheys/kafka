-module(goethe_session, [Id,Rev,Listener,Name,Email,Role]).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_entity).

% default exports
-export([new/1,get/1,set/2,add/2,remove/2,save/0,delete/0,json/0]).

% public constructor
-export([build/3]).

% public socket server functions
-export([send_msg/1,
         pencrypt/1,
         fencrypt/1,
         ready/1,
         close/0,
         timeout/0]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(TYPE, <<"session">>).
-define(PROC, <<"proc">>).
-define(NAME, <<"name">>).
-define(EMAIL, <<"email">>).
-define(ROLE, <<"role">>).

new({Json}) when is_list(Json) ->
    ?TYPE = proplists:get_value(<<"g_type">>, Json),
    Id = proplists:get_value(?ID, Json),
    Rev = proplists:get_value(?REV,Json),
    Pid = proplists:get_value(?PROC,Json),
    Name = proplists:get_value(?NAME, Json),
    Email = proplists:get_value(?EMAIL, Json),
    Role = proplists:get_value(?ROLE, Json),
    new(Id, Rev, list_to_pid(binary_to_list(Pid)), Name, Email, Role);
new(Listener) when is_pid(Listener) -> new(nil,nil,Listener,nil,nil,nil).
new(Id,Rev,Listener,Name,Email,Role) -> {goethe_session,Id,Rev,Listener,Name,Email,Role}.

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
get(role) -> {ok, Role};
get(_) -> {error, unknown_value}.

set(name, NewName) ->
    new(Id, Rev, Listener, NewName, Email, Role);
set(email, NewEmail) ->
    new(Id, Rev, Listener, Name, NewEmail, Role);
set(role, NewRole) ->
    new(Id, Rev, Listener, Name, Email, NewRole);
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
        {?ROLE,Role},
        {<<"g_type">>, ?TYPE}
    ]}.

build(NewName, NewEmail, NewRole) ->
    new(Id, Rev, Listener, NewName, NewEmail, NewRole).


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
ready(NewPrinciple) -> 
    Listener ! {ready, {NewPrinciple}},
    ok.
close() ->
    Listener ! close,
    ok.
timeout() ->
    Listener ! timeout,
    ok.