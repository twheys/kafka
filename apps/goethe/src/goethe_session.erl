-module(goethe_session, [Id,Listener,Principle]).
-author('Tim Heys twheys@gmail.com').

% public socket server functions
-export([send_msg/1,pencrypt/1,fencrypt/1,auth/1,admin/1,cloud/0,close/0,timeout/0]).
-export([new/1,new/3,get/1,set/2,add/2,remove/2,save/0,delete/0]).

% [{<<"_id">>,Id},
%  {<<"_rev">>,_},
%  {<<"proc_id">>,Pid},
%  {<<"created">>,_},
%  {<<"updated">>,_},
%  {<<"g_type">>,<<"auth">>}]
new({Json}) when is_list(Json) ->
    Id = proplists:get_value(<<"_id">>, Json),
    Pid = proplists:get_value(<<"proc_id">>,Json),
    new(Id, list_to_pid(binary_to_list(Pid)), {});
new(Listener) when is_pid(Listener) -> new(nil,Listener,nil).
new(Id,Listener,Principle) -> {goethe_session,Id,Listener,Principle}.

% sessions can only be saved once!!!
save() ->
    {ok, UserName} = Principle:get(name),
    {ok, NewId} = goethe:save({[
            {<<"_id">>,Id},
            {<<"_rev">>,nil},
            {<<"proc_id">>,list_to_binary(pid_to_list(Listener))},
            {<<"username">>,UserName},
            {<<"g_type">>,<<"session">>}
        ]}),
    new(NewId, Listener, Principle).


delete() ->
    ok.

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


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(principle) -> {ok, Principle}.

set(principle, NewPrinciple) ->
    {?MODULE,Id,Listener,NewPrinciple}.

add(_, _) -> {error, unknown_value}.
remove(_, _) -> {error, unknown_value}.