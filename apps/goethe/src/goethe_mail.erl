-module(goethe_mail, [Id,Rev,From,To,Body,Attachments]).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_entity).

% default exports
-export([new/1,get/1,set/2,add/2,remove/2,save/0,delete/0,json/0]).

% public constructor
-export([new/3,
         new/4]).

-define(ID, <<"_id">>).
-define(REV, <<"_rev">>).
-define(FROM, <<"from">>).
-define(TO, <<"to">>).
-define(BODY, <<"body">>).
-define(ATTACHMENTS, <<"attachments">>).

-define(TYPE, <<"mail">>).

new({Json}) when is_list(Json) ->
    ?TYPE = proplists:get_value(<<"g_type">>, Json),
    Id = proplists:get_value(?ID, Json),
    Rev = proplists:get_value(?REV, Json),
    From = proplists:get_value(?FROM, Json),
    To = proplists:get_value(?TO, Json),
    Body = proplists:get_value(?BODY, Json),
    Attachments = goethe_entity:clfjson(goethe_session,
                        proplists:get_value(?ATTACHMENTS, Json)),
    new(Id, Rev, From, To, Body, Attachments).
new(From, To, Body) -> new(nil, nil, From, To, Body, []).
new(From, To, Body, Attachments) -> new(nil, nil, From, To, Body, Attachments).
new(Id, Rev, From, To, Body, Attachments) -> {?MODULE, Id, Rev, From, To, Body, Attachments}.


save() ->
    {ok, WithId} = goethe:save(json()),
    new(WithId).


%%==========================================================================
%
%  public accessor functions
%
%%==========================================================================
get(from) -> {ok, From};
get(to) -> {ok, To};
get(body) -> {ok, Body};
get(attachments) -> {ok, Attachments};
get(_) -> {error, unknown_value}.

set(_, _) -> {error, unknown_value}.

add(_, _) -> {error, unknown_value}.

remove(_, _) -> {error, unknown_value}.


delete() ->
    ok.

json() ->
    {[
        {?ID,Id},
        {?REV,Rev},
        {?FROM,From},
        {?TO,To},
        {?BODY,Body},
        {?ATTACHMENTS,goethe_entity:cltjson(Attachments)},
        {<<"g_type">>, ?TYPE}
    ]}.