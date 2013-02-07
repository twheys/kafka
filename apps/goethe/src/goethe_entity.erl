-module(goethe_entity).
-author('Tim Heys twheys@gmail.com').

-export([cltjson/1,clfjson/2,rchild/2]).

%===========================================================================
%
%  API
%
%===========================================================================
-type json() :: {[{binary(), attribute() | json()}]}.
-type entity() :: term(). 
-type attribute() :: binary() | list() | entity(). 

-callback new(Json :: json()) -> Entity :: entity().
-callback get(Key :: atom()) -> {ok, Value :: attribute()} | {error, unknown_value}.
-callback set(Key :: atom(), Value :: attribute()) -> Entity :: entity() | {error, unknown_value}.
-callback add(Key :: atom(), Value :: attribute()) -> Entity :: entity() | {error, unknown_value}.
-callback remove(Key :: atom(), Value :: attribute()) -> Entity :: entity() | {error, unknown_value}.

-callback save() -> Entity :: entity() | {error, Reason :: atom()}.
-callback delete() -> ok | {error, Reason :: atom()}.
-callback json() -> json().


%===========================================================================
%
%  util functions
%
%===========================================================================
%% @doc child list to json
cltjson(CL) when is_list(CL) ->
	lists:map(fun(X) -> X:json() end, CL).
%% @doc child list from json
clfjson(T, CLJson) when is_atom(T), is_list(CLJson) ->
	lists:map(fun(Json) -> T:new(Json) end, CLJson).

%% @doc remove child
rchild(C, L) ->
	lists:filter(fun(X) -> X =/= C end, L).