-module (web_util).

-export([get_parameter/2,get_parameter_int/2]).
-export([get_template/2]).

get_parameter(N, [{K,V}|_]) when K == N -> V;
get_parameter(N, [_|T]) -> get_parameter(N, T);
get_parameter(_, _) -> [].

get_parameter_int(Name, X) ->
	case string:to_integer(get_parameter(Name, X)) of
		{error, _} -> 0;
		{Y, _} -> Y
	end.

get_template(Name, Vars) ->
	erlydtl:compile("templates/" ++ Name ++ ".html", list_to_atom(Name)),
	{ok, Tpl} = erlang:apply(list_to_atom(Name), render, [Vars]),
	list_to_binary(Tpl).