%%==========================================================================
%===========================================================================
%
%  Goethe Chat Module
%  
%    This API is used for online chat.  Messages are delivered immediately
%    and can only be sent to online users.
%         
%
%===========================================================================
-module(goethe_chat).
-author('Tim Heys twheys@gmail.com').
-behaviour(goethe_module).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_internal/2,handle_inbound/4,handle_event/3,terminate/2,code_change/3]).

% Module application exports
-export([send_chat/5,
		 add_handler/2]).

% Module namespace - Must be an atom.
-define(NAME, chat).

% Module state
-record(state, {
handlers=[]
}).
-record(chat_handler, {
name,callback
}).


%%==========================================================================
%
%  module startup functions
%    Functions required for starting and stopping the application.
%    They do not need to be modified but can be if necessary.
%
%%==========================================================================
start() -> goethe_module:start(?NAME, ?MODULE, [], []).
start_link() -> goethe_module:start_link(?NAME, ?MODULE, [], []).


%%==========================================================================
%
%  init
%    Handles creation of a Module record and other necessary
%  terminate
%    Called when the application shuts down.
%  code change
%    Called when a code update occurs
%
%%==========================================================================
init(_Args) ->
	PrivateChat = fun(Text, From, Role) ->
			case parse_private_chat(Text) of
				{ok, {To, Body}} -> private_chat(From, To, Body, Role);
				{error, parse_error} -> {nack, {<<"parse_error">>}}
			end
		end,
	{ok, #state{handlers=[#chat_handler{name=private_chat,callback=PrivateChat}]}}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%==========================================================================
%
%  internal api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%==========================================================================
send_chat(Target, From, Type, Body, SenderRole)
    	when is_binary(From), is_atom(Type), is_binary(Body) ->
	goethe_module:internal(?NAME, {send_chat, {Target, From, Type, Body, SenderRole}}).

add_handler(Name, Callback) when is_atom(Name), is_function(Callback) ->
	goethe_module:internal(?NAME, {add_handler, {Name, Callback}}).


%%==========================================================================
%
%  internal api impl
%    The implementation for the Public API in the previous section.
%
%%==========================================================================
handle_internal({send_chat, {Target, From, Type, Body, SenderRole}}, State) ->
    logger:debug("Delivering chat ~p -> ~p(~p) [~p]", [From, Target, Type, Body]),
	Target:send_Text(
        {[{<<"chat.deliver">>,
            {[
                {<<"from">>,From},
                {<<"type">>,Type},
                {<<"body">>,Body}
                | case goethe_auth:is_admin(SenderRole) of
                    true -> [{<<"is_admin">>,<<"true">>}];
                    _ -> []
                  end
            ]}
        }]}
    ),
    {ok, State};

handle_internal({add_handler, {Name, Callback}}, #state{handlers=Handlers} = State) ->
    {ok, State#state{handlers=[#chat_handler{name=Name,callback=Callback} | Handlers]}};

handle_internal(_Request, _State) -> no_match.


%%==========================================================================
%
%  inbound api
%    Public API for Inbound Messages. These functions receive incoming
%    messages directly from the client.
%
%%==========================================================================
handle_inbound(chat, {[
            {<<"text">>, Text}
        ]}, Session, #state{handlers=Handlers} = State) ->
	{ok, From} = Session:get(name),
	{ok, Role} = Session:get(role),
    logger:debug("~p-> ~p", [From, Text]),
	spawn(lists, foreach, [fun(Action, Fun) ->
			case catch Fun(Text, From, Role) of
			    ack -> goethe:ack(Session, Action);
			    {ack} -> goethe:ack(Session, Action);
			    {ack, Reply} when is_list(Reply) -> goethe:ack(Session, Action, Reply);
			    {ack, Reply} -> goethe:ack(Session, Action, [Reply]);
			
			    {nack, {Code, Reply}} when is_list(Reply) -> goethe:nack(Session, Action, {Code, Reply});
			    {nack, {Code, Reply}} -> goethe:nack(Session, Action, {Code, [Reply]});
			    {nack, Code} -> goethe:nack(Session, Action, Code);
				_ -> ok %ignore
			end
		end, Handlers]),
    {ok, State};

handle_inbound(_Action, _Data, _Session, _State) -> no_match.


%%==========================================================================
%
%  events api
%    Public API for Events. These functions are called when the expected
%    event occurs, such as start up or user authentication.
%
%%==========================================================================
handle_event(_Event, _Data, _State) -> no_match.


%%==========================================================================
%
%  util functions
%
%%==========================================================================
parse_private_chat("/w " ++ Text) when 0 < length(Text) ->
    Pos = string:str(Text, " "),
    if
       Pos > 1, length(Text) > (Pos + 1) ->
            To = string:substr(Text, 1, Pos - 1),
            Body = string:substr(Text, Pos+1),
            {ok,
                {list_to_binary(To),
                list_to_binary(Body)}};  
       true ->
            {error, parse_error}
    end.

private_chat(From, To, Body, Role) ->
    case goethe:get_session_by_username(To) of
    {ok, Target} ->
        spawn(goethe_chat, send_chat, [Target, From, private, Body, Role]),
        {ack, [
            {<<"to">>,To},
            {<<"type">>,private},
            {<<"body">>,Body}]};
    {error, not_found} ->
        {nack, {<<"not_found">>, {<<"user">>, To}}}
    end.