-module(game).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([join/2,leave/2,wait/3,wait_finish/2,chat_message/2,priv_message/3,get_users/1,get_msg_id/2,find_idle_clients/0]).

% Time before a client is considered gone
-define(MAX_IDLE_TIME, 160).
-define(CHECK_IDLE_TIME, 60).

% Rate limiting for messages
-define(RATE_MSG_INTERVAL, 3).
-define(RATE_MSG_COUNT, 10).

% Max connections per host
-define(MAX_CONNECTIONS, 1).

-define(ADMIN_PASSWORD, "demopass").

-include_lib("stdlib/include/qlc.hrl").

-record(client_state, {
id,nick,host,last_action,admin=false,last_msg=never,msg_count=0
}).

-record(state, {
clients=[],bans_table
}).

-record(user_ban, {
host,last_nick,reason,until
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Nick, Host) -> gen_server:call(?MODULE, {join, {Nick, Host}}, infinity).
leave(Sess, Reason) -> gen_server:cast(?MODULE, {leave, {Sess, Reason}}).
chat_message(Sess, Msg) -> gen_server:cast(?MODULE, {chat_message, {Sess, Msg}}).
priv_message(Sess, Msg, Target) -> gen_server:cast(?MODULE, {priv_message, {Sess, Msg, Target}}).
wait(Sess, MsgID, Pid) -> gen_server:cast(?MODULE, {wait, {Sess, MsgID, Pid}}).
wait_finish(Sess, Pid) -> gen_server:cast(?MODULE, {wait_finish, {Sess, Pid}}).
get_users(Sess) -> gen_server:call(?MODULE, {get_users, Sess}, infinity).
get_msg_id(Sess, Pid) -> gen_server:cast(?MODULE, {get_msg_id, {Sess, Pid}}).
find_idle_clients() -> gen_server:cast(?MODULE, find_idle_clients).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  chat_room helpers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%is_banned(Host, #state{bans_table=Table}) ->
%    Now = now(),
%    case qlc:e(qlc:q([Ban || Ban <- dets:table(Table), Ban#user_ban.host == Host])) of
%        [] -> no;
%        [#user_ban{until=Until} | _] when Until >= Now -> {yes, Until};
%        _ -> no
%    end.

%validate_nick([], _) -> {error, bad_format};
%validate_nick(Nick, #state{clients=Clients}) ->
%    Shortened = list_to_binary(lists:sublist(string:strip(Nick), 16)),
%    case {re:run(binary_to_list(Shortened), "^([A-Za-z0-9]+)$"), lists:filter(fun(#client_state{nick=N}) -> N == Shortened end, Clients)} of
%        {{match, _}, []} -> {ok, Shortened};
%        {nomatch, _} -> {error, bad_format};
%        _ -> {error, not_available}
%    end.
	
get_session(SessionID, #state{clients=Clients}) ->
	case lists:filter(fun(#client_state{id=ID}) -> ID == SessionID end, Clients) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end.

get_user_by_nick(Nick, State) when is_list(Nick) -> get_user_by_nick(list_to_binary(Nick), State);
get_user_by_nick(Nick, #state{clients=Clients}) when is_binary(Nick) ->
	case lists:filter(fun(#client_state{nick=N}) -> N == Nick end, Clients) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end;
get_user_by_nick(_, _) -> {error, not_found}.

send_system_msg(Client, Msg) when is_binary(Msg) -> 
    goethe_server:send_chat({<<"SERVER">>, system_msg, Msg}, Client#client_state.id);
send_system_msg(Client, Msg) when is_list(Msg) -> send_system_msg(Client, list_to_binary(Msg));
send_system_msg(_, _) -> ok.

should_rate_limit(#client_state{last_msg=never} = C, State) -> 
    NewState = update_client(C#client_state{last_msg=now(),msg_count=1}, State),
    {no, NewState};
should_rate_limit(#client_state{last_msg=LastMsg,msg_count=Count} = C, State) ->
	RateSecs = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())) - calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(LastMsg)),
	case {RateSecs =< ?RATE_MSG_INTERVAL, (Count + 1) > ?RATE_MSG_COUNT} of
	    {true, true} -> {yes, update_client(C#client_state{msg_count=Count}, State)};
	    {false, _} -> {no, update_client(C#client_state{msg_count=0}, State)};
	    _ -> {no, update_client(C#client_state{msg_count=Count+1}, State)}
	end;
should_rate_limit(_, State) -> {no, State}.

% Decrements a host
host_disconnected(Host) ->
	case ets:lookup(conns, Host) of
		[] -> ok;
		[{_, Num}] when is_integer(Num) -> ets:insert(conns, {Host, Num - 1});
		_ -> ets:insert(conns, {Host, 0})
	end.

% Updates a host in connections ETS table
%update_host_conns(Host, Conns) -> ets:insert(conns, {Host, Conns}).

% Looks up host in connections ETS table
%can_connect(Host) ->
%	case ets:lookup(conns, Host) of
%		[] -> {yes, 0};
%		[{_, Num}] when Num < ?MAX_CONNECTIONS -> {yes, Num};
%		_ -> no
%	end.		

update_client(Client, #state{clients=Clients} = State) -> 
    NewClient = Client#client_state{last_action=now()},
    Others = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
    {NewClient, State#state{clients=[NewClient | Others]}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, BansTable} = dets:open_file("bans.dets", []),
	_ConnTable = ets:new(conns, [named_table]),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{ok, #state{bans_table=BansTable}}.

handle_call({join, {Nick, _Host}}, _From, State) when is_list(Nick) ->
    {reply, {error, not_available}, State};

handle_call({get_users, Sess}, _From, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> {reply, {error, not_found}, State};
        {ok, C} -> 
            {_, NewState} = update_client(C, State),
            {reply, {ok, lists:map(fun(#client_state{nick=Nick}) -> Nick end, State#state.clients)}, NewState}
    end;

handle_call(_Req, _From, State) -> {noreply, State}.


handle_cast(find_idle_clients, #state{clients=Clients} = State) ->
	lists:foreach(fun(Client) ->
		LastAction = calendar:now_to_datetime(Client#client_state.last_action),
		Now = calendar:now_to_datetime(now()),
		IdleSecs =  calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(LastAction),
		case IdleSecs > ?MAX_IDLE_TIME of
			true -> 
			    error_logger:info_msg("User timed out: ~p, secs: ~p", [Client#client_state.nick, IdleSecs]),
				timer:apply_after(0, ?MODULE, leave, [Client#client_state.id, "timeout"]);
			_ -> noop
		end
	end, Clients),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{noreply, State};
    
handle_cast({chat_message, {Sess, Msg}}, State) when is_list(Msg) and (length(Msg) > 0) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, #client_state{nick=Nick,id=ID} = C} ->
            CleanMsg = game_util:unicode_clean(lists:sublist(Msg, 256)),
            {NewC, NewState} = update_client(C, State),
                case should_rate_limit(NewC, NewState) of
                    {yes, {_, S2}} ->
                        send_system_msg(NewC, "You are sending messages to fast. Please wait a few seconds and try again."),
                        {noreply, S2};
                    {no, {RateClient, S2}} ->
                        goethe_server:broadcast({Nick, public, CleanMsg}, [ID]),
                        goethe_server:send_chat({public, CleanMsg}, [ID]),
                        {_, S3} = update_client(RateClient#client_state{last_msg=now()}, S2),
                        {noreply, S3}
                end
    end;

handle_cast({priv_message, {Sess, Msg, Target}}, State) when is_list(Msg) and (length(Msg) > 0) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, #client_state{nick=Nick,id=SenderID} = C} ->
            CleanMsg = game_util:unicode_clean(lists:sublist(Msg, 256)),
            {NewC, NewState} = update_client(C, State),
            case should_rate_limit(NewC, NewState) of
                {yes, {_, S2}} ->
                    send_system_msg(NewC, "You are sending messages to fast. Please wait a few seconds and try again."),
                    {noreply, S2};
                {no, {RateClient, S2}} ->
                    #client_state{nick=RecipientID} = get_user_by_nick(Target, State),
                    goethe_server:send_chat({Nick, private, CleanMsg}, RecipientID),
                    goethe_server:send_chat({private, CleanMsg}, SenderID),
                    {_, S3} = update_client(RateClient#client_state{last_msg=now()}, S2),
                    {noreply, S3}
            end
    end;

handle_cast({leave, {Sess, Reason}}, #state{clients=Clients} = State) when is_list(Reason) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, Client} ->
			host_disconnected(Client#client_state.host),
            goethe_server:end_session(Client#client_state.id),
            CleanReason =  game_util:unicode_clean(lists:sublist(Reason, 32)),
            goethe_server:broadcast({Client#client_state.nick, CleanReason}, Client#client_state.id),
            OtherClients = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
            {noreply, State#state{clients=OtherClients}}
    end;    

handle_cast(_Request, State) -> {noreply, State}.
	
	
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
