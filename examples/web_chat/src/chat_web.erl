-module(chat_web).

-export([start_link/0]).

-export([loop/1]).

-export([wait_msg_id/1,wait/1,timeout_wait/1]).

-define(COMET_TIMEOUT, 90000).

start_link() -> mochiweb_http:start([{port, 34987}, {loop, {?MODULE, loop}}]).

% Response Helpers
html_ok(Req, Data) -> Req:ok({"text/html;charset=UTF-8", Data}).

% Request handlers
    
handle_request(Req, "/chat/send_msg/") ->
    client:chat_message(get_session(Req), web_util:get_parameter("msg", Req:parse_post())),
    json_respond(json_client_ok(<<"">>), Req);

handle_request(Req, "/chat/send_priv_msg/") ->
    Msg = web_util:get_parameter("msg", Req:parse_post()),
    Target = web_util:get_parameter("target", Req:parse_post()),
    client:priv_message(get_session(Req), Msg, Target),
    json_respond(json_client_ok(<<"">>), Req);

handle_request(Req, "/chat/leave/") -> 
    client:leave(get_session(Req), "normal"),
    json_respond(json_client_ok(<<>>), Req);

handle_request(Req, "/chat/wait/") ->
    MsgID = web_util:get_parameter_int("msg_id", Req:parse_qs()),
    client:wait(get_session(Req), MsgID, self()),
    timer:apply_after(?COMET_TIMEOUT, ?MODULE, timeout_wait, [self()]),
    proc_lib:hibernate(?MODULE, wait, [Req]);

handle_request(Req, "/chat/start/") ->
    client:get_msg_id(get_session(Req), self()),
    proc_lib:hibernate(?MODULE, wait_msg_id, [Req]);
    
handle_request(Req, "/chat/online/") ->
    case client:get_users(get_session(Req)) of
        {ok, Users} -> json_respond(json_client_ok(Users), Req);
        _ -> bad_session(Req)
    end;

handle_request(Req, "/login/") ->
    Post = Req:parse_post(),
	case client:join(web_util:get_parameter("nick", Post), Req:get(peer)) of
	    {ok, SessID} -> 
	        SessCookie = mochiweb_cookies:cookie("chat_sess", SessID, [{path, "/"}]),
	        Req:respond({302, [SessCookie, {"Location", "/chat/"}], <<>>});
		{error, too_many_conns} -> html_ok(Req, web_util:get_template("index", [{error, "Your host has too many connections to the chat server."}]));
		{error, not_available} -> html_ok(Req, web_util:get_template("index", [{error, "The nickname is not available."}]));
		{error, {banned, Until}} ->
		    TimeStr = web_util:time_interval_str(Until),
		    html_ok(Req, web_util:get_template("index", [{error, "You are banned from the chat server. Time remaining: " ++ TimeStr}]));
	    _ -> html_ok(Req, web_util:get_template("index", [{error, "The nickname must be alphanumeric and not blank."}]))
	end;

handle_request(Req, Path) ->
	Req:serve_file(string:sub_string(Path, 2), "web", []).
	
loop(Req) ->
	catch case Req:get(version) of
	    Version when Version >= {1, 0} -> 
	        Path = Req:get(path),
	        handle_request(Req, Path);
	    _ -> ok
	end.
