-module(goethe_user).
-author('twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% Module application exports
-export([get_user/1]).
% Module outbound exports
-export([confirm_registration/2]).
% Module inbound exports
-export([register/4,login/3]).

% Module namespace
-define(NAME, auth).

-record(state, {
cookies=[]
}).
-record(principle, {
id,user,session
}).


start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  application functions api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_user(UserName) ->
    gen_server:handle_call(?MODULE, {get_user, {UserName}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  outbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
confirm_registration(Session, UserName) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
register(Session, UserName, Password, Email) ->
    gen_server:handle_cast(?MODULE, {register, {Session, UserName, Password, Email}}).
login(Session, UserName, Password) ->
    gen_server:handle_cast(?MODULE, {login, {Session, UserName, Password}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(_Args) ->
    logger:info("Loading User API Module."),
    goethe:register_module(
    	{?NAME, ?MODULE, 
        {functions, [
    		{register, {register, 4, [fcrypto]}},
    		{login, {login, 3, [all]}}
    	]}}
    ),
    {ok, #state{}}.

handle_call(_Req, _From, State) -> {noreply, State}.


handle_cast({confirm_registration, {Session, UserName}}, State) ->
    Msg = goethe_util:output("Registration for ~p complete, please log in!", [UserName]),
    Session:send_msg(
        {[{<<"user.registered">>,
            [
            {[{<<"msg">>,Msg}]}
            ]
        }]}
    ),
    {noreply, State};

handle_cast({register, {Session, UserName, _Password, _Email}}, State) ->
    confirm_registration(Session, {ok, UserName}),
    {noreply, State};
    
handle_cast({login, {Session, UserName, _Password}}, #state{cookies=Cookies} = State) ->
    SessionId = goethe_util:generate_hash(),
    NewCookie = #principle{id=SessionId, user=UserName},
    Session:authenticate({SessionId, UserName}),
    {noreply, State#state{cookies=[NewCookie | Cookies]}};

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
