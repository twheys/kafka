-module(goethe_core).
-author('twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% Module outbound exports
-export([server_hello/1,send_error_code/3,pong/1]).
% Module inbound exports
-export([client_hello/1,ping/1,encrypt/2]).
% Module application exports
-export([register_node/3]).

% Module namespace
-define(NAME, server).

% Include the module records
-include("goethe.hrl").

% Module state
-record(state, {
privkey,pubkey
}).


start() ->  gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  outbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server_hello(Session) ->
    gen_server:call(?MODULE, {server_hello, {Session}}).
send_error_code(Session, Blame, Code) ->
    gen_server:call(?MODULE, {send_error_code, {Session, Blame, Code}}).
pong(Session) ->
    gen_server:call(?MODULE, {pong, {Session}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound messages api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client_hello(Session) ->
    gen_server:cast(?MODULE, {client_hello, {Session}}).
ping(Session) ->
    gen_server:cast(?MODULE, {ping, {Session}}).
encrypt(Session, Key) ->
    gen_server:cast(?MODULE, {encrypt, {Session, Key}}).
register_node(Session, Name, Port) -> 
    gen_server:cast(?MODULE, {register_node, {Session, Name, Port}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(_Args) ->
    logger:info("Loading Core API Module."),
    goethe:register_module(#module{
        name = ?NAME,
        emod = ?MODULE,
        actions=[
            #action{
                name = hello,
                efun = client_hello,
                arity = 1,
                roles = [plain]
            },
            #action{
                name = ping,
                efun = ping,
                arity = 1,
                roles = [all]
            },
            #action{
                name = encrypt,
                efun = encrypt,
                arity = 2,
                roles = [pcrypto]
            }
        ]
    }),
    {ok, #state{}}.

handle_call({server_hello, {Session}}, _, #state{pubkey=PubKey} = State) ->
    Session:send_msg(
        {[{<<"server.welcome">>,
            [
            {[{<<"msg">>,<<"Welcome to Goethe! Please encrypt...">>}]},
            {[{<<"pubkey">>,PubKey}]}
            ]
        }]}
    ),
    {reply, ok, State};

handle_call({send_error_code, {Session, Blame, Code}}, _, State) ->
    BlameToken = goethe_util:output("server.error.~p_side", [Blame]),
    logger:trace("Sending error code with blame ~p and code ~p", 
        [BlameToken, Code]),
    Session:send_msg(
        {[{
            list_to_binary(BlameToken),
            [
                {[{<<"code">>,atom_to_binary(Code, utf8)}]}
            ]
        }]}
    ),
    {reply, ok, State};

handle_call({pong, {Session}}, _, State) ->
    logger:debug("Sending pong to ~p", [Session]),
    Session:send_msg(
        {[{<<"action">>,<<"server.pong">>}]}
    ),
    {reply, ok, State};

handle_call(Req, _From, State) -> 
    logger:debug("goethe:handle_call catch all: ~p", [Req]),
    {reply, {error, no_match}, State}.


handle_cast({client_hello, {Session}}, #state{privkey=PrivKey} = State) -> 
    logger:info("Received get_apple from client"),
    spawn(?MODULE, server_hello, [Session]),
    Session:pencrypt(PrivKey),
    {noreply, State};

handle_cast({ping, {Session}}, State) -> 
    logger:info("Received ping from client"),
    % FIXME find a better solution.. events???
    spawn(?MODULE, pong, [Session]),
    {noreply, State};

handle_cast({encrypt, {Session, Key}}, State) -> 
    logger:info("Encrypting connection"),
    Session:fencrypt(Key),
    {noreply, State};

handle_cast({register_node, {Session, Name, Port}}, State) ->
    logger:info("Registering node"),
    {ok, {Address, _}} = inet:peername(Session:get(host)),
    case goethe:register_node(Name, Address, Port) of
    ok -> ok; % Send Node network status
    Other -> logger:info("Register Node response ~p", [Other])
    end,
    {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.