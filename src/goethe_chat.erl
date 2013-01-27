%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Goethe Chat Module
%%  
%%    This API is used for online chat.  Messages are delivered immediately
%%    and can only be sent to online users.
%%         
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(goethe_chat).
-author('twheys@gmail.com').
-behaviour(gen_server).

-export([start/0,start_link/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

% Module application exports
-export([send_system/2]).
% Module outbound exports
-export([send/5]).
% Module inbound exports
-export([recv/2]).

% Module namespace - Must be an atom.
-define(NAME, chat).

% Include the module records
-include("goethe.hrl").

% Module state
-record(state, {
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  module startup functions
%    Functions required for starting and stopping the application.
%    They do not need to be modified but can be if necessary.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  application functions api
%    Public API for application functions. These functions handle calls
%    from other modules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send_system(Session, Msg) ->
	{ok, {_, To}} = Session:get(principle),
	gen_server:call(?MODULE, {outgoing, {Session, "System", To, Msg, system}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  outbound messages api
%    Public API for Outbound Messages. These functions should accept
%    a session as a parameter and send some sort of message to that
%	 client.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
send(Session, From, To, Msg, Type) ->
	gen_server:call(?MODULE, {outgoing, {Session, From, To, Msg, Type}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  inbound messages api
%	 Public API for Inbound Messages. These messages receive incoming
%    calls directly from the client.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recv(Session, Msg) when is_list(Msg) ->
	gen_server:cast(?MODULE, {incoming, {Session, Msg}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server functions
%     Implementation of the Module, following the Generic Server design
%	  pattern.
%
%	  init description
%	    At this stage in development, modules need to register themselves
%	    on startup by calling goethe:register_module/1.
%		
%		json_name: The name used in the json "action" that maps to this
%		  function.
%		erl_name: The name of the erlang function to be called.
%		arity: The arity of the function.
%		role requiment: A list of authentication roles that the function
%		  is available in:
%		  plain: Plain text connection; User is connection but not
%		    encrypted nor authenticated.
%		  pcrypto: User is partially encrypted.  Only incoming messages
% 		    are encrypted.
%		  fcrypto: User is fully encrypted.  All messages are encrypted
% 		    but the user has not authenticated.
%		  auth: User is authenticated and has a session. Typically this
% 		    is the only role required in custom modules.
%		  all: Function is available to everyone.  Use this role with
% 		    care.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
init(_Args) ->
    goethe:register_module(#module{
        name = ?NAME,
        emod = ?MODULE,
        actions=[
            #action{
                name = send,
                efun = send_chat,
                arity = 2,
                roles = [auth,admin]
            }
        ]
    }),
    {ok, #state{}}.
    

handle_call({outgoing, {Session, From, To, Msg, Type}}, _, State) ->
	Session:send_msg(
        {[{<<"chat.incoming">>,
            [
            {[{<<"from">>,list_to_binary(From)}]},
            {[{<<"to">>,list_to_binary(To)}]},
            {[{<<"type">>,atom_to_binary(Type, utf8)}]},
            {[{<<"msg">>,list_to_binary(Msg)}]}
            ]
        }]}
    ),
    {reply, ok, State};

handle_call(_Req, _From, State) -> {reply, {error, no_match}, State}.


handle_cast({incoming, {Session, Msg}}, State) ->
	{ok, {_, From}} = Session:get(principle),
	case parse_chat(Msg) of
		{private, {To, Msg}} -> gen_server:handle_cast(?MODULE, {private, {Session, From, To, Msg}});
		{room, {Room, Msg}} -> gen_server:handle_cast(?MODULE, {room, {Session, From, Room, Msg}});
		{broadcast, {Msg}} -> gen_server:handle_cast(?MODULE, {broadcast, {Session, From, Msg}})
	end,
    {noreply, State};

handle_cast({send_private, {Session, From, To, Msg}}, State) ->
    case goethe_user:get_user(To) of 
    	{ok, Target} -> 
    		send(Target, From, To, Msg, private),
    		send(Session, From, To, Msg, private);
    	_ -> send_system(Session, io_lib:format("User does not exist: ~p", [To]))
    end,
    {noreply, State};

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> normal.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  util functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_chat(_Msg) ->
	% TODO implement me
	ok.