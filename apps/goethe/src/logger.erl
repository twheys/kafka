-module(logger).
-author('Tim Heys twheys@gmail.com').
-behaviour(gen_event).

-export([start/0,start_link/0]).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2,code_change/3]).

-export([fatal/1,warn/1,info/1,debug/1,trace/1]).
-export([fatal/2,warn/2,info/2,debug/2,trace/2]).

-record(state, {
logfile,level
}).

start() ->
    LogFile = application:get_env(log_level),
    LogLevel = application:get_env(log_file),
    gen_event:start({local, logger}),
    gen_event:add_handler(logger, ?MODULE, [LogFile, LogLevel]),
    {ok, self()}.

start_link() ->
    LogFile = application:get_env(log_level),
    LogLevel = application:get_env(log_file),
    gen_event:start_link({local, logger}),
    gen_event:add_handler(logger, ?MODULE, [LogFile, LogLevel]),
    {ok, self()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  public api
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fatal(Msg) -> gen_event:notify(logger, {fatal, Msg}).
warn(Msg) -> gen_event:notify(logger, {warn, Msg}).
info(Msg) -> gen_event:notify(logger, {info, Msg}).
debug(Msg) -> gen_event:notify(logger, {debug, Msg}).
trace(Msg) -> gen_event:notify(logger, {trace, Msg}).

fatal(Msg, Params) when is_list(Params) -> gen_event:notify(logger, {fatal, Msg, Params}).
warn(Msg, Params) when is_list(Params) -> gen_event:notify(logger, {warn, Msg, Params}).
info(Msg, Params) when is_list(Params) -> gen_event:notify(logger, {info, Msg, Params}).
debug(Msg, Params) when is_list(Params) -> gen_event:notify(logger, {debug, Msg, Params}).
trace(Msg, Params) when is_list(Params) -> gen_event:notify(logger, {trace, Msg, Params}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  work functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ok_state(State) -> {ok, State, hibernate}.
rm_handler_state() -> remove_handler.
report_error(Reason) -> {error, Reason}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_event exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([File, LogLevel]) ->
    case file:open(File, [append]) of
    {ok, Fd} -> ok_state(#state{logfile=Fd,level=LogLevel});
    {error, Reason} -> report_error(Reason)
    end.

% TODO need to check logger level before logging
handle_event({_LogLevel, Msg}, #state{logfile=_Fd,level=_FilterLogLevel} = State) ->
    error_logger:info_msg(Msg),
    ok_state(State);
handle_event({_LogLevel, Msg, Params}, #state{logfile=_Fd,level=_FilterLogLevel} = State) ->
    error_logger:info_msg(Msg, Params),
    %OutputMsg = lists:flatten(io_lib:format(Msg, Params)),
    %io:format(Fd, "[~p] ~p~n", [LogLevel, OutputMsg]),
    ok_state(State).
 
handle_call(_, State) -> {ok, ok, State}.
handle_info(_Info, State) -> ok_state(State).
terminate(_Args, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> ok_state(State).