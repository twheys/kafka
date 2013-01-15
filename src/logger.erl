-module(logger).
-behaviour(gen_event).

-export([start_link/2]).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2,code_change/3]).

-export([fatal/1,warn/1,info/1,debug/1,trace/1]).

-record(state, {
logfile,level
}).

start_link(LogFile, LogLevel) -> 
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

handle_event({fatal, Msg}, #state{logfile=Fd,level=debug} = State) ->
    io:format(Fd, "[error] ~p~n", [Msg]),
    ok_state(State);
handle_event({warn, Msg}, #state{logfile=Fd,level=debug} = State) ->
    io:format(Fd, "[warn] ~p~n", [Msg]),
    ok_state(State);
handle_event({info, Msg}, #state{logfile=Fd,level=debug} = State) ->
    io:format(Fd, "[info] ~p~n", [Msg]),
    ok_state(State);
handle_event({debug, Msg}, #state{logfile=Fd,level=debug} = State) ->
    io:format(Fd, "[debug] ~p~n", [Msg]),
    ok_state(State);
handle_event({trace, Msg}, #state{logfile=Fd,level=debug} = State) ->
    io:format(Fd, "[trace] ~p~n", [Msg]),
    ok_state(State).
 
handle_call(_, State) -> {ok, ok, State}.
handle_info(_Info, State) -> ok_state(State).
terminate(_Args, #state{logfile=Fd}) -> 
    file:close(Fd),
    rm_handler_state().

code_change(_OldVsn, State, _Extra) -> ok_state(State).
