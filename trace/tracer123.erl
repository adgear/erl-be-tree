-module(tracer123).

%%
%% Module to facilitate FAQ - How to trace functions in remote Erlang node
%%

%% API
-export([
  observe/0,
  observe_to_file/2,
  observe_event_to_file/2,

  forward/1,
  trace_with_duration_limit/3,

  forward_with_count_limit/2,
  trace_with_count_limit/3
]).

observe() ->
  receive
    Msg ->
      io:format("[observe] ~p~n", [Msg]),
      observe()
  end.

observe_to_file(FileName, Count) ->
  receive
    Msg ->
      io:format("[observe_to_file] ~p~n", [Count+1]),
      file:write_file(FileName, [io_lib:format("~tw.~n", [Msg])], [append]),
      observe_to_file(FileName, Count+1)
  end.

observe_event_to_file(FileName, Count) ->
  receive
    {trace, _Pid, call, {erl_betree, betree_make_event, [_Betree, Event, _ClockType]}} ->
      io:format("[observe_event_to_file] ~p~n", [Count+1]),
      file:write_file(FileName, [io_lib:format("~tw.~n", [Event])], [append]),
      observe_event_to_file(FileName, Count+1);
    _ ->
      observe_event_to_file(FileName, Count)
  end.

forward(PidToForward) ->
  receive
    Msg ->
      PidToForward ! Msg,
      forward(PidToForward)
  end.

trace_with_duration_limit(PidTraceReceiver,
    DurationInSeconds,
    {_Module, _Function, _Arity} = Mfa) ->
  io:format("[trace_with_duration_limit] starting with trace receiver: ~p. Limited to ~p seconds ~n",
    [PidTraceReceiver, DurationInSeconds]),
  erlang:trace_pattern(Mfa, true, [global]),
  erlang:trace(all, true, [call, {tracer, PidTraceReceiver}]),
  timer:sleep(DurationInSeconds * 1_000),
  erlang:trace(all, false, [call]),
  io:format("[trace_with_duration_limit]  stopped with trace receiver: ~p~n", [PidTraceReceiver]).

forward_with_count_limit(_PidToForward, 0) ->
  io:format("[forward_with_count_limit]  stopped with trace receiver: ~p~n", [self()]),
  erlang:trace(all, false, [call]);
forward_with_count_limit(PidToForward, Count) ->
  receive
    Msg ->
      PidToForward ! Msg,
      forward_with_count_limit(PidToForward, Count - 1)
  end.

trace_with_count_limit(PidTraceReceiver,
    Count,
    {_Module, _Function, _Arity} = Mfa) ->
  io:format("[trace_with_count_limit] starting with trace receiver: ~p. Limited to ~p counts ~n",
    [PidTraceReceiver, Count]),
  erlang:trace_pattern(Mfa, true, [global]),
  erlang:trace(all, true, [call, {tracer, PidTraceReceiver}]).
