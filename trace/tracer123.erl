-module(tracer123).

%%
%% Module to facilitate FAQ - How to trace functions in remote Erlang node
%%

%%
%% $ export KUBECONFIG=~/.kube/use1-dsp-prod.yaml
%%
%% $ kubectl cp -n rtb-gateway -c gateway \
%%  /Users/v.savkov/projects/binary-expression/adgear/erl-be-tree/trace/tracer123.erl \
%%  gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz:/app/tracer123.erl
%%
%% $ kubectl -n rtb-gateway -c gateway \
%% exec --stdin --tty gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz -- /bin/sh
%%
%% $ bin/gateway remote

%% > RemoteNode = 'gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz'.

%% Check that remote node is reachable:
%% > net_adm:ping(RemoteNode).
%% 'pong' will be returned if the node is reachable,
%% 'pang' otherwise.

%% > Module = tracer123.

%% Check if the module is loaded:
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).

%% To delete module:
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:delete(Module)]) end).
%% Use 'code:soft_purge' if 'code:delete' returns 'false' and 'code:is_loaded' is still 'true
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:soft_purge(Module)]) end).
%% Now, 'code:delete' should returns 'true':
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:delete(Module)]) end).
%% And, 'code:is_loaded' should return 'false':
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).

%% To load module:
%% > c("tracer123.erl").
%% > Dir = "/app".
%% > spawn(RemoteNode, fun() -> code:add_path(Dir), io:format("~p~n", [code:load_file(Module)]) end).
%% Check that module has been loaded:
%% > spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).

%% To trace with output to console:
%% Start trace observer on local node:
%% > PidObserver = spawn(fun tracer123:observe/0).

%% To trace with output to file:
%% TraceFile = "/app/e.txt".
%% Start trace observer on local node:
%% > PidObserver = spawn(fun() -> tracer123:observe_to_file(TraceFile, 0) end).

%% To trace with output to file, BE-Tree events ony:
%% TraceFile = "/app/events.txt".
%% Start trace observer on local node:
%% > PidObserver = spawn(fun() -> tracer123:observe_event_to_file(TraceFile, 0) end).

%% Start trace receiver with count limit on the remote node.
%% The tracer receiver will forward trace messages from the remote node
%% to the trace observer on local node.
%% The trace is limited to the 1(one) call to trace:
%% > PidTraceReceiver = spawn(RemoteNode, fun() -> tracer123:forward_with_count_limit(PidObserver, 1) end).

%% Finally, start tracer itself.
%% The tracer traces:
%% - calls to 'erl_betree:betree_make_event/3';
%% - 1(one), number of calls to trace, is for information purpose only, the actual limit is set in the trace receiver above.
%% > PidTracer = spawn(RemoteNode, fun() -> tracer123:trace_with_count_limit(PidTraceReceiver, 1, {erl_betree, betree_make_event, 3}) end).

%% Start trace receiver with time limit on the remote node.
%% > PidTraceReceiver = spawn(RemoteNode, fun() -> tracer123:forward(PidObserver) end).
%% > PidTracer = spawn(RemoteNode, fun() -> tracer123:trace_with_duration_limit(PidTraceReceiver, 60, {erl_betree, betree_make_event, 3}) end).

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
