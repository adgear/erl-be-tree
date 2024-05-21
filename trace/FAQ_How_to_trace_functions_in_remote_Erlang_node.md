# FAQ - How to trace functions in remote Erlang node

Created based on
- [erlang:trace/3](https://www.erlang.org/doc/apps/erts/erlang.html#trace/3), [erlang:trace_pattern/3](https://www.erlang.org/doc/apps/erts/erlang.html#trace_pattern/3);
- and [recon_trace.erl](https://github.com/ferd/recon/blob/master/src/recon_trace.erl) source code.

As an example, demonstrates how to collect `boolean expressions` events using `erlang:trace/3` and `erlang:trace_pattern/3`.

## 1. Tracing functions in an `rtb-gateway` running in a kubernetes pod

### 1.1. Setup for tracing `rtb-gateway` in kubernetes pod

On your local machine create directory `trace`.

Copy file [trace123.erl](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/tracer123.erl)
 into the directory `trace`.

Make directory `trace` the current directory.

Let's assume that we will use

pod `gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz` in zone `use1`.
```shell
$ export KUBECONFIG=~/.kube/use1-dsp-prod.yaml
```

Copy file `trace123.erl` to the pod:
```shell
$ kubectl cp -n rtb-gateway -c gateway ./tracer123.erl gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz:/app/tracer123.erl
```
Connect to the pod:
```shell
$ kubectl -n rtb-gateway -c gateway exec --stdin --tty gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz -- /bin/sh
```
Start Interactive Elixir shell:
```shell
$ bin/gateway remote
iex(gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz)1>
```
Switch to Erlang shell:

- Press `Ctrl+G`
- Type in `s`<Enter>
```shell
--> s
```
- Type in `c 2`<Enter>
```shell
--> c 2
Eshell V14.2.5 (press Ctrl+G to abort, type help(). for help)
(rem-9240-gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz)1>
```
Now we have 2 Erlang nodes:
- `rem-9240-gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz`;
- `gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz`.

We will call

`rem-9240-gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz` the `local` node, or the `observer` node,

`gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz` the `remote` node, or the `gateway` node.

So, we are in the Eshell on the `local` node.

Compile `tracer123.erl`:
```erlang
(rem-9240-gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz)1> c("tracer123.erl").
{ok,tracer123}
```

On `observer` node set variable `RemoteNode`:
```erlang
> RemoteNode = 'gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz'.
```
Check that remote node is reachable: 'pong' will be returned if the node is reachable, 'pang' otherwise:
```erlang
> net_adm:ping(RemoteNode).
pong
```

### 1.2. Load module `tracer123` into remote node, i.e. into the node running `rtb-gateway`.

Set module variable:
```erlang
> Module = tracer123.
```
Set directory where the module is located:
```erlang
> Dir = "/app".
```
Load the module into the `gateway` node:
```erlang
> spawn(RemoteNode, fun() -> code:add_path(Dir), io:format("~p~n", [code:load_file(Module)]) end).
{module,tracer123}
```
If the response is
```erlang
{error,not_purged}
```
then it means that there is a module `tracer123` loaded in the`gateway` node already.

Check path from which this module is loaded:
```erlang
> spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).
```
If the response is
```erlang
{file,"/app/tracer123.beam"}
```
then it is safe to remove the previous version of the `tracer123` module.

How to do that, see [Section 1.5.](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/FAQ_How_to_trace_functions_in_remote_Erlang_node.md#15-remove-tracer123-module-from-the-remote-ie-gateway-node) below.

Then, repeat load the module into the `gateway` node:
```erlang
> spawn(RemoteNode, fun() -> code:add_path(Dir), io:format("~p~n", [code:load_file(Module)]) end).
{module,tracer123}
```

At this point we have module `tracer123` loaded into remote `gateway` node.

### 1.3. Start trace messages `observer`.

There are 3 types of `observers` provided:
- `observer` that prints out trace messages to console, `observe/0`;
- `observer` that save trace messages into file, `observe_to_file/2`;
- `observer` that saves `Event` parameter of `erl_betree:betree_make_event/3` from trace messages into file, `observe_event_to_file/2`.

We will use `observe_event_to_file/2`. The use of `observe/0` and `observe_to_file/2` is similar.

Start `observer` on the local node:
```erlang
> TraceFile = "/app/events.txt".
> PidObserver = spawn(fun() -> tracer123:observe_event_to_file(TraceFile, 0) end).
```

### 1.4. Perform tracing on the `remote`, i.e. `gateway`, node.

#### 1.4.1. Tracing on the `remote` node with the limit on the number of trace messages.

Set the limit to the number of trace messages:
```erlang
> NLimit = 3.
```
Start the `trace receiver` on the `gateway` node with the limit to the number of trace messages.

This will NOT initiate the trace:
```erlang
> PidTraceReceiver = spawn(RemoteNode, fun() -> tracer123:forward_with_count_limit(PidObserver, NLimit) end).
```
Start the `tracer` on the `gateway` node.

This WILL initiate the trace for `erl_betree:betree_make_event/3` calls:
```erlang
> PidTracer = spawn(RemoteNode, fun() -> tracer123:trace_with_count_limit(PidTraceReceiver, NLimit, {erl_betree, betree_make_event, 3}) end).
[trace_with_count_limit] starting with trace receiver: <PidTraceReceiver>. Limited to 3 counts
[observe_event_to_file] 1
[observe_event_to_file] 2
[forward_with_count_limit]  stopped with trace receiver: <PidTraceReceiver>
[observe_event_to_file] 3
```
At this point the `PidTraceReceiver` and `PidTracer` processes will be stopped.
```erlang
> spawn(RemoteNode, fun() -> io:format("PidTraceReceiver is_alive: ~p~n", [erlang:is_process_alive(PidTraceReceiver)]) end).
PidTraceReceiver is_alive: false
> spawn(RemoteNode, fun() -> io:format("PidTracer is_alive: ~p~n", [erlang:is_process_alive(PidTracer)]) end).
PidTracer is_alive: false
```
The collected events should be readable from file:
```erlang
> {ok, Events} = file:consult(TraceFile), ok.
```

Stop the `observer` on the `local` node:
```erlang
> exit(PidObserver, ok).
```

#### 1.4.2. Tracing on the `remote` node with the limit on the duration for how long the trace is active.

Set the limit to the number of trace messages. The limit is in seconds:
```erlang
> TLimit = 1.
```
Start the `trace receiver` on the `gateway` node to trace with time limit.

This will NOT initiate the trace:
```erlang
> PidTraceReceiver = spawn(RemoteNode, fun() -> tracer123:forward(PidObserver) end).
```
Start the `tracer` on the `gateway` node to trace with time limit.

This WILL initiate the trace for `erl_betree:betree_make_event/3` calls:
```erlang
> PidTracer = spawn(RemoteNode, fun() -> tracer123:trace_with_duration_limit(PidTraceReceiver, TLimit, {erl_betree, betree_make_event, 3}) end).
[trace_with_duration_limit] starting with trace receiver: <PidTraceReceiver>. Limited to 1 seconds
[observe_event_to_file] 1
[observe_event_to_file] 2
[observe_event_to_file] 3
...
[observe_event_to_file] 1201
[observe_event_to_file] 1202
[observe_event_to_file] 1203
[trace_with_duration_limit]  stopped with trace receiver: <PidTraceReceiver>
```
At this point the `PidTracer` process will be stopped:
```erlang
> spawn(RemoteNode, fun() -> io:format("PidTracer is_alive: ~p~n", [erlang:is_process_alive(PidTracer)]) end).
PidTracer is_alive: false
```
The `PidTraceReceiver` process will still be running and should be stopped:
```erlang
> spawn(RemoteNode, fun() -> io:format("PidTraceReceiver is_alive: ~p~n", [erlang:is_process_alive(PidTraceReceiver)]) end).
PidTraceReceiver is_alive: true
> spawn(RemoteNode, fun() -> io:format("PidTraceReceiver exit: ~p~n", [exit(PidTraceReceiver, ok)]) end).
PidTraceReceiver exit: true
> spawn(RemoteNode, fun() -> io:format("PidTraceReceiver is_alive: ~p~n", [erlang:is_process_alive(PidTraceReceiver)]) end).
PidTraceReceiver is_alive: false
```

The collected events should be readable from file:
```erlang
> {ok, Events} = file:consult(TraceFile), ok.
```

Stop the `observer` on the `local` node:
```erlang
> exit(PidObserver, ok).
```

### 1.5. Remove `tracer123` module from the `remote`, i.e. `gateway`, node.

Check if the module is loaded:
```erlang
> spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).
```
If the module is loaded, the response will be:
```erlang
{file,"/app/tracer123.beam"}
```
Request for `purge` first
```erlang
> spawn(RemoteNode, fun() -> io:format("~p~n", [code:soft_purge(Module)]) end).
true
```
Then, delete:
```erlang
> spawn(RemoteNode, fun() -> io:format("~p~n", [code:delete(Module)]) end).
true
```
Now, the check will indicate that the module is not loaded:
```erlang
> spawn(RemoteNode, fun() -> io:format("~p~n", [code:is_loaded(Module)]) end).
false
```

### 1.6. Exit Erlang shells and kubernetes pod.

To exit EShell:
```erlang
(rem-9240-gateway@gateway-use1-dsp-prod-us-east-1b-6bc94d45b8-9mjkz)>
```
press `Ctrl+G`
```erlang
-->
```
and type in `q`<Enter> to exit Eshells:
```erlang
--> q
```
then `exit`<Enter> to exit kubernetes pod:
```bash
$ exit
<your-local-prompt> $
```

## 2. Tracing functions in an Erlang node running on a local machine

This section is to practice what is described in [Section 1.](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/FAQ_How_to_trace_functions_in_remote_Erlang_node.md#1-tracing-functions-in-an-rtb-gateway-running-in-a-kubernetes-pod) on a local machine.

### 2.1. Setup with an `rtb-gateway` function running on a node on a local machine

On your local machine create directory `trace`.

Copy file [trace123.erl](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/tracer123.erl)
into the directory `trace`.

Copy mock version of [erl_betree.erl](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/erl_betree.erl) from the `erl-be-tree/trace`
into the directory `trace`.

Make directory `trace` the current directory.

From first terminal start Erlang node:
```shell
$ erl -sname observer
Eshell V14.0.2 (press Ctrl+G to abort, type help(). for help)
(observer@<your-local-machine-name>)1>
```
From the second terminal start another Erlang node:
```shell
$ erl -sname gateway
Eshell V14.0.2 (press Ctrl+G to abort, type help(). for help)
(gateway@<your-local-machine-name>)1>
```
We will call

`observer@<your-local-machine-name>` the `local` node, or `observer` node,

`gateway@<your-local-machine-name>` the `remote` node, or `gateway` node.

In `observer` node compile `tracer123.erl`:
```erlang
observer> c("tracer123.erl").
{ok,tracer123}
```

In `gateway` node compile `erl_betree.erl`:
```erlang
gateway> c("erl_betree.erl").
{ok,erl_betree}
```
On `observer` node set variable `RemoteNode`:
```erlang
observer> RemoteNode = 'gateway@<your-local-machine-name>'.
```
Check that remote node is reachable from `observer` node: 'pong' will be returned if the node is reachable, 'pang' otherwise:
```erlang
observer> net_adm:ping(RemoteNode).
pong
```
If the remote node is not reachable, disable your network connection and try `net_adm:ping` again.

### 2.2. Load module `tracer123` into remote node.

Set module variable:
```erlang
observer> Module = tracer123.
```
Set directory where the module is located:
```erlang
observer> Dir = ".".
```
Load the module into the `gateway` node:
```erlang
observer> spawn(RemoteNode, fun() -> code:add_path(Dir), io:format("~p~n", [code:load_file(Module)]) end).
{module,tracer123}
```

At this point we have module `tracer123` loaded into remote `gateway` node.

### 2.3. Start trace messages `observer`.

There are 3 types of `observers` provided:
- `observer` that prints out trace messages to console, `observe/0`;
- `observer` that save trace messages into file, `observe_to_file/2`;
- `observer` that saves `Event` parameter of `erl_betree:betree_make_event/3` from trace messages into file, `observe_event_to_file/2`.

We will use `observe_event_to_file/2`. The use of `observe/0` and `observe_to_file/2` is similar.

Start `observer` on the local node:
```erlang
observer> TraceFile = "./events.txt".
observer> PidObserver = spawn(fun() -> tracer123:observe_event_to_file(TraceFile, 0) end).
```

### 2.4. Perform tracing on the `remote`, i.e. `gateway`, node.

#### 2.4.1. Tracing on the `remote` node with the limit on the number of trace messages.

Set the limit to the number of trace messages:
```erlang
observer> NLimit = 3.
```
Start the `trace receiver` on the `gateway` node with the limit to the number of trace messages.

This will NOT initiate the trace:
```erlang
observer> PidTraceReceiver = spawn(RemoteNode, fun() -> tracer123:forward_with_count_limit(PidObserver, NLimit) end).
```
Start the `tracer` on the `gateway` node.

```erlang
> PidTracer = spawn(RemoteNode, fun() -> tracer123:trace_with_count_limit(PidTraceReceiver, NLimit, {erl_betree, betree_make_event, 3}) end).
[trace_with_count_limit] starting with trace receiver: <PidTraceReceiver>. Limited to 3 counts
```
On the `gateway` node call function `erl_betree:betree_make_event/3`:
```erlang
gateway> erl_betree:betree_make_event("Parameter1", {event, 1}, 0).
```
On the `observer` node see:
```erlang
[observe_event_to_file] 1
```
On the `gateway` node call function `erl_betree:betree_make_event/3` twice:
```erlang
gateway> erl_betree:betree_make_event("Parameter1", {event, 2}, 0).
gateway> erl_betree:betree_make_event("Parameter1", {event, 3}, 0).
```
On the `observer` node see:
```erlang
[observe_event_to_file] 2
[forward_with_count_limit]  stopped with trace receiver: <PidTraceReceiver>
[observe_event_to_file] 3
```
The collected events should be readable from file:
```erlang
observer> {ok, Events} = file:consult(TraceFile), ok.
observer> Events.
[{event,1},{event,2},{event,3}]
```

#### 2.4.2. Tracing on the `remote` node with the limit on the duration for how long the trace is active.

Similar to the [Section 1.4.2.](https://github.com/adgear/erl-be-tree/blob/benchmark/trace/FAQ_How_to_trace_functions_in_remote_Erlang_node.md#142-tracing-on-the-remote-node-with-the-limit-on-the-duration-for-how-long-the-trace-is-active)
