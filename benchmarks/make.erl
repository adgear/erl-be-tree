-module(make).

%%
%% Compile list of Erlang source files
%%

%% API
-export([
  all/0
]).

all() ->
  Files = [
    "benchmarks/term_reader.erl",
    "benchmarks/term_writer.erl",
    "benchmarks/term_eval.erl",
    "benchmarks/be_loader.erl",
    "benchmarks/be_eval.erl",
    "benchmarks/be_bm_utils.erl",
    "benchmarks/be_bm.erl",
    "benchmarks/be_loader_err.erl",
    "benchmarks/be_eval_err.erl"
  ],
  compile(Files, []).

compile([], Acc) ->
  lists:reverse(Acc);
compile([File | Rest], Acc) ->
  case compile:file(File) of
    {ok, Atom} ->
      compile(Rest, [Atom | Acc]);
    Err ->
      lists:reverse([Err | Acc])
  end.
