-module(make).

%%
%% Compile list of Erlang source files
%%

%% API
-export([
  all/0
]).

-define(SRC, [
  "benchmarks/term_reader.erl",
  "benchmarks/term_writer.erl",
  "benchmarks/term_eval.erl",
  "benchmarks/be_loader.erl",
  "benchmarks/be_eval.erl",
  "benchmarks/be.erl",
  "benchmarks/be_tools.erl",
  "benchmarks/be_bm_utils.erl",
  "benchmarks/be_bm.erl",
  "benchmarks/be_experiment.erl",
  "benchmarks/be_bm_test.erl"
]).

-define(MODULES, [
  term_reader,
  term_writer,
  term_eval,
  be_loader,
  be_eval,
  be,
  be_tools,
  be_bm_utils,
  be_bm,
  be_experiment,
  be_bm_test
]).

all() ->
  compile(?SRC, []).

compile([], Acc) ->
  lists:reverse(Acc);
compile([File | Rest], Acc) ->
  case compile:file(File) of
    {ok, Atom} ->
      compile(Rest, [Atom | Acc]);
    Err ->
      lists:reverse([Err | Acc])
  end.
