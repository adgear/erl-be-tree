-module(be_tools).

%% To use:
%% 1. Run `rebar3 shell';
%% 2. `> c("examples/be_tools.erl").`
%% 3. `> be_tools:random_trees("par", 10, 3)`.
%% 4. `> be_tools:write_random_trees_to_file("par", 10, 3, "./FileName")`.

%% API
-export([
  read_expr_file/1,
  write_terms/2,
  random_trees/3,
  write_random_trees_to_file/4,
  write_forest_union_to_file/3,
  write_forest_union_to_file/4,
  write_random_0_1_events_to_file/3,
  write_forest_union_random_0_1_events_to_files/3,
  write_forest_union_random_0_1_events_to_files/4,
  random_0_1_events/2,
  random_0_1_event/1,
  write_random_bool_events_to_file/3,
  random_bool_events/2,
  random_bool_event/1,
  random_bool/0,
  sort_increasing/1
]).

-record(forest_config, {
  param_prefix,
  n_params,
  n_trees,
  file_name
}).

-record(events_config, {
  n_params,
  n_events,
  file_name
}).

read_expr_file(File) ->
  {ok, Exprs} = file:consult(File),
  Exprs.

write_terms(File, Ts) ->
  Lines = [io_lib:format("~tp.~n", [T]) || T <- Ts],
  file:write_file(File, Lines).

random_trees(Prefix, N_parameters, N_trees) ->
  [be:mk_random_tree(Prefix, N_parameters) || _ <- lists:seq(1, N_trees)].

%% Example:
%%  create 5,000 boolean expressions
%%  with 48 parameters "par1_1", "par1_2", ..., "par1_48"
%%  and write into file "P_48_Ex_5_000.txt".
%% > be_tools:write_random_trees_to_file("par1_", 48, 5_000, "examples/P_48_Ex_5_000.txt").
write_random_trees_to_file(Prefix, N_parameters, N_trees, File) ->
  Trees = random_trees(Prefix, N_parameters, N_trees),
  Exprs = [be:pp(T) || T <- Trees],
  Params = [
    {list_to_atom(Prefix ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_parameters)],
  be_tools:write_terms(File, [Params | Exprs]).

%% Usage:
%% > F1 = #forest_config{param_prefix = "p1_", n_params = 10, n_trees = 10, file_name = "examples/F1.txt"}.
%% > F2 = #forest_config{param_prefix = "p2_", n_params = 20, n_trees = 10, file_name = "examples/F2.txt"}.
%% > be_tools:write_forest_union_to_file(F1, F2, F3, "examples/F.txt").
write_forest_union_to_file(
    #forest_config{
      param_prefix = Prefix1, n_params = N_params1,
      n_trees = N_trees, file_name = FN1},
    #forest_config{
      param_prefix = Prefix2, n_params = N_params2,
      n_trees = N_trees, file_name = FN2},
    UnionForestFile) ->

  F1 = random_trees(Prefix1, N_params1, N_trees),
  F2 = random_trees(Prefix2, N_params2, N_trees),
  Zipped = lists:zip(F1, F2),
  F = [be:ast_and(T1, T2) || {T1, T2} <- Zipped],

  P1 = [
    {list_to_atom(Prefix1 ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_params1)],
  P2 = [
    {list_to_atom(Prefix2 ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_params2)],
  P = lists:flatten([P1, P2]),

  Ex1 = [be:pp(T) || T <- F1],
  be_tools:write_terms(FN1, [P1 | Ex1]),
  Ex2 = [be:pp(T) || T <- F2],
  be_tools:write_terms(FN2, [P2 | Ex2]),
  Ex = [be:pp(T) || T <- F],
  be_tools:write_terms(UnionForestFile, [P | Ex]);

write_forest_union_to_file(_, _, _) ->
  {error, wrong_parameters}.

%% Usage:
%% > F1 = #forest_config{param_prefix = "p1_", n_params = 10, n_trees = 10, file_name = "examples/F1.txt"}.
%% > F2 = #forest_config{param_prefix = "p2_", n_params = 20, n_trees = 10, file_name = "examples/F2.txt"}.
%% > F3 = #forest_config{param_prefix = "p3_", n_params = 30, n_trees = 10, file_name = "examples/F3.txt"}.
%% > be_tools:write_forest_union_to_file(F1, F2, "examples/F.txt").
write_forest_union_to_file(
    #forest_config{
      param_prefix = Prefix1, n_params = N_params1,
      n_trees = N_trees, file_name = FN1},
    #forest_config{
      param_prefix = Prefix2, n_params = N_params2,
      n_trees = N_trees, file_name = FN2},
    #forest_config{
      param_prefix = Prefix3, n_params = N_params3,
      n_trees = N_trees, file_name = FN3},
    UnionForestFile) ->

  F1 = random_trees(Prefix1, N_params1, N_trees),
  F2 = random_trees(Prefix2, N_params2, N_trees),
  F3 = random_trees(Prefix3, N_params3, N_trees),
  Zipped = lists:zip3(F1, F2, F3),
  F = [be:ast_and(be:ast_and(T1, T2), T3) || {T1, T2, T3} <- Zipped],

  P1 = [
    {list_to_atom(Prefix1 ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_params1)],
  P2 = [
    {list_to_atom(Prefix2 ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_params2)],
  P3 = [
    {list_to_atom(Prefix3 ++ integer_to_list(I)), bool, disallow_undefined}
    || I <- lists:seq(1, N_params3)],
  P = lists:flatten([P1, P2, P3]),

  Ex1 = [be:pp(T) || T <- F1],
  be_tools:write_terms(FN1, [P1 | Ex1]),
  Ex2 = [be:pp(T) || T <- F2],
  be_tools:write_terms(FN2, [P2 | Ex2]),
  Ex3 = [be:pp(T) || T <- F3],
  be_tools:write_terms(FN3, [P3 | Ex3]),
  Ex = [be:pp(T) || T <- F],
  be_tools:write_terms(UnionForestFile, [P | Ex]);

write_forest_union_to_file(_, _, _, _) ->
  {error, wrong_parameters}.

%% Example:
%%  write 3,000 random boolean, 0/1, events,
%%  each event consisting of 128 parameters, i.e.
%%  [0, 1, 1, ..., 0, 1, 0]
%%  \____________________/
%%           128
%% > be_tools:write_random_0_1_events_to_file(128, 3_000, "./P_128_Ev_3_000.txt").
write_random_0_1_events_to_file(N_parameters, N_events, File) ->
  Events = random_0_1_events(N_parameters, N_events),
  be_tools:write_terms(File, Events).

%% Usage:
%% > E1 = #events_config{n_params = 10, n_events = 100, file_name = "examples/E1.txt"}.
%% > E2 = #events_config{n_params = 20, n_events = 100, file_name = "examples/E2.txt"}.
%% > be_tools:write_forest_union_random_0_1_events_to_files(E1, E2, "examples/E.txt").
write_forest_union_random_0_1_events_to_files(
    #events_config{n_params = N_params1, n_events = N_events, file_name = FN1},
    #events_config{n_params = N_params2, n_events = N_events, file_name = FN2},
    UnionEventsFile) ->
  Events1 = random_0_1_events(N_params1, N_events),
  be_tools:write_terms(FN1, Events1),
  Events2 = random_0_1_events(N_params2, N_events),
  be_tools:write_terms(FN2, Events2),

  Zipped = lists:zip(Events1, Events2),
  Events = [lists:flatten([E1, E2]) || {E1, E2} <- Zipped],
  be_tools:write_terms(UnionEventsFile, Events);

write_forest_union_random_0_1_events_to_files(_, _, _) ->
  {error, wrong_parameters}.

%% Usage:
%% > E1 = #events_config{n_params = 10, n_events = 100, file_name = "examples/E1.txt"}.
%% > E2 = #events_config{n_params = 20, n_events = 100, file_name = "examples/E2.txt"}.
%% > E3 = #events_config{n_params = 30, n_events = 100, file_name = "examples/E3.txt"}.
%% > be_tools:write_forest_union_random_0_1_events_to_files(E1, E2, E3, "examples/E.txt").
write_forest_union_random_0_1_events_to_files(
    #events_config{n_params = N_params1, n_events = N_events, file_name = FN1},
    #events_config{n_params = N_params2, n_events = N_events, file_name = FN2},
    #events_config{n_params = N_params3, n_events = N_events, file_name = FN3},
    UnionEventsFile) ->
  Events1 = random_0_1_events(N_params1, N_events),
  be_tools:write_terms(FN1, Events1),
  Events2 = random_0_1_events(N_params2, N_events),
  be_tools:write_terms(FN2, Events2),
  Events3 = random_0_1_events(N_params3, N_events),
  be_tools:write_terms(FN3, Events3),

  Zipped = lists:zip3(Events1, Events2, Events3),
  Events = [lists:flatten([E1, E2, E3]) || {E1, E2, E3} <- Zipped],
  be_tools:write_terms(UnionEventsFile, Events);

write_forest_union_random_0_1_events_to_files(_, _, _, _) ->
  {error, wrong_parameters}.

random_0_1_events(N_parameters, N_events) ->
  [random_0_1_event(N_parameters) || _ <- lists:seq(1, N_events)].

random_0_1_event(N_parameters) ->
  [case rand:uniform(2) of 1 -> 1; _ -> 0 end
    || _ <- lists:seq(1, N_parameters)].

write_random_bool_events_to_file(N_parameters, N_events, File) ->
  Events = random_bool_events(N_parameters, N_events),
  be_tools:write_terms(File, Events).

random_bool_events(N_parameters, N_events) ->
  [random_bool_event(N_parameters) || _ <- lists:seq(1, N_events)].

random_bool_event(N_parameters) ->
  [random_bool() || _ <- lists:seq(1, N_parameters)].

random_bool() ->
  case rand:uniform(2) of
    1 -> true;
    _ -> false
  end.

sort_increasing(Exprs) ->
  lists:reverse(
    lists:sort(
      [{size(E), E} || E <- Exprs])).
