-module(be_tools).
-include("be_eval.hrl").

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
  create_random_exprs/3,
  forest_configs_to_be_tree_configs/2,
  combine_be_trees/1,
  combine_be_tree_configs/1,
  prepare_be_tree_configs/1,
  prepare_be_trees/2,
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
  all_bool_events/1,
  sort_increasing/1,
  zip_many/2,
  concatenate_and/1
]).

read_expr_file(File) ->
  {ok, Exprs} = file:consult(File),
  Exprs.

write_terms(File, Ts) ->
  Lines = [io_lib:format("~tp.~n", [T]) || T <- Ts],
  file:write_file(File, Lines).

random_trees(Prefix, N_parameters, N_trees) ->
  [be:mk_random_tree(Prefix, N_parameters) || _ <- lists:seq(1, N_trees)].

create_random_exprs([] = _Configs, _N_trees_in_forest, ForestsAsExprs) ->
  lists:reverse(ForestsAsExprs);
create_random_exprs([#forest_config{
  param_prefix = Prefix, n_params = N_params} | Rest] = _Configs,
    N_trees_in_forest, ForestsAsExprs) ->
  Forest = be_tools:random_trees(Prefix, N_params, N_trees_in_forest),
  Exprs = [be:pp(T) || T <- Forest],
  create_random_exprs(Rest, N_trees_in_forest, [Exprs | ForestsAsExprs]).

forest_configs_to_be_tree_configs(ForestConfigs, N_trees_in_forest) ->
  ForestsAsExprs = be_tools:create_random_exprs(ForestConfigs, N_trees_in_forest, []),
  [#be_tree_config{
    params = [be:enum_prefix(Prefix, I) || I <- lists:seq(1, N_params)],
    exprs = Exprs} ||
    {#forest_config{param_prefix = Prefix, n_params = N_params}, Exprs}
      <- lists:zip(ForestConfigs, ForestsAsExprs)].

combine_be_trees(BetreeConfigs) ->
  ListOfListsOfExprs = [Exprs || #be_tree_config{exprs = Exprs} <- BetreeConfigs],
  Concatenated = be_tools:concatenate_and(ListOfListsOfExprs),
  Concatenated.

combine_be_tree_configs(BetreeConfigs) ->
  CombinedBeTrees = combine_be_trees(BetreeConfigs),
  {Params, Consts} = lists:unzip(
    [{P, case C =:= undefined of true -> []; _ -> C end}
      || #be_tree_config{params = P, consts = C} <- BetreeConfigs]),
  #be_tree_config{
    params = lists:flatten(Params),
    consts = lists:flatten(Consts),
    exprs = CombinedBeTrees
  }.

prepare_be_tree_configs(BetreeConfigs) ->
  #be_tree_config{params = Params} = CombinedBetreeConfig = combine_be_tree_configs(BetreeConfigs),
  N_params = length(Params),
  Typed = [{P, bool, disallow_undefined} || P <- Params],
  CombinedBetreeConfigTyped = CombinedBetreeConfig#be_tree_config{params = Typed},
  BetreeConfigsWithUpdatedParams = [
    BtCfg#be_tree_config{params = Typed, consts = case C =:= undefined of true -> []; _ -> C end}
    || #be_tree_config{consts = C} = BtCfg <- BetreeConfigs],
  {N_params, BetreeConfigsWithUpdatedParams, CombinedBetreeConfigTyped}.

prepare_be_trees(NumberOfTreeParameters, NumberOfTreesInForest) ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = NumberOfTreeParameters},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = NumberOfTreeParameters},
  Cfg3 = #forest_config{param_prefix = "p3_", n_params = NumberOfTreeParameters},
  BtCfgs = be_tools:forest_configs_to_be_tree_configs([Cfg1, Cfg2, Cfg3], NumberOfTreesInForest),
  Cfgs = {_N_params, [BetreeCfg1, BetreeCfg2, BetreeCfg3], CombinedBetreeConfig} =
    be_tools:prepare_be_tree_configs(BtCfgs),

  Betree1 = mk_be_tree(BetreeCfg1),
  Betree2 = mk_be_tree(BetreeCfg2),
  Betree3 = mk_be_tree(BetreeCfg3),
  CombinedBetree = mk_be_tree(CombinedBetreeConfig),
  Betrees = {[Betree1, Betree2, Betree3], CombinedBetree},
  {Cfgs, Betrees}.

mk_be_tree(#be_tree_config{params = P, consts = C, exprs = Exprs}) ->
  {ok, Betree} = erl_betree:betree_make([P]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree, I, C, E),
    ok = erl_betree:betree_insert_sub(Betree, Sub)
                end, lists:enumerate(Exprs)),
  Betree.

%% Example:
%%  create 5,000 boolean expressions
%%  with 48 parameters "par1_1", "par1_2", ..., "par1_48"
%%  and write into file "P_48_Ex_5_000.txt".
%% > be_tools:write_random_trees_to_file("par1_", 48, 5_000, "examples/P_48_Ex_5_000.txt").
write_random_trees_to_file(Prefix, N_parameters, N_trees, File) ->
  Trees = random_trees(Prefix, N_parameters, N_trees),
  Exprs = [be:pp(T) || T <- Trees],
  Params = [
%%    {list_to_atom(Prefix ++ integer_to_list(I)), bool, disallow_undefined}
    {be:enum_prefix(Prefix, I), bool, disallow_undefined}
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

all_bool_events(0) ->
  [];
all_bool_events(1) ->
  [[true], [false]];
all_bool_events(N_parameters) ->
  all_bool_events_acc(N_parameters -1, [[true], [false]]).

all_bool_events_acc(0, Acc) ->
  Acc;
all_bool_events_acc(N_parameters, Acc) ->
  AccT = [[true | X] || X <- Acc],
  AccF = [[false | X] || X <- Acc],
  all_bool_events_acc(N_parameters-1, lists:append([AccT, AccF])).

sort_increasing(Exprs) ->
  lists:reverse(
    lists:sort(
      [{size(E), E} || E <- Exprs])).

%%          L1   L2 ...  LN ->  L
%% Combine(L1_1 L2_1 ... LN_1) -> L_1
%% Combine(L1_2 L2_2 ... LN_2) -> L_2
%% ...
%% Combine(L1_M L2_M ... LN_M) -> L_M
zip_many(_Combine, []) ->
  [];
zip_many(_Combine, [[]]) ->
  [[]];
zip_many(Combine, ListOfLists) ->
  zip_many_acc(Combine,
    ListOfLists, _ListOfListsAcc = [],
    _RowAcc = [], _ResultListAcc = []).

zip_many_acc(_Combine,
    [[] | _RestOfListOfLists], % i.e. L1_(M+1), get out of recursion
    _ListOfListsAcc,
    _RowAcc, ResultListAcc = []) ->
  [ResultListAcc];
zip_many_acc(_Combine,
    [[] | _RestOfListOfLists], % i.e. L1_(M+1), get out of recursion
    _ListOfListsAcc,
    _RowAcc, ResultListAcc) ->
  lists:reverse(ResultListAcc);
zip_many_acc(Combine,
    _RestOfListOfLists = [], ListOfListsAcc,
    RowAcc, ResultListAcc) ->
  Combined = Combine(lists:reverse(RowAcc)),
  zip_many_acc(Combine,
    lists:reverse(ListOfListsAcc), _ListOfListsAcc = [],
    _RowAcc = [], [Combined | ResultListAcc]);
zip_many_acc(Combine,
    [[Item | RestOfList] | RestOfListOfLists], ListOfListsAcc,
    RowAcc, ResultListAcc) ->
  zip_many_acc(Combine,
    RestOfListOfLists, [RestOfList | ListOfListsAcc],
    [Item | RowAcc], ResultListAcc).

concatenate_and([]) ->
  <<>>;
concatenate_and(ListOfListOfBinary) ->
  zip_many(fun and_binaries/1, ListOfListOfBinary).

and_binaries([]) ->
  <<>>;
and_binaries([Bin]) ->
  Bin;
and_binaries([Bin | Rest]) ->
  lists:foldl(fun (E, B) ->
    be_bm_utils:append_expression('and', B, E) end,
    <<$(, Bin/binary, $)>>, Rest).
