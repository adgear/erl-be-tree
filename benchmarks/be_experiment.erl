-module(be_experiment).
-include("be_eval.hrl").

%% API
-export([
  std_output_Ex_300_Ev_3_000/0,
  pipe_output_Ex_300_Ev_3_000/0,
  piped_with_cached_ids_output_Ex_300_Ev_3_000/0,

  cmp_output_std_vs_pipe/0,
  cmp_output_std_vs_piped_with_cached_ids/0,

  create_random_exprs_tester/0,
  forest_configs_to_be_tree_configs_tester/0,
  combine_be_trees_tester/0,
  combine_be_tree_configs_tester/0,
  prepare_be_tree_configs_tester/0,
  prepare_be_tree_tester/0,
  run_event_tester/0,
  run_event_2_parameters/0,
  run_events_until_error/3,
  run_events/3,
  run_event/2,
  run_event/4
]).

std_output_Ex_300_Ev_3_000() ->
  Exprs = "benchmarks/data/pipe/P0_128_Ex_300.txt",
  Events = "benchmarks/data/pipe/P_128_Ev_3_000.txt",
  Output = "benchmarks/data/pipe/output/output_std_Ex_300_Ev_3_000.txt",
  be_bm:std_output(Exprs, Events, Output).

pipe_output_Ex_300_Ev_3_000() ->
  Exprs1 = "benchmarks/data/pipe/P1_48_Ex_300.txt",
  Exprs2 = "benchmarks/data/pipe/P2_40_Ex_300.txt",
  Exprs3 = "benchmarks/data/pipe/P3_40_Ex_300.txt",
  Events = "benchmarks/data/pipe/P_128_Ev_3_000.txt",
  Output = "benchmarks/data/pipe/output/output_pipe_Ex_300_Ev_3_000.txt",
  be_bm:pipe_with_cached_ids_output([Exprs1, Exprs2, Exprs3], Events, Output).

piped_with_cached_ids_output_Ex_300_Ev_3_000() ->
  Exprs1 = "benchmarks/data/pipe/P1_48_Ex_300.txt",
  Exprs2 = "benchmarks/data/pipe/P2_40_Ex_300.txt",
  Exprs3 = "benchmarks/data/pipe/P3_40_Ex_300.txt",
  Events = "benchmarks/data/pipe/P_128_Ev_3_000.txt",
  Output = "benchmarks/data/pipe/output/output_piped_with_cached_ids_Ex_300_Ev_3_000.txt",
  be_bm:pipe_with_cached_ids_output([Exprs1, Exprs2, Exprs3], Events, Output).

cmp_output_std_vs_pipe() ->
  OutputStd = "benchmarks/data/pipe/output/output_std_Ex_300_Ev_3_000.txt",
  OutputPiped = "benchmarks/data/pipe/output/output_pipe_Ex_300_Ev_3_000.txt",
  be_bm:diff(OutputStd, OutputPiped).

cmp_output_std_vs_piped_with_cached_ids() ->
  OutputStd = "benchmarks/data/pipe/output/output_std_Ex_300_Ev_3_000.txt",
  OutputPiped = "benchmarks/data/pipe/output/output_piped_with_cached_ids_Ex_300_Ev_3_000.txt",
  be_bm:diff(OutputStd, OutputPiped).

create_random_exprs_tester() ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = 3},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = 2},
  ForestsAsExprs = be_tools:create_random_exprs([Cfg1, Cfg2], 10, []),
  ForestsAsExprs.

forest_configs_to_be_tree_configs_tester() ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = 3},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = 2},
  BtCfgs = be_tools:forest_configs_to_be_tree_configs([Cfg1, Cfg2], 10),
  BtCfgs.

combine_be_trees_tester() ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = 3},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = 2},
  BtCfgs = be_tools:forest_configs_to_be_tree_configs([Cfg1, Cfg2], 10),
  CombinedBeTrees = be_tools:combine_be_trees(BtCfgs),
  CombinedBeTrees.

combine_be_tree_configs_tester() ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = 3},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = 2},
  BtCfgs = be_tools:forest_configs_to_be_tree_configs([Cfg1, Cfg2], 10),
  CombinedBetreeCfg = be_tools:combine_be_tree_configs(BtCfgs),
  io:format("~p~n", [CombinedBetreeCfg]).

prepare_be_tree_configs_tester() ->
  Cfg1 = #forest_config{param_prefix = "p1_", n_params = 3},
  Cfg2 = #forest_config{param_prefix = "p2_", n_params = 2},
  BtCfgs = be_tools:forest_configs_to_be_tree_configs([Cfg1, Cfg2], 10),
  PreparedBetreeCfgs = be_tools:prepare_be_tree_configs(BtCfgs),
  io:format("~p~n", [PreparedBetreeCfgs]).

prepare_be_tree_tester() ->
  {{_N_params, [_Cfg1, _Cfg2, _Cfg3], _CfgCombined} = Cfgs,
    {[_Betree1, _Betree2, _Betree3], _BetreeCombined} = Betrees}
      = be_tools:prepare_be_trees(2, 10),
  io:format("~p~n", [Cfgs]),
  io:format("~p~n", [Betrees]).

run_event_tester() ->
  {{N_params, [_Cfg1, _Cfg2, _Cfg3], _CfgCombined} = _Cfgs,
    {[Betree1, Betree2, Betree3], BetreeCombined} = _Betrees}
    = be_tools:prepare_be_trees(2, 10),
  Event = [list_to_tuple([bool_event | be_tools:random_bool_event(N_params)])],
  io:format("Event: ~p~n", [Event]),

  {ok, Ids} = Ret_search = erl_betree:betree_search(BetreeCombined, Event),
  io:format("betree_search: ~p~n", [Ret_search]),

  {{ok, CompiledEvent}, _} = _Ret_make_event = erl_betree:betree_make_event(Betree1, Event),
%%  io:format("betree_make_event: ~p~n", [Ret_make_event]),

  {{ok, Ids1}, _} = Ret_search_with_event = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  io:format("betree_search with event: ~p~n", [Ret_search_with_event]),

  {{ok, Ids2}, _} = Ret_search_ids1 = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
  io:format("betree_search_ids1: ~p~n", [Ret_search_ids1]),

  {{ok, Ids3}, _} = Ret_search_ids2 = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
  io:format("betree_search_ids2: ~p~n", [Ret_search_ids2]),

  io:format(" std: ~p~n", [lists:sort(Ids)]),
  io:format("pipe: ~p~n", [lists:sort(Ids3)]).

run_event_2_parameters() ->
  {{N_params, [_Cfg1, _Cfg2, _Cfg3], _CfgCombined} = _Cfgs,
    {[Betree1, Betree2, Betree3], BetreeCombined} = _Betrees}
    = be_tools:prepare_be_trees(2, 10),
  AllEvents = be_tools:all_bool_events(N_params),
  io:format("Number of events: ~p~n", [length(AllEvents)]),
  lists:foreach(fun (Event) ->
      E = [list_to_tuple([bool_event | Event])],

      {ok, Ids} = erl_betree:betree_search(BetreeCombined, E),

      {{ok, CompiledEvent}, _} = _Ret_make_event = erl_betree:betree_make_event(Betree1, E),
      {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
      {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
      {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
      IdsSorted = lists:sort(Ids),
      Ids3Sorted = lists:sort(Ids3),
      case IdsSorted =:= Ids3Sorted of
        true -> true;
        _ ->
          io:format("Event: ~p~n", [Event]),
          io:format("  std: ~p~n", [IdsSorted]),
          io:format(" pipe: ~p~n", [Ids3Sorted]),
          false
      end
    end, AllEvents),
    ok.

report_discrepancy({Event, Ids, IdsFromPipe},
    {_N_params, [Cfg1, Cfg2, Cfg3], CfgCombined},
    ReportFile) ->
  {Ids1, Ids2, Ids3} = IdsFromPipe,

  IdsUnion = lists:uniq(lists:sort(lists:flatten(lists:append([Ids, Ids1, Ids2, Ids3])))),

  #be_tree_config{exprs = Exprs, params = Params} = CfgCombined,
  ExprSelected = [lists:nth(I, Exprs) || I <- IdsUnion],
  IdsExprs = lists:zip(IdsUnion, ExprSelected),

  #be_tree_config{exprs = Exprs1} = Cfg1,
  Expr1Selected = [lists:nth(I, Exprs1) || I <- IdsUnion],
  Ids1Exprs = lists:zip(IdsUnion, Expr1Selected),

  #be_tree_config{exprs = Exprs2} = Cfg2,
  Expr2Selected = [lists:nth(I, Exprs2) || I <- IdsUnion],
  Ids2Exprs = lists:zip(IdsUnion, Expr2Selected),

  #be_tree_config{exprs = Exprs3} = Cfg3,
  Expr3Selected = [lists:nth(I, Exprs3) || I <- IdsUnion],
  Ids3Exprs = lists:zip(IdsUnion, Expr3Selected),

  Report = [
    {params, Params},
    {event, Event},
    {std_ids, Ids},
    {p1_ids, Ids1},
    {p2_ids, Ids2},
    {p3_ids, Ids3},
    {std, IdsExprs},
    {p1, Ids1Exprs},
    {p2, Ids2Exprs},
    {p3, Ids3Exprs},
    {std_exprs, Exprs},
    {p1_exprs, Exprs1},
    {p2_exprs, Exprs2},
    {p3_exprs, Exprs3}
  ],
  io:format("Report:~n~p~n~p~n", [Ids, Ids3]),
  be_tools:write_terms(ReportFile, Report).

run_events_until_error(0, _N_pipe_step_params, _N_trees) ->
  ok;
run_events_until_error(N_runs, N_pipe_step_params, N_trees) ->
  OutputFlag = case N_runs rem 100 of
                 0 ->
                   io:format("Runs left: ~p~n", [N_runs]),
                   true;
                 _ -> false
               end,
  case run_events(N_pipe_step_params, N_trees, OutputFlag) of
    error ->
      {error, {runs_left, N_runs}};
    ok ->
      run_events_until_error(N_runs-1, N_pipe_step_params, N_trees)
  end.

run_events(N_pipe_step_params, N_trees, OutputFlag) ->
  {{N_params, [_Cfg1, _Cfg2, _Cfg3], _CfgCombined} = Cfgs,
    {[Betree1, Betree2, Betree3], BetreeCombined} = _Betrees}
    = be_tools:prepare_be_trees(N_pipe_step_params, N_trees),
  AllEvents = be_tools:all_bool_events(N_params),
  case OutputFlag of
    true ->
      io:format("Number of events: ~p~n", [length(AllEvents)]);
    _ -> ok
  end,
  case run_events(Betree1, Betree2, Betree3, BetreeCombined, AllEvents) of
    ok -> ok;
    {error, {_Event, _Ids, _IdsFromPipe} = EventIds} ->
      report_discrepancy(EventIds, Cfgs, "benchmarks/discrepancy_report.txt"),
      error
  end.
%%  lists:foreach(fun (Event) ->
%%    E = [list_to_tuple([bool_event | Event])],
%%
%%    {ok, Ids} = erl_betree:betree_search(BetreeCombined, E),
%%
%%    {{ok, CompiledEvent}, _} = _Ret_make_event = erl_betree:betree_make_event(Betree1, E),
%%    {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
%%    {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
%%    {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
%%    IdsSorted = lists:sort(Ids),
%%    Ids3Sorted = lists:sort(Ids3),
%%    case IdsSorted =:= Ids3Sorted of
%%      true -> true;
%%      _ ->
%%        io:format("Event: ~p~n", [Event]),
%%        io:format("  std: ~p~n", [IdsSorted]),
%%        io:format(" pipe: ~p~n", [Ids3Sorted]),
%%        false
%%    end
%%  end, AllEvents),

run_events(_Betree1, _Betree2, _Betree3, _BetreeCombined, []) ->
  ok;

run_events(Betree1, Betree2, Betree3, BetreeCombined, [Event | Rest]) ->
  E = [list_to_tuple([bool_event | Event])],

%%  {ok, Ids} = erl_betree:betree_search(BetreeCombined, E),
  Ids = run_event(BetreeCombined, E),

%%  {{ok, CompiledEvent}, _} = _Ret_make_event = erl_betree:betree_make_event(Betree1, E),
%%  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
%%  {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
%%  {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
  {_Ids1, _Ids2, Ids3} = IdsFromPipe = run_event(Betree1, Betree2, Betree3, E),
  IdsSorted = lists:sort(Ids),
  Ids3Sorted = lists:sort(Ids3),
  case IdsSorted =:= Ids3Sorted of
    true ->
      run_events(Betree1, Betree2, Betree3, BetreeCombined, Rest);
    _ ->
      {error, {Event, Ids, IdsFromPipe}}
  end.

run_event(Betree, Event) ->
  {ok, Ids} = erl_betree:betree_search(Betree, Event),
  Ids.

run_event(Betree1, Betree2, Betree3, Event) ->
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, Event),
  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
  {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
  {Ids1, Ids2, Ids3}.
