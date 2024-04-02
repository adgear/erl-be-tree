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
  prepare_be_tree_configs_tester/0
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