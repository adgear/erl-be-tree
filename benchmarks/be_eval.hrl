
-record(be_evaluator, {
  betree,
  consts,
  index
}).

-record(be_loader_stats, {
  start_time,
  current_time,
  index,
  snapshot_freq,
  initial_allocations,
  current_allocations,
  snapshot_allocations,
  allocation_diffs,
  initial_nano,
  current_nano,
  snapshot_nano,
  nano_diffs
}).

-record(be_evaluator_stats, {
  start_time,
  current_time,
  info,
  index,
  snapshot_freq,
  initial_allocations,
  current_allocations,
  snapshot_allocations,
  allocation_diffs,
  snapshot_nano_acc,
  nano_diffs
}).

-record(forest_config, {
  param_prefix,
  n_params,
  n_trees,
  file_name
}).

-record(be_tree_config, {
  params,
  consts, % for future
  exprs
}).

-record(events_config, {
  n_params,
  n_events,
  file_name
}).
