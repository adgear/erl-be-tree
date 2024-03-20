
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
