# v1.5.1 (2026-07-10)

## Removed

Dropped several code paths that have no callers in any downstream consumer,
to shrink the NIF surface area ahead of further optimization work:

- **`TRACE_LAST_VAR` instrumentation** — the `#ifdef TRACE_LAST_VAR` debug
  blocks in the NIF C layer (see the "Conditional tracing" note under
  v1.5.0 below; the underlying be-tree submodule also drops this).
- **`*_err` error-reason API** — `betree_make_err/1`, `betree_make_event_err/2,3`,
  `betree_make_sub_err/4`, `betree_insert_sub_err/2`, `betree_search_err/2,3`,
  `betree_search_ids_err/3,4`, `betree_parse_reasons/1`, `betree_write_dot_err/2`,
  and the dedicated `betree_search_reason_tests.erl` suite.
- **Yield-variant search API** — `betree_search_yield/2,3`, `search_yield_count/2,5`,
  `search_yield/3,4`, `search_next_yield/3`, `betree_search_ids_yield/3,4`,
  `search_ids_yield/5`, and `betree_yield_tests.erl`.
- **`search_iterator` API** — `search_iterator/2`, `search_next/1`, `search_all/1`,
  `search_iterator_release/1`, and `betree_iterator_test.erl`. This was the
  Erlang-scheduler-cooperation mechanism described in
  `Erlang_Scheduler_friendly_processing_with_BE_Tree.md` (now removed along
  with the feature); the counting-based reduction bookkeeping it relied on
  is removed from the be-tree submodule as well.
- **`*_ids` subset-search API** — `betree_search_ids/3,4` and the NIF
  functions backing them, plus the corresponding ids-specific test cases in
  `betree_search_tests.erl`. This path restricted matching to a
  caller-supplied subset of subscription IDs; the underlying be-tree
  submodule also drops its ids-search implementation.
- **be-tree submodule bumped to `v1.5.2`**, which drops the corresponding
  `TRACE_LAST_VAR`, `_counting`, `*_err`, and `*_ids` code paths on the C
  side (see be-tree's own `CHANGES.md` for details).

---

# v1.5.0 (2026-06-26)

## Summary

This branch extends the erl-be-tree NIF with domain ranking, per-subscription
and per-group no-match statistics tracking, a debug search mode that reports
per-subscription pass/fail reasons, and a session-based group statistics accumulator.
Build infrastructure is modernized by replacing the ad-hoc clone script with a git
submodule and a top-level Makefile.

---

## Build System

- **Git submodule**: `be-tree` is now a git submodule (pinned at `v1.5.1` from
  `github.com/x0id/be-tree`) instead of being cloned at build time by
  `c_src/build_betree`. The old clone script and `rebar3` binary are removed.
- **Makefile**: New top-level `Makefile` wraps `rebar3 compile/eunit/clean` with
  automatic `git submodule update --init --recursive`.
- **rebar.config**: Pre/post hooks updated to `make -C be-tree NIF=true` and
  `make -C be-tree clean`.

---

## New Erlang API Functions

| Function | Description |
|----------|-------------|
| `betree_make/2` | Create betree with domain ranking map |
| `betree_add_sub/4` | Add subscription (make + insert in one call) |
| `betree_add_sub/5` | Add subscription with group ID |
| `betree_prepare_subs/1` | Allocate internal stats arrays after all subs are inserted |
| `betree_search_debug/2,3` | Search returning per-sub pass/fail with failure-reason atoms |
| `betree_search_stats/2,3` | Search accumulating no-match statistics |
| `betree_stats/1,2` | Retrieve per-subscription stats map (optional atomic reset) |
| `betree_group_stats/1,2` | Retrieve per-group stats map (optional atomic reset) |
| `betree_stats_start/1` | Start a group stats accumulator session |
| `betree_stats_stop/1` | Stop session, transfer counts to global group stats |
| `betree_stats_stop_return/1` | Stop session, return `#{Var => [GroupId]}` map |
| `betree_group_vars/1` | Return `#{GroupId => [VarAtom]}` — variables used by each group |
| `betree_print/1` | Print betree structure to stderr (debug/IO-bound) |

---

## NIF Layer (c_src/betree.c)

### Resource model refactoring

- The NIF resource is now `struct betree_resource` wrapping the raw `struct betree*`
  plus subscription index, group mappings, and statistics counters.
- New `MEM_SEARCH_STATS` resource type for the session-based stats accumulator.
- `cleanup_betree` updated to free all associated arrays and the separately-allocated
  `struct betree`.

### Domain ranking

- `betree_make/2` accepts an optional `#{AtomName => Rank}` map.
- `add_domains()` calls `betree_add_ranked_*_variable()` variants passing
  per-variable rank values.
- After adding domains, `attr_var.data` is set to the corresponding Erlang atom for
  efficient variable-name lookup during callbacks.

### Subscription indexing and groups

- `struct sub_info` stores `sub_id`, `group_idx`, and a direct pointer to
  `betree_sub`.
- `index_sub()` dynamically grows the subscription index and assigns the sub a slot.
- `find_or_add_global_group()` maintains a deduplicated group ID array in the
  betree resource.
- `betree_prepare_subs()` allocates `subs_data`, `sub_stats`, and `group_stats`
  arrays.

### Search variants

- **`betree_search_debug`**: Uses `acc_ret`/`acc_arr` callbacks that populate
  thread-local `subs[]` and `rets[]` arrays. Returns `{ok, [SubId], [Reason]}`
  where reason is `ok` for pass or the failing variable atom.
- **`betree_search_stats`**: Uses `update_stats`/`bulk_update_stats` callbacks that
  atomically increment per-sub failure counters and collect matches.
- **`betree_search_stats` with accumulator**: When called with a `stats_resource`,
  uses `update_group_stats`/`bulk_update_group_stats` callbacks. Records the
  *first* failing variable per group per search (value 0 = no decision, 1 = passed,
  var_idx+2 = first failure reason).

### Statistics retrieval

- `build_stats_map()`: Returns `#{SubId => #{VarAtom => Count}}`, optionally
  resetting counters atomically.
- `build_group_stats_map()`: Returns `#{GroupId => #{VarAtom => Count}}`.
- `build_accumulator_stats_map()`: Returns `#{VarAtom => [GroupId]}` — groups
  grouped by their first failing variable.

### Group variable introspection

- `nif_betree_group_vars()`: Walks all subs, ORs their `attr_vars` bitmasks per
  group, then returns `#{GroupId => [VarAtom]}`.

### Thread-local storage

- `subs`, `rets`, `allocated_size`, `subs_count` are `__thread`-local dynamic
  arrays shared across debug/stats search variants, avoiding per-call allocation.

### NIF variant selection

- Two `ErlNifFunc` tables: `nif_functions` (normal) and `nif_functions_` (dirty
  schedulers for search). Selected at load time via `DIRTY_SEARCH` environment
  variable using a custom `ERL_NIF_INIT_` macro.

### Robustness

- All `enif_alloc`/`enif_alloc_resource` calls now check for NULL and return
  `{error, mem_alloc_failed}`.
- `match_be_tree()` call signature updated to pass `config` and an optional
  report pointer.

---

## Conditional tracing (`TRACE_LAST_VAR`)

When compiled with `-DTRACE_LAST_VAR`, the group stats callbacks emit
`fprintf(stderr, ...)` traces for a hardcoded flight ID (`TRACE_FLIGHT_ID`),
showing per-callback pass/fail decisions. This is a debug aid, not enabled in
normal builds.
