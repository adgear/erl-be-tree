# erl-be-tree benchmarks

From `erl-be-tree` project root directory run
```shell
$ rebar3 shell
```
To compile the benchmark runner
```erlang
> c("benchmarks/make.erl").
> make:all().
[term_reader,term_writer,term_eval,be_loader,be_eval,be_bm_utils,be_bm]
```

`be_bm` is a module containing benchmark runner functions.

Unpack files 'P_20_Ex_300.txt' and 'P_20_Ev_3_000.txt' from  `P_20.tar.gz`:
- 'P_20_Ex_300.txt' contains 300 `boolean expressions` with 20 parameters;
- 'P_20_Ev_3_000.txt' contains 3,000 events.

## To run benchmarks using `erl_betree:betree_search/2`:
```erlang
> be_bm:std("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt").
```
### save statistics into a file:
```erlang
> be_bm:std_stats("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt", "benchmarks/stats_std.txt").
```
### save Ids into a file:
```erlang
> be_bm:std_output("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt", "benchmarks/output_std.txt").
```
## To run benchmarks using `erl_betree:search_iterator/search_all`:
```erlang
> be_bm:iterated_all("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt").
```
### save statistics into a file:
```erlang
> be_bm:iterated_all_stats("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt", "benchmarks/stats_all.txt").
```
### save Ids into a file:
```erlang
> be_bm:iterated_all_output("benchmarks/P_20_Ex_300.txt", "benchmarks/P_20_Ev_3_000.txt", "benchmarks/output_all.txt").
```

## To verify that `erl_betree:betree_search/2` and `erl_betree:search_iterator/search_all` produce the same output:
```erlang
> be_bm:diff("benchmarks/output_std.txt", "benchmarks/output_all.txt").
{3000,[]}
```
`{3000,[]}` indicates that the results of evaluations of 3,000 events were compared, no differences were found.


## To compare statistics for `erl_betree:betree_search/2` and `erl_betree:search_iterator/search_all`:
```erlang
> be_bm:compare_stats("benchmarks/stats_std.txt", "benchmarks/stats_all.txt").
```
