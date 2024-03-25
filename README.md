# erl_betree - `benchmark` branch for boolean expressions evaluation

## Overview

`benchmark` branch provides functions and benchmark data to
- run experiments related to boolean expressions evaluations;
- collect experiments statistics;
- compare statistics from different experiments;
- collect output data of experiments;
- compare output data from different experiments.

NOTE: The `benchmark` experiments depend only on
- Erlang `erl_betree` project;
- and C `be-tree` project.  

The following experiments are currently implemented:
- `baseline`: boolean expressions evaluation using `erl_betree:betree_search/2`;
- `with iterator`: evaluation using
  - `erl_betree:search_iterator/2` / `erl_betree:search_all/1`;
  - or `erl_betree:search_iterator/2` / `erl_betree:search_next/1`;
- `with event`: using `erl_betree:betree_make_event/2` / `erl_betree:betree_search/3`;
- `piped`: `erl_betree:betree_make_event/2` / `erl_betree:betree_search/3` / `erl_betree:betree_search_ids/4`.

The following data for experiments are provided (`benchmarks/data/boolean_expressions_and_events.zip`):
- 64 parameters, 5,000 boolean expressions, 100,000 events:
  
  directory `benchmarks/data`, files:
  - `P_64_Ex_5_000.txt` - boolean expressions;
  - `P_64_Ev_100_000.txt` - events;
- 96 parameters, 5,000 boolean expressions, 100,000 events:
  
  directory `benchmarks/data`, files:
  - `P_96_Ex_5_000.txt` - boolean expressions;
  - `P_96_Ev_100_000.txt` - events;
- 128 parameters, 5,000 boolean expressions, 100,000 events:
  
  directory `benchmarks/data`, files:
  - `P_128_Ex_5_000.txt` - boolean expressions;
  - `P_128_Ev_100_000.txt` - events.

For the `piped` experiments the data are in `benchmarks/data/pipe/pipe_boolean_expressions_and_events.zip`.

Instructions how to run experiments are below in corresponding chapters.

## Build

    $ rebar3 compile

## Test

    $ rebar3 eunit

## Run

To run experiments

    $ rebar3 shell
    > c("benchmarks/make.erl").
    > make:all().
    [term_reader,term_writer,term_eval,be_loader,be_eval,be_bm_utils,be_bm]

## Experiments

### Baseline
To collect `baseline` stats:

Evaluate 100,000 events on 5,000 boolean expressions with **_64_** parameters (will take about 20 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_64_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_64_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/base_P_64_Ex_5_000.txt".
> be_bm:std_stats(Exprs, Events, Stats).
```
Evaluate 100,000 events on 5,000 boolean expressions with **_96_** parameters (will take about 30 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_96_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_96_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/base_P_96_Ex_5_000.txt".
> be_bm:std_stats(Exprs, Events, Stats).
```
Evaluate 100,000 events on 5,000 boolean expressions with **_128_** parameters (will take about 40 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_128_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_128_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/base_P_128_Ex_5_000.txt".
> be_bm:std_stats(Exprs, Events, Stats).
```

### With iterator
To collect stats for `with iterator` experiment:

Evaluate 100,000 events on 5,000 boolean expressions with **_64_** parameters (will take about 25 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_64_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_64_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/iter_P_64_Ex_5_000.txt".
> be_bm:iterated_all_stats(Exprs, Events, Stats).
```
Evaluate 100,000 events on 5,000 boolean expressions with **_96_** parameters (will take about 30 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_96_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_96_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/iter_P_96_Ex_5_000.txt".
> be_bm:iterated_all_stats(Exprs, Events, Stats).
```
Evaluate 100,000 events on 5,000 boolean expressions with **_128_** parameters (will take about 40 min.):
```erlang
> f(Exprs), Exprs = "benchmarks/data/P_128_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_128_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/stats/iter_P_128_Ex_5_000.txt".
> be_bm:iterated_all_stats(Exprs, Events, Stats).
```

### Comparison `with iterator` and `baseline` experiments
Function `be_bm:compare_stats/2` compares experiments statistics.

Let's compare statistics of `baseline` experiment and `with iterator` experiment,  with **_128_** parameters.

```erlang
> Baseline = "benchmarks/data/stats/base_P_128_Ex_5_000.txt".
> WithIterator = "benchmarks/data/stats/iter_P_128_Ex_5_000.txt".
> be_bm:compare_stats(Baseline, WithIterator).
[{min,{relative,-13,'%'},{value,-1207,microsecond}},
{max,{relative,20,'%'},{value,1604,microsecond}},
{avg,{relative,-2,'%'},{value,-266,microsecond}},
{median,{relative,-1,'%'},{value,-160,microsecond}},
{'99th',{relative,3,'%'},{value,168,microsecond}},
{better,{84,'%'}}]
```

### Statistics explanation
An example for statistics explanation.

Imagine, that we have a `baseline` that is 100 numbers `10`, i.e.
```
10, 10, ..., 10
\             /
 -----100-----  
```
Also, we have a sequence of other 100 numbers, will call them `target`, that we want to compare with `baseline` such that

first 20 of them are 9s, next 20 are 10s, next 50 are 14s, and the rest are 16s, i.e.
```
9, ..., 9, 10, ..., 10, 14, ..., 14, 16, ..., 16
\       /   \        /   \        /  \         /
 --20---     ---20---     ---50---    ---10----
```
Then, the subtraction of the baseline from the `target` will give us sequence
```
-1, ...,-1, 0, ...,  0, 4, ...,   4, 6, ..., 6
\       /   \        /   \        /  \       /
 --20---     ---20---     ---50---    ---10--
```
Further, the division by the `baseline` values and expressing the result in percentages will produce
```
-10%, ...,-10%, 0, ...,  0, 40%, ..., 40%, 60%, ..., 60%
\           /   \        /   \          /  \           /
 ----20-----     ---20---     ----50----    -----10----
```
Such representation gives better picture how the `target` behaves relative to the `baseline`.

In this case we can say that
- the `target` deviates from the `baseline` in the `range` (-10%, 60%);
- the `median` is 40%;
- the `99th` is 60%;
- `better` is 20%, because 20 numbers out 100 are less than 0.

### Statistics comparison summary for `with iterator` and `baseline` experiments
`be_bm:compare_stats(Baseline, WithIterator)` produced output:
```erlang
[{min,{relative,-13,'%'},{value,-1207,microsecond}},
{max,{relative,20,'%'},{value,1604,microsecond}},
{avg,{relative,-2,'%'},{value,-266,microsecond}},
{median,{relative,-1,'%'},{value,-160,microsecond}},
{'99th',{relative,3,'%'},{value,168,microsecond}},
{better,{84,'%'}}]
```
which means that:
- (-13%, 20%) is the range the `with iterator` boolean expression evaluation deviates from `baseline`;
- (-1%) is the `median` deviation of the `with iterator` from `baseline`;
- 3% is the 99th percentile of the deviation `with iterator` from `baseline`;
- for 84% of events `with iterator` evaluated faster than `baseline`.


### `piped` experiment

#### `piped` processing details
For a boolean expression `P0` such that it can be represented as
conjunction of boolean expressions `P1`, `P2`, `P3`
with `and` operator, i.e.
```
P0 = P1 and P2 and P3
```
the result of evaluation `P0` can be obtained
by intersection of results of evaluations of `P1`, `P2`, `P3`.

The `piped` processing implements that the following way:
```
Diagram 1. 'Pipe' processing

 betree_make_event  +================+
   Event ---------->|                |     
                    |                |
 Compiled Event<----|                |
                    | P1-expressions |
 betree_search      |    BE-Tree     |
   Compiled Event-->|                |
                    |                |
 P1-exprs Ids<------|                | 
                    +================+
                    
 betree_search_ids      +================+
   Compiled Event-+     | P2-expressions |
                  |---->|    BE-Tree     |
   P1-exprs Ids---+     |                |
                        | P2-exprs Ids   |
                        | intersect with |
 P1P2-exprs Ids<--------| P1-exprs Ids   | 
                        +================+
                    
 betree_search_ids          +================+
   Compiled Event-+         | P3-expressions |
                  |-------->|    BE-Tree     |
   P1P2-exprs Ids-+         |                |
                            | P3-exprs Ids   |
                            | intersect with |
 P1P2P3-exprs Ids<----------| P1P2-exprs Ids | 
                            +================+
```
```
Diagram 2. 'Baseline' processing

 betree_search  +================+
   Event ------>|                |     
                | P0-expressions |
                |    BE-Tree     |
 P0-exprs Ids<--|                | 
                +===============-+
```

#### `piped` experiment running
Evaluate 100,000 events on 5,000 boolean expressions with **_128_** parameters (will take about 40 min.), where
- `P1_48_Ex_5_000.txt` contains 5,000 boolean expressions with **_48_** parameters;
- `P2_40_Ex_5_000.txt` - 5,000 boolean expressions with **_40_** parameters;
- `P3_40_Ex_5_000.txt` - 5,000 boolean expressions with **_40_** parameters.
```erlang
> f(Expr1), Expr1 = "benchmarks/data/pipe/P1_48_Ex_5_000.txt".
> f(Expr2), Expr2 = "benchmarks/data/pipe/P2_40_Ex_5_000.txt".
> f(Expr3), Expr3 = "benchmarks/data/pipe/P3_40_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_128_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/pipe/stats/pipe_P_128_Ex_5_000.txt".
> be_bm:pipe_stats([Expr1, Expr2, Expr3], Events, Stats).
```
Evaluate 100,000 events on 5,000 boolean expressions with **_128_** parameters (will take about 40 min.), where
```erlang
> f(Exprs), Exprs = "benchmarks/data/pipe/P0_128_Ex_5_000.txt".
> f(Events), Events = "benchmarks/data/P_128_Ev_100_000.txt".
> f(Stats), Stats = "benchmarks/data/pipe/stats/base_P_128_Ex_5_000.txt".
> be_bm:std_stats(Exprs, Events, Stats).
```

#### Statistics comparison for `piped` and `baseline` experiments
```erlang
> Baseline = "benchmarks/data/pipe/stats/base_P_128_Ex_5_000.txt".
> Piped = "benchmarks/data/pipe/stats/pipe_P_128_Ex_5_000.txt".
> be_bm:compare_stats(Baseline, Piped).
[{min,{relative,39,'%'},{value,2795,microsecond}},
{max,{relative,73,'%'},{value,4624,microsecond}},
{avg,{relative,57,'%'},{value,3739,microsecond}},
{median,{relative,55,'%'},{value,3713,microsecond}},
{'99th',{relative,72,'%'},{value,4562,microsecond}},
{better,{0,'%'}}]
```
which means that:
- (39%, 73%) is the range the `piped` boolean expression evaluation deviates from `baseline`;
- 55% is the `median` deviation of the `piped` from `baseline`;
- 72% is the 99th percentile of the deviation `piped` from `baseline`;
- 0% of events in the `piped` experiment evaluated faster than `baseline`.
