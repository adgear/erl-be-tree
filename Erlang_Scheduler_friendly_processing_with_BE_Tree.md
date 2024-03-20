# Erlang Scheduler-friendly Processing with BE-Tree

[1] emphasizes that "it is of vital importance that a native function returns relatively fast".

`erl_betree:betree_search/2` function might take long time to complete.

This document describes an approach to represent the `erl_betree:betree_search/2` functionality 
with shorter execution time calls to native functions.

# Contents

1. [Introduction](https://github.com/adgear/erl-be-tree/blob/PEDSP-2941/Erlang_Scheduler_friendly_processing_with_BE_Tree.md#introduction)

2. [Implementation](https://github.com/adgear/erl-be-tree/blob/PEDSP-2941/Erlang_Scheduler_friendly_processing_with_BE_Tree.md#implementation)

3. [Benchmarks](https://github.com/adgear/erl-be-tree/blob/PEDSP-2941/Erlang_Scheduler_friendly_processing_with_BE_Tree.md#benchmarks)

4. [Resources](https://github.com/adgear/erl-be-tree/blob/PEDSP-2941/Erlang_Scheduler_friendly_processing_with_BE_Tree.md#resources)


# Introduction

The implementation follows recommendations from [1], Section "Yielding NIF",
and project `jiffy, JSON NIFs for Erlang`, [2].

There are two essential goals to be achieved
by representing the `erl_betree:betree_search/2` functionality
with a series of shorter NIF calls:

1. Provide information to the Eralng Runtime System (ERTS)
   how many `reductions` were consumed during the NIF call;
2. Give the Erlang Scheduler an opportunity to schedule another process for execution
   if the NIF call consumed all allocated `reductions`.

The details about the Scheduler and Reductions are in [3] and [4].

# Implementation

## Shorter NIF calls

Currently `erl_betree:betree_search/2` performs matching of an `event` and `boolean expressions`
and returns list of matched `boolean expressions Ids`.

[5], [9.7-10. Boolean Expressions: Evaluation](https://github.com/adgear/beval?tab=readme-ov-file#9-boolean-expressions-evaluation)
describes that the matching process consists of two steps:

1. Select `boolean expression candidates` for evaluation. On this step some `boolean expressions` are rejected without evaluation;
2. Evaluate the selected `boolean expression candidates`.
```
 erl_betree:        +------------+   +-----------+
 betree_search/2--->|            |-->|           |-->--+ select
 (Betree, Event)    | erl_betree |   | be_tree   |     | expression
                    |   NIF      |   | C library |<----+ candidates
                    |  C code    |   |           |
                    |            |   |           |-->--+ evaluate
                    |            |   |           |     | candidate 1
                    |            |   |           |<----+
                    |            |   |           | ...
                    |            |   |           |-->--+ evaluate
                    |            |   |           |     | candidate N
                    |            |   |           |<----+
 Ids <--------------|            |<--|           |
                    +------------+   +-----------+
```

The implementation extends `erl_betree` with the following functions:

1. `search_iterator/2` - to perform selection `boolean expression candidates` for evaluation;
```
 erl_betree:        +------------+   +-----------+
 search_iterator/2->|            |-->|           |-->--+ select
 (Betree, Event)    | erl_betree |   | be_tree   |     | expression
                    |   NIF      |   | C library |<----+ candidates
                    |  C code    |   |           |
 Iterator <---------|            |<--|           |
                    +------------+   +-----------+
```
2. `search_all/1` - to evaluate the selected `boolean expression candidates` all at once;
```
 erl_betree:        +------------+   +-----------+
 search_all/1------>|            |-->|           |-->--+ evaluate
 (Iterator)         | erl_betree |   | be_tree   |     | candidate 1
                    |   NIF      |   | C library |<----+
                    |  C code    |   |           | ...
                    |            |   |           |-->--+ evaluate
                    |            |   |           |     | candidate N
                    |            |   |           |<----+
 Ids <--------------|            |<--|           |
                    +------------+   +-----------+
```
3. `search_next/1` - to evaluate the selected `boolean expression candidates` one by one.
```
 erl_betree:        +------------+   +-----------+
 search_next/1----->|            |-->|           |-->--+ evaluate
 (Iterator)         | erl_betree |   | be_tree   |     | candidate
                    |   NIF      |   | C library |<----+
 `continue` to      |  C code    |   |           |
 indicate a need<---|            |<--|           |
 for another        +------------+   +-----------+
 `search_next`
 
 ...
 
 erl_betree:        +------------+   +-----------+
 search_next/1----->|            |-->|           |-->--+ evaluate
 (Iterator)         | erl_betree |   | be_tree   |     | candidate
                    |   NIF      |   | C library |<----+
 `ok`               |  C code    |   |           |
 Ids <--------------|            |<--|           |
                    +------------+   +-----------+
```

## Reductions counting

### Counting reductions for `search_iterator/2`

`search_iterator/2` traverses nodes of BE-Tree Index.

A visit to the BE-Tree Index node counts as `one` reduction.

### Counting reductions for `search_all/1` and `search_next/1`

`search_all/1` and `search_next/1` traverse nodes of `boolean expressions`.

A visit to the `boolean expression` node counts as `one` reduction.

Also `search_all/1` and `search_next/1` perform `boolean expressions` evaluations.

Such evaluations can consist of a time-wise short operations, like comparing two numbers,
or a time-consuming operations, like search in a list.

One time-wise short operation counts as `one` reduction.

For the `boolean expression` evaluation requiring iterations each iteration counts as `one` reduction.

# Benchmarks

I will compare how yielding control back to the Erlang Scheduler
using `search_iterator/search_all` and `search_iterator/search_next`
affects `boolean expressions` evaluation.

`betree_search` evaluation will be the baseline.

## Benchmark data

The following data for benchmarking will be generated:
1. 5,000 randomized `boolean expressions`, each `boolean expression` with 64 parameters;

   100,000 randomized boolean events, each event with 64 parameters;
2. 5,000 randomized `boolean expressions`, each `boolean expression` with 96 parameters;
   
   100,000 randomized boolean events, each event with 96 parameters;
3. 5,000 randomized `boolean expressions`, each `boolean expression` with 128 parameters;
   
   100,000 randomized boolean events, each event with 128 parameters.

Project `beval`, [5], is used for `boolean expressions` and `boolean events` generations.

Examples:

To generate 5,000 randomized `boolean expressions`, each `boolean expression` with 128 parameters run
```erlang
> be_tools:write_random_trees_to_file("par", 128, 5_000, "./P_128_Ex_5_000.txt").
```
It will
- create 128 boolean parameters `par1`, `par2`, ..., `par128`;
- create 5,000 boolean expressions with parameters `par1`, `par2`, ..., `par128`;
- write the boolean expressions to file `P_128_Ex_5_000.txt`.

To generate 100,000 randomized boolean events corresponding to 128 parameters run
```erlang
> be_tools:write_random_0_1_events_to_file(128, 100_000, "./P_128_Ev_100_000.txt").
```
It will write 100,000 events to file `P_128_Ev_100_000.txt`.

The following files were used for benchmarks and can be provided upon request (11 MB gzipped data):

- `P_64_Ex_5_000.txt`, `P_64_Ev_100_000.txt`;
- `P_96_Ex_5_000.txt`, `P_96_Ev_100_000.txt`;
- `P_128_Ex_5_000.txt`, `P_128_Ev_100_000.txt`.

## Correctness

Correctness verifies that output from `betree_search`

is equal to the `search_iterator/search_all` output and `search_iterator/search_next` output.

`betree_search` evaluation of 100,000 events was recorded using
```erlang
> be_bm:std_output("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/output_std.txt").
```
`search_iterator/search_all` evaluation of 100,000 events was recorded using
```erlang
> be_bm:iterated_all_output("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/output_all.txt").
```
`search_iterator/search_next` evaluation of 100,000 events was recorded using
```erlang
> be_bm:iterated_next_output("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/output_next.txt").
```
Then, `betree_search` evaluation output was compared to `search_iterator/search_all`
```erlang
> be_bm:diff("benchmarks/data/output_std.txt", "benchmarks/data/output_all.txt").
```
and to `search_iterator/search_next`
```erlang
> be_bm:diff("benchmarks/data/output_std.txt", "benchmarks/data/output_next.txt").
```
No discrepancies were found. The correctness confirmed.

## Performance statistics collection

### How event evaluation duration is measured

#### `betree_search` event evaluation duration
```
       erl_betree:        +------------+   +-----------+
T1 +-- betree_search/2--->|            |-->|           |-->--+ select
   |   (Betree, Event)    | erl_betree |   | be_tree   |     | expression
   |                      |   NIF      |   | C library |<----+ candidates
   |                      |  C code    |   |           |
   |                      |            |   |           |-->--+ evaluate
   |                      |            |   |           |     | candidate 1
   |                      |            |   |           |<----+
   |                      |            |   |           | ...
   |                      |            |   |           |-->--+ evaluate
   |                      |            |   |           |     | candidate N
   |                      |            |   |           |<----+
T2 +-- Ids <--------------|            |<--|           |
                          +------------+   +-----------+
```
`betree_search` event evaluation duration = `T2 - T1`.

#### `search_iterator/search_all` event evaluation duration
```
       erl_betree:        +------------+   +-----------+
T1 +-- search_iterator/2->|            |-->|           |-->--+ select
   |   (Betree, Event)    | erl_betree |   | be_tree   |     | expression
   |                      |   NIF      |   | C library |<----+ candidates
   |                      |  C code    |   |           |
   |   Iterator <---------|            |<--|           |
   |                      +------------+   +-----------+
   |                 
   |   erl_betree:        +------------+   +-----------+
   |   search_all/1------>|            |-->|           |-->--+ evaluate
   |   (Iterator)         | erl_betree |   | be_tree   |     | candidate 1
   |                      |   NIF      |   | C library |<----+
   |                      |  C code    |   |           | ...
   |                      |            |   |           |-->--+ evaluate
   |                      |            |   |           |     | candidate N
   |                      |            |   |           |<----+
T2 +-- Ids <--------------|            |<--|           |
                          +------------+   +-----------+
```
`search_iterator/search_all` event evaluation duration = `T2 - T1`.

#### `search_iterator/search_next` event evaluation duration
```
       erl_betree:        +------------+   +-----------+
T1 +-- search_iterator/2->|            |-->|           |-->--+ select
   |   (Betree, Event)    | erl_betree |   | be_tree   |     | expression
   |                      |   NIF      |   | C library |<----+ candidates
   |                      |  C code    |   |           |
   |   Iterator <---------|            |<--|           |
   |                      +------------+   +-----------+
   |          
   |   erl_betree:        +------------+   +-----------+
   |   search_next/1----->|            |-->|           |-->--+ evaluate
   |   (Iterator)         | erl_betree |   | be_tree   |     | candidate
   |                      |   NIF      |   | C library |<----+
   |   `continue` to      |  C code    |   |           |
   |   indicate a need<---|            |<--|           |
   |   for another        +------------+   +-----------+
   |   `search_next`
   |
   |   ...
   |
   |   erl_betree:        +------------+   +-----------+
   |   search_next/1----->|            |-->|           |-->--+ evaluate
   |   (Iterator)         | erl_betree |   | be_tree   |     | candidate
   |                      |   NIF      |   | C library |<----+
   |   `ok`               |  C code    |   |           |
T2 +-- Ids <--------------|            |<--|           |
                          +------------+   +-----------+
```
`search_iterator/search_next` event evaluation duration = `T2 - T1`.

### How event evaluation durations are collected

The event evaluation durations are collected by thousands events.

Thus, for 100,000 events 100 numbers are collected,
where each number is a sum of durations of 1,000 events. 

### How to collect statistics

#### To collect statistics for `betree_search` run:
```erlang
% 5,000 expressions, each expression with 64 parameters
> be_bm:std_stats("benchmarks/data/P_64_Ex_5_000.txt", "benchmarks/data/P_64_Ev_100_000.txt", "benchmarks/data/stats_P_64_std.txt").
% 5,000 expressions, each expression with 96 parameters
> be_bm:std_stats("benchmarks/data/P_96_Ex_5_000.txt", "benchmarks/data/P_96_Ev_100_000.txt", "benchmarks/data/stats_P_96_std.txt").
% 5,000 expressions, each expression with 128 parameters
> be_bm:std_stats("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/stats_P_128_std.txt").
```
The statistics outputs are in files `stats_P_64_std.txt`, `stats_P_96_std.txt`, `stats_P_128_std.txt`.

#### To collect statistics for `search_iterator/search_all` run:
```erlang
% 5,000 expressions, each expression with 64 parameters
> be_bm:iterated_all_stats("benchmarks/data/P_64_Ex_5_000.txt", "benchmarks/data/P_64_Ev_100_000.txt", "benchmarks/data/stats_P_64_all.txt").
% 5,000 expressions, each expression with 96 parameters
> be_bm:iterated_all_stats("benchmarks/data/P_96_Ex_5_000.txt", "benchmarks/data/P_96_Ev_100_000.txt", "benchmarks/data/stats_P_96_all.txt").
% 5,000 expressions, each expression with 128 parameters
> be_bm:iterated_all_stats("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/stats_P_128_all.txt").
```
The statistics outputs are in files `stats_P_64_all.txt`, `stats_P_96_all.txt`, `stats_P_128_all.txt`.

#### To collect statistics for `search_iterator/search_next` run:
```erlang
% 5,000 expressions, each expression with 64 parameters
> be_bm:iterated_next_stats("benchmarks/data/P_64_Ex_5_000.txt", "benchmarks/data/P_64_Ev_100_000.txt", "benchmarks/data/stats_P_64_next.txt").
% 5,000 expressions, each expression with 96 parameters
> be_bm:iterated_next_stats("benchmarks/data/P_96_Ex_5_000.txt", "benchmarks/data/P_96_Ev_100_000.txt", "benchmarks/data/stats_P_96_next.txt").
% 5,000 expressions, each expression with 128 parameters
> be_bm:iterated_next_stats("benchmarks/data/P_128_Ex_5_000.txt", "benchmarks/data/P_128_Ev_100_000.txt", "benchmarks/data/stats_P_128_next.txt").
```
The statistics outputs are in files `stats_P_64_all.txt`, `stats_P_96_all.txt`, `stats_P_128_all.txt`.

### Compare with baseline

#### Compare `search_iterator/search_all` with baseline
```erlang
% Compare stats for expressions with 64 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_64_std.txt", "benchmarks/data/stats_P_64_all.txt").
% Compare stats for expressions with 96 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_96_std.txt", "benchmarks/data/stats_P_96_all.txt").
% Compare stats for expressions with 128 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_128_std.txt", "benchmarks/data/stats_P_128_all.txt").
```

#### Compare `search_iterator/search_next` with baseline
```erlang
% Compare stats for expressions with 64 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_64_std.txt", "benchmarks/data/stats_P_64_next.txt").
% Compare stats for expressions with 96 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_96_std.txt", "benchmarks/data/stats_P_96_next.txt").
% Compare stats for expressions with 128 parameters
> be_bm:compare_stats("benchmarks/data/stats_P_128_std.txt", "benchmarks/data/stats_P_128_next.txt").
```

## Benchmark results

### Explanations of numbers in Table 1. and Table 2.

Imagine, that we have a `baseline` that is 100 numbers `10`, i.e.
```
10, 10, ..., 10
\             /
 -----100-----  
```
Also, we have a sequence of other 100 numbers, will call them `target`, that we want to compare with `baseline` such that 

first 20 of them are 9, next 20 are 10, next 50 are 14, and the rest are 16, i.e.
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
Such representation gives better picture how the `target` behaves with comparison to the `baseline`.

In this case we can say that
- the `target` deviates from the `baseline` in the `range` (-10%, 60%);
- the `median` is 40%;
- the `99th` is 60%;
- `better` is 20%, because 20 numbers out 100 are less than 0.

### Conclusions

Conclusion for benchmark results on Xeon, Table 1.:

`search_iterator/search_all` comparison to `betree_search`:
- Overall, penalty to use `search_iterator/search_all` instead of `betree_search` is not bigger than `11%`;
- penalty to use `search_iterator/search_all` for the expressions with 128 parameters is not bigger than `5%`;
- `search_iterator/search_all` produces better results on 85% of events for the expressions with 96 parameters.

`search_iterator/search_next` comparison to `betree_search`:
- Penalty to use `search_iterator/search_next` instead of `betree_search` is not bigger than `20%`.

```
Table 1.
Xeon CPU E5-2630 v4 @ 2.20GHz, Cores: 20, RAM: 128 GB
+=========+=====================+====================+
|         | search_iterator/    | search_iterator/   |
|  Params | search_all          | search_next        |
+---------+---------------------+--------------------+    
|         |  range: (0%, 11%)   |  range: (7%, 20%)  |
|      64 | median: 2%          | median: 10%        |
|         |   99th: 11%         |   99th: 19%        |
|         | better: 0%          | better: 0%         |
+---------+---------------------+--------------------+    
|         |  range: (-8%, 11%)  |  range: (-5%, 15%) | 
|      96 | median: -4%         | median: 0%         |
|         |   99th: 9%          |   99th: 13%        |
|         | better: 85%         | better: 38%        |
+---------+---------------------+--------------------+    
|         |  range: (-11%, 5%)  |  range: (-4%, 16%) |
|     128 | median: 2%          | median: 2%         |
|         |   99th: 5%          |   99th: 14%        |
|         | better: 40%         | better: 18%        |
+=========+=====================+====================+
```

Conclusion for benchmark results on Intel, Table 2.:

`search_iterator/search_all` comparison to `betree_search`:
- Overall, penalty to use `search_iterator/search_all` instead of `betree_search` is not bigger than `23%`;
- penalty to use `search_iterator/search_all` for the expressions with 128 parameters is not bigger than `7%`;
- `search_iterator/search_all` produces better results on 94% of events for the expressions with 128 parameters.

`search_iterator/search_next` comparison to `betree_search`:
- Penalty to use `search_iterator/search_next` instead of `betree_search` is not bigger than `25%`.

```
Table 2.
Intel, 2.4 GHz Quad-Core Intel Core i5, RAM 16 GB 2133 MHz LPDDR3
+=========+=====================+====================+
|         | search_iterator/    | search_iterator/   |
|  Params | search_all          | search_next        |
+---------+---------------------+--------------------+    
|         |  range: (-11%, 23%) |  range: (-5%, 25%) |
|      64 | median: 1%          | median: 10%        |
|         |   99th: 20%         |   99th: 22%        |
|         | better: 31%         | better: 4%         |
+---------+---------------------+--------------------+    
|         |  range: (1%, 20%)   |  range: (3%, 23%)  | 
|      96 | median: 9%          | median: 21%        |
|         |   99th: 17%         |   99th: 23%        |
|         | better: 0%          | better: 0%         |
+---------+---------------------+--------------------+    
|         |  range: (-10%, 7%)  |  range: (-3%, 20%) |
|     128 | median: -2%         | median: 5%         |
|         |   99th: 1%          |   99th: 19%        |
|         | better: 94%         | better: 3%         |
+=========+=====================+====================+
```

# Resources

###### 1. [erl_nif, Section Long-running NIFs](https://www.erlang.org/doc/man/erl_nif)
###### 2. [jiffy, JSON NIFs for Erlang](https://github.com/davisp/jiffy)
###### 3. [The Erlang Runtime System, 11. Scheduling](https://blog.stenmans.org/theBeamBook/#CH-Scheduling)
###### 4. [The Erlang Runtime System, 11.3. Reductions](https://blog.stenmans.org/theBeamBook/#_reductions)
###### 5. [Boolean Expression Evaluator](https://github.com/adgear/beval) 
###### 6. [An Approach To Make an Erlang NIF the Erlang Scheduler Friendly](https://adgear.atlassian.net/wiki/spaces/ENG/pages/19922256160/An+Approach+To+Make+an+Erlang+NIF+the+Erlang+Scheduler+Friendly) 
