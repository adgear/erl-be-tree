# Effect of Erlang Scheduler-friendly processing with BE-Tree on Matching Time

## Executive Summary

It was observed that BE expressions _**Match time**_ is
- in the `2.5 - 6.7 ms` range for regular evaluations;
- in the `5 - 14 ms` range for Erlang Scheduler-friendly evaluations.

Thus, the cost of the `friendliness` is in the range `from 2 to 7 ms`.

## Experiment Details

At the time of the experiment the evaluation of a single event against the set of `boolean expressions`
consists of 4 calls to BE-Tree:

`make_event`, `search`, `search_ids` (2 calls).

`Diagram 1` shows the sequence of the calls.

`Diagram 1` also shows timings for each call.

To check how passing control to Erlang Scheduler will affect the evaluation time
the call to `erlang:yield/0` was introduced after `make_event`, `search`, `search_ids`.

`Diagram 1` indicates points of execution where `erlang:yield/0` was called.

Experiment was ran on `usw2`, on 3 pods.

[Grafana link for the experiment](https://grafana.int.adgear.com/d/nJbjBDC7k/boolean-expressions?orgId=1&var-instance=experiment-be-yield-2&var-timing=0.99&from=now-24h&to=now)

```
Diagram 1. BE evaluation duration: Regular vs Scheduler-friendly

----------------------------------------------------------------
Regular evaluation  |                    | Evaluation w/ YIELD
----------------------------------------------------------------
        2.5 - 6.7 ms |     Match time    | 5 - 14 ms
----------------------------------------------------------------

                     +===================+
betree_make_event--->| Basic BE-Tree     |<---betree_make_event
                     |                   |
0.99 ms              |                   | 0.99 ms
                     |                   |
Event<---------------|                   |--------------->Event
                     |                   | YIELD
betree_search------->|                   |<-------betree_search
                     |                   |
1.5 - 3.5 ms         |                   | 1.9 - 3.3 ms
                     |                   |
Ids(basic)<----------|                   |---------->Ids(basic)
                     +===================+ YIELD

                     +===================+
betree_search_ids--->| Creatives BE-Tree |<---betree_search_ids
                     |                   |
0.99 ms              |                   | 0.99 ms
                     |                   |
Ids(creatives)<------|                   |------>Ids(creatives)
                     +===================+ YIELD

                     +===================+
betree_search_ids--->|   FCaps BE-Tree   |<---betree_search_ids
                     |                   |
0.99 ms              |                   | 0.99 ms
                     |                   |
Ids(fcaps)<----------|                   |---------->Ids(fcaps)
                     +===================+ YIELD
----------------------------------------------------------------
        2.5 - 6.7 ms |     Match time    | 5 - 14 ms
----------------------------------------------------------------

Notation:
betree_make_event - erl_betree:betree_make_event/3;
betree_search     - erl_betree:betree_search/3;
betree_search_ids - erl_betree:betree_search_ids/4;
YIELD             - erlang:yield/0
```


## Experiment Observation Explanation

The `Match time` for the experiment is increased by `2 to 7 ms`.

The increase of the `Match time` can be attributed to the facts that
- `yield` causes placement of the process performing the matching at the end of the Scheduler queue;
- the process performing the matching is not the only process on the Scheduler.

Thus, the waiting time in the Scheduler queue contributes to the increase of the `Match time`.

## Conclusion

The task to make the `boolean expressions` evaluation the Erlang Scheduler-friendly
was intended to prevent a possible adverse effect of the long run evaluation
on the `rtb-gateway`.

The experiment shows that the `friendliness` comes with the cost of the increased `Match time`.

Thus, the trade-off between `friendliness` and `Match time` requires further discussion.
