-module(erl_betree).
-include("erl_betree.hrl").

-export([
    betree_print/1,
    betree_make/1,
    betree_make/2,
    betree_make_event/2,
    betree_make_event/3,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_add_sub/4,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_write_dot/2,
    betree_search_debug/2,
    betree_search_debug/3,
    betree_search_stats/2,
    betree_search_stats/3,

    betree_prepare_subs/1,
    betree_stats/1,
    betree_stats/2,
    betree_group_stats/1,
    betree_group_stats/2,
    betree_add_sub/5,
    betree_stats_start/1,
    betree_stats_stop/1,
    betree_stats_stop_return/1,
    betree_group_vars/1
]).


betree_print(Betree) ->
    erl_betree_nif:betree_print(Betree).
betree_make(Domains) ->
    erl_betree_nif:betree_make(Domains).
betree_make(Domains, Ranks) ->
    erl_betree_nif:betree_make(Domains, Ranks).
betree_make_event(Betree, Event) ->
    betree_make_event(Betree, Event, ?CLOCK_MONOTONIC).

betree_make_event({_, Betree}, Event, ClockType) when is_integer(ClockType) ->
    erl_betree_nif:betree_make_event(Betree, Event, ClockType);
betree_make_event(Betree, Event, ClockType) when is_integer(ClockType) ->
    erl_betree_nif:betree_make_event(Betree, Event, ClockType).
betree_make_sub(Betree, SubId, Constants, Expr) ->
    erl_betree_nif:betree_make_sub(Betree, SubId, Constants, Expr).
betree_insert_sub(Betree, Sub) ->
    erl_betree_nif:betree_insert_sub(Betree, Sub).
betree_add_sub(Betree, SubId, Constants, Expr) ->
    erl_betree_nif:betree_add_sub(Betree, SubId, Constants, Expr).
betree_exists(Betree, Event) ->
    erl_betree_nif:betree_exists(Betree, Event).
betree_search(Betree, Event) ->
    erl_betree_nif:betree_search(Betree, Event).
betree_search_debug(Betree, Event) ->
    erl_betree_nif:betree_search_debug(Betree, Event).
betree_search_debug(Betree, Event, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_debug(Betree, Event, ClockType).
betree_search_stats(Betree, Event) ->
    erl_betree_nif:betree_search_stats(Betree, Event).
betree_search_stats(Betree, Event, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_stats(Betree, Event, ClockType).

% @doc Calculates time spend in NIF. 
% Time value is in microseconds - the erlang:timestamp resolution.  
betree_search(Betree, Event, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search(Betree, Event, ClockType);
betree_search(Betree, Event, ClockType) when is_reference(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_evt(Betree, Event, ClockType).

betree_write_dot(Betree, FileName) when is_list(FileName) ->
    erl_betree_nif:betree_write_dot(Betree, FileName).

betree_prepare_subs(Betree) ->
    erl_betree_nif:betree_prepare_subs(Betree).

betree_stats(Betree) ->
    erl_betree_nif:betree_stats(Betree, false, false).

betree_stats(Betree, Reset) ->
    erl_betree_nif:betree_stats(Betree, false, Reset).

betree_group_stats(Betree) ->
    erl_betree_nif:betree_stats(Betree, true, false).

betree_group_stats(Betree, Reset) ->
    erl_betree_nif:betree_stats(Betree, true, Reset).

betree_add_sub(Betree, SubId, GroupId, Constants, Expr) ->
    erl_betree_nif:betree_add_sub(Betree, SubId, GroupId, Constants, Expr).

betree_stats_start(Betree) ->
    erl_betree_nif:betree_stats_start(Betree).

betree_stats_stop(StatsAccumulator) ->
    erl_betree_nif:betree_stats_stop(StatsAccumulator, false).

betree_stats_stop_return(StatsAccumulator) ->
    erl_betree_nif:betree_stats_stop(StatsAccumulator, true).

betree_group_vars(Betree) ->
    erl_betree_nif:betree_group_vars(Betree).
