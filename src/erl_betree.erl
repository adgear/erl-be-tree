-module(erl_betree).

-export([
    betree_make/1,
    betree_make_event/2,
    betree_make_event/3,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_search_ids/3,
    betree_search_ids/4,
    betree_write_dot/2,
    search_iterator/2,
    search_next/1,
    search_all/1,
    search_iterator_release/1,

    search_and_cache_ids/2,
    search_with_cached_ids/3
]).


% -define(CLOCK_REALTIME, 0). 
-define(CLOCK_MONOTONIC, 1). 
% -define(CLOCK_PROCESS_CPUTIME_ID, 2). 
% -define(CLOCK_THREAD_CPUTIME_ID, 3). 

betree_make(Domains) ->
    erl_betree_nif:betree_make(Domains).
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
betree_exists(Betree, Event) ->
    erl_betree_nif:betree_exists(Betree, Event).
betree_search(Betree, Event) ->
    erl_betree_nif:betree_search(Betree, Event).

% @doc Calculates time spend in NIF. 
% Time value is in microseconds - the erlang:timestamp resolution.  
betree_search(Betree, Event, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search(Betree, Event, ClockType);
betree_search(Betree, Event, ClockType) when is_reference(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_evt(Betree, Event, ClockType).

betree_write_dot(Betree, FileName) when is_list(FileName) ->
    erl_betree_nif:betree_write_dot(Betree, FileName).

betree_search_ids(Betree, Event, Ids) ->
    betree_search_ids(Betree, Event, Ids, ?CLOCK_MONOTONIC). 

% @doc Do search for only those ids which are presented in Ids list
% Also calculates time spend in NIF. 
% Time value is in microseconds - the erlang:timestamp resolution.  
betree_search_ids(_Betree, _Event, [], _CLockType)  ->
    {{ok, []}, 0};
betree_search_ids(Betree, Event, Ids, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_ids(Betree, Event, Ids, ClockType);
betree_search_ids(Betree, Event, Ids, ClockType) when is_reference(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_evt(Betree, Event, Ids, ClockType).

search_iterator(Betree, Event) ->
    erl_betree_nif:search_iterator(Betree, Event).
search_next(Iterator) ->
    erl_betree_nif:search_next(Iterator).
search_all(Iterator) ->
    erl_betree_nif:search_all(Iterator).
search_iterator_release(Iterator) ->
    erl_betree_nif:search_iterator_release(Iterator).

search_and_cache_ids(Betree, Event) ->
    erl_betree_nif:search_and_cache_ids(Betree, Event).

search_with_cached_ids(Betree, Event, Ids) ->
    erl_betree_nif:search_with_cached_ids(Betree, Event, Ids).
