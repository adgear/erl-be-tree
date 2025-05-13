-module(erl_betree).
-include("erl_betree.hrl").

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

    % search with iterator
    search_iterator/2,
    search_next/1,
    search_all/1,
    search_iterator_release/1,

    % search with yield API
    betree_search_yield/3,
    betree_search_ids_yield/4,

    % search with yield API, helper functions
    betree_search_yield/2,
    search_yield_count/2,
    search_yield_count/5,
    search_yield/3,
    search_yield/4,
    search_next_yield/3,
    betree_search_ids_yield/3,
    search_ids_yield/5,

    % search error reason
    betree_make_sub_ids/1, 
    betree_make_err/1,
    betree_make_event_err/2,
    betree_make_event_err/3,
    betree_make_sub_err/4,
    betree_insert_sub_err/2,
    betree_search_err/2,
    betree_search_err/3,
    betree_parse_reasons/1,
    betree_write_dot_err/2,
    betree_search_ids_err/3,
    betree_search_ids_err/4
]).


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

betree_search_yield(Betree, Event) ->
    {{ok, _Ids}, Elapsed, _Acc} = search_yield_count(Betree, Event),
    {{ok, _Ids}, Elapsed}.

% @doc Search expressions in Betree that match the Event.
% The search is the subject to 1 millisecond threshold.
% When the threshold is reached
% the reduction count is adjusted and
% the control is returned back to the Erlang Scheduler.
% The Erlang Scheduler schedulers the search continuation.
% The search continuation respects the 1 millisecond threshold and
% the reduction count.
% Parameters:
% Betree - BE-Tree of expressions;
% Event - compiled event, result of betree_make_event;
% ClockType - one of the clock types: monotonic, etc;
% Return: {{ok, Ids}, Elapsed}, where
% Ids - list of matched expressions ids;
% Elapsed - the search elapsed time, in microseconds.
betree_search_yield(Betree, Event, ClockType) ->
    {{ok, _Ids}, Elapsed, _Acc} = search_yield_count(Betree, Event, ClockType, ?THRESHOLD_1_000_MICROSECONDS, 0),
    {{ok, _Ids}, Elapsed}.

search_yield_count(Betree, Event) ->
    search_yield_count(Betree, Event, ?CLOCK_MONOTONIC, ?THRESHOLD_1_000_MICROSECONDS, 0).

search_yield_count(Betree, Event, ClockType, YieldThresholdInMicroseconds, Acc)
    when is_reference(Event),
    is_integer(ClockType),
    is_integer(YieldThresholdInMicroseconds) ->
    case search_yield(Betree, Event, ClockType, YieldThresholdInMicroseconds) of
        {{ok, _Ids}, _Elapsed} ->
            {{ok, _Ids}, _Elapsed, Acc+1};
        {{continue, SearchState}, _} ->
            search_next_yield_count(SearchState, ClockType, YieldThresholdInMicroseconds, Acc+1)
    end.

search_next_yield_count(SearchState, ClockType, YieldThresholdInMicroseconds, Acc) ->
    case search_next_yield(SearchState, ClockType, YieldThresholdInMicroseconds) of
        {{ok, _Ids}, _Elapsed} ->
            {{ok, _Ids}, _Elapsed, Acc+1};
        {{continue, SearchState}, _} ->
            search_next_yield_count(SearchState, ClockType, YieldThresholdInMicroseconds, Acc+1)
    end.

search_yield(Betree, Event, ClockType) ->
    search_yield(Betree, Event, ClockType, ?THRESHOLD_1_000_MICROSECONDS).

search_yield(Betree, Event, ClockType, YieldThresholdInMicroseconds)
    when is_reference(Event),
    is_integer(ClockType),
    is_integer(YieldThresholdInMicroseconds) ->
    erl_betree_nif:search_yield(Betree, Event, ClockType, YieldThresholdInMicroseconds).

search_next_yield(SearchState, ClockType, YieldThresholdInMicroseconds)
    when is_reference(SearchState),
    is_integer(ClockType),
    is_integer(YieldThresholdInMicroseconds) ->
    erl_betree_nif:search_next_yield(SearchState, ClockType, YieldThresholdInMicroseconds).

betree_search_ids_yield(Betree, Event, Ids) ->
    search_ids_yield(Betree, Event, Ids, ?CLOCK_MONOTONIC, ?THRESHOLD_1_000_MICROSECONDS).

% @doc Search expressions in Betree that match the Event
% and are a subset of Ids.
% Before returning the result the reduction count is adjusted.
% Parameters:
% Betree - BE-Tree of expressions;
% Event - compiled event, result of betree_make_event;
% Ids - list of ids;
% ClockType - one of the clock types: monotonic, etc;
% Return: {{ok, SubsetIds}, Elapsed}, where
% SubsetIds - list of matched expressions ids;
% Elapsed - the search elapsed time, in microseconds.
betree_search_ids_yield(Betree, Event, Ids, ClockType) ->
    search_ids_yield(Betree, Event, Ids, ClockType, ?THRESHOLD_1_000_MICROSECONDS).

search_ids_yield(_Betree, _Event, _Ids = [], _ClockType, _YieldThresholdInMicroseconds) ->
    {{ok, []}, 0};
search_ids_yield(Betree, Event, Ids = [_|_], ClockType, YieldThresholdInMicroseconds)
    when is_reference(Event),
    is_integer(ClockType),
    is_integer(YieldThresholdInMicroseconds) ->
    erl_betree_nif:search_ids_yield(Betree, Event, Ids, ClockType, YieldThresholdInMicroseconds).

%%
%% betree search error reason begin
%%
betree_make_sub_ids(Betree) -> 
    erl_betree_nif:betree_make_sub_ids(Betree).

betree_make_err(Domains) ->
    erl_betree_nif:betree_make_err(Domains).
betree_make_event_err(Betree, Event) ->
    betree_make_event_err(Betree, Event, ?CLOCK_MONOTONIC).

betree_make_event_err({_, Betree}, Event, ClockType) when is_integer(ClockType) ->
    erl_betree_nif:betree_make_event_err(Betree, Event, ClockType);
betree_make_event_err(Betree, Event, ClockType) when is_integer(ClockType) ->
    erl_betree_nif:betree_make_event_err(Betree, Event, ClockType).
betree_make_sub_err(Betree, SubId, Constants, Expr) ->
    erl_betree_nif:betree_make_sub_err(Betree, SubId, Constants, Expr).
betree_insert_sub_err(Betree, Sub) ->
    erl_betree_nif:betree_insert_sub_err(Betree, Sub).
betree_search_err(Betree, Event) ->
    erl_betree_nif:betree_search_err(Betree, Event).

betree_search_err(Betree, Event, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_err(Betree, Event, ClockType);
betree_search_err(Betree, Event, ClockType) when is_reference(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_evt_err(Betree, Event, ClockType).

betree_parse_reasons([]) ->
    {ok, []};
betree_parse_reasons(NonMatches) when is_reference(NonMatches) ->
    erl_betree_nif:betree_parse_reasons(NonMatches).

betree_write_dot_err(Betree, FileName) when is_list(FileName) ->
    erl_betree_nif:betree_write_dot_err(Betree, FileName).

betree_search_ids_err(Betree, Event, Ids) ->
    betree_search_ids_err(Betree, Event, Ids, ?CLOCK_MONOTONIC). 

betree_search_ids_err(_Betree, _Event, [], _CLockType)  ->
    {{ok, [], []}, 0};
betree_search_ids_err(Betree, Event, Ids, ClockType) when is_list(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_ids_err(Betree, Event, Ids, ClockType);
betree_search_ids_err(Betree, Event, Ids, ClockType) when is_reference(Event), is_integer(ClockType) ->
    erl_betree_nif:betree_search_evt_err(Betree, Event, Ids, ClockType).
