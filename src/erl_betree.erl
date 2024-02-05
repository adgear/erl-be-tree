-module(erl_betree).

-export([
    betree_make/1,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_write_dot/2
]).

-inline([check_clock_type/1]).

betree_make(Domains) ->
    erl_betree_nif:betree_make(Domains).
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
betree_search(Betree, Event, CLockType) ->
    erl_betree_nif:betree_search(Betree, Event, check_clock_type(CLockType)).

betree_write_dot(Betree, FileName) when is_list(FileName) ->
    erl_betree_nif:betree_write_dot(Betree, FileName).

-define(CLOCK_REALTIME, 0). 
-define(CLOCK_MONOTONIC, 6). 
-define(CLOCK_PROCESS_CPUTIME_ID, 12). 
-define(CLOCK_THREAD_CPUTIME_ID, 16). 

check_clock_type(?CLOCK_REALTIME) -> ?CLOCK_REALTIME;
check_clock_type(?CLOCK_PROCESS_CPUTIME_ID) -> ?CLOCK_PROCESS_CPUTIME_ID;
check_clock_type(?CLOCK_THREAD_CPUTIME_ID) -> ?CLOCK_THREAD_CPUTIME_ID;
check_clock_type(_) -> ?CLOCK_MONOTONIC.
