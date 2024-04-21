-module(erl_betree_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    betree_make/1,
    betree_make_event/3,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_search_evt/3,
    betree_search_evt/4,
    betree_search_ids/4,
    betree_write_dot/2,

    % search with iterator
    search_iterator/2,
    search_next/1,
    search_all/1,
    search_iterator_release/1,

    % search with yield
    search_yield/4,
    search_next_yield/3,

    search_ids_yield/5
]).

-spec on_load() -> ok.

on_load() ->
    SoName = case code:priv_dir(erl_betree) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, erl_betree]);
                _ ->
                    filename:join([priv, erl_betree])
            end;
        Dir ->
            filename:join(Dir, erl_betree)
    end,
    ok = erlang:load_nif(SoName, 0).

%% shamelessly stolen from crypto.erl
-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

betree_make(_Domains) ->
    ?nif_stub.
betree_make_event(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_make_sub(_Betree, _SubId, _Constants, _Expr) ->
    ?nif_stub.
betree_insert_sub(_Betree, _Sub) ->
    ?nif_stub.
betree_exists(_Betree, _Event) ->
    ?nif_stub.
betree_search(_Betree, _Event) ->
    ?nif_stub.
betree_search(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_search_evt(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_search_evt(_Betree, _Event, _Ids, _ClockType) ->
    ?nif_stub.
betree_search_ids(_Betree, _Event, _Ids, _ClockType) ->
    ?nif_stub.

betree_write_dot(_Betree, _FileName) ->
    ?nif_stub.

search_iterator(_Betree, _Event) ->
    ?nif_stub.
search_next(_Iterator) ->
    ?nif_stub.
search_all(_Iterator) ->
    ?nif_stub.
search_iterator_release(_Iterator) ->
    ?nif_stub.

search_yield(_Betree, _Event, _ClockType, _YieldThresholdInMicroseconds) ->
    ?nif_stub.
search_next_yield(_SearchState, _ClockType, _YieldThresholdInMicroseconds) ->
    ?nif_stub.

search_ids_yield(_Betree, _Event, _Ids, _ClockType, _YieldThresholdInMicroseconds) ->
    ?nif_stub.
