-module(erl_betree_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    betree_print/1,
    betree_make/1,
    betree_make/2,
    betree_make_event/3,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_add_sub/4,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_search_debug/2,
    betree_search_debug/3,
    betree_search_stats/2,
    betree_search_stats/3,
    betree_search_evt/3,
    betree_write_dot/2,

    betree_prepare_subs/1,
    betree_stats/3,
    betree_add_sub/5,
    betree_stats_start/1,
    betree_stats_stop/2,
    betree_group_vars/1
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

betree_print(_Betree) ->
    ?nif_stub.
betree_make(_Domains) ->
    ?nif_stub.
betree_make(_Domains, _Ranks) ->
    ?nif_stub.
betree_make_event(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_make_sub(_Betree, _SubId, _Constants, _Expr) ->
    ?nif_stub.
betree_insert_sub(_Betree, _Sub) ->
    ?nif_stub.
betree_add_sub(_Betree, _SubId, _Constants, _Expr) ->
    ?nif_stub.
betree_exists(_Betree, _Event) ->
    ?nif_stub.
betree_search(_Betree, _Event) ->
    ?nif_stub.
betree_search(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_search_debug(_Betree, _Event) ->
    ?nif_stub.
betree_search_debug(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_search_stats(_Betree, _Event) ->
    ?nif_stub.
betree_search_stats(_Betree, _Event, _ClockType) ->
    ?nif_stub.
betree_search_evt(_Betree, _Event, _ClockType) ->
    ?nif_stub.

betree_write_dot(_Betree, _FileName) ->
    ?nif_stub.

betree_prepare_subs(_Betree) ->
    ?nif_stub.

betree_stats(_Betree, _Group, _Reset) ->
    ?nif_stub.
betree_add_sub(_Betree, _SubId, _GroupId, _Constants, _Expr) ->
    ?nif_stub.
betree_stats_start(_Betree) ->
    ?nif_stub.
betree_stats_stop(_StatsAccumulator, _ReturnStats) ->
    ?nif_stub.
betree_group_vars(_Betree) ->
    ?nif_stub.
