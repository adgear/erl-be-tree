-module(erl_betree_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    betree_make/1,
    betree_make_sub/4,
    betree_insert_sub/2,
    betree_exists/2,
    betree_search/2,
    betree_search/3,
    betree_search_yielding/3
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
betree_search_yielding(_Betree, _Event, _Timeslice) ->
    ?nif_stub.
