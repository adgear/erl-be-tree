-module(betree_iterator_test).

-include_lib("eunit/include/eunit.hrl").

-record(event, { a }).

instantiate_iterator_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = true }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)).

search_all_found_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = true }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),
  Ret_search_all = erl_betree:search_all(Iterator),
  ?assertEqual({ok, [1]}, Ret_search_all).

search_all_not_found_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = false }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),
  Ret_search_all = erl_betree:search_all(Iterator),
  ?assertEqual({ok, []}, Ret_search_all).

search_iterator_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = false }],
  Before_search_iterator = betree_allocations(),
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),
  Ret_search_iterator_release = erl_betree:search_iterator_release(Iterator),
  ?assertEqual(ok, Ret_search_iterator_release),
  After_search_iterator_release = betree_allocations(),
  {_Binary, NifInternal} = _Diff = betree_allocations_diff(Before_search_iterator, After_search_iterator_release),
  ?assert(lists:all(fun (N) -> N =< 0 end, NifInternal)).

search_all_false_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = false }],
  Before_search_iterator = betree_allocations(),
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),

  Ret_search_all = erl_betree:search_all(Iterator),
  ?assertEqual({ok, []}, Ret_search_all),

  Ret_search_iterator_release = erl_betree:search_iterator_release(Iterator),
  ?assertEqual(ok, Ret_search_iterator_release),
  After_search_iterator_release = betree_allocations(),
  {_Binary, NifInternal} = _Diff = betree_allocations_diff(Before_search_iterator, After_search_iterator_release),
  ?assert(lists:all(fun (N) -> N =< 0 end, NifInternal)).

search_all_true_test() ->
  Betree = mk_betree(),
  Event = [#event{ a = true }],
  Before_search_iterator = betree_allocations(),
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),

  Ret_search_all = erl_betree:search_all(Iterator),
  ?assertEqual({ok, [1]}, Ret_search_all),

  Ret_search_iterator_release = erl_betree:search_iterator_release(Iterator),
  ?assertEqual(ok, Ret_search_iterator_release),
  After_search_iterator_release = betree_allocations(),
  {_Binary, NifInternal} = _Diff = betree_allocations_diff(Before_search_iterator, After_search_iterator_release),
  ?assert(lists:all(fun (N) -> N =< 0 end, NifInternal)).

memory_usage_test() ->
  Betree = be_tree_from_file("test/Params_10_Exprs_100.txt"),
  Events = bool_events_from_file("test/Params_10_Events_20.txt"),
  Memory_start = betree_allocations(),
  lists:foreach(fun (Event) -> search_iterator(Betree, Event) end, Events),
  Memory_stop = betree_allocations(),
  {_Binary, NifInternal} = _Diff = betree_allocations_diff(Memory_start, Memory_stop),
  ?assert(lists:all(fun (N) -> N =< 0 end, NifInternal)).

search_next_1_expr_found_test() ->
  Domains = [[{a, bool, disallow_undefined}]],
  {ok, Betree} = erl_betree:betree_make(Domains),
  Expr = <<"a">>,
  Consts = [],
  {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
  ok = erl_betree:betree_insert_sub(Betree, Sub),
  Event = [#event{ a = true }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),

  Ret_search_next = erl_betree:search_next(Iterator),
  ?assertEqual({ok, [1]}, Ret_search_next).

search_next_1_expr_not_found_test() ->
  Domains = [[{a, bool, disallow_undefined}]],
  {ok, Betree} = erl_betree:betree_make(Domains),
  Expr = <<"a">>,
  Consts = [],
  {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
  ok = erl_betree:betree_insert_sub(Betree, Sub),
  Event = [#event{ a = false }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),

  Ret_search_next = erl_betree:search_next(Iterator),
  ?assertEqual({ok, []}, Ret_search_next).

search_next_2_exprs_test() ->
  Domains = [[{a, bool, disallow_undefined}]],
  {ok, Betree} = erl_betree:betree_make(Domains),
  Consts = [],
  Expr1 = <<"a">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr1),
  ok = erl_betree:betree_insert_sub(Betree, Sub1),
  Expr2 = <<"not a">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
  ok = erl_betree:betree_insert_sub(Betree, Sub2),
  Event = [#event{ a = true }],
  Ret_iter = erl_betree:search_iterator(Betree, Event),
  ?assertMatch({ok, {_, _, _}}, Ret_iter),
  {ok, {Iterator, _, _}} = Ret_iter,
  ?assert(is_reference(Iterator)),

  Ret_search_1 = erl_betree:search_next(Iterator),
  ?assertMatch({continue, _}, Ret_search_1),
  {continue, Ret_L1} = Ret_search_1,
  ?assert(is_list(Ret_L1)),

  Ret_search_2 = erl_betree:search_next(Iterator),
  ?assertEqual({ok, [1]}, Ret_search_2).

mk_betree() ->
  Domains = [[{a, bool, disallow_undefined}]],
  {ok, Betree} = erl_betree:betree_make(Domains),
  Expr = <<"a">>,
  Consts = [],
  {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
  ok = erl_betree:betree_insert_sub(Betree, Sub),
  Betree.

be_tree_from_file(File) ->
  {Domain, Exprs} = read_be_tree(File),
  Domains = [Domain],
  {ok, Betree} = erl_betree:betree_make(Domains),
  Consts = [],
  {_, Subs} = lists:foldl(fun (E, {I, Acc}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree, I, Consts, E),
    {I+1, [Sub | Acc]}
                          end, {1, []}, Exprs),
  lists:foreach(fun (S) -> erl_betree:betree_insert_sub(Betree, S) end, Subs),
  Betree.

bool_events_from_file(File) ->
  {ok, Events} = file:consult(File),
  [[list_to_tuple(
    [bool_event |
      [case N of 1 -> true; _ -> false end|| N <- Event]
    ])] || Event <- Events].

search_iterator(Betree, Event) ->
  {ok, {Iterator, _, _}} = erl_betree:search_iterator(Betree, Event),
  Ret = erl_betree:search_all(Iterator),
  erl_betree:search_iterator_release(Iterator),
  Ret.

betree_allocations() ->
  case instrument:allocations() of
    {error, _Reason} = Err -> Err;
    {ok, {_HistogramStart, _UnscannedSize, Allocations}} ->
      case maps:find(erl_betree_nif, Allocations) of
        error -> {error, betree_allocations_not_found};
        {ok, BetreeAllocations} -> BetreeAllocations
      end
  end.

betree_allocations_diff(Alloc1, Alloc2)
  when is_map(Alloc1) andalso is_map(Alloc2) ->
  Binary1 = maps:get(binary, Alloc1, error),
  Binary2 = maps:get(binary, Alloc2, error),
  BinaryDiff = diff_tuple(Binary1, Binary2),
  NifInternal1 = maps:get(nif_internal, Alloc1, error),
  NifInternal2 = maps:get(nif_internal, Alloc2, error),
  NifInternalDiff = diff_tuple(NifInternal1, NifInternal2),
  {BinaryDiff, NifInternalDiff};
betree_allocations_diff(_, _) ->
  {[], []}.

diff_tuple(error, error) ->
  [];
diff_tuple(error, T2) ->
  diff_tuple({}, T2);
diff_tuple(T1, error) ->
  diff_tuple(T1, {});
diff_tuple(T1, T2) ->
  L1 = tuple_to_list(T1),
  L2 = tuple_to_list(T2),
  L = lists:zip(L1, L2, {pad, {0, 0}}),
  Diff = [X2 - X1 || {X1, X2} <- L],
  Diff.

read_be_tree(File) ->
  {ok, [Domain | Exprs]} = file:consult(File),
  {Domain, Exprs}.
