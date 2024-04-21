-module(betree_yield_tests).

-include_lib("eunit/include/eunit.hrl").

default_threshold_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1).

threshold_1_000_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 1_000),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1).

match_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search),
  {{continue, SearchState}, _} = Ret_betree1_search,
  ?assert(is_reference(SearchState)).

no_sub_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([], Ids).

two_exprs_threshold_1_000_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 1_000),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([1, 2], lists:sort(Ids)).

two_exprs_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search),
  {{continue, SearchState}, _} = Ret_betree1_search,
  ?assert(is_reference(SearchState)),

  Ret_betree1_search_next1 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search_next1),
  {{continue, SearchState}, _} = Ret_betree1_search,

  Ret_betree1_search_next2 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search_next2),
  {{ok, Ids}, _} = Ret_betree1_search_next2,
  ?assertEqual([1, 2], lists:sort(Ids)).

one_expr_no_match_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search),
  {{continue, SearchState}, _} = Ret_betree1_search,
  ?assert(is_reference(SearchState)),

  Ret_betree1_search_next1 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search_next1),
  {{ok, Ids}, _} = Ret_betree1_search_next1,
  ?assertEqual([], Ids).

four_expr_no_match_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),

  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree1, 3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree1, Sub3),

  Expr4 = <<"par1 and par2">>,
  {ok, Sub4} = erl_betree:betree_make_sub(Betree1, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree1, Sub4),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search),
  {{continue, SearchState}, _} = Ret_betree1_search,
  ?assert(is_reference(SearchState)),

  Ret_betree1_search_next1 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search_next1),
  {{continue, SearchState}, _} = Ret_betree1_search_next1,

  Ret_betree1_search_next2 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search_next2),
  {{continue, SearchState}, _} = Ret_betree1_search_next2,

  Ret_betree1_search_next3 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{continue, _}, _}, Ret_betree1_search_next3),
  {{continue, SearchState}, _} = Ret_betree1_search_next3,

  Ret_betree1_search_next4 = erl_betree:search_next_yield(SearchState, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search_next4),
  {{ok, Ids}, _} = Ret_betree1_search_next4,
  ?assertEqual([], Ids).

wrapper_default_threshold_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1).

wrapper_threshold_1_000_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 1_000),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1).

wrapper_no_sub_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([], Ids).

wrapper_two_exprs_threshold_1_000_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 1_000),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([1, 2], lists:sort(Ids)).

wrapper_two_exprs_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([1, 2], lists:sort(Ids)).

wrapper_one_expr_no_match_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([], Ids).

wrapper_four_expr_no_match_threshold_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),

  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree1, 3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree1, Sub3),

  Expr4 = <<"par1 and par2">>,
  {ok, Sub4} = erl_betree:betree_make_sub(Betree1, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree1, Sub4),

  Event = [{bool_event, false, false, false}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, _} = Ret_betree1_search,
  ?assertEqual([], Ids).

timing_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),

  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree1, Sub2),

  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree1, 3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree1, Sub3),

  Expr4 = <<"par1 and par2">>,
  {ok, Sub4} = erl_betree:betree_make_sub(Betree1, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree1, Sub4),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, ElapsedMakeEvent} = Ret_betree_make_event,
%%  ?debugFmt("~nbetree_make_event: ~p~n", [ElapsedMakeEvent]),

  Ret_betree1_search_yield = erl_betree:betree_search_yield(Betree1, Evt, 0, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search_yield),
  {{ok, IdsYield}, ElapsedSearchYield} = Ret_betree1_search_yield,
  ?assertEqual([1, 2, 3, 4], lists:sort(IdsYield)),
%%  ?debugFmt("~nbetree_search_yield: ~p~n", [ElapsedSearchYield]),

  Ret_betree1_search = erl_betree:betree_search(Betree1, Event, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Ids}, Elapsed} = Ret_betree1_search_yield,
  ?assertEqual([1, 2, 3, 4], lists:sort(Ids)),
%%  ?debugFmt("~nbetree_search: ~p~n", [Elapsed]).

  ?assert(ElapsedMakeEvent + ElapsedSearchYield >= Elapsed).
