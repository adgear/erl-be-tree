-module(betree_search_tests).

-include_lib("eunit/include/eunit.hrl").


-record(all, { b, i, f, s, il, sl, seg, freq, now }).

atom_all_search_term_test() ->
    Domains = [[
                {b, bool, disallow_undefined}, 
                {i, int, disallow_undefined}, 
                {f, float, disallow_undefined}, 
                {s, bin, disallow_undefined}, 
                {il, int_list, disallow_undefined}, 
                {sl, bin_list, disallow_undefined}, 
                {seg, segments, disallow_undefined}, 
                {frequency_caps, frequency_caps, disallow_undefined},
                {now, int64, disallow_undefined}
               ]],
    Expr1 = <<
             "b and "
             "i = 10 and "
             "f > 3.13 and "
             "s = \"good\" and "
             "1 in il and "
             "sl none of (\"good\") and "
             "segment_within(seg, 1, 20) and "
             "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
           >>,
    Expr2 = <<
            % "b and "
            "i = 10 and "
            "f > 3.13 and "
            "s = \"good\" and "
            "1 in il and "
            "sl none of (\"good\") and "
            "segment_within(seg, 1, 20) and "
            "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
          >>,
   Consts = [{flight_id, 10},
              {advertiser_id, 20},
              {campaign_id, 30},
              {product_id, 40}],
    Usec = 1000 * 1000,
    Event = [#all{
               b = true,
               i = 10,
               f = 3.14,
               s = <<"good">>,
               il = [1, 2, 3],
               sl = [<<"bad">>],
               seg = [{1, 10 * Usec}],
               freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
               now = 0
              }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub(Betree, 3, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub3),
    {Res, _} = erl_betree:betree_search(Betree, Event, 0),
    ?assertEqual({ok, [1, 2, 3]}, Res).


atom_event_search_term_test() ->
    Domains = [[
                {b, bool, disallow_undefined}, 
                {i, int, disallow_undefined}, 
                {f, float, disallow_undefined}, 
                {s, bin, disallow_undefined}, 
                {il, int_list, disallow_undefined}, 
                {sl, bin_list, disallow_undefined}, 
                {seg, segments, disallow_undefined}, 
                {frequency_caps, frequency_caps, disallow_undefined},
                {now, int64, disallow_undefined}
               ]],
    Expr1 = <<
             "b and "
             "i = 10 and "
             "f > 3.13 and "
             "s = \"good\" and "
             "1 in il and "
             "sl none of (\"good\") and "
             "segment_within(seg, 1, 20) and "
             "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
           >>,
    Expr2 = <<
            % "b and "
            "i = 10 and "
            "f > 3.13 and "
            "s = \"good\" and "
            "1 in il and "
            "sl none of (\"good\") and "
            "segment_within(seg, 1, 20) and "
            "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
          >>,
   Consts = [{flight_id, 10},
              {advertiser_id, 20},
              {campaign_id, 30},
              {product_id, 40}],
    Usec = 1000 * 1000,
    Event = [#all{
               b = true,
               i = 10,
               f = 3.14,
               s = <<"good">>,
               il = [1, 2, 3],
               sl = [<<"bad">>],
               seg = [{1, 10 * Usec}],
               freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
               now = 0
              }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub(Betree, 3, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub3),
%%    erl_betree:betree_write_dot(Betree, "test/betree_search_tests.dot"),
    {Res, _} = erl_betree:betree_search(Betree, Event, 0),
    ?assertEqual({ok, [1, 2, 3]}, Res),
    {Res1, _} = erl_betree:betree_search_ids(Betree, Event, [1, 3], 0),
    ?assertEqual({ok, [1, 3]}, Res1),
    {Res2, _} = erl_betree:betree_search_ids(Betree, Event, [1], 0),
    ?assertEqual({ok, [1]}, Res2),
    {Res3, _} = erl_betree:betree_search_ids(Betree, Event, [3], 0),
    ?assertEqual({ok, [3]}, Res3),
    ok.


    atom_ids_search_term_test() ->
      Domains = [[
                  {b, bool, disallow_undefined}, 
                  {i, int, disallow_undefined}, 
                  {f, float, disallow_undefined}, 
                  {s, bin, disallow_undefined}, 
                  {il, int_list, disallow_undefined}, 
                  {sl, bin_list, disallow_undefined}, 
                  {seg, segments, disallow_undefined}, 
                  {frequency_caps, frequency_caps, disallow_undefined},
                  {now, int64, disallow_undefined}
                 ]],
      Expr1 = <<
               "b and "
               "i = 10 and "
               "f > 3.13 and "
               "s = \"good\" and "
               "1 in il and "
               "sl none of (\"good\") and "
               "segment_within(seg, 1, 20) and "
               "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
             >>,
      Expr2 = <<
              % "b and "
              "i = 10 and "
              "f > 3.13 and "
              "s = \"good\" and "
              "1 in il and "
              "sl none of (\"good\") and "
              "segment_within(seg, 1, 20) and "
              "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
            >>,
     Consts = [{flight_id, 10},
                {advertiser_id, 20},
                {campaign_id, 30},
                {product_id, 40}],
      Usec = 1000 * 1000,
      Event = [#all{
                 b = true,
                 i = 10,
                 f = 3.14,
                 s = <<"good">>,
                 il = [1, 2, 3],
                 sl = [<<"bad">>],
                 seg = [{1, 10 * Usec}],
                 freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
                 now = 0
                }],
      {ok, Betree} = erl_betree:betree_make(Domains),
      {ok, Sub1} = erl_betree:betree_make_sub(Betree, 3, Consts, Expr1),
      ok = erl_betree:betree_insert_sub(Betree, Sub1),
      {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
      ok = erl_betree:betree_insert_sub(Betree, Sub2),
      {ok, Sub3} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr2),
      ok = erl_betree:betree_insert_sub(Betree, Sub3),

      {{ok, Evt}, _} = erl_betree:betree_make_event(Betree, Event),

      {Res0, _} = erl_betree:betree_search(Betree, Evt, 0),
      ?assertEqual({ok, [1, 2, 3]}, Res0),
      {Res, _} = erl_betree:betree_search_ids(Betree, Evt, [1, 2, 3], 0),
      ?assertEqual({ok, [1, 2, 3]}, Res),
      {Res1, _} = erl_betree:betree_search_ids(Betree, Evt, [1, 3], 0),
      ?assertEqual({ok, [1, 3]}, Res1),
      {Res2, _} = erl_betree:betree_search_ids(Betree, Event, [1], 0),
      ?assertEqual({ok, [1]}, Res2),
      {Res3, _} = erl_betree:betree_search_ids(Betree, Event, [3], 0),
      ?assertEqual({ok, [3]}, Res3),
      {Res2, _} = erl_betree:betree_search_ids(Betree, Evt, [1], 0),
      ?assertEqual({ok, [1]}, Res2),
      {Res3, _} = erl_betree:betree_search_ids(Betree, Evt, [3], 0),
      ?assertEqual({ok, [3]}, Res3),
      ok.

two_betrees_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined}]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  Event = [{bool_event, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search(Betree1, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),

  Ret_betree2_search = erl_betree:betree_search(Betree2, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree2_search),
  {{ok, Matched2}, _} = Ret_betree2_search,
  ?assertEqual([1], Matched2).

two_betrees_search_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined}]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  Event = [{bool_event, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search(Betree1, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),

  Ret_betree2_search = erl_betree:betree_search_ids(Betree2, Evt, Matched1, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree2_search),
  {{ok, Matched2}, _} = Ret_betree2_search,
  ?assertEqual([1], Matched2).

three_betrees_search_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search(Betree1, Evt, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),

  Ret_betree2_search_ids = erl_betree:betree_search_ids(Betree2, Evt, Matched1, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree2_search_ids),
  {{ok, Matched2}, _} = Ret_betree2_search_ids,
  ?assertEqual([1], Matched2),

  Ret_betree3_search_ids = erl_betree:betree_search_ids(Betree3, Evt, Matched2, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree3_search_ids),
  {{ok, Matched3}, _} = Ret_betree3_search_ids,
  ?assertEqual([1], Matched3).

three_betrees_search_then_betrees_search_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  Event = [{bool_event, true, true, true}],

  Ret_betree1_search = erl_betree:betree_search(Betree1, Event, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree1_search),
  {{ok, Matched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),

  Ret_betree2_search_ids = erl_betree:betree_search_ids(Betree2, Event, Matched1, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree2_search_ids),
  {{ok, Matched2}, _} = Ret_betree2_search_ids,
  ?assertEqual([1], Matched2),

  Ret_betree3_search_ids = erl_betree:betree_search_ids(Betree3, Event, Matched2, 0),
  ?assertMatch({{ok, _}, _}, Ret_betree3_search_ids),
  {{ok, Matched3}, _} = Ret_betree3_search_ids,
  ?assertEqual([1], Matched3).

search_and_cache_ids_test() ->
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

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Ids1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Ids1),
  ?assert(is_reference(CachedIds)).

search_with_cached_ids_test() ->
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

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Ids1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Ids1),
  ?assert(is_reference(CachedIds)),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  Ret_betree2_search = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search),
  {ok, Ids2} = Ret_betree2_search,
  ?assertEqual([1], Ids2).

three_betrees_search_with_cached_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  Event = [{bool_event, true, true, true}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([1], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([1], Matched3).

three_betrees_search_with_cached_ids_0_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 000
  Event = [{bool_event, false, false, false}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_1_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 001
  Event = [{bool_event, false, false, true}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_2_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 010
  Event = [{bool_event, false, true, false}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_3_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 011
  Event = [{bool_event, false, true, true}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_4_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 100
  Event = [{bool_event, true, false, false}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_5_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 101
  Event = [{bool_event, true, false, true}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_6_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 110
  Event = [{bool_event, true, true, false}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([1], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([], Matched3).

three_betrees_search_with_cached_ids_7_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make(Domains),
  Expr3 = <<"par3">>,
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  % 111
  Event = [{bool_event, true, true, true}],

  Ret_betree1_search = erl_betree:search_and_cache_ids(Betree1, Event),
  ?assertMatch({ok, _, _}, Ret_betree1_search),
  {ok, Matched1, CachedIds} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assert(is_reference(CachedIds)),

  Ret_betree2_search_ids = erl_betree:search_with_cached_ids(Betree2, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree2_search_ids),
  {ok, Matched2} = Ret_betree2_search_ids,
  ?assertEqual([1], Matched2),

  Ret_betree3_search_ids = erl_betree:search_with_cached_ids(Betree3, Event, CachedIds),
  ?assertMatch({ok, _}, Ret_betree3_search_ids),
  {ok, Matched3} = Ret_betree3_search_ids,
  ?assertEqual([1], Matched3).
