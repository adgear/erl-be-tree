-module(betree_search_reason_tests).

-include_lib("eunit/include/eunit.hrl").


-record(all, { b, i, f, s, il, sl, seg, freq, now }).

atom_validate_event_failed_test() ->
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
               freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}]
              }],
    {ok, Betree} = erl_betree:betree_make_err(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, 3, Consts, Expr2),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub3),
    ok = erl_betree:betree_make_sub_ids(Betree),
    erl_betree:betree_write_dot_err(Betree, "test/betree_search_tests.dot"),
    {Res, _} = erl_betree:betree_search_err(Betree, Event, 0),
    ?assertEqual({error, [], [{invalid_event, [1,2,3]}]}, Res),
    ok.

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
    Expr3 = <<
            % "b and "
            "i = 10 and "
            "f < 3.13 and "
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
    {ok, Betree} = erl_betree:betree_make_err(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, 3, Consts, Expr3),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub3),
    ok = erl_betree:betree_make_sub_ids(Betree),
    {Res, _} = erl_betree:betree_search_err(Betree, Event, 0),
    ?assertEqual({ok, [1, 2], [{f, [3]}]}, Res).

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
    {ok, Betree} = erl_betree:betree_make_err(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, 3, Consts, Expr2),
    ok = erl_betree:betree_insert_sub_err(Betree, Sub3),
    ok = erl_betree:betree_make_sub_ids(Betree),
    erl_betree:betree_write_dot_err(Betree, "test/betree_search_tests.dot"),
    {Res, _} = erl_betree:betree_search_err(Betree, Event, 0),
    ?assertEqual({ok, [1, 2, 3], []}, Res),
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
              "s = \"goods\" and "
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
                 s = <<"goods">>,
                 il = [1, 2, 3],
                 sl = [<<"bad">>],
                 seg = [{1, 10 * Usec}],
                 freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
                 now = 0
                }],
      {ok, Betree} = erl_betree:betree_make_err(Domains),
      {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, 3, Consts, Expr1),
      ok = erl_betree:betree_insert_sub_err(Betree, Sub1),
      {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, 2, Consts, Expr2),
      ok = erl_betree:betree_insert_sub_err(Betree, Sub2),
      {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, 1, Consts, Expr2),
      ok = erl_betree:betree_insert_sub_err(Betree, Sub3),
      ok = erl_betree:betree_make_sub_ids(Betree),

      {{ok, Evt}, _} = erl_betree:betree_make_event(Betree, Event),

      {Res0, _} = erl_betree:betree_search_err(Betree, Evt, 0),
      ?assertEqual({ok, [1,2], [{s,[3]}]}, Res0),
      ok.

two_betrees_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined}]],
  {ok, Betree1} = erl_betree:betree_make_err(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub1),
  ok = erl_betree:betree_make_sub_ids(Betree1),

  {ok, Betree2} = erl_betree:betree_make_err(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree2, Sub2),
  ok = erl_betree:betree_make_sub_ids(Betree2),

  Event = [{bool_event, false, true}],
  Ret_betree_make_event = erl_betree:betree_make_event(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_err(Betree1, Evt, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree1_search),
  {{ok, Matched1, NonMatched1}, _} = Ret_betree1_search,
  ?assertEqual([], Matched1),
  ?assertEqual([{par1,[1]}], NonMatched1),

  Ret_betree2_search = erl_betree:betree_search_err(Betree2, Evt, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree2_search),
  {{ok, Matched2, NonMatched2}, _} = Ret_betree2_search,
  ?assertEqual([1], Matched2),
  ?assertEqual([], NonMatched2).

two_betrees_search_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined}]],
  {ok, Betree1} = erl_betree:betree_make_err(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make_err(Domains),
  Expr2 = <<"par2">>,
  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree2, Sub2),

  Event = [{bool_event, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event_err(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_err(Betree1, Evt, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree1_search),
  {{ok, Matched1, NonMatched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assertEqual([], NonMatched1),

  Ret_betree2_search = erl_betree:betree_search_ids_err(Betree2, Evt, Matched1, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree2_search),
  {{ok, Matched2, NonMatched2}, _} = Ret_betree2_search,
  ?assertEqual([1], Matched2),
  ?assertEqual([], NonMatched2).

three_betrees_search_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined},
    {par3,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make_err(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub1),

  Expr2 = <<"not par1">>,
  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub2),

  Expr3 = <<"par2">>,
  {ok, Sub3} = erl_betree:betree_make_sub_err(Betree1, 3, [], Expr3),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub3),

  Expr4 = <<"par3">>,
  {ok, Sub4} = erl_betree:betree_make_sub_err(Betree1, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub4),

  {ok, Betree2} = erl_betree:betree_make_err(Domains),
  {ok, Sub5} = erl_betree:betree_make_sub_err(Betree2, 3, [], Expr3),
  ok = erl_betree:betree_insert_sub_err(Betree2, Sub5),

  {ok, Sub6} = erl_betree:betree_make_sub_err(Betree2, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub_err(Betree2, Sub6),

  {ok, Betree3} = erl_betree:betree_make_err(Domains),
  {ok, Sub7} = erl_betree:betree_make_sub_err(Betree3, 4, [], Expr4),
  ok = erl_betree:betree_insert_sub_err(Betree3, Sub7),

  Event = [{bool_event, true, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event_err(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_err(Betree1, Evt, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree1_search),
  {{ok, Matched1, NonMatched1}, _} = Ret_betree1_search,
  ?assertEqual([1,3,4], Matched1),
  ?assertEqual([{par1,[2]}], NonMatched1),

  Ret_betree2_search_ids = erl_betree:betree_search_ids_err(Betree2, Evt, Matched1, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree2_search_ids),
  {{ok, Matched2, NonMatched2}, _} = Ret_betree2_search_ids,
  ?assertEqual([3,4], Matched2),
  ?assertEqual([], NonMatched2),

  Ret_betree3_search_ids = erl_betree:betree_search_ids_err(Betree3, Evt, Matched2, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree3_search_ids),
  {{ok, Matched3, NonMatched3}, _} = Ret_betree3_search_ids,
  ?assertEqual([4], Matched3),
  ?assertEqual([], NonMatched3).

search_empty_list_of_ids_test() ->
  Domains = [[
    {par1,bool,disallow_undefined},
    {par2,bool,disallow_undefined}
  ]],
  {ok, Betree1} = erl_betree:betree_make_err(Domains),
  Expr1 = <<"par1">>,
  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub1),

  Expr2 = <<"not par1">>,
  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree1, 2, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree1, Sub2),

  {ok, Betree2} = erl_betree:betree_make_err(Domains),
  Expr3 = <<"par2">>,
  {ok, Sub3} = erl_betree:betree_make_sub_err(Betree2, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub_err(Betree2, Sub3),

  Event = [{bool_event, true, true}],
  Ret_betree_make_event = erl_betree:betree_make_event_err(Betree1, Event),
  ?assertMatch({{ok, _}, _}, Ret_betree_make_event),
  {{ok, Evt}, _} = Ret_betree_make_event,

  Ret_betree1_search = erl_betree:betree_search_err(Betree1, Evt, 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree1_search),
  {{ok, Matched1, NonMatched1}, _} = Ret_betree1_search,
  ?assertEqual([1], Matched1),
  ?assertEqual([{par1,[2]}], NonMatched1),

  % Search empty list of Ids
  Ret_betree2_search_ids = erl_betree:betree_search_ids_err(Betree2, Evt, [], 0),
  ?assertMatch({{ok, _, _}, _}, Ret_betree2_search_ids),
  {{ok, Matched2, NonMatched2}, Elapsed} = Ret_betree2_search_ids,
  ?assertEqual([], Matched2),
  ?assertEqual([], NonMatched2),
  ?assertEqual(0, Elapsed).

not_and_test() ->
  Params = [
    {p1, bool, disallow_undefined},
    {p2, bool, disallow_undefined},
    {p3, bool, disallow_undefined}
  ],

  Event = [{bool_event, true, true, false}],
  % i.e. p1 = true, p2 = true, p3 = false

  Expr1 = <<"not (p1 or (p2 and p3))">>,
  % p1 or (p2 and p3) = true or (true and false) = true
  Id1 = 101,

  Expr2 = <<"not (p3 and p1) or p2">>,
  % (p3 and p1) or p2 = (false and true) or true = true
  Id2 = 202,

  Expr3 = <<"not (not (p3 and p1)) and p2">>,
  % (not (p3 and p1)) and p2 = (not (false and true)) and true = true
  Id3 = 303,

  Expr4 = <<"not (p2 or p3) or p1">>,
  % (p2 or p3) or p1 = (true or false) or true = true
  Id4 = 404,

  {ok, Betree} = erl_betree:betree_make_err([Params]),

  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, Id1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub1),

  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, Id2, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub2),

  {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, Id3, [], Expr3),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub3),

  {ok, Sub4} = erl_betree:betree_make_sub_err(Betree, Id4, [], Expr4),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub4),

  ok = erl_betree:betree_make_sub_ids(Betree),

  Ret = erl_betree:betree_search_err(Betree, Event),
  ?assertMatch({ok, _, _}, Ret),
  {ok, Ids, NonMatches} = Ret,
  ?assertEqual([Id2, Id4], lists:sort(Ids)),
  ?assertEqual([{p1,[101]},{p3,[303]}], lists:sort(NonMatches)).

not_or_test() ->
  Params = [
    {p1, bool, disallow_undefined},
    {p2, bool, disallow_undefined},
    {p3, bool, disallow_undefined}
  ],

  Event = [{bool_event, false, true, false}],
  % i.e. p1 = false, p2 = true, p3 = false

  Expr2 = <<"p1 or (p2 or p3)">>,
  % p1 or (p2 or p3) = false or (true or false) = true
  Id2 = 202,

  Expr3 = <<"(p1 and p2) or p3">>,
  % (p1 and p2) or p3 = (false and true) or false = false
  Id3 = 303,

  Expr4 = <<"(not ((not p1) and p3)) and p2">>,
  % (not ((not p1) and p3)) and p2 =
  % (not ((not false) and false)) and true =
  % (not (true and false)) and true =
  % (not false) and true = true
  Id4 = 404,

  Expr5 = <<"((not p1) or (not p2)) and p3">>,
  % (p1 and p2) or p3 = (false and true) or false = false
  Id5 = 505,

  Expr1 = <<"p2 and not (p3 or p1)">>,
  % p2 and not (p3 or p1) = true and not (false or false) = true
  Id1 = 101,

  {ok, Betree} = erl_betree:betree_make_err([Params]),

  {ok, Sub3} = erl_betree:betree_make_sub_err(Betree, Id3, [], Expr3),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub3),

  {ok, Sub2} = erl_betree:betree_make_sub_err(Betree, Id2, [], Expr2),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub2),

  {ok, Sub4} = erl_betree:betree_make_sub_err(Betree, Id4, [], Expr4),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub4),

  {ok, Sub1} = erl_betree:betree_make_sub_err(Betree, Id1, [], Expr1),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub1),

  {ok, Sub5} = erl_betree:betree_make_sub_err(Betree, Id5, [], Expr5),
  ok = erl_betree:betree_insert_sub_err(Betree, Sub5),

  ok = erl_betree:betree_make_sub_ids(Betree),

  {Ret, _} = erl_betree:betree_search_err(Betree, Event, 0),
  ?assertMatch({ok, _, _}, Ret),
  {ok, Ids, Ids2} = Ret,
  ?assert(is_list(Ids)),
  ?assert(is_list(Ids2)),
  ?assertEqual([Id1, Id2, Id4], lists:sort(Ids)),
  ?assertEqual([{p1, [Id3]}, {p3, [Id5]}], lists:sort(Ids2)).

atom_event_search_reason_test() ->
    Domains = [[
                {b, bool, disallow_undefined}, 
                {i, int, disallow_undefined}, 
                {f, float, disallow_undefined}, 
                {s, bin, disallow_undefined}
               ]],
    Expr1 = <<"b and i = 10 and f < 3.13 and s = \"good\"">>,
    Expr2 = <<"b and i = 10 and f > 3.13 and s = \"bad\"">>,
    Expr3 = <<"b and i = 10 and f < 3.13 and s = \"good\"">>,
    Expr4 = <<"not b and i = 11 and f > 3.13 and s = \"bad\"">>,
    Expr5 = <<"not b and i = 11 and f < 3.13 and s = \"good\"">>,
    Expr6 = <<"not b and i = 11 and f > 3.13 and s = \"bad\"">>,
    Expr7 = <<"not b and i = 11 and f < 3.13 and s = \"good\"">>,
    Event = [#all{
               b = true,
               i = 10,
               f = 3.14,
               s = <<"cool">>
              }],
    {ok, Betree} = erl_betree:betree_make_err(Domains),
    lists:foldl(
      fun(Expr, Num) -> 
              {ok, Sub} = erl_betree:betree_make_sub_err(Betree, Num, [], Expr), 
              ok = erl_betree:betree_insert_sub_err(Betree, Sub), 
              Num + 1
      end, 
      1,
      [Expr1, Expr2, Expr3, Expr4, Expr5, Expr6, Expr7]
    ),
    ok = erl_betree:betree_make_sub_ids(Betree),
    {{ok, [], Res}, _} = erl_betree:betree_search_err(Betree, Event, 0),
    ?assertEqual([{b,[4,5,6,7]},{s,[1,2,3]}], lists:sort(Res)),
    ok.


