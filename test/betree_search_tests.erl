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
    erl_betree:betree_write_dot(Betree, "test/betree_search_tests.dot"),
    {Res, _} = erl_betree:betree_search(Betree, Event, 0),
    ?assertEqual({ok, [1, 2, 3]}, Res),
    {Res1, _} = erl_betree:betree_search_ids(Betree, Event, [1, 3], 0),
    ?assertEqual({ok, [1, 3]}, Res1),
    {Res2, _} = erl_betree:betree_search_ids(Betree, Event, [1], 0),
    ?assertEqual({ok, [1]}, Res2),
    {Res3, _} = erl_betree:betree_search_ids(Betree, Event, [3], 0),
    ?assertEqual({ok, [3]}, Res3).


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
      {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr1),
      ok = erl_betree:betree_insert_sub(Betree, Sub1),
      {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
      ok = erl_betree:betree_insert_sub(Betree, Sub2),
      {ok, Sub3} = erl_betree:betree_make_sub(Betree, 3, Consts, Expr2),
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
  