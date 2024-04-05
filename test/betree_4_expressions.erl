-module(betree_4_expressions).

-include_lib("eunit/include/eunit.hrl").

four_expressions_case_1_test() ->
  Params = [
    {p1, bool, disallow_undefined},
    {p2, bool, disallow_undefined},
    {p3, bool, disallow_undefined}
  ],

  Event = [{bool_event, true, true, false}],
  % i.e. p1 = true, p2 = true, p3 = false

  Expr1 = <<"p1 or (p2 and p3)">>,
  % manual calculation:
  % p1 or (p2 and p3) = true or (true and false) = true
  Id1 = 101,

  Expr2 = <<"(p3 and p1) or p2">>,
  % manual calculation:
  % (p3 and p1) or p2 = (false and true) or true = true
  Id2 = 202,

  Expr3 = <<"(not (p3 and p1)) and p2">>,
  % manual calculation:
  % (not (p3 and p1)) and p2 = (not (false and true)) and true = true
  Id3 = 303,

  Expr4 = <<"(p2 or p3) or p1">>,
  % manual calculation:
  % (p2 or p3) or p1 = (true or false) or true = true
  Id4 = 404,

  {ok, Betree} = erl_betree:betree_make([Params]),

  {ok, Sub1} = erl_betree:betree_make_sub(Betree, Id1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree, Sub1),

  {ok, Sub2} = erl_betree:betree_make_sub(Betree, Id2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree, Sub2),

  {ok, Sub3} = erl_betree:betree_make_sub(Betree, Id3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree, Sub3),

  {ok, Sub4} = erl_betree:betree_make_sub(Betree, Id4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree, Sub4),

  Ret = erl_betree:betree_search(Betree, Event),
  ?assertMatch({ok, _}, Ret),
  {ok, Ids} = Ret,
  ?assert(is_list(Ids)),
%%  ?assertEqual(4, length(Ids)),
  ?assertEqual([Id1, Id2, Id3, Id4], lists:sort(Ids)).

four_expressions_case_2_test() ->
  Params = [
    {p1, bool, disallow_undefined},
    {p2, bool, disallow_undefined},
    {p3, bool, disallow_undefined}
  ],

  Event = [{bool_event, false, true, false}],
  % i.e. p1 = false, p2 = true, p3 = false

  Expr1 = <<"p2 and (p3 or p1)">>,
  % manual calculation:
  % p2 and (p3 or p1) = true and (false or false) = false
  Id1 = 101,

  Expr2 = <<"p1 or (p2 or p3)">>,
  % manual calculation:
  % p1 or (p2 or p3) = false or (true or false) = true
  Id2 = 202,

  Expr3 = <<"(p1 and p2) or p3">>,
  % manual calculation:
  % (p1 and p2) or p3 = (false and true) or false = false
  Id3 = 303,

  Expr4 = <<"(not ((not p1) and p3)) and p2">>,
  % manual calculation:
  % (not ((not p1) and p3)) and p2 =
  % (not ((not false) and false)) and true =
  % (not (true and false)) and true =
  % (not false) and true = true
  Id4 = 404,

  {ok, Betree} = erl_betree:betree_make([Params]),

  {ok, Sub1} = erl_betree:betree_make_sub(Betree, Id1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree, Sub1),

  {ok, Sub2} = erl_betree:betree_make_sub(Betree, Id2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree, Sub2),

  {ok, Sub3} = erl_betree:betree_make_sub(Betree, Id3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree, Sub3),

  {ok, Sub4} = erl_betree:betree_make_sub(Betree, Id4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree, Sub4),

  Ret = erl_betree:betree_search(Betree, Event),
  ?assertMatch({ok, _}, Ret),
  {ok, Ids} = Ret,
  ?assert(is_list(Ids)),
  ?assertEqual([Id2, Id4], lists:sort(Ids)).

four_expressions_case_3_test() ->
  Params = [
    {p1, bool, disallow_undefined},
    {p2, bool, disallow_undefined},
    {p3, bool, disallow_undefined}
  ],

  Event = [{bool_event, false, true, false}],
  % i.e. p1 = false, p2 = true, p3 = false

  Expr3 = <<"(p1 and p2) or p3">>,
  % manual calculation:
  % (p1 and p2) or p3 = (false and true) or false = false
  Id3 = 303,

  Expr2 = <<"p1 or (p2 or p3)">>,
  % manual calculation:
  % p1 or (p2 or p3) = false or (true or false) = true
  Id2 = 202,

  Expr4 = <<"(not ((not p1) and p3)) and p2">>,
  % manual calculation:
  % (not ((not p1) and p3)) and p2 =
  % (not ((not false) and false)) and true =
  % (not (true and false)) and true =
  % (not false) and true = true
  Id4 = 404,

  Expr1 = <<"p2 and not (p3 or p1)">>,
  % manual calculation:
  % p2 and not (p3 or p1) = true and not (false or false) = true
  Id1 = 101,

  {ok, Betree} = erl_betree:betree_make([Params]),

  {ok, Sub3} = erl_betree:betree_make_sub(Betree, Id3, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree, Sub3),

  {ok, Sub2} = erl_betree:betree_make_sub(Betree, Id2, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree, Sub2),

  {ok, Sub4} = erl_betree:betree_make_sub(Betree, Id4, [], Expr4),
  ok = erl_betree:betree_insert_sub(Betree, Sub4),

  {ok, Sub1} = erl_betree:betree_make_sub(Betree, Id1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree, Sub1),

  Ret = erl_betree:betree_search(Betree, Event),
  ?assertMatch({ok, _}, Ret),
  {ok, Ids} = Ret,
  ?assert(is_list(Ids)),
  ?assertEqual([Id1, Id2, Id4], lists:sort(Ids)).
