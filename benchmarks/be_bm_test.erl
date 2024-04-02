-module(be_bm_test).

-include_lib("eunit/include/eunit.hrl").

-export([
  trace_1/0,
  trace_2/0
]).

trace_1() ->
  L = [1, 2, 3],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L]),
  io:format("~p~n", [Zipped]).

trace_2() ->
  L1 = [1, 2, 3],
  L2 = [a, b, c],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  io:format("~p~n", [Zipped]).

zip_many_1_test() ->
  L = [1, 2, 3],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L]),
%%  ?debugFmt("~n~p~n", [Zipped]).
  ?assertEqual(length(L), length(Zipped)),
  ?assertEqual([[1], [2], [3]], Zipped).

zip_many_2_test() ->
  L1 = [1, 2, 3],
  L2 = [a, b, c],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  ?assertEqual(length(L1), length(Zipped)),
  ?assertEqual([[1, a], [2, b], [3, c]], Zipped).

zip_many_1_string_test() ->
  L = ["one", "two", "three"],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L]),
  ?assertEqual(length(L), length(Zipped)),
  ?assertEqual([["one"],["two"],["three"]], Zipped).

zip_many_3_test() ->
  L1 = [1, 2, 3],
  L2 = [a, b, c],
  L3 = ["one", "two", "three"],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2, L3]),
  ?assertEqual(length(L1), length(Zipped)),
  ?assertEqual([[1, a, "one"], [2, b, "two"], [3, c, "three"]], Zipped).

zip_many_zip3_test() ->
  L1 = [1, 2, 3],
  L2 = [a, b, c],
  L3 = ["one", "two", "three"],
  Combine = fun (Xs) -> list_to_tuple(Xs) end,
  Zipped = be_tools:zip_many(Combine, [L1, L2, L3]),
  ?assertEqual(length(L1), length(Zipped)),
  ?assertEqual(lists:zip3(L1, L2, L3), Zipped).

zip_many_empty_test() ->
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, []),
  ?assertEqual([], Zipped).

zip_many_one_of_empty_test() ->
  L = [],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L]),
  ?assertEqual([[]], Zipped).

zip_many_empty_non_empty_test() ->
  L1 = [],
  L2 = [2],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  ?assertEqual([[]], Zipped).

zip_many_non_empty_empty_test() ->
  L1 = [1],
  L2 = [],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  ?assertEqual([[]], Zipped).

zip_many_short_long_test() ->
  L1 = [1, 2],
  L2 = [a, b, c],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  ?assertEqual([[1, a], [2, b]], Zipped).

zip_many_long_short_test() ->
  L1 = [1, 2, 3],
  L2 = [a, b],
  Combine = fun (Xs) -> Xs end,
  Zipped = be_tools:zip_many(Combine, [L1, L2]),
  ?assertEqual([[1, a], [2, b]], Zipped).

concatenate_and_empty_test() ->
  Bin = be_tools:concatenate_and([]),
  ?assertEqual(<<>>, Bin).

concatenate_and_one_list_one_expr_test() ->
  Exprs = [<<"par">>],
  Bin = be_tools:concatenate_and([Exprs]),
  ?assertEqual(Exprs, Bin).

concatenate_and_one_list_two_exprs_test() ->
  Exprs = [<<"par1">>, <<"par2">>],
  Bin = be_tools:concatenate_and([Exprs]),
  ?assertEqual(Exprs, Bin).

concatenate_and_two_lists_one_expr_test() ->
  Exprs1 = [<<"par1">>],
  Exprs2 = [<<"par2">>],
  Bin = be_tools:concatenate_and([Exprs1, Exprs2]),
  ?assertEqual([<<"(par1) and (par2)">>], Bin).

concatenate_and_two_lists_two_expr_test() ->
  Exprs1 = [<<"par1">>, <<"par1 or par2">>],
  Exprs2 = [<<"par2">>, <<"par1">>],
  Bin = be_tools:concatenate_and([Exprs1, Exprs2]),
  ?assertEqual([
    <<"(par1) and (par2)">>,
    <<"(par1 or par2) and (par1)">>
    ], Bin).

concatenate_and_three_lists_two_expr_test() ->
  Exprs1 = [<<"par1">>, <<"par1 or par2">>],
  Exprs2 = [<<"par2">>, <<"par1">>],
  Exprs3 = [<<"par3">>, <<"par3 and par1">>],
  Bin = be_tools:concatenate_and([Exprs1, Exprs2, Exprs3]),
  ?assertEqual([
    <<"(par1) and (par2) and (par3)">>,
    <<"(par1 or par2) and (par1) and (par3 and par1)">>
  ], Bin).

concatenate_and_three_lists_three_expr_test() ->
  Exprs1 = [<<"par1">>, <<"par1 or par2">>, <<"par3 or par2">>],
  Exprs2 = [<<"par2">>, <<"par1">>, <<"par3">>],
  Exprs3 = [<<"par3">>, <<"par3 and par1">>, <<"par1">>],
  Bin = be_tools:concatenate_and([Exprs1, Exprs2, Exprs3]),
  ?assertEqual([
    <<"(par1) and (par2) and (par3)">>,
    <<"(par1 or par2) and (par1) and (par3 and par1)">>,
    <<"(par3 or par2) and (par3) and (par1)">>
  ], Bin).

all_bool_events_0_test() ->
  Events = be_tools:all_bool_events(0),
  ?assertEqual([], Events).

all_bool_events_1_test() ->
  Events = be_tools:all_bool_events(1),
  ?assertEqual([
    [true],
    [false]],
    Events).

all_bool_events_2_test() ->
  Events = be_tools:all_bool_events(2),
  ?assertEqual([
    [true, true], [true, false],
    [false, true], [false, false]],
    Events).

all_bool_events_3_test() ->
  Events = be_tools:all_bool_events(3),
  ?assertEqual([
    [true, true, true], [true, true, false],
    [true, false, true], [true, false, false],
    [false, true, true], [false, true, false],
    [false, false, true], [false, false, false]
  ], Events).

all_bool_events_4_test() ->
  Events = be_tools:all_bool_events(4),
  ?assertEqual([
    [true, true, true, true], [true, true, true, false],
    [true, true, false, true], [true, true, false, false],
    [true, false, true, true], [true, false, true, false],
    [true, false, false, true], [true, false, false, false],
    [false, true, true, true], [false, true, true, false],
    [false, true, false, true], [false, true, false, false],
    [false, false, true, true], [false, false, true, false],
    [false, false, false, true], [false, false, false, false]
  ], Events).

% discrepancy_report_P_3_Ex_12.txt
% Expression 8
discrepancy_report_P_3_Ex_12_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],
  Expr1 = <<"(p1_3 or p1_1) and p1_2">>,
  Expr2 = <<"p2_3 and (not (p2_1 and p2_2))">>,
  Expr3 = <<"(p3_1 or (not p3_2)) and p3_3">>,
  Expr = <<"((p1_3 or p1_1) and p1_2) and (p2_3 and (not (p2_1 and p2_2))) and ((p3_1 or (not p3_2)) and p3_3)">>,
  Event = [true,true,true,true,false,true,true,true,true],

  % Results from discrepancy_report_P_3_12.txt
  _StdIds = {std_ids,[2,4,7,8,9,12]},
  _P1Ids = {p1_ids,[2,3,4,5,6,7,8,9,10,11,12]},
  _P2Ids = {p2_ids,[2,4,6,7,9,12]},
  _P3Ids = {p3_ids,[2,4,7,9,12]},

  {ok, Betree} = erl_betree:betree_make([Params]),
  {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
  ok = erl_betree:betree_insert_sub(Betree, Sub),

  {ok, Betree1} = erl_betree:betree_make([Params]),
  {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, [], Expr1),
  ok = erl_betree:betree_insert_sub(Betree1, Sub1),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 1, [], Expr2),
  ok = erl_betree:betree_insert_sub(Betree2, Sub2),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  {ok, Sub3} = erl_betree:betree_make_sub(Betree3, 1, [], Expr3),
  ok = erl_betree:betree_insert_sub(Betree3, Sub3),

  E = [list_to_tuple([bool_event | Event])],

  {ok, Ids} = erl_betree:betree_search(Betree, E),

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),
  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  {{ok, Ids2}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  {{ok, Ids3}, _} = erl_betree:betree_search(Betree3, CompiledEvent, 0),

%%  ?debugFmt("~n Ids: ~p~n", [Ids]),
%%  ?debugFmt("~nIds1: ~p~n", [Ids1]),
%%  ?debugFmt("~nIds2: ~p~n", [Ids2]),
%%  ?debugFmt("~nIds3: ~p~n", [Ids3]),
  ?assertEqual([1], Ids),
  ?assertEqual([1], Ids1),
  ?assertEqual([1], Ids2),
  ?assertEqual([1], Ids3),

  {{ok, Ids2Piped}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
  {{ok, Ids3Piped}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids2Piped, 0),

%%  ?debugFmt("~nIds2Piped: ~p~n", [Ids2Piped]),
%%  ?debugFmt("~nIds3Piped: ~p~n", [Ids3Piped]).
  ?assertEqual([1], Ids2Piped),
  ?assertEqual([1], Ids3Piped).

% discrepancy_report_P_3_Ex_4.txt
discrepancy_report_P_3_Ex_4_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  Exprs = [
    <<"((p1_1 and p1_2) and p1_3) and (p2_1 or (p2_2 and p2_3)) and ((p3_2 or p3_1) and p3_3)">>,
    <<"((not p1_3) and (p1_2 and p1_1)) and ((not (p2_3 and p2_1)) and p2_2) and ((p3_2 and p3_1) or p3_3)">>,
    <<"(p1_3 and (p1_1 or (not p1_2))) and ((p2_3 and p2_1) or p2_2) and (p3_3 and (p3_1 and (not p3_2)))">>,
    <<"(p1_2 and (p1_1 or p1_3)) and ((p2_2 or p2_3) or p2_1) and (p3_2 and (not (p3_3 or p3_1)))">>
  ],
  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,
    <<"(not p1_3) and (p1_2 and p1_1)">>,
    <<"p1_3 and (p1_1 or (not p1_2))">>,
    <<"p1_2 and (p1_1 or p1_3)">>
  ],
  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,
    <<"(not (p2_3 and p2_1)) and p2_2">>,
    <<"(p2_3 and p2_1) or p2_2">>,
    <<"(p2_2 or p2_3) or p2_1">>
  ],
  Exprs3 = [
    <<"(p3_2 or p3_1) and p3_3">>,
    <<"(p3_2 and p3_1) or p3_3">>,
    <<"p3_3 and (p3_1 and (not p3_2))">>,
    <<"p3_2 and (not (p3_3 or p3_1))">>
  ],
  Event = [true,true,false,true,true,false,true,true,true],

  % Results from discrepancy_report_P_3_Ex_4.txt
  _StdIds = {std_ids,[2]},
  _P1Ids = {p1_ids,[2,4]},
  _P2Ids = {p2_ids,[4]},
  _P3Ids = {p3_ids,[]},

  {ok, Betree} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree, Sub)
    end, lists:enumerate(Exprs)),

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
      {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
      ok = erl_betree:betree_insert_sub(Betree1, Sub)
    end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
      {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
      ok = erl_betree:betree_insert_sub(Betree2, Sub)
    end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
      {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
      ok = erl_betree:betree_insert_sub(Betree3, Sub)
    end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {ok, Ids} = erl_betree:betree_search(Betree, E),
  ?assertEqual([2], Ids),

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),
  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([2, 4], Ids1),

  {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
%%  ?assertEqual([4], Ids2),
  ?assertEqual([2, 4], Ids2),

  {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
%%  ?assertEqual([], Ids3).
  ?assertEqual([2], Ids3).

exprs_1_2_3_event_from_1_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,
    <<"(not p1_3) and (p1_2 and p1_1)">>,
    <<"p1_3 and (p1_1 or (not p1_2))">>,
    <<"p1_2 and (p1_1 or p1_3)">>
  ],
  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,
    <<"(not (p2_3 and p2_1)) and p2_2">>,
    <<"(p2_3 and p2_1) or p2_2">>,
    <<"(p2_2 or p2_3) or p2_1">>
  ],
  Exprs3 = [
    <<"(p3_2 or p3_1) and p3_3">>,
    <<"(p3_2 and p3_1) or p3_3">>,
    <<"p3_3 and (p3_1 and (not p3_2))">>,
    <<"p3_2 and (not (p3_3 or p3_1))">>
  ],
  Event = [true,true,false,true,true,false,true,true,true],

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree3, Sub)
                end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),

  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([2, 4], Ids1),

  {{ok, Ids2}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
%%  ?assertEqual([1, 3, 4], Ids2),
  ?assertEqual([1, 2, 3, 4], Ids2),

  {{ok, Ids3}, _} = erl_betree:betree_search(Betree3, CompiledEvent, 0),
  ?assertEqual([1, 2], Ids3).

exprs_1_2_3_event_from_2_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,
    <<"(not p1_3) and (p1_2 and p1_1)">>,
    <<"p1_3 and (p1_1 or (not p1_2))">>,
    <<"p1_2 and (p1_1 or p1_3)">>
  ],
  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,
    <<"(not (p2_3 and p2_1)) and p2_2">>,
    <<"(p2_3 and p2_1) or p2_2">>,
    <<"(p2_2 or p2_3) or p2_1">>
  ],
  Exprs3 = [
    <<"(p3_2 or p3_1) and p3_3">>,
    <<"(p3_2 and p3_1) or p3_3">>,
    <<"p3_3 and (p3_1 and (not p3_2))">>,
    <<"p3_2 and (not (p3_3 or p3_1))">>
  ],
  Event = [true,true,false,true,true,false,true,true,true],

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree3, Sub)
                end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([2, 4], Ids1),

  {{ok, Ids2}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
%%  ?assertEqual([1, 3, 4], Ids2),
  ?assertEqual([1, 2, 3, 4], Ids2),

  {{ok, Ids3}, _} = erl_betree:betree_search(Betree3, CompiledEvent, 0),
  ?assertEqual([1, 2], Ids3).

exprs_1_2_3_event_from_3_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [true,true,false,true,true,false,true,true,true],

  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,       % (true and true) and false       = false
    <<"(not p1_3) and (p1_2 and p1_1)">>, % (not false) and (true and true) = true
    <<"p1_3 and (p1_1 or (not p1_2))">>,  % false and (true or (not true))  = false
    <<"p1_2 and (p1_1 or p1_3)">>         % true and (true or false)        = true
  ],
  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],
  Exprs3 = [
    <<"(p3_2 or p3_1) and p3_3">>,        % (true or true) and true         = true
    <<"(p3_2 and p3_1) or p3_3">>,        % (true and true) or true         = true
    <<"p3_3 and (p3_1 and (not p3_2))">>, % true and (true and (not true))  = false
    <<"p3_2 and (not (p3_3 or p3_1))">>   % true and (not (true or true))   = false
  ],

  % Exprs1 and Exprs2 and Exprs3  = Exprs
  % false and true and true       = false
  % true and true and true        = true
  % false and true and false      = false
  % true and true and false       = false
  % Result: [2]

  % Pipe: Exprs1 => Exprs2 => Exprs3 => Result
  % Pipe: [2, 4] => [1, 2, 3, 4] x [2, 4] = [2, 4] => [1, 2] x [2, 4] = [2]

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree3, Sub)
                end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree3, E),

  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([2, 4], Ids1),

  {{ok, Ids2}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
%%  ?assertEqual([1, 3, 4], Ids2),
  ?assertEqual([1, 2, 3, 4], Ids2),

  {{ok, Ids3}, _} = erl_betree:betree_search(Betree3, CompiledEvent, 0),
  ?assertEqual([1, 2], Ids3).

pipe_3_2_1_event_from_1_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,
    <<"(not p1_3) and (p1_2 and p1_1)">>,
    <<"p1_3 and (p1_1 or (not p1_2))">>,
    <<"p1_2 and (p1_1 or p1_3)">>
  ],
  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,
    <<"(not (p2_3 and p2_1)) and p2_2">>,
    <<"(p2_3 and p2_1) or p2_2">>,
    <<"(p2_2 or p2_3) or p2_1">>
  ],
  Exprs3 = [
    <<"(p3_2 or p3_1) and p3_3">>,
    <<"(p3_2 and p3_1) or p3_3">>,
    <<"p3_3 and (p3_1 and (not p3_2))">>,
    <<"p3_2 and (not (p3_3 or p3_1))">>
  ],
  Event = [true,true,false,true,true,false,true,true,true],

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree3, Sub)
                end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),

  % [1, 2]
  {{ok, Ids3}, _} = erl_betree:betree_search(Betree3, CompiledEvent, 0),
  ?assertEqual([1, 2], Ids3),

  % [1, 3, 4]
  {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids3, 0),
%%  ?assertEqual([1], Ids2),
  ?assertEqual([1, 2], Ids2),

  % [2, 4]
  {{ok, Ids1}, _} = erl_betree:betree_search_ids(Betree1, CompiledEvent, Ids2, 0),
%%  ?assertEqual([], Ids1).
  ?assertEqual([2], Ids1).

discrepancy_P_3_Ex_3_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  Exprs = [
%%    <<"((p1_1 and p1_2) and p1_3) and (p2_1 or (p2_2 and p2_3)) and ((p3_2 or p3_1) and p3_3)">>,
    <<"((not p1_3) and (p1_2 and p1_1)) and ((not (p2_3 and p2_1)) and p2_2) and ((p3_2 and p3_1) or p3_3)">>,
    <<"(p1_3 and (p1_1 or (not p1_2))) and ((p2_3 and p2_1) or p2_2) and (p3_3 and (p3_1 and (not p3_2)))">>,
    <<"(p1_2 and (p1_1 or p1_3)) and ((p2_2 or p2_3) or p2_1) and (p3_2 and (not (p3_3 or p3_1)))">>
  ],
  Exprs1 = [
%%    <<"(p1_1 and p1_2) and p1_3">>,
    <<"(not p1_3) and (p1_2 and p1_1)">>,
    <<"p1_3 and (p1_1 or (not p1_2))">>,
    <<"p1_2 and (p1_1 or p1_3)">>
  ],
  Exprs2 = [
%%    <<"p2_1 or (p2_2 and p2_3)">>,
    <<"(not (p2_3 and p2_1)) and p2_2">>,
    <<"(p2_3 and p2_1) or p2_2">>,
    <<"(p2_2 or p2_3) or p2_1">>
  ],
  Exprs3 = [
%%    <<"(p3_2 or p3_1) and p3_3">>,
    <<"(p3_2 and p3_1) or p3_3">>,
    <<"p3_3 and (p3_1 and (not p3_2))">>,
    <<"p3_2 and (not (p3_3 or p3_1))">>
  ],
  Event = [true,true,false,true,true,false,true,true,true],

  % Results from discrepancy_report_P_3_Ex_4.txt
  _StdIds = {std_ids,[2]},
  _P1Ids = {p1_ids,[2,4]},
  _P2Ids = {p2_ids,[4]},
  _P3Ids = {p3_ids,[]},

  {ok, Betree} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree, Sub)
                end, lists:enumerate(Exprs)),

  {ok, Betree1} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Betree2} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Betree3} = erl_betree:betree_make([Params]),
  lists:foreach(fun ({I, E}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree3, I, [], E),
    ok = erl_betree:betree_insert_sub(Betree3, Sub)
                end, lists:enumerate(Exprs3)),

  E = [list_to_tuple([bool_event | Event])],

  {ok, Ids} = erl_betree:betree_search(Betree, E),
  ?assertEqual([1], Ids),

  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),
  {{ok, Ids1}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([1, 3], Ids1),

  {{ok, Ids2}, _} = erl_betree:betree_search_ids(Betree2, CompiledEvent, Ids1, 0),
  ?assertEqual([1, 3], Ids2),

  {{ok, Ids3}, _} = erl_betree:betree_search_ids(Betree3, CompiledEvent, Ids2, 0),
  ?assertEqual([1], Ids3).

event_vs_compiled_event_exprs1_test() ->
  Params = [
    {p1_1,bool,disallow_undefined},
    {p1_2,bool,disallow_undefined},
    {p1_3,bool,disallow_undefined},
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined},
    {p3_1,bool,disallow_undefined},
    {p3_2,bool,disallow_undefined},
    {p3_3,bool,disallow_undefined}],

  {ok, Betree1} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [true,true,false,true,true,false,true,true,true],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree1, E),

  Exprs1 = [
    <<"(p1_1 and p1_2) and p1_3">>,       % (true and true) and false       = false
    <<"(not p1_3) and (p1_2 and p1_1)">>, % (not false) and (true and true) = true
    <<"p1_3 and (p1_1 or (not p1_2))">>,  % false and (true or (not true))  = false
    <<"p1_2 and (p1_1 or p1_3)">>         % true and (true or false)        = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree1, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree1, Sub)
                end, lists:enumerate(Exprs1)),

  {ok, Ids1} = erl_betree:betree_search(Betree1, E),
  ?assertEqual([2, 4], Ids1),

  {{ok, Ids1UsingCompiledEvent}, _} = erl_betree:betree_search(Betree1, CompiledEvent, 0),
  ?assertEqual([2, 4], Ids1UsingCompiledEvent),


  {ok, Betree2} = erl_betree:betree_make([Params]),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.

%%
%%  Exprs3 = [
%%    <<"(p3_2 or p3_1) and p3_3">>,        % (true or true) and true         = true
%%    <<"(p3_2 and p3_1) or p3_3">>,        % (true and true) or true         = true
%%    <<"p3_3 and (p3_1 and (not p3_2))">>, % true and (true and (not true))  = false
%%    <<"p3_2 and (not (p3_3 or p3_1))">>   % true and (not (true or true))   = false
%%  ],

exprs2_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.

exprs2_2_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
%%    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>    % (not (false and true)) and true = true
%%    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
%%    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1], Ids2), % THIS IS RIGHT ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
  ok.

exprs2_1_2_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>  % (not (false and true)) and true = true
%%    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
%%    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2], Ids2), % THIS IS RIGHT ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
  ok.

exprs2_1_2_3_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_3 and p2_1) or p2_2">>         % (false and true) or true        = true
%%    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3], Ids2), % THIS IS RIGHT ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
  ok.

exprs2_1_2_3_4_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 3, 4], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.

exprs2_2_1_3_4_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([2, 3, 4], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([2, 3, 4], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.

exprs2_1_3_2_4_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(not (p2_3 and p2_1)) and p2_2">>, % (not (false and true)) and true = true
    <<"(p2_2 or p2_3) or p2_1">>          % (true or false) or true         = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 2, 4], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 2, 4], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.

exprs2_1_3_4_2_exact_params_test() ->
  Params = [
    {p2_1,bool,disallow_undefined},
    {p2_2,bool,disallow_undefined},
    {p2_3,bool,disallow_undefined}
  ],

  {ok, Betree2} = erl_betree:betree_make([Params]),

  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
%%  Event = [true,true,false,true,true,false,true,true,true],
  %        p1_1 p1_2 p1_3  p2_1 p2_2 p2_3  p3_1 p3_2 p3_3
  Event = [                true,true,false                 ],
  E = [list_to_tuple([bool_event | Event])],
  {{ok, CompiledEvent}, _} = erl_betree:betree_make_event(Betree2, E),

  Exprs2 = [
    <<"p2_1 or (p2_2 and p2_3)">>,        % true or (true and false)        = true
    <<"(p2_3 and p2_1) or p2_2">>,        % (false and true) or true        = true
    <<"(p2_2 or p2_3) or p2_1">>,         % (true or false) or true         = true
    <<"(not (p2_3 and p2_1)) and p2_2">>  % (not (false and true)) and true = true
  ],

  lists:foreach(fun ({I, Expr}) ->
    {ok, Sub} = erl_betree:betree_make_sub(Betree2, I, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree2, Sub)
                end, lists:enumerate(Exprs2)),

  {ok, Ids2} = erl_betree:betree_search(Betree2, E),
  ?assertEqual([1, 2, 3, 4], Ids2), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 2, 3], Ids2), % THIS IS WRONG ASSERT

  {{ok, Ids2UsingCompiledEvent}, _} = erl_betree:betree_search(Betree2, CompiledEvent, 0),
  ?assertEqual([1, 2, 3, 4], Ids2UsingCompiledEvent), % THIS IS RIGHT ASSERT
%%  ?assertEqual([1, 2, 3], Ids2UsingCompiledEvent), % THIS IS WRONG ASSERT
  ok.
