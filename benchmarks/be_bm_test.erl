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
