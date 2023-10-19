-module(betree_tests).

-include_lib("eunit/include/eunit.hrl").

-record(freq_test, {freq, now}).
freq_test() ->
    Domains = [[
        {frequency_caps, frequency_caps, disallow_undefined},
        {now, int64, disallow_undefined}
        ]],
     Consts = [{flight_id, 10},
               {advertiser_id, 20},
               {campaign_id, 30},
               {product_id, 40},
               {unknown, 50}],
    Expr = <<"within_frequency_cap(\"invalid\", \"ns\", 0, 0)">>,
    {ok, Betree} = erl_betree:betree_make(Domains),
    ?assertError(undef, erb_betree:betree_make_sub(Betree, 1, Consts, Expr)),
    Event = [#freq_test{freq = [{{<<"secret">>, 0, <<"ns">>}, 0, undefined}], now = 0}],
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)).

-record(iolist_test, { a, b, c }).
iolist_test() ->
    Domains = [[{a, int, disallow_undefined}, {b, int, disallow_undefined}, {c, int, disallow_undefined}]],
    Expr = ["a = ", [integer_to_list(3), [" and b = 6"], <<" and c =">>, [[integer_to_binary(9)]]]],
    Event = [#iolist_test{ a = 3, b = 6, c = 9 }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1]} = erl_betree:betree_search(Betree, Event).

-record(basic_test, { i }).
basic_test() ->
    Domains = [[{i, int, disallow_undefined, 0, 10}]],
    Expr = <<"i = 5">>,
    Event = [#basic_test{ i = 5 }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1]} = erl_betree:betree_search(Betree, Event).
basic_t_test() ->
    Domains = [[{i, int, disallow_undefined, 0, 10}]],
    Expr = <<"i = 5">>,
    Event = [#basic_test{ i = 5 }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1], N} = erl_betree:betree_search(Betree, Event, 6),
    true = is_integer(N).

bad_insert_no_domain_test() ->
    Expr = <<"i = 5">>,
    {ok, Betree} = erl_betree:betree_make([]),
    {error, failed} = erl_betree:betree_make_sub(Betree, 1, [], Expr).

atom_all_domain_types_test() ->
    Domains = [[
                {integer, int, allow_undefined},
                {integer2, int, disallow_undefined, 0, 10},
                {integer_list, int_list, allow_undefined},
                {integer_list2, int_list, disallow_undefined, 0, 10},
                {boolean, bool, allow_undefined},
                {boolean2, bool, disallow_undefined},
                {string, bin, allow_undefined},
                {string2, bin, disallow_undefined, 5},
                {string_list, bin_list, allow_undefined},
                {string_list2, bin_list, disallow_undefined, 5},
                {float, float, allow_undefined},
                {float2, float, disallow_undefined, 0.0, 10.0},
                {frequency, frequency_caps, allow_undefined},
                {frequency2, frequency_caps, disallow_undefined},
                {segment, segments, allow_undefined},
                {segment2, segments, disallow_undefined}
               ]],
    {ok, _Betree} = erl_betree:betree_make(Domains).

-record(single, { a, b }).
atom_single_domain_list_test() ->
    Domains = [[
                {a, bool, disallow_undefined}, 
                {b, bool, disallow_undefined} 
               ]],
    Expr = <<"a and b">>,
    Event = [#single{ a = true, b = true }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1]} = erl_betree:betree_search(Betree, Event).

-record(first, { a }).
-record(second, { b }).
atom_multiple_domain_list_test() ->
    Domains = [
               [ {a, bool, disallow_undefined} ], 
               [ {b, bool, disallow_undefined} ]
              ],
    Expr = <<"a and b">>,
    Event = [#first{ a = true },#second{ b = true }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1]} = erl_betree:betree_search(Betree, Event).

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
    Expr = <<
             "b and "
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
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [1]} = erl_betree:betree_search(Betree, Event).

-record(constant_test, { frequency_caps, now }).
constant_test() ->
    Domains = [[
                {frequency_caps, frequency_caps, disallow_undefined},
                {now, int64, disallow_undefined}
               ]],
    Consts = [{flight_id, 10},
              {advertiser_id, 20},
              {campaign_id, 30},
              {product_id, 40}],
    Expr = <<"within_frequency_cap(\"flight\", \"ns\", 100, 0)">>,
    Event = [#constant_test{ 
               frequency_caps = [{{<<"flight">>, 10, <<"ns">>}, 100, undefined}],
               now = 0
              }],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {error, failed} = erl_betree:betree_make_sub(Betree, 1, [], Expr),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, []} = erl_betree:betree_search(Betree, Event).

-record(bad_type_test, { bool }).
bad_type_test() ->
    Domains = [[
                {bool, bool, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"bool">>,
    Event = [#bad_type_test{bool = 1}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)).

-record(bad_frequency_caps, { l }).
bad_frequency_caps_test() ->
    Domains = [[
                {l, frequency_caps, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"true">>,
    Event = [#bad_frequency_caps{l = [10]}],
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = true}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = 10}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = 10.0}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = <<"a">>}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = [10]}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = [<<"a">>]}], error)
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)).

-record(handle_undef_in_search, {def}).
handle_undef_in_search_test() ->
    Domains = [[
                {def, int, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"def = 10">>,
    Event = [#handle_undef_in_search{}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)).

-record(bug_geo_request,{exchange, member_id, latitude, longitude}).
-record(bug_geo_impression, {width, height, types}).
bug_geo_test() ->
    Domains = [[{exchange, int, disallow_undefined},
                {member_id, int, disallow_undefined},
                {latitude, float, allow_undefined},
                {longitude, float, allow_undefined}],
               [{width, int, disallow_undefined},
                {height, int, disallow_undefined},
                {types, int_list, disallow_undefined}]],
    Consts = [],
    Expr1 = <<"(((width = 100) and (height = 200) " 
              "and (1 in types) and true and true)) and (((exchange = 2) " 
              "and (member_id = 0) and true))">>,
    Expr2 = <<"(((width = 100) and (height = 200) " 
              "and (1 in types) and true and true)) and (((exchange = 2) " 
              "and (member_id = 0) and true)) and geo_within_radius(100.0, 100.0, 10.0)">>,
    Event = [#bug_geo_request{exchange = 2, member_id = 0, latitude = 100.0, longitude = 100.0},
             #bug_geo_impression{width = 100, height = 200, types = [1]}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub2),
    {ok, [1,2]} = erl_betree:betree_search(Betree, Event).

-record(multiple,{i}).
multiple_trees_test() ->
    Domains = [[{i, int, disallow_undefined}]],
    Consts = [],
    Expr1 = <<"i < 5">>,
    Expr2 = <<"i > 5">>,
    Event1 = [#multiple{i = 3}],
    Event2 = [#multiple{i = 8}],
    {ok, Betree1} = erl_betree:betree_make(Domains),
    {ok, Betree2} = erl_betree:betree_make(Domains),
    {ok, Sub1} = erl_betree:betree_make_sub(Betree1, 1, Consts, Expr1),
    ok = erl_betree:betree_insert_sub(Betree1, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub(Betree2, 2, Consts, Expr2),
    ok = erl_betree:betree_insert_sub(Betree2, Sub2),
    {ok, [1]} = erl_betree:betree_search(Betree1, Event1),
    {ok, [ ]} = erl_betree:betree_search(Betree1, Event2),
    {ok, [ ]} = erl_betree:betree_search(Betree2, Event1),
    {ok, [2]} = erl_betree:betree_search(Betree2, Event2),
    {ok, [1]} = erl_betree:betree_search(Betree1, Event1),
    {ok, [ ]} = erl_betree:betree_search(Betree1, Event2).

bad_domain_type_test() ->
    Domains = [[{i1, int, allow_undefined},
                {i2, innt, allow_undefined},
                {i3, int, allow_undefined}]],
    ?assertError(badarg, erl_betree:betree_make(Domains)).

bad_domain_undef_test() ->
    Domains = [[{i1, int, allow_undefined},
                {i2, int, alloww_undefined},
                {i3, int, allow_undefined}]],
    ?assertError(badarg, erl_betree:betree_make(Domains)).

out_of_bound_integer_expression_test() ->
    Domains = [[{i, int, allow_undefined, 0, 10}]],
    {ok, Betree} = erl_betree:betree_make(Domains),
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 1, [], <<"i > 12">>),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    Event = [#multiple{i = 15}],
    {ok, [1]} = erl_betree:betree_search(Betree, Event).

-record(list_bug, {il}).

list_bug_test() ->
    Domains = [[{il, int_list, disallow_undefined, 1, 2}]],
    Event  = [#list_bug{il = [1,2]}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Ids = [1,2,3,4,5],
    Exprs = [{Id, <<"1 in il">>} || Id <- Ids],
    lists:foreach(fun ({Id, Expr}) -> 
        {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
        ok = erl_betree:betree_insert_sub(Betree, Sub),
        ok
    end, Exprs),
    {ok, Ids} = erl_betree:betree_search(Betree, Event).

-record(int_enum, {i}).

int_enum_test() ->
    Domains = [[{i, int_enum, disallow_undefined}]],
    Event = [#int_enum{i = 9}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Expr1 = <<"i = 8329">>,
    Expr2 = <<"i = 9">>,
    Expr3 = <<"i = 9988767">>,
    Expr4 = <<"i = 28">>,
    Expr5 = <<"i = 456">>,
    {ok, Sub1} = erl_betree:betree_make_sub(Betree, 1, [], Expr1),
    ok = erl_betree:betree_insert_sub(Betree, Sub1),
    {ok, Sub2} = erl_betree:betree_make_sub(Betree, 2, [], Expr2),
    ok = erl_betree:betree_insert_sub(Betree, Sub2),
    {ok, Sub3} = erl_betree:betree_make_sub(Betree, 3, [], Expr3),
    ok = erl_betree:betree_insert_sub(Betree, Sub3),
    {ok, Sub4} = erl_betree:betree_make_sub(Betree, 4, [], Expr4),
    ok = erl_betree:betree_insert_sub(Betree, Sub4),
    {ok, Sub5} = erl_betree:betree_make_sub(Betree, 5, [], Expr5),
    ok = erl_betree:betree_insert_sub(Betree, Sub5),
    {ok, [2]} = erl_betree:betree_search(Betree, Event).

-record(memory, {i}).
memory_test() ->
    Domains = [[{i, int, disallow_undefined}]],
    Event = [#memory{i = 5}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Ids = [1,2,3,4,5,6,7,8,9],
    Exprs = [ {Id, list_to_binary(io_lib:format("i = ~p", [Id]))} || Id <- Ids ],
    lists:foreach(fun ({Id, Expr}) -> 
        {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
        ok = erl_betree:betree_insert_sub(Betree, Sub),
        ok
    end, Exprs),
    {ok, [5]} = erl_betree:betree_search(Betree, Event),
    io:format(user, "memory = ~p~n", [erlang:memory()]).

-record(exists, {i}).
exists_test() ->
    Domains = [[{i, int, disallow_undefined}]],
    GoodEvent = [#exists{i = 0}],
    BadEvent = [#exists{i = 1}],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Exprs = lists:map(fun (I) -> {I, <<"i = 0">>} end, lists:seq(1, 10000)),
    lists:foreach(fun ({Id, Expr}) -> 
        {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
        ok = erl_betree:betree_insert_sub(Betree, Sub),
        ok
    end, Exprs),
    T1 = os:timestamp(),
    {ok, _Subs} = erl_betree:betree_search(Betree, GoodEvent),
    T2 = os:timestamp(),
    D1 = timer:now_diff(T2, T1),
    T3 = os:timestamp(),
    {ok, true} = erl_betree:betree_exists(Betree, GoodEvent),
    T4 = os:timestamp(),
    D2 = timer:now_diff(T4, T3),
    ?assertEqual(true, D1 > D2),
    io:format(user, "good: search = ~p, exists = ~p~n", [D1, D2]),
    T5 = os:timestamp(),
    {ok, []} = erl_betree:betree_search(Betree, BadEvent),
    T6 = os:timestamp(),
    D3 = timer:now_diff(T6, T5),
    T7 = os:timestamp(),
    {ok, false} = erl_betree:betree_exists(Betree, BadEvent),
    T8 = os:timestamp(),
    D4 = timer:now_diff(T8, T7),
    io:format(user, "bad: search = ~p, exists = ~p~n", [D3, D4]).

-record(sub, {i}).
sub_test() ->
    Domains = [[{i, int, disallow_undefined}]],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Id = 0,
    Expr = <<"i = 0">>,
    {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    Event = [#sub{i = 0}],
    {ok, [Id]} = erl_betree:betree_search(Betree, Event).

-record(freq_bug, {now, frequency_caps}).
frequency_bug_test() ->
    Domains = [[{now, int, disallow_undefined}, {frequency_caps, frequency_caps, disallow_undefined}]],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Expr = <<"within_frequency_cap(\"flight:ip\", \"3495614\", 1, 5184000)">>,
    Consts = [
        {campaign_id, 50650},
        {advertiser_id, 6573},
        {flight_id, 101801}
    ],
    {ok, Sub} = erl_betree:betree_make_sub(Betree, 0, Consts, Expr),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    Event = [#freq_bug{now = 1541704800, frequency_caps = [{{<<"flight:ip">>, 101801, <<"3495614">>}, 1, 1546537569676283}]}],
    {ok, []} = erl_betree:betree_search(Betree, Event).

make_sub_exceptions_test() ->
    {ok, Betree} = erl_betree:betree_make([]),
    %Can't actually test bad_arity
    {error, bad_id} = erl_betree:betree_make_sub(Betree, bad_id, [], <<"true">>),
    {error, bad_constant_list} = erl_betree:betree_make_sub(Betree, 0, bad_constant_list, <<"true">>),
    {error, bad_constant, 0, unknown} = erl_betree:betree_make_sub(Betree, 0, [not_a_tuple], <<"true">>),
    {error, bad_constant, 0, unknown} = erl_betree:betree_make_sub(Betree, 0, [{"not atom", value}], <<"true">>),
    {error, bad_constant, 0, bad} = erl_betree:betree_make_sub(Betree, 0, [{bad, value}], <<"true">>),
    {error, bad_binary} = erl_betree:betree_make_sub(Betree, 0, [], bad_binary),
    ok.

-record(unsorted, {il}).
unsorted_test() ->
    Domains = [[{il, int_list, allow_undefined}, {sl, bin_list, allow_undefined}]],
    {ok, Betree} = erl_betree:betree_make(Domains),
    Id = 0,
    {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], <<"2 in il">>),
    ok = erl_betree:betree_insert_sub(Betree, Sub),
    {ok, [Id]} = erl_betree:betree_search(Betree, [#unsorted{il = [9, 10, 77, 29, 29, 84, 2]}]),
    ok.


-type domain() :: int | float | bool | bin | int_list | bin_list | int_enum.
-record(valid, {var}).
-type valid_value() :: null | term().
-type validity() :: invalid | {valid, valid_value(), valid_value()}.
-spec run_valid(domain(), boolean(), binary(), validity()) -> ok.
run_valid(Domain, AllowUndefinedBool, Expr, Validity) ->
    AllowUndefined =
            case AllowUndefinedBool of
                true -> allow_undefined;
                false -> disallow_undefined
            end,
    Domains = [[{var, Domain, AllowUndefined}]],
    Id = random:uniform(100),
    {ok, Betree} = erl_betree:betree_make(Domains),
    case Validity of
        invalid -> 
            {error, _} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
            ok;
        {valid, MatchVar, NoMatchVar} ->
            {ok, Sub} = erl_betree:betree_make_sub(Betree, Id, [], Expr),
            ok = erl_betree:betree_insert_sub(Betree, Sub),
            MatchValue =
                case MatchVar of
                    null -> [];
                    _ -> [#valid{var = MatchVar}]
                end,
            {ok, [Id]} = erl_betree:betree_search(Betree, MatchValue),
            NoMatchValue =
                case NoMatchVar of
                    null -> [];
                    _ -> [#valid{var = NoMatchVar}]
                end,
            {ok, []} = erl_betree:betree_search(Betree, NoMatchValue),
            ok
    end.

valid_comparison_integer_test() ->
    Expr = <<"var < 1">>,
    ok = run_valid(int, true, Expr, {valid, 0, 2}),
    ok = run_valid(float, true, Expr, {valid, 0.5, 2.0}), % fix_float_with_no_fractions fixes this
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_comparison_float_test() ->
    Expr = <<"var < 1.2">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, {valid, 0.9, 2.4}),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_equality_integer_test() ->
    Expr = <<"var = 1">>,
    ok = run_valid(int, true, Expr, {valid, 1, 0}),
    ok = run_valid(float, true, Expr, {valid, 1.0, 0.0}), % fix_float_with_no_fractions fixes this
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, {valid, 1, 0}),
    ok.

valid_equality_float_test() ->
    Expr = <<"var = 1.0">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, {valid, 1.0, 0.0}),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_equality_string_test() ->
    Expr = <<"var = \"value\"">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, {valid, <<"value">>, <<"wrong">>}),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_boolean_test() ->
    Expr = <<"var">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, {valid, true, false}),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_set_left_integer_test() ->
    Expr = <<"1 in var">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, {valid, [0,1,2], [0,2]}),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_set_left_string_test() ->
    Expr = <<"\"value\" in var""">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, {valid, [<<"wrong">>, <<"value">>, <<"another">>], [<<"wrong">>, <<"another">>]}),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_set_right_integer_list_test() ->
    Expr = <<"var in (0, 2)">>,
    ok = run_valid(int, true, Expr, {valid, 2, 1}),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_set_right_string_list_test() ->
    Expr = <<"var in (\"value\", \"another\")">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, {valid, <<"value">>, <<"wrong">>}),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_list_integer_list_test() ->
    Expr = <<"var one of (0, 2)">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, {valid, [1, 2], [1, 3]}),
    ok = run_valid(bin_list, true, Expr, invalid),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_list_string_list_test() ->
    Expr = <<"var one of (\"value\", \"nope\")">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, {valid, [<<"wrong">>, <<"value">>], [<<"wrong">>, <<"no">>]}),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.

valid_null_test() ->
    Expr = <<"var is null">>,
    ok = run_valid(int, true, Expr, {valid, null, 1}),
    ok = run_valid(int, false, Expr, invalid),
    ok = run_valid(float, true, Expr, {valid, null, 1.1}),
    ok = run_valid(float, false, Expr, invalid),
    ok = run_valid(bool, true, Expr, {valid, null, true}),
    ok = run_valid(bool, false, Expr, invalid),
    ok = run_valid(bin, true, Expr, {valid, null, <<"value">>}),
    ok = run_valid(bin, false, Expr, invalid),
    ok = run_valid(int_list, true, Expr, {valid, null, [1, 2]}),
    ok = run_valid(int_list, false, Expr, invalid),
    ok = run_valid(bin_list, true, Expr, {valid, null, [<<"value">>, <<"another">>]}),
    ok = run_valid(bin_list, false, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, {valid, null, 1}),
    ok = run_valid(int_enum, false, Expr, invalid),
    ok.

valid_empty_test() ->
    Expr = <<"var is empty">>,
    ok = run_valid(int, true, Expr, invalid),
    ok = run_valid(float, true, Expr, invalid),
    ok = run_valid(bool, true, Expr, invalid),
    ok = run_valid(bin, true, Expr, invalid),
    ok = run_valid(int_list, true, Expr, {valid, [], [1,2]}),
    ok = run_valid(bin_list, true, Expr, {valid, [], [<<"value">>, <<"another">>]}),
    ok = run_valid(segments, true, Expr, invalid),
    ok = run_valid(frequency_caps, true, Expr, invalid),
    ok = run_valid(int_enum, true, Expr, invalid),
    ok.
