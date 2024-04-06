-module(be).

-compile(nowarn_unused_type). % WIP

%% API
-export_type([
]).

-type int64_t() :: integer().
-type double() :: float().

-type val_integer() :: int64_t().
-type val_float() :: double().
-type val_string() :: string().
-type val_bool() :: boolean().

-type global_id() :: int64_t().
-type memoize_id() :: int64_t().

-type var_name() :: atom().
-type var_id() :: int64_t().

-type string_id() :: int64_t().
-type enum_id() :: int64_t().

-type ast_integer() :: {'int', val_integer()}.
-type ast_float() :: {'float', val_float()}.
-type ast_string() ::
  {'bin', val_string()} |
  {'bin', val_string(), var_id(), string_id()}.
-type ast_enum() :: {'int_enum', val_integer()} | {'int_enum', val_integer(), var_id(), enum_id()}.

-type attr_var() :: {'var', var_name()} | {'var', var_name(), var_id()}.

%% WIP
-type expr_list() :: any().
-type expr_special() :: any().
-type expr_null() :: any().

%% WIP - AST_TYPE_SET_EXPR
-type expr_set() :: any().

%% AST_TYPE_BOOL_EXPR
-type op_bool_binary() :: 'or' | 'and'.
-type expr_bool_binary() ::
  {op_bool_binary(), expr(), expr()} |
  {op_bool_binary(), expr(), expr(), global_id(), memoize_id()}.
-type op_bool_unary() :: 'not'.
-type expr_bool_unary() ::
  {op_bool_unary(), expr()} |
  {op_bool_unary(), expr(), global_id(), memoize_id()}.
-type op_bool_var() :: 'bool_var'.
-type attr_bool_var() ::
  {op_bool_var(), var_name()} |
  {op_bool_var(), var_name(), var_id(), global_id(), memoize_id()}.
-type expr_bool_var() :: attr_bool_var().
-type op_bool_lit() :: 'bool_lit'.
-type expr_bool_lit() ::
  {op_bool_lit(), val_bool()} |
  {op_bool_lit(), val_bool(), global_id(), memoize_id()}.
-type expr_bool() ::
  expr_bool_binary() | expr_bool_unary() |
  expr_bool_var() | expr_bool_lit().

%% AST_TYPE_EQUALITY_EXPR
-type op_eq() :: 'eq' | 'ne'.
-type val_eq() :: ast_integer() | ast_float() | ast_string() | ast_enum().
-type expr_eq() ::
  {op_eq(), attr_var(), val_eq()} |
  {op_eq(), attr_var(), val_eq(), global_id(), memoize_id()}.

%% AST_TYPE_COMPARE_EXPR
-type op_cmp() :: 'lt' | 'le' | 'gt' | 'ge'.
-type val_cmp() :: {'int', val_integer()} | {'float', val_float()}.
-type expr_cmp() ::
  {op_cmp(), attr_var(), val_cmp()} |
  {op_cmp(), attr_var(), val_cmp(), global_id(), memoize_id()}.

%% AST
-type expr() ::
  expr_cmp() | expr_eq() | expr_bool() |
  expr_set() | expr_list() |
  expr_special() | expr_null().
-type ast() :: expr().

-export([
  % eval
  eval/2,
  eval_ast/2,

  % pretty_print
  pp/1,
  pp/2,
  % Substitute parameters in AST with values
  reify/2,
  reify/3,
  % extract parameters from AST
  ast_parameters/1,
  % TODO: extract parameters from AST
  %   together with expressions the parameter participate in
  % ast_parameters_with_expressions/1,
  % extract leaf nodes from AST
  ast_leaves/1,

  ast_and/2,
  ast_or/2,
  ast_not/1,
  mk_atoms_map/2,
  union/3,
  union/4,
  mk_random_tree/2,
  enum_prefix/2
]).

%% eval section
eval(Expr, Bs) when is_list(Expr) andalso is_map(Bs) ->
  eval(list_to_binary(Expr), Bs);
eval(Expr, Bs) when is_binary(Expr) andalso is_map(Bs) ->
  case beval:parse(Expr) of
    {ok, Ast} -> eval(Ast, Bs);
    Err -> Err
  end;
eval(Ast, Bs) when is_reference(Ast) andalso is_map(Bs) ->
  case beval:ast(Ast) of
    {ok, AstTerm} -> eval_ast(AstTerm, Bs);
    Err -> Err
  end.

eval_ast(true, _Bs) ->
  true;
eval_ast(false, _Bs) ->
  false;

eval_ast({bool, Atom}, _Bs) ->
  Atom;
eval_ast({bool_lit, Atom}, _Bs) ->
  Atom;
eval_ast({bool_var, Atom} = Ast, Bs) when is_map(Bs) ->
  case maps:get(Atom, Bs, no_param) of
    no_param -> Ast;
    Value -> Value
  end;
eval_ast({var, Atom} = Ast, Bs) when is_map(Bs) ->
  case maps:get(Atom, Bs, no_param) of
    no_param -> Ast;
    Value -> Value
  end;

eval_ast({'not', true}, _Bs) ->
  false;
eval_ast({'not', false}, _Bs) ->
  true;
eval_ast({'not', Ast}, Bs) ->
  case eval_ast(Ast, Bs) of
    true -> false;
    false -> true;
    A -> {'not', A}
  end;

eval_ast({'and', false, _Rhs}, _Bs) ->
  false;
eval_ast({'and', _Lhs, false}, _Bs) ->
  false;
eval_ast({'and', Lhs, Rhs}, Bs) ->
  case eval_ast(Lhs, Bs) of
    false -> false;
    true ->
      case eval_ast(Rhs, Bs) of
        false -> false;
        true -> true;
        R ->
          {'and', true, R}
      end;
    L ->
      case eval_ast(Rhs, Bs) of
        false -> false;
        true ->
          {'and', L, true};
        R ->
          {'and', L, R}
      end
  end;

eval_ast({'or', true, _Rhs}, _Bs) ->
  true;
eval_ast({'or', _Lhs, true}, _Bs) ->
  true;
eval_ast({'or', Lhs, Rhs}, Bs) ->
  case eval_ast(Lhs, Bs) of
    true -> true;
    false ->
      case eval_ast(Rhs, Bs) of
        true -> true;
        false -> false;
        R ->
          {'or', false, R}
      end;
    L ->
      case eval_ast(Rhs, Bs) of
        true -> true;
        false ->
          {'or', L, false};
        R ->
          {'or', L, R}
      end
  end;

eval_ast(AstTerm, _Bs) when is_tuple(AstTerm) ->
  AstTerm.

%% eval section. End


%% pretty print section
pp({bool, true}) ->
  <<"true">>;
pp({bool, false}) ->
  <<"false">>;

pp({bool_lit, true}) ->
  <<"true">>;
pp({bool_lit, false}) ->
  <<"false">>;

pp({bool_var, Lhs}) ->
  atom_to_binary(Lhs);

pp({'and', {bool_var, Lhs}, {bool_var, Rhs}}) ->
  L = atom_to_binary(Lhs),
  R = atom_to_binary(Rhs),
  <<L/binary, " and ", R/binary>>;
pp({'and', {bool_var, Lhs}, Rhs}) ->
  L = atom_to_binary(Lhs),
  R = pp_par(Rhs),
  <<L/binary, " and ", R/binary>>;
pp({'and', Lhs, {bool_var, Rhs}}) ->
  L = pp_par(Lhs),
  R = atom_to_binary(Rhs),
  <<L/binary, " and ", R/binary>>;
pp({'and', Lhs, Rhs}) ->
  L = pp_par(Lhs),
  R = pp_par(Rhs),
  <<L/binary, " and ", R/binary>>;

pp({'or', {bool_var, Lhs}, {bool_var, Rhs}}) ->
  L = atom_to_binary(Lhs),
  R = atom_to_binary(Rhs),
  <<L/binary, " or ", R/binary>>;
pp({'or', {bool_var, Lhs}, Rhs}) ->
  L = atom_to_binary(Lhs),
  R = pp_par(Rhs),
  <<L/binary, " or ", R/binary>>;
pp({'or', Lhs, {bool_var, Rhs}}) ->
  L = pp_par(Lhs),
  R = atom_to_binary(Rhs),
  <<L/binary, " or ", R/binary>>;
pp({'or', Lhs, Rhs}) ->
  L = pp_par(Lhs),
  R = pp_par(Rhs),
  <<L/binary, " or ", R/binary>>;

pp({'not', {bool_var, Atom}}) ->
  E = atom_to_binary(Atom),
  <<"not ", E/binary>>;
pp({'not', Expr}) ->
  E = pp_par(Expr),
  <<"not ", E/binary>>;

pp({eq,{var,Atom},{int_enum,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " = ", R/binary>>;

pp({eq,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " = ", R/binary>>;
pp({ne,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " <> ", R/binary>>;
pp({lt,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " < ", R/binary>>;
pp({le,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " <= ", R/binary>>;
pp({gt,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " > ", R/binary>>;
pp({ge,{var,Atom},{int,Int}}) ->
  L = atom_to_binary(Atom),
  R = integer_to_binary(Int),
  <<L/binary, " >= ", R/binary>>;

pp({eq,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " = ", R/binary>>;
pp({ne,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " <> ", R/binary>>;
pp({lt,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " < ", R/binary>>;
pp({le,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " <= ", R/binary>>;
pp({gt,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " > ", R/binary>>;
pp({ge,{var,Atom},{float,Float}}) ->
  L = atom_to_binary(Atom),
  R = float_bin(Float),
  <<L/binary, " >= ", R/binary>>;

pp({eq,{var,Atom},{bin,Str}}) ->
  L = atom_to_binary(Atom),
  R = list_to_binary(Str),
  <<L/binary, " = ", $", R/binary, $">>;
pp({ne,{var,Atom},{bin,Str}}) ->
  L = atom_to_binary(Atom),
  R = list_to_binary(Str),
  <<L/binary, " <> ", $", R/binary, $">>;

pp({in, {int, Int}, {int_list, [_|_] = Ns}}) ->
  L = integer_to_binary(Int),
  R = pp(Ns, <<>>),
  <<L/binary, " in ", R/binary>>;
pp({not_in, {int, Int}, {int_list, [_|_] = Ns}}) ->
  L = integer_to_binary(Int),
  R = pp(Ns, <<>>),
  <<L/binary, " not in ", R/binary>>;

pp({in, {bin, Str}, {bin_list, [_|_] = Ns}}) ->
  L = list_to_binary(Str),
  R = pp(Ns, <<>>),
  <<$", L/binary, $", " in ", R/binary>>;
pp({not_in, {bin, Str}, {bin_list, [_|_] = Ns}}) ->
  L = list_to_binary(Str),
  R = pp(Ns, <<>>),
  <<$", L/binary, $", " not in ", R/binary>>;

pp({in, {int, Int}, {var, Atom}}) ->
  L = integer_to_binary(Int),
  R = atom_to_binary(Atom),
  <<L/binary, " in ", R/binary>>;
pp({not_in, {int, Int}, {var, Atom}}) ->
  L = integer_to_binary(Int),
  R = atom_to_binary(Atom),
  <<L/binary, " not in ", R/binary>>;

pp({in, {bin, Str}, {var, Atom}}) ->
  L =list_to_binary(Str),
  R = atom_to_binary(Atom),
  <<$", L/binary, $", " in ", R/binary>>;
pp({not_in, {bin, Str}, {var, Atom}}) ->
  L = list_to_binary(Str),
  R = atom_to_binary(Atom),
  <<$", L/binary, $", " not in ", R/binary>>;

pp({in, {var, Atom}, {int_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " in ", R/binary>>;
pp({not_in, {var, Atom}, {int_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " not in ", R/binary>>;

pp({in, {var, Atom}, {bin_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " in ", R/binary>>;
pp({not_in, {var, Atom}, {bin_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " not in ", R/binary>>;

pp({one_of, {var, Atom}, {int_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " one of ", R/binary>>;
pp({none_of, {var, Atom}, {int_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " none of ", R/binary>>;
pp({all_of, {var, Atom}, {int_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " all of ", R/binary>>;

pp({one_of, {var, Atom}, {bin_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " one of ", R/binary>>;
pp({none_of, {var, Atom}, {bin_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " none of ", R/binary>>;
pp({all_of, {var, Atom}, {bin_list, [_|_] = Ns}}) ->
  L = atom_to_binary(Atom),
  R = pp(Ns, <<>>),
  <<L/binary, " all of ", R/binary>>;

pp({is_null,{var,Atom}}) ->
  L = atom_to_binary(Atom),
  <<L/binary, " is null">>;
pp({is_not_null,{var,Atom}}) ->
  L = atom_to_binary(Atom),
  <<L/binary, " is not null">>;
pp({is_empty,{var,Atom}}) ->
  L = atom_to_binary(Atom),
  <<L/binary, " is empty">>;

pp({within_fcap, {var, frequency_caps}, {advertiser, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"advertiser\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {advertiser_ip, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"advertiser:ip\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {campaign, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"campaign\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {campaign_ip, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"campaign:ip\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {campaign_group, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"campaign_group\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {campaign_group_ip, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"campaign_group:ip\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {flight, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"flight\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {flight_ip, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"flight:ip\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {product, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"product\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;
pp({within_fcap, {var, frequency_caps}, {product_ip, _}, {bin, Str}, Int1, Int2, {var, now}}) ->
  S = list_to_binary(Str),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"within_frequency_cap(\"product:ip\", ", $",S/binary,$", ", ", B1/binary, ", ", B2/binary,$)>>;

pp({seg_before, {var, Atom}, Int1, Int2, {var, now}}) ->
  A = atom_to_binary(Atom),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"segment_before(", A/binary, ", ", B1/binary, ", ", B2/binary, $)>>;
pp({seg_before, Int1, Int2, {var, now}}) ->
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"segment_before(", B1/binary, ", ", B2/binary, $)>>;
pp({seg_within, {var, Atom}, Int1, Int2, {var, now}}) ->
  A = atom_to_binary(Atom),
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"segment_within(", A/binary, ", ", B1/binary, ", ", B2/binary, $)>>;
pp({seg_within, Int1, Int2, {var, now}}) ->
  B1 = integer_to_binary(Int1),
  B2 = integer_to_binary(Int2),
  <<"segment_within(", B1/binary, ", ", B2/binary, $)>>;

pp({geo_within_rad,{var,latitude},N1,{var,longitude},N2,N3}) ->
  B1 = number_bin(N1),
  B2 = number_bin(N2),
  B3 = number_bin(N3),
  <<"geo_within_radius(", B1/binary, ", ", B2/binary, ", ", B3/binary, $)>>;

pp({contains,{var,Atom},{string,Str}}) ->
  A = atom_to_binary(Atom),
  S = list_to_binary(Str),
  <<"contains(", A/binary, ", ", $",S/binary,$",$)>>;
pp({starts_with,{var,Atom},{string,Str}}) ->
  A = atom_to_binary(Atom),
  S = list_to_binary(Str),
  <<"starts_with(", A/binary, ", ", $",S/binary,$",$)>>;
pp({ends_with,{var,Atom},{string,Str}}) ->
  A = atom_to_binary(Atom),
  S = list_to_binary(Str),
  <<"ends_with(", A/binary, ", ", $",S/binary,$",$)>>;

pp(X) ->
  E = list_to_binary(lists:flatten(io_lib:format("~tp", [X]))),
  <<$(, E/binary, $)>>.

pp([], Bin) ->
  <<$(, Bin/binary, $)>>;
pp([{int, Int}| T], <<>>) ->
  L = integer_to_binary(Int),
  pp(T, L);
pp([{int, Int}| T], Bin) ->
  L = integer_to_binary(Int),
  pp(T, <<Bin/binary, ", ", L/binary>>);

pp([{bin, Str}| T], <<>>) ->
  L = list_to_binary(Str),
  pp(T, <<$", L/binary, $">>);
pp([{bin, Str}| T], Bin) ->
  L = list_to_binary(Str),
  pp(T, <<Bin/binary, ", ", $", L/binary, $">>);

pp([X | T], Bin) ->
  E = list_to_binary(lists:flatten(io_lib:format("~tp", [X]))),
  pp(T, <<Bin/binary, $(, E/binary, $)>>).

pp_par(X) when is_atom(X) ->
  atom_to_binary(X);
pp_par(X) ->
  E = pp(X),
  <<$(, E/binary, $)>>.

%% utils for pp
float_bin(Float) ->
  case float_bin_float(Float, 8) of
    {Float, Bin} -> Bin;
    _ ->
      case float_bin_float(Float, 16) of
        {Float, Bin} -> Bin;
        _ ->
          {_, Bin} = float_bin_float(Float, 32),
          Bin
      end
  end.

float_bin_float(Float, Decimals) ->
  Bin = float_to_binary(Float, [{decimals, Decimals}, compact]),
  Float1 = binary_to_float(Bin),
  {Float1, Bin}.

number_bin(N) when is_integer(N) ->
  integer_to_binary(N);
number_bin(N) when is_float(N) ->
  float_bin(N).

%% pretty print section. End

%% reify section
reify(Expr, Pvs, Consts) when is_list(Expr)
  andalso is_reference(Pvs) andalso is_reference(Consts) ->
  reify(list_to_binary(Expr), Pvs, Consts);
reify(Expr, Pvs, Consts) when is_binary(Expr)
  andalso is_reference(Pvs) andalso is_reference(Consts) ->
  case beval:parse(Expr) of
    {ok, Ast} -> reify(Ast, Pvs, Consts);
    Err -> Err
  end;
reify(Ast, Pvs, Consts) when is_reference(Ast)
  andalso is_reference(Pvs) andalso is_reference(Consts) ->
  case beval:ast(Ast) of
    {ok, AstTerm} ->
      case be_utils:pvs_and_consts_to_map(Pvs, Consts) of
        {ok, PvMap} ->
          Reified = reify(AstTerm, PvMap),
          {ok, Reified};
        Err -> Err
      end;
    Err -> Err
  end.

%% {bool, Atom} comes from parameter description, like {a, bool, disallow_undefined}
%% {bool_lit, Atom} comes from AST_BOOL_LITERAL, see ast_node.h
%% {bool_var, Atom} comes from AST_BOOL_VARIABLE, see ast_node.h
reify({bool, _Atom} = Ast, _PvMap) ->
  Ast;
reify({bool_lit, _Atom} = Ast, _PvMap) ->
  Ast;
reify({bool_var, Atom} = Ast, PvMap) when is_map(PvMap) ->
  case maps:get(Atom, PvMap, no_param) of
    no_param -> Ast;
    {_T, _V} = Value -> Value
  end;

reify({int, _N} = Ast, _PvMap) ->
  Ast;
reify({int_enum, _N} = Ast, _PvMap) ->
  Ast;
reify({float, _F} = Ast, _PvMap) ->
  Ast;

reify({bin, _S} = Ast, _PvMap) ->
  Ast;

reify({int_list, _L} = Ast, _PvMap) ->
  Ast;

reify({bin_list, _L} = Ast, _PvMap) ->
  Ast;

reify({string, _S} = Ast, _PvMap) ->
  Ast;

reify({var, Atom} = Ast, PvMap) when is_map(PvMap) ->
  case maps:get(Atom, PvMap, no_param) of
    no_param -> Ast;
    {_T, _V} = Value -> Value
  end;

reify({'and', Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {'and', L, R};

reify({'or', Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {'or', L, R};

reify({'not', Ast}, PvMap) when is_map(PvMap) ->
  A = reify(Ast, PvMap),
  {'not', A};

reify({eq, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {eq, L, R};

reify({ne, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {ne, L, R};

reify({lt, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {lt, L, R};

reify({le, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {le, L, R};

reify({gt, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {gt, L, R};

reify({ge, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {ge, L, R};

reify({in, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {in, L, R};

reify({not_in, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {not_in, L, R};

reify({one_of, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {one_of, L, R};

reify({none_of, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {none_of, L, R};

reify({all_of, Lhs, Rhs}, PvMap) when is_map(PvMap) ->
  L = reify(Lhs, PvMap),
  R = reify(Rhs, PvMap),
  {all_of, L, R};

reify({'is_null', Ast}, PvMap) when is_map(PvMap) ->
  A = reify(Ast, PvMap),
  {'is_null', A};

reify({'is_not_null', Ast}, PvMap) when is_map(PvMap) ->
  A = reify(Ast, PvMap),
  {'is_not_null', A};

reify({'is_empty', Ast}, PvMap) when is_map(PvMap) ->
  A = reify(Ast, PvMap),
  {'is_empty', A};

reify(Ast, PvMap) when is_tuple(Ast) andalso is_map(PvMap) ->
  todo.

%% reify section. End

%% AST parameters section

ast_parameters(Ast) when is_tuple(Ast) ->
  lists:sort(
    lists:map(fun ({K, L}) -> {K, lists:sum(L)} end,
      maps:to_list(
        maps:groups_from_list(fun (X) -> X end, fun (_) -> 1 end,
          ast_parameters(Ast, [])))));
ast_parameters(_) ->
  error.

ast_parameters({bool, true}, Acc) ->
  Acc;

ast_parameters({bool, false}, Acc) ->
  Acc;

ast_parameters({bool_lit, true}, Acc) ->
  Acc;

ast_parameters({bool_lit, false}, Acc) ->
  Acc;

ast_parameters({bool_var, Lhs}, Acc) ->
  [{Lhs, bool_var} | Acc];

ast_parameters({'and', {bool_var, Lhs}, {bool_var, Rhs}}, Acc) ->
  [{Rhs, bool_var} | [{Lhs, bool_var} | Acc]];
ast_parameters({'and', {bool_var, Lhs}, Rhs}, Acc) ->
  ast_parameters(Rhs, [{Lhs, bool_var} | Acc]);
ast_parameters({'and', Lhs, {bool_var, Rhs}}, Acc) ->
  ast_parameters(Lhs, [{Rhs, bool_var} | Acc]);
ast_parameters({'and', Lhs, Rhs}, Acc) ->
  ast_parameters(Rhs, (ast_parameters(Lhs, Acc)));

ast_parameters({'or', {bool_var, Lhs}, {bool_var, Rhs}}, Acc) ->
  [{Rhs, bool_var} | [{Lhs, bool_var} | Acc]];
ast_parameters({'or', {bool_var, Lhs}, Rhs}, Acc) ->
  ast_parameters(Rhs, [{Lhs, bool_var} | Acc]);
ast_parameters({'or', Lhs, {bool_var, Rhs}}, Acc) ->
  ast_parameters(Lhs, [{Rhs, bool_var} | Acc]);
ast_parameters({'or', Lhs, Rhs}, Acc) ->
  ast_parameters(Rhs, (ast_parameters(Lhs, Acc)));

ast_parameters({'not', {bool_var, Atom}}, Acc) ->
  [{Atom, bool_var} | Acc];
ast_parameters({'not', Expr}, Acc) ->
  ast_parameters(Expr, Acc);

ast_parameters({eq,{var,Atom},{int_enum,_V}}, Acc) ->
  [{Atom, int_enum} | Acc];

ast_parameters({eq,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({ne,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({lt,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({le,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({gt,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({ge,{var,Atom},{int,_V}}, Acc) ->
  [{Atom, int} | Acc];

ast_parameters({eq,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];
ast_parameters({ne,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];
ast_parameters({lt,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];
ast_parameters({le,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];
ast_parameters({gt,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];
ast_parameters({ge,{var,Atom},{float,_V}}, Acc) ->
  [{Atom, float} | Acc];

ast_parameters({eq,{var,Atom},{bin,_V}}, Acc) ->
  [{Atom, bin} | Acc];
ast_parameters({ne,{var,Atom},{bin,_V}}, Acc) ->
  [{Atom, bin} | Acc];

ast_parameters({in, {int, _Int}, {int_list, [_|_] = _Ns}}, Acc) ->
  Acc;
ast_parameters({not_in, {int, _Int}, {int_list, [_|_] = _Ns}}, Acc) ->
  Acc;

ast_parameters({in, {bin, _Str}, {bin_list, [_|_] = _Ns}}, Acc) ->
  Acc;
ast_parameters({not_in, {bin, _Str}, {bin_list, [_|_] = _Ns}}, Acc) ->
  Acc;

ast_parameters({in, {int, _V}, {var, Atom}}, Acc) ->
  [{Atom, int_list} | Acc];
ast_parameters({not_in, {int, _V}, {var, Atom}}, Acc) ->
  [{Atom, int_list} | Acc];

ast_parameters({in, {bin, _V}, {var, Atom}}, Acc) ->
  [{Atom, bin_list} | Acc];
ast_parameters({not_in, {bin, _V}, {var, Atom}}, Acc) ->
  [{Atom, bin_list} | Acc];

ast_parameters({in, {var, Atom}, {int_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({not_in, {var, Atom}, {int_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, int} | Acc];

ast_parameters({in, {var, Atom}, {bin_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, bin} | Acc];
ast_parameters({not_in, {var, Atom}, {bin_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, bin} | Acc];

ast_parameters({one_of, {var, Atom}, {int_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({none_of, {var, Atom}, {int_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, int} | Acc];
ast_parameters({all_of, {var, Atom}, {int_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, int_list} | Acc];

ast_parameters({one_of, {var, Atom}, {bin_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, bin} | Acc];
ast_parameters({none_of, {var, Atom}, {bin_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, bin} | Acc];
ast_parameters({all_of, {var, Atom}, {bin_list, [_|_] = _Ns}}, Acc) ->
  [{Atom, bin_list} | Acc];

ast_parameters({is_null,{var,Atom}}, Acc) ->
  [{Atom, nullable} | Acc];
ast_parameters({is_not_null,{var,Atom}}, Acc) ->
  [{Atom, nullable} | Acc];
ast_parameters({is_empty,{var,Atom}}, Acc) ->
  [{Atom, emptyable} | Acc];

ast_parameters({within_fcap, {var, frequency_caps}, {advertiser, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{advertiser, advertiser_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {advertiser_ip, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{advertiser_ip, advertiser_ip_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {campaign, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{campaign, campaign_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {campaign_ip, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{campaign_ip, campaign_ip_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {campaign_group, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{campaign_group, campaign_group_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {campaign_group_ip, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{campaign_group_ip, campaign_group_ip_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {flight, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{flight, flight_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {flight_ip, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{flight_ip, flight_ip_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {product, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{product, product_t} | [{frequency_caps, fcaps_t} | Acc]]];
ast_parameters({within_fcap, {var, frequency_caps}, {product_ip, _}, {bin, _Str}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{product_ip, product_ip_t} | [{frequency_caps, fcaps_t} | Acc]]];

ast_parameters({seg_before, {var, Atom}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{Atom, seg_t} | Acc]];
ast_parameters({seg_before, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | Acc];
ast_parameters({seg_within, {var, Atom}, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | [{Atom, seg_t} | Acc]];
ast_parameters({seg_within, _Int1, _Int2, {var, now}}, Acc) ->
  [{now, now_t} | Acc];

ast_parameters({geo_within_rad,{var,latitude},_N1,{var,longitude},_N2,_N3}, Acc) ->
  [{longitude, lon_t} | [{latitude, lat_t} | Acc]];

ast_parameters({contains,{var,Atom},{string,_Str}}, Acc) ->
  [{Atom, string} | Acc];
ast_parameters({starts_with,{var,Atom},{string,_Str}}, Acc) ->
  [{Atom, string} | Acc];
ast_parameters({ends_with,{var,Atom},{string,_Str}}, Acc) ->
  [{Atom, string} | Acc];

ast_parameters(X, Acc) ->
  E = lists:flatten(io_lib:format("~tp", [X])),
  [{E, unknown} | Acc].

%% AST parameters section. end

%% AST leaves section

ast_leaves(Ast) when is_tuple(Ast) ->
  lists:map(fun ({K, L}) -> {K, lists:sum(L)} end,
    maps:to_list(
      maps:groups_from_list(fun (X) -> X end, fun (_) -> 1 end,
        ast_leaves(Ast, []))));
ast_leaves(_) ->
  error.

ast_leaves({bool, true} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({bool, false} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({bool_lit, true} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({bool_lit, false} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({bool_var, _Lhs} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({'and', {bool_var, _Lhs}, {bool_var, _Rhs}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({'and', {bool_var, _Lhs} = Leaf, Rhs}, Acc) ->
  ast_leaves(Rhs, [Leaf | Acc]);
ast_leaves({'and', Lhs, {bool_var, _Rhs} = Leaf}, Acc) ->
  ast_leaves(Lhs, [Leaf | Acc]);
ast_leaves({'and', Lhs, Rhs}, Acc) ->
  ast_leaves(Rhs, (ast_leaves(Lhs, Acc)));

ast_leaves({'or', {bool_var, _Lhs}, {bool_var, _Rhs}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({'or', {bool_var, _Lhs} = Leaf, Rhs}, Acc) ->
  ast_leaves(Rhs, [Leaf | Acc]);
ast_leaves({'or', Lhs, {bool_var, _Rhs} = Leaf}, Acc) ->
  ast_leaves(Lhs, [Leaf | Acc]);
ast_leaves({'or', Lhs, Rhs}, Acc) ->
  ast_leaves(Rhs, (ast_leaves(Lhs, Acc)));

ast_leaves({'not', {bool_var, _Atom}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({'not', Expr}, Acc) ->
  ast_leaves(Expr, Acc);

ast_leaves({eq,{var,_Atom},{int_enum,_V}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({eq,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ne,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({lt,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({le,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({gt,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ge,{var,_Atom},{int,_V}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({eq,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ne,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({lt,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({le,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({gt,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ge,{var,_Atom},{float,_V}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({eq,{var,_Atom},{bin,_V}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ne,{var,_Atom},{bin,_V}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {int, _Int}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {int, _Int}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {bin, _Str}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {bin, _Str}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {int, _V}, {var, _Atom}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {int, _V}, {var, _Atom}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {bin, _V}, {var, _Atom}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {bin, _V}, {var, _Atom}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {var, _Atom}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {var, _Atom}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({in, {var, _Atom}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({not_in, {var, _Atom}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({one_of, {var, _Atom}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({none_of, {var, _Atom}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({all_of, {var, _Atom}, {int_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({one_of, {var, _Atom}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({none_of, {var, _Atom}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({all_of, {var, _Atom}, {bin_list, [_|_] = _Ns}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({is_null,{var,_Atom}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({is_not_null,{var,_Atom}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({is_empty,{var,_Atom}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({within_fcap, {var, frequency_caps}, {_Type, _}, {bin, _Str}, _Int1, _Int2, {var, now}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({seg_before, {var, _Atom}, _Int1, _Int2, {var, now}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({seg_before, _Int1, _Int2, {var, now}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({seg_within, {var, _Atom}, _Int1, _Int2, {var, now}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({seg_within, _Int1, _Int2, {var, now}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({geo_within_rad,{var,latitude},_N1,{var,longitude},_N2,_N3} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves({contains,{var,_Atom},{string,_Str}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({starts_with,{var,_Atom},{string,_Str}} = Leaf, Acc) ->
  [Leaf | Acc];
ast_leaves({ends_with,{var,_Atom},{string,_Str}} = Leaf, Acc) ->
  [Leaf | Acc];

ast_leaves(_X = Leaf, Acc) ->
  [Leaf | Acc].

%% AST leaves section. end

%% Make AST section

ast_and(Lhs, Rhs) ->
  {'and', Lhs, Rhs}.

ast_or(Lhs, Rhs) ->
  {'or', Lhs, Rhs}.

ast_not(Expr) ->
  {'not', Expr}.

mk_atoms_map(Prefix, N) ->
  maps:from_list(
%%    [{I, list_to_atom(Prefix ++ integer_to_list(I))}
    [{I, enum_prefix(Prefix, I)}
      || I <- lists:seq(1, N)]).

enum_prefix(Prefix, N) ->
  list_to_atom(Prefix ++ integer_to_list(N)).

union('and', N_Lhs, N_Rhs, Map) ->
  #{N_Lhs := Lhs, N_Rhs := Rhs} = Map,
  Term = ast_and(Lhs, Rhs),
  union_random(N_Lhs, N_Rhs, Term, Map);
union('or', N_Lhs, N_Rhs, Map) ->
  #{N_Lhs := Lhs, N_Rhs := Rhs} = Map,
  Term = ast_or(Lhs, Rhs),
  union_random(N_Lhs, N_Rhs, Term, Map).

union('not', N, Map) ->
  #{N := T} = Map,
  Term = ast_not(T),
  maps:update(N, Term, Map).

union_random(N_Lhs, N_Rhs, Term, Map) ->
  case rand:uniform(2) of
    1 ->
      union_update_remove(N_Lhs, N_Rhs, Term, Map);
    2 ->
      union_update_remove(N_Rhs, N_Lhs, Term, Map)
  end.

union_update_remove(Key_to_update, Key_to_remove, Term, Map) ->
  M1 = maps:update(Key_to_update, Term, Map),
  M2 = maps:remove(Key_to_remove, M1),
  Vs = maps:values(M2),
  Enumerated = lists:enumerate(Vs),
  maps:from_list(Enumerated).

mk_random_tree(Prefix, N) ->
  [V] = maps:values(
          mk_random_tree(
            mk_atoms_map(Prefix, N))),
  V.

mk_random_tree(Redux) ->
  case maps:size(Redux) of
    0 -> error;
    1 -> Redux;
    N ->
      case mk_random_op() of
        'not' ->
          mk_random_tree(union('not', mk_random(N), Redux));
        'and' ->
          {Lhs, Rhs} = mk_random_pair(N),
          mk_random_tree(union('and', Lhs, Rhs, Redux));
        'or' ->
          {Lhs, Rhs} = mk_random_pair(N),
          mk_random_tree(union('or', Lhs, Rhs, Redux))
      end
  end.

mk_random_op() ->
  case rand:uniform(10) of
    1 -> 'and';
    2 -> 'or';
    3 -> 'not';
    4 -> 'and';
    5 -> 'and';
    6 -> 'and';
    7 -> 'not';
    8 -> 'or';
    9 -> 'or';
    10 -> 'or'
  end.

mk_random(N) ->
  rand:uniform(N).

mk_random_pair(N) ->
  L = mk_random(N),
  R = mk_random_not_equal(L, N),
  {L, R}.

mk_random_not_equal(A, N) ->
  case rand:uniform(N) of
    A -> mk_random_not_equal(A, N);
    M -> M
  end.

%% Make AST section. End
