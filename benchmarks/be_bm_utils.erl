-module(be_bm_utils).

%%
%% Util functions for benchmarking
%%

%% API
-export([
  better_percentage/1,

  file_exists/1,

  betree_allocations/0,
  betree_allocations_diff/2,

  diff_tuple/2,

  write_terms/2
]).

%% Consider a sorted list of numbers where
%%  - negative numbers indicates a 'better';
%%  - positive number indicates a 'worse'.
%% better_percentage finds how many 'betters' are in the list
%%  expressed as percentage.
better_percentage([]) ->
  not_applicable;
better_percentage([Min | _] = NsSorted) ->
  case Min >= 0 of
    true -> 0;
    _ ->
      Max = lists:last(NsSorted),
      case Max =< 0 of
        true -> 100;
        _ ->
          Enumed = lists:enumerate(NsSorted),
          case lists:search(fun ({_N, Rel}) -> Rel >= 0 end, Enumed) of
            false -> unknown;
            {value, {N0, _Rel0}} ->
              Total = length(NsSorted),
              floor((N0/Total) * 100)
          end
      end
  end.

file_exists(FileName) ->
  case file:open(FileName, [read]) of
    {ok, Handle} ->
      file:close(Handle),
      true;
    _ -> false
  end.

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

write_terms(undefined, _Ts) ->
  ok;
write_terms(File, Ts) ->
  Lines = [io_lib:format("~tp.~n", [T]) || T <- Ts],
  file:write_file(File, Lines).
