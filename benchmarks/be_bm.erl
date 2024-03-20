-module(be_bm).

-include("be_eval.hrl").

%%
%% Boolean Expression benchmark runner
%%

%% Usage:

%% API
-export([
  std/2,
  std_stats/3,
  std_output/3,
  iterated_all/2,
  iterated_all_stats/3,
  iterated_all_output/3,
  iterated_next/2,
  iterated_next_stats/3,
  iterated_next_output/3,

  std_betree_search/2,
  iterated_betree_search_all/2,
  iterated_betree_search_next/2,
  stats_collector/2,

  % intersect outputs
  intersect/3,
  % compare evaluation outputs
  diff/2,

  compare_stats/2
]).

std(BetreeFile, EventsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event",
    BetreeFile, EventsFile,
    fun std_betree_search/2, fun stats_collector/2),
  be_eval:stop(PidEval),
  Ret.

std_stats(BetreeFile, EventsFile, StatsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event",
    BetreeFile,
    EventsFile, fun std_betree_search/2, _EventEvalOutputFile = undefined,
    fun stats_collector/2, StatsFile),
  be_eval:stop(PidEval),
  Ret.

std_output(BetreeFile, EventsFile, EventEvalOutputFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event",
    BetreeFile,
    EventsFile, fun std_betree_search/2, EventEvalOutputFile,
    fun stats_collector/2, undefined),
  be_eval:stop(PidEval),
  Ret.

iterated_all(BetreeFile, EventsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, all subscriptions",
    BetreeFile, EventsFile,
    fun iterated_betree_search_all/2, fun stats_collector/2),
  be_eval:stop(PidEval),
  Ret.

iterated_all_stats(BetreeFile, EventsFile, StatsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, all subscriptions",
    BetreeFile,
    EventsFile, fun iterated_betree_search_all/2, _EventEvalOutputFile = undefined,
    fun stats_collector/2, StatsFile),
  be_eval:stop(PidEval),
  Ret.

iterated_all_output(BetreeFile, EventsFile, EventEvalOutputFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, all subscriptions",
    BetreeFile,
    EventsFile, fun iterated_betree_search_all/2, EventEvalOutputFile,
    fun stats_collector/2, undefined),
  be_eval:stop(PidEval),
  Ret.

iterated_next(BetreeFile, EventsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, subscriptions one by one",
    BetreeFile, EventsFile,
    fun iterated_betree_search_next/2, fun stats_collector/2),
  be_eval:stop(PidEval),
  Ret.

iterated_next_stats(BetreeFile, EventsFile, StatsFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, subscriptions one by one",
    BetreeFile,
    EventsFile, fun iterated_betree_search_next/2, _EventEvalOutputFile = undefined,
    fun stats_collector/2, StatsFile),
  be_eval:stop(PidEval),
  Ret.

iterated_next_output(BetreeFile, EventsFile, EventEvalOutputFile) ->
  {ok, PidEval} = be_eval:start_link(),
  Ret = be_eval:run(PidEval, "BE-Tree search event with iterator, subscriptions one by one",
    BetreeFile,
    EventsFile, fun iterated_betree_search_next/2, EventEvalOutputFile,
    fun stats_collector/2, undefined),
  be_eval:stop(PidEval),
  Ret.

std_betree_search(Term, #be_evaluator{betree = Betree} = Context) ->
  Event = [list_to_tuple(
    [event | [case N of 1 -> true; _ -> false end|| N <- Term]])],
  BeginNano = erlang:monotonic_time(nanosecond),
  SearchRet = erl_betree:betree_search(Betree, Event),
  EndNano = erlang:monotonic_time(nanosecond),
  CurrentAllocations = be_bm_utils:betree_allocations(),
  DiffNano = EndNano - BeginNano,
  case SearchRet of
    {ok, Ids} -> {{ok, {Ids, {DiffNano, CurrentAllocations}}}, Context};
    X -> {{error, {betree_search, X}}, Context}
  end.

iterated_betree_search_all(Term, #be_evaluator{betree = Betree} = Context) ->
  Event = [list_to_tuple(
    [event | [case N of 1 -> true; _ -> false end|| N <- Term]])],
  BeginNano = erlang:monotonic_time(nanosecond),
  case erl_betree:search_iterator(Betree, Event) of
    {ok, {Iterator, _, _}} ->
      case erl_betree:search_all(Iterator) of
        {ok, Ids} ->
          EndNano = erlang:monotonic_time(nanosecond),
          CurrentAllocations = be_bm_utils:betree_allocations(),
          DiffNano = EndNano - BeginNano,
          {{ok, {Ids, {DiffNano, CurrentAllocations}}}, Context};
        X -> {{error, {search_all, X}}, Context}
      end;
    X -> {{error, {search_iterator, X}}, Context}
  end.

iterated_betree_search_next(Term, #be_evaluator{betree = Betree} = Context) ->
  Event = [list_to_tuple(
    [event | [case N of 1 -> true; _ -> false end|| N <- Term]])],
  BeginNano = erlang:monotonic_time(nanosecond),
  case erl_betree:search_iterator(Betree, Event) of
    {ok, {Iterator, _, _}} ->
      case iterated_betree_search_next(Iterator) of
        {ok, Ids} ->
          EndNano = erlang:monotonic_time(nanosecond),
          CurrentAllocations = be_bm_utils:betree_allocations(),
          DiffNano = EndNano - BeginNano,
          {{ok, {Ids, {DiffNano, CurrentAllocations}}}, Context};
        {error, Err} -> {{error, {search_all, Err}}, Context}
      end;
    X -> {{error, {search_iterator, X}}, Context}
  end.

iterated_betree_search_next(Iterator) ->
  case erl_betree:search_next(Iterator) of
    {continue, _} ->
      iterated_betree_search_next(Iterator);
    {ok, _Ids} = Ret ->
      Ret;
    X -> {error, {search_next, X}}
  end.

stats_collector({DiffNano, Allocations}, #be_evaluator_stats{
  info = Info,
  index = Index,
  snapshot_freq = SnapshotFreq,
  snapshot_allocations = SnapshotAllocations,
  allocation_diffs = AllocationsDiffs,
  snapshot_nano_acc = SnapshotNanoAcc,
  nano_diffs = NanoDiffs
} = Stats) ->
  Index1 = Index + 1,
  CurrentTime = calendar:universal_time_to_local_time(erlang:universaltime()),
  SnapshotNanoAcc1 = SnapshotNanoAcc + DiffNano,
  Stats1 =
    case Index1 rem SnapshotFreq of
      0 ->
        NanoPerEvent = SnapshotNanoAcc1 / SnapshotFreq,
        MicroPerEvent = NanoPerEvent / 1_000,
        io:format(Info ++ ": ~p, ~p microseconds/event~n", [Index1, ceil(MicroPerEvent)]),
        AllocationDiff = be_bm_utils:betree_allocations_diff(SnapshotAllocations, Allocations),
        io:format(Info ++ " allocations diff:~n~p~n", [AllocationDiff]),
        Stats#be_evaluator_stats{
          current_time = CurrentTime,
          index = Index1,
          current_allocations = Allocations,
          snapshot_allocations = Allocations,
          allocation_diffs = [AllocationDiff | AllocationsDiffs],
          snapshot_nano_acc = 0,
          nano_diffs = [SnapshotNanoAcc1 | NanoDiffs]};
      _ ->
        Stats#be_evaluator_stats{
          current_time = CurrentTime,
          index = Index1,
          current_allocations = Allocations,
          snapshot_nano_acc = SnapshotNanoAcc1}
    end,
  {ok, Stats1}.

intersect([_|_] = FileName1, [_|_] = FileName2, [_|_] = FileNameOutput) ->
  {ok, PidIn1} = term_reader:start_link(FileName1),
  {ok, PidIn2} = term_reader:start_link(FileName2),
  {ok, PidOut} = term_writer:start_link(FileNameOutput),
  Ret = intersect(PidIn1, PidIn2, PidOut),
  term_writer:stop(PidOut),
  term_reader:stop(PidIn2),
  term_reader:stop(PidIn1),
  Ret;

intersect(PidIn1, PidIn2, PidOut) when
  is_pid(PidIn1), is_pid(PidIn2), is_pid(PidOut) ->
  Ret1 = term_reader:read(PidIn1),
  case Ret1 of
    eof -> ok;
    {error, _Reason} = Err -> Err;
    {ok, Term1} ->
      Ret2 = term_reader:read(PidIn2),
      case Ret2 of
        eof -> ok;
        {error, _Reason} = Err -> Err;
        {ok, Term2} ->
          Map1 = maps:from_keys(Term1, undefined),
          Map2 = maps:from_keys(Term2, undefined),
          Map = maps:intersect(Map1, Map2),
          TermOut = maps:keys(Map),
          term_writer:write(PidOut, TermOut),
          intersect(PidIn1, PidIn2, PidOut)
      end
  end;

intersect(_, _, _) ->
  {error, wrong_parameters}.


diff(FileName1, FileName2) ->
  {ok, _} = term_reader:start_link(in1, FileName1),
  {ok, _} = term_reader:start_link(in2, FileName2),
  diff(in1, in2, 0, []).

diff(Reader1, Reader2, Index, Acc) ->
  Index1 = Index + 1,
  Ret1 = term_reader:read(Reader1),
  case Ret1 of
    eof ->
      {Index, lists:reverse(Acc)};
    {error, Reason} ->
      {Index1, lists:reverse([{error, {Reader1, Reason}} | Acc])};
    {ok, Term1} ->
      Ret2 = term_reader:read(Reader2),
      case Ret2 of
        eof ->
          {Index, lists:reverse(Acc)};
        {error, Reason} ->
          {Index1, lists:reverse([{error, {Reader2, Reason}} | Acc])};
        {ok, Term2} ->
          Sorted1 = lists:sort(Term1),
          Sorted2 = lists:sort(Term2),
          case Sorted1 =:= Sorted2 of
            true ->
              diff(Reader1, Reader2, Index1, Acc);
            _ ->
              diff(Reader1, Reader2, Index1, [{Index1, Term1, Term2} | Acc])
          end
      end
  end.

compare_stats(BaseFileName, TargetFileName) ->
  case file:consult(BaseFileName) of
    {ok, BaseStats} ->
      case file:consult(TargetFileName) of
        {ok, TargetStats} ->
          Base_n_events = proplists:get_value(n_events, BaseStats),
          Target_n_events = proplists:get_value(n_events, TargetStats),
          case Base_n_events =/= Target_n_events of
            true ->
              {error, {{base_n_events, Base_n_events}, {target_n_events, Target_n_events}}};
            _ ->
              Base_snapshot_freq = proplists:get_value(snapshot_freq, BaseStats),
              Target_snapshot_freq = proplists:get_value(snapshot_freq, TargetStats),
              case Base_snapshot_freq =/= Target_snapshot_freq of
                true ->
                  {error, {{base_snapshot_freq, Base_snapshot_freq}, {target_snapshot_freq, Target_snapshot_freq}}};
                _ ->
                  _Base_info = proplists:get_value(info, BaseStats),
                  _Target_info = proplists:get_value(info, TargetStats),
                  Base_nano_diffs = proplists:get_value(nano_diffs, BaseStats),
                  Target_nano_diffs = proplists:get_value(nano_diffs, TargetStats),
                  Diffs = lists:zip(Base_nano_diffs, Target_nano_diffs),
                  AbsRelDiffs = [{Target-Base, ceil((Target-Base)*100/Base)} || {Base, Target} <- Diffs],
                  {AbsDiffs, RelDiffs} = lists:unzip(AbsRelDiffs),

                  SortedAbsDiffs = lists:sort(AbsDiffs),
                  SortedRelDiffs = lists:sort(RelDiffs),

                  % min / max
                  [AbsMinDiff | _] = SortedAbsDiffs,
                  [RelMinDiff | _] = SortedRelDiffs,
                  AbsMaxDiff = lists:last(SortedAbsDiffs),
                  RelMaxDiff = lists:last(SortedRelDiffs),
                  AbsMinDiffPerEvent = AbsMinDiff / Base_snapshot_freq,
                  AbsMaxDiffPerEvent = AbsMaxDiff / Base_snapshot_freq,

                  N_snapshots = ceil(Base_n_events / Base_snapshot_freq),

                  % median
                  MedianIdx = ceil(N_snapshots/2),
                  AbsMedian = lists:nth(MedianIdx, SortedAbsDiffs),
                  RelMedian = lists:nth(MedianIdx, SortedRelDiffs),
                  AbsMedianPerEvent = AbsMedian / Base_snapshot_freq,

                  % 99th percentile
                  NinetyNineIdx = ceil(N_snapshots * 99 / 100),
                  AbsNinetyNine = lists:nth(NinetyNineIdx, SortedAbsDiffs),
                  RelNinetyNine = lists:nth(NinetyNineIdx, SortedRelDiffs),
                  AbsNinetyNinePerEvent = AbsNinetyNine / Base_snapshot_freq,

                  % average
                  AbsAvgPerEvent = lists:sum(AbsDiffs) / Base_n_events,
                  RelAvg = lists:sum(RelDiffs) / N_snapshots,

                  % better
                  RelBetter = be_bm_utils:better_percentage(SortedRelDiffs),

                  [
                    {min, {relative, RelMinDiff, '%'}, {value, ceil(AbsMinDiffPerEvent/1_000), microsecond}},
                    {max, {relative, RelMaxDiff, '%'}, {value, ceil(AbsMaxDiffPerEvent/1_000), microsecond}},
                    {avg, {relative, ceil(RelAvg), '%'}, {value, ceil(AbsAvgPerEvent/1_000), microsecond}},
                    {median, {relative, RelMedian, '%'}, {value, ceil(AbsMedianPerEvent/1_000), microsecond}},
                    {'99th', {relative, RelNinetyNine, '%'}, {value, ceil(AbsNinetyNinePerEvent/1_000), microsecond}},
                    {better, {RelBetter, '%'}}
                  ]
              end
          end;
        X -> X
      end;
    X -> X
  end.
