-module(be_loader).

-behaviour(gen_server).

%%
%% Loads Boolean Expressions into BE-Tree
%%

-include("be_eval.hrl").

%% API
-export([
  start_link/0,
  start_link/1,
  stop/1,
  load/2,
  load_many/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(be_loader_state, {
  id :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?SERVER, [], []).
-spec(start_link(Id :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id) ->
  gen_server:start_link({local, Id}, ?MODULE, [Id], []).

%% @doc Stops the server
stop(ServerRef) ->
  gen_server:stop(ServerRef).

%% @doc Loads Boolean Expressions from file
load(ServerRef, FileName) ->
  gen_server:call(ServerRef, {load, FileName}, infinity).

%% @doc Loads Boolean Expressions from files.
%%  Each file contains boolean expressions for a separate BE-Tree, i.e.
%%  number of created BE-Trees will correspond to the number of files.
load_many(ServerRef, FileNames) ->
  gen_server:call(ServerRef, {load_many, FileNames}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #be_loader_state{}} | {ok, State :: #be_loader_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #be_loader_state{}};
init([Id]) ->
  {ok, #be_loader_state{id = Id}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #be_loader_state{}) ->
  {reply, Reply :: term(), NewState :: #be_loader_state{}} |
  {reply, Reply :: term(), NewState :: #be_loader_state{}, timeout() | hibernate} |
  {noreply, NewState :: #be_loader_state{}} |
  {noreply, NewState :: #be_loader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #be_loader_state{}} |
  {stop, Reason :: term(), NewState :: #be_loader_state{}}).
handle_call({load, FileName}, _From, State) ->
  case be_bm_utils:file_exists(FileName) of
    false ->
      {reply, {error, {file_does_not_exist, FileName}}, State};
    true ->
      case term_reader:start_link(FileName) of
        {error, _Reason} = Err ->
          {reply, Err, State};
        {ok, PidReader} ->
          case term_reader:read(PidReader) of
            eof ->
              term_reader:stop(PidReader),
              {reply, {error, be_tree_parameters_not_provided}, State};
            {error, _Reason} = Err ->
              term_reader:stop(PidReader),
              {reply, Err, State};
            {ok, Term} ->
              Domains = [Term],
              case erl_betree:betree_make(Domains) of
                {ok, Betree} ->
                  Consts = [],
                  Index = 0,
                  BeEvaluator = #be_evaluator{
                    betree = Betree,
                    consts = Consts,
                    index = Index
                  },
                  {ok, PidEval} = term_eval:start_link(fun add_expr/2, BeEvaluator),

                  StartTime = calendar:universal_time_to_local_time(erlang:universaltime()),
                  SnapshotFreq = 100,
                  Allocations = be_bm_utils:betree_allocations(),
                  AllocationDiffs = [],
                  NanoDiffs = [],
                  Nano = erlang:monotonic_time(nanosecond),
                  LoaderStats = #be_loader_stats{
                    start_time = StartTime,
                    index = Index,
                    snapshot_freq = SnapshotFreq,
                    initial_allocations = Allocations,
                    current_allocations = Allocations,
                    snapshot_allocations = Allocations,
                    allocation_diffs = AllocationDiffs,
                    initial_nano = Nano,
                    current_nano = Nano,
                    snapshot_nano = Nano,
                    nano_diffs = NanoDiffs},
                  {ok, PidStats} = term_eval:start_link(fun add_stats/2, LoaderStats),

                  case read_eval_stats_loop(PidReader, PidEval, PidStats) of
                    {error, _Reason} = Err ->
                      term_eval:stop(PidStats),
                      term_eval:stop(PidEval),
                      term_reader:stop(PidReader),
                      {reply, Err, State};

                    ok ->
                      term_reader:stop(PidReader),
                      {reply, {ok, {PidEval, PidStats}}, State}
                  end;
                Err ->
                  term_reader:stop(PidReader),
                  {reply, Err, State}
              end
          end
      end
  end;

handle_call({load_many, FileNames}, _From, State) ->
  case be_bm_utils:files_exists(FileNames) of
    {error, _Reason} = Err ->
      {reply, Err, State};
    ok ->
      % Start Evaluator
      %   with `undefined` evaluator function
      %   and empty array of contexts
      {ok, PidEval} = term_eval:start_link(undefined, []),
      case read_eval(FileNames, PidEval) of
        {error, Reason} ->
          term_eval:stop(PidEval),
          {reply, {error, {failed_to_load_be_tree, Reason}}, State};
        {ok, BetreeFileNamePairs} ->
          {reply, {ok, {PidEval, BetreeFileNamePairs}}, State}
      end
  end;

handle_call(_Request, _From, State = #be_loader_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #be_loader_state{}) ->
  {noreply, NewState :: #be_loader_state{}} |
  {noreply, NewState :: #be_loader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #be_loader_state{}}).
handle_cast(_Request, State = #be_loader_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #be_loader_state{}) ->
  {noreply, NewState :: #be_loader_state{}} |
  {noreply, NewState :: #be_loader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #be_loader_state{}}).
handle_info(_Info, State = #be_loader_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #be_loader_state{}) -> term()).
terminate(_Reason, _State = #be_loader_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #be_loader_state{},
    Extra :: term()) ->
  {ok, NewState :: #be_loader_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #be_loader_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_expr(Expr, #be_evaluator{
  betree = Betree,
  consts = Consts,
  index = Index
} = State) ->
  Index1 = Index + 1,
  case erl_betree:betree_make_sub(Betree, Index1, Consts, Expr) of
    {ok, Sub} ->
      case erl_betree:betree_insert_sub(Betree, Sub) of
        ok ->
          {ok, State#be_evaluator{index = Index1}};
        X ->
          {{error, {betree_insert_sub, X}}, State}
      end;
    X ->
      {{error, {betree_make_sub, X}}, State}
  end.

add_stats(_Stats, #be_loader_stats{
  index = Index,
  snapshot_freq = SnapshotFreq,
  snapshot_allocations = SnapshotAllocations,
  allocation_diffs = AllocationsDiffs,
  snapshot_nano = SnapshotNano,
  nano_diffs = NanoDiffs
} = Stats) ->
  Index1 = Index + 1,
  CurrentTime = calendar:universal_time_to_local_time(erlang:universaltime()),
  CurrentAllocations = be_bm_utils:betree_allocations(),
  CurrentNano = erlang:monotonic_time(nanosecond),
  Stats1 = case Index1 rem SnapshotFreq of
    0 ->
      NanoDiff = CurrentNano - SnapshotNano,
      NanoPerIndex = NanoDiff / SnapshotFreq,
      MilliPerIndex = ceil(NanoPerIndex / 1_000_000),
      io:format("BE-Tree indexes: ~p, ~p milliseconds/index~n", [Index1, MilliPerIndex]),
      AllocationDiff = be_bm_utils:betree_allocations_diff(SnapshotAllocations, CurrentAllocations),
      Stats#be_loader_stats{
        current_time = CurrentTime,
        index = Index1,
        current_allocations = CurrentAllocations,
        snapshot_allocations = CurrentAllocations,
        allocation_diffs = [AllocationDiff | AllocationsDiffs],
        current_nano = CurrentNano,
        snapshot_nano = CurrentNano,
        nano_diffs = [NanoDiff | NanoDiffs]};
    _ ->
      Stats#be_loader_stats{
        current_time = CurrentTime,
        index = Index1,
        current_allocations = CurrentAllocations,
        current_nano = CurrentNano}
  end,
  {ok, Stats1}.

read_eval([], PidEval) ->
  Context = term_eval:get_context(PidEval),
  term_eval:update_context(PidEval, undefined, undefined),
  {ok, lists:reverse(Context)};
read_eval([FileName | Rest], PidEval) ->
  io:format("Loading BE-Tree from ~p...~n", [FileName]),
  case term_reader:start_link(FileName) of
    {error, _Reason} = Err -> Err;
    {ok, PidReader} ->
      case read_params(PidReader) of
        {error, be_tree_parameters_not_provided} ->
          term_reader:stop(PidReader),
          {error, {be_tree_parameters_not_provided, FileName}};
        {error, _Reason} = Err ->
          term_reader:stop(PidReader),
          Err;
        {ok, Term} ->
          Domains = [Term],
          case erl_betree:betree_make(Domains) of
            {ok, Betree} ->
              Context = term_eval:get_context(PidEval),
              Consts = [],
              Index = 0,
              BeEvaluator = #be_evaluator{
                betree = Betree,
                consts = Consts,
                index = Index
              },
              term_eval:update_context(PidEval, fun add_expr/2, BeEvaluator),

              StartTime = calendar:universal_time_to_local_time(erlang:universaltime()),
              SnapshotFreq = 100,
              Allocations = be_bm_utils:betree_allocations(),
              AllocationDiffs = [],
              NanoDiffs = [],
              Nano = erlang:monotonic_time(nanosecond),
              LoaderStats = #be_loader_stats{
                start_time = StartTime,
                index = Index,
                snapshot_freq = SnapshotFreq,
                initial_allocations = Allocations,
                current_allocations = Allocations,
                snapshot_allocations = Allocations,
                allocation_diffs = AllocationDiffs,
                initial_nano = Nano,
                current_nano = Nano,
                snapshot_nano = Nano,
                nano_diffs = NanoDiffs},
              {ok, PidStats} = term_eval:start_link(fun add_stats/2, LoaderStats),

              case read_eval_loop(PidReader, PidEval, PidStats) of
                ok ->
                  term_eval:stop(PidStats),
                  term_reader:stop(PidReader),
                  Context1 = [{Betree, FileName} | Context],
                  term_eval:update_context(PidEval, undefined, Context1),
                  read_eval(Rest, PidEval);

                {error, _Reason} = Err ->
                  term_eval:stop(PidStats),
                  term_reader:stop(PidReader),
                  term_eval:update_context(PidEval, undefined, undefined),
                  Err
              end;
            Err ->
              term_reader:stop(PidReader),
              {error, {betree_make, Err, FileName}}
          end
      end
  end.

read_params(PidReader) ->
  case term_reader:read(PidReader) of
    eof -> {error, be_tree_parameters_not_provided};
    {error, _Reason} = Err -> Err;
    {ok, _Term} = Ret -> Ret
  end.

read_eval_loop(ReaderId, EvalId, StatsId) ->
  case term_reader:read(ReaderId) of
    eof -> ok;
    {error, _Reason} = Err -> Err;

    {ok, Term} ->
      case term_eval:eval(EvalId, Term) of
        {{error, _Reason} = Err, _EvaluatedState} ->
          Err;

        ok ->
          Allocations = be_bm_utils:betree_allocations(),
          Nano = erlang:monotonic_time(nanosecond),
          term_eval:eval(StatsId, {Allocations, Nano}),
          read_eval_loop(ReaderId, EvalId, StatsId)
      end
  end.

read_eval_stats_loop(ReaderId, EvalId, StatsId) ->
  case term_reader:read(ReaderId) of
    eof -> ok;
    {error, _Reason} = Err -> Err;

    {ok, Term} ->
      case term_eval:eval(EvalId, Term) of
        {{error, _Reason} = Err, _EvaluatedState} ->
          Err;

        ok ->
          Allocations = be_bm_utils:betree_allocations(),
          Nano = erlang:monotonic_time(nanosecond),
          term_eval:eval(StatsId, {Allocations, Nano}),
          read_eval_stats_loop(ReaderId, EvalId, StatsId)
      end
  end.
