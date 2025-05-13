-module(be_eval_err).

-behaviour(gen_server).

-include("be_eval.hrl").

%% API
-export([
  start_link/0,
  start_link/1,
  stop/1,
  run/6,
  run/8
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(be_eval_state, {
  id
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

%% @doc Loads a BE and evaluates events
run(ServerRef, Info, BetreeFile, EventsFile, EventEvalFunc, StatsFunc) ->
  run(ServerRef, Info, BetreeFile, EventsFile, EventEvalFunc, _EventEvalOutputFile = undefined,
    StatsFunc, _StatsOutputFile = undefined).

run(ServerRef, Info, BetreeFile, EventsFile, EventEvalFunc, EventEvalOutputFile, StatsFunc, StatsOutputFile) ->
  gen_server:call(ServerRef,
    {run, Info, BetreeFile, EventsFile, EventEvalFunc, EventEvalOutputFile, StatsFunc, StatsOutputFile},
    infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #be_eval_state{}} | {ok, State :: #be_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #be_eval_state{}};
init([Id]) ->
  {ok, #be_eval_state{id = Id}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #be_eval_state{}) ->
  {reply, Reply :: term(), NewState :: #be_eval_state{}} |
  {reply, Reply :: term(), NewState :: #be_eval_state{}, timeout() | hibernate} |
  {noreply, NewState :: #be_eval_state{}} |
  {noreply, NewState :: #be_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #be_eval_state{}} |
  {stop, Reason :: term(), NewState :: #be_eval_state{}}).
handle_call({run, Info, BetreeFile, EventsFile, EventEvalFunc, EventEvalOutputFile, StatsFunc, StatsOutputFile},
    _From, State) ->
  case be_bm_utils:file_exists(EventsFile) of
    false ->
      {reply, {error, {file_does_not_exist, EventsFile}}, State};
    true ->
      {ok, PidLoader} = be_loader_err:start_link(),
      io:format("Loading BE-Tree...~n"),
      case be_loader_err:load(PidLoader, BetreeFile) of
        {ok, {PidEval, PidStats} = _LoaderRet} ->
          be_loader_err:stop(PidLoader),
          case term_reader:start_link(EventsFile) of
            {error, _Reason} = Err ->
              {reply, Err, State};
            {ok, PidReader} ->
              ok = term_eval:update_context(PidEval, EventEvalFunc),
              {ok, PidWriter} = term_writer:start_link(EventEvalOutputFile),

              StartTime = calendar:universal_time_to_local_time(erlang:universaltime()),
              Index = 0,
              SnapshotFreq = 1000,
              Allocations = be_bm_utils:betree_allocations(),
              AllocationDiffs = [],
              SnapshotNanoAcc = 0,
              NanoDiffs = [],
              EvaluatorStats = #be_evaluator_stats{
                start_time = StartTime,
                info = Info,
                index = Index,
                snapshot_freq = SnapshotFreq,
                initial_allocations = Allocations,
                current_allocations = Allocations,
                snapshot_allocations = Allocations,
                allocation_diffs = AllocationDiffs,
                snapshot_nano_acc = SnapshotNanoAcc,
                nano_diffs = NanoDiffs},

              ok = term_eval:update_context(PidStats, StatsFunc, EvaluatorStats),

              io:format("Processing events...~n"),
              case read_eval_loop(PidReader, PidEval, PidWriter, PidStats) of
                {error, _Reason1} = Err1 ->
                  term_writer:stop(PidWriter),
                  term_eval:stop(PidStats),
                  term_eval:stop(PidEval),
                  term_reader:stop(PidReader),
                  {reply, Err1, State};

                ok ->
                  StatsContext = term_eval:get_context(PidStats),
                  Stats = report_stats(StatsContext),
                  be_bm_utils:write_terms(StatsOutputFile, Stats),
                  term_writer:stop(PidWriter),
                  term_eval:stop(PidStats),
                  term_eval:stop(PidEval),
                  term_reader:stop(PidReader),
                  {reply, ok, State}
              end
          end;
        Err ->
          be_loader_err:stop(PidLoader),
          {reply, Err, State}
      end
  end;

handle_call(_Request, _From, State = #be_eval_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #be_eval_state{}) ->
  {noreply, NewState :: #be_eval_state{}} |
  {noreply, NewState :: #be_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #be_eval_state{}}).
handle_cast(_Request, State = #be_eval_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #be_eval_state{}) ->
  {noreply, NewState :: #be_eval_state{}} |
  {noreply, NewState :: #be_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #be_eval_state{}}).
handle_info(_Info, State = #be_eval_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #be_eval_state{}) -> term()).
terminate(_Reason, _State = #be_eval_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #be_eval_state{},
    Extra :: term()) ->
  {ok, NewState :: #be_eval_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #be_eval_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_eval_loop(PidReader, PidEval, PidWriter, PidStats) ->
  case term_reader:read(PidReader) of % read a term
    eof -> ok;
    {error, _Reason} = Err -> Err;

    {ok, Term} ->
      case term_eval:eval(PidEval, Term) of % evaluate the term
        {error, _Reason} = Err -> Err;

        % an evaluated term and stats are produced
        {ok, {EvaluatedTerm, TermEvaluationStats}} ->
          ok = term_writer:write(PidWriter, EvaluatedTerm), % write the evaluated term
          term_eval:eval(PidStats, TermEvaluationStats), % process the stats
          read_eval_loop(PidReader, PidEval, PidWriter, PidStats) % repeat
      end
  end.

report_stats(#be_evaluator_stats{
  start_time = StartTime, current_time = CurrentTime,
  info = Info,
  index = Index, snapshot_freq = SnapshotFreq,
  initial_allocations = InitialAllocations,
  current_allocations = CurrentAllocations,
  allocation_diffs = AllocationDifs,
  nano_diffs = NanoDiffs
}) ->
  StartSec = calendar:datetime_to_gregorian_seconds(StartTime),
  CurrentSec = calendar:datetime_to_gregorian_seconds(CurrentTime),
  {_Days, Duration} = calendar:seconds_to_daystime(CurrentSec - StartSec),
  [
    {start_local_time, StartTime}, {end_local_time, CurrentTime},
    {duration, Duration},
    {info, Info},
    {n_events, Index}, {snapshot_freq, SnapshotFreq},
    {initial_allocations, InitialAllocations},
    {current_allocations, CurrentAllocations},
    {nano_diffs, lists:reverse(NanoDiffs)},
    {allocation_diffs, lists:reverse(AllocationDifs)}
  ];
report_stats(X) ->
  [{error, {not_a_betree_event_processor_state, X}}].
