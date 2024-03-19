-module(term_eval).

-behaviour(gen_server).

%%
%% Term evaluator within a given context.
%%

%% API
-export([
    start_link/2,
    start_link/3,
    stop/1,
    eval/2,
    eval/3,
    get_context/1,
    update_context/2,
    update_context/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(term_eval_state, {
  % `eval` is a `Func(Term, Context) -> {Evaluated, NewContext}`
  eval,
  % `context` is a second parameter in `Func(Term, Context)`
  context
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Func :: fun(), Context :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Func, Context) ->
  gen_server:start_link(?SERVER, [Func, Context], []).
-spec(start_link(Id :: atom(), Func :: fun(), Context :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id, Func, Context) ->
  gen_server:start_link({local, Id}, ?MODULE, [Func, Context], []).

%% @doc Stops the server
stop(ServerRef) ->
  gen_server:stop(ServerRef).

%% @doc Evaluates a term
eval(ServerRef, Term) ->
  eval(ServerRef, Term, infinity).

eval(ServerRef, Term, Timeout) ->
  gen_server:call(ServerRef, {eval, Term}, Timeout).

get_context(ServerRef) ->
  gen_server:call(ServerRef, get_context).

update_context(ServerRef, Func) ->
  gen_server:call(ServerRef, {update_context, Func}).

update_context(ServerRef, Func, Context) ->
  gen_server:call(ServerRef, {update_context, Func, Context}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #term_eval_state{}} | {ok, State :: #term_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Func, Context]) ->
  {ok, #term_eval_state{eval = Func, context = Context}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #term_eval_state{}) ->
  {reply, Reply :: term(), NewState :: #term_eval_state{}} |
  {reply, Reply :: term(), NewState :: #term_eval_state{}, timeout() | hibernate} |
  {noreply, NewState :: #term_eval_state{}} |
  {noreply, NewState :: #term_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #term_eval_state{}} |
  {stop, Reason :: term(), NewState :: #term_eval_state{}}).
handle_call({eval, Term}, _From, State = #term_eval_state{eval = Eval, context = Context}) ->
  {Evaluated, Context1} = Eval(Term, Context),
  {reply, Evaluated, State#term_eval_state{context = Context1}};

handle_call(get_context, _From, State = #term_eval_state{context = Context}) ->
  {reply, Context, State};

handle_call({update_context, Func}, _From, State = #term_eval_state{}) ->
  {reply, ok, State#term_eval_state{eval = Func}};

handle_call({update_context, Func, Context}, _From, State = #term_eval_state{}) ->
  {reply, ok, State#term_eval_state{eval = Func, context = Context}};

handle_call(_Request, _From, State = #term_eval_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #term_eval_state{}) ->
  {noreply, NewState :: #term_eval_state{}} |
  {noreply, NewState :: #term_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_eval_state{}}).
handle_cast(_Request, State = #term_eval_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #term_eval_state{}) ->
  {noreply, NewState :: #term_eval_state{}} |
  {noreply, NewState :: #term_eval_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_eval_state{}}).
handle_info(_Info, State = #term_eval_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #term_eval_state{}) -> term()).
terminate(_Reason, _State = #term_eval_state{context = _Context}) ->
  % Do nothing with `Context` because `Context` came from a caller.
  % So, it is the caller responsibility
  % to handle the `Context` when the evaluation has completed.
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #term_eval_state{},
    Extra :: term()) ->
  {ok, NewState :: #term_eval_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #term_eval_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
