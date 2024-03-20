-module(term_writer).

-behaviour(gen_server).

%%
%% Term writer to file.
%%  Append terms to file one by one.
%%

%% API
-export([
  start_link/1,
  start_link/2,
  stop/1,
  write/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(term_writer_state, {
  name
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(FileName :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(FileName) ->
  gen_server:start_link(?SERVER, [FileName], []).
-spec(start_link(Id :: atom(), FileName :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(undefined, _FileName) ->
  {error, {wrong_id}};
start_link(Id, FileName) ->
  gen_server:start_link({local, Id}, ?MODULE, [FileName], []).

%% @doc Stops the server
stop(undefined) ->
  ok;
stop(ServerRef) ->
  gen_server:stop(ServerRef).

%% @doc Writes a term
write(undefined, _Term) ->
  ok;
write(ServerRef, Term) ->
  gen_server:call(ServerRef, {write, Term}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #term_writer_state{}} | {ok, State :: #term_writer_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([FileName]) ->
  {ok, #term_writer_state{name = FileName}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #term_writer_state{}) ->
  {reply, Reply :: term(), NewState :: #term_writer_state{}} |
  {reply, Reply :: term(), NewState :: #term_writer_state{}, timeout() | hibernate} |
  {noreply, NewState :: #term_writer_state{}} |
  {noreply, NewState :: #term_writer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #term_writer_state{}} |
  {stop, Reason :: term(), NewState :: #term_writer_state{}}).
handle_call(_Request, _From, State = #term_writer_state{name = undefined}) ->
  {reply, ok, State};
handle_call({write, _Term}, _From, State = #term_writer_state{name = undefined}) ->
  {reply, ok, State};
handle_call({write, Term}, _From, State = #term_writer_state{name = FileName}) ->
  Line = [io_lib:format("~tp.~n", [Term])],
  file:write_file(FileName, Line, [append]),
  {reply, ok, State};

handle_call(_Request, _From, State = #term_writer_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #term_writer_state{}) ->
  {noreply, NewState :: #term_writer_state{}} |
  {noreply, NewState :: #term_writer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_writer_state{}}).
handle_cast(_Request, State = #term_writer_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #term_writer_state{}) ->
  {noreply, NewState :: #term_writer_state{}} |
  {noreply, NewState :: #term_writer_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_writer_state{}}).
handle_info(_Info, State = #term_writer_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #term_writer_state{}) -> term()).
terminate(_Reason, _State = #term_writer_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #term_writer_state{},
    Extra :: term()) ->
  {ok, NewState :: #term_writer_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #term_writer_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
