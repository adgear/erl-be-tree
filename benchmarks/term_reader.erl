-module(term_reader).

-behaviour(gen_server).

%%
%% Term reader from file.
%%  Reads terms from file one by one unlike `file:consult/1`.
%%

%% API
-export([
  start_link/1,
  start_link/2,
  stop/1,
  read/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(term_reader_state, {
  name,
  handle,
  eof,
  error
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
start_link(Id, FileName) ->
  gen_server:start_link({local, Id}, ?MODULE, [FileName], []).

%% @doc Stops the server
stop(ServerRef) ->
  gen_server:stop(ServerRef).

%% @doc Reads a term
read(ServerRef) ->
  gen_server:call(ServerRef, read).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #term_reader_state{}} | {ok, State :: #term_reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([FileName]) ->
  case file:open(FileName, [read]) of
    {error, Reason} ->
      {stop, {error, {Reason, FileName}}};
    {ok, FileHandle} ->
      {ok, #term_reader_state{
        name = FileName,
        handle = FileHandle,
        eof = false}}
  end.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #term_reader_state{}) ->
  {reply, Reply :: term(), NewState :: #term_reader_state{}} |
  {reply, Reply :: term(), NewState :: #term_reader_state{}, timeout() | hibernate} |
  {noreply, NewState :: #term_reader_state{}} |
  {noreply, NewState :: #term_reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #term_reader_state{}} |
  {stop, Reason :: term(), NewState :: #term_reader_state{}}).
handle_call(read, _From, State = #term_reader_state{handle = Handle}) ->
  case io:read(Handle, '') of
    eof ->
      {reply, eof, State#term_reader_state{eof = true}};
    {error, Reason} = Err ->
      {reply, Err, State#term_reader_state{error = Reason}};
    {ok, _Term} = Reply ->
      {reply, Reply, State}
  end;

handle_call(_Request, _From, State = #term_reader_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #term_reader_state{}) ->
  {noreply, NewState :: #term_reader_state{}} |
  {noreply, NewState :: #term_reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_reader_state{}}).
handle_cast(_Request, State = #term_reader_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #term_reader_state{}) ->
  {noreply, NewState :: #term_reader_state{}} |
  {noreply, NewState :: #term_reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #term_reader_state{}}).
handle_info(_Info, State = #term_reader_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #term_reader_state{}) -> term()).
terminate(_Reason, _State = #term_reader_state{handle = Handle}) ->
  file:close(Handle),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #term_reader_state{},
    Extra :: term()) ->
  {ok, NewState :: #term_reader_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #term_reader_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
