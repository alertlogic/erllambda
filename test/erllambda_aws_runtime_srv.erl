-module(erllambda_aws_runtime_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-type client() :: {pid(), term()}.
-record(state, {tasks    :: queue:queue({client(), term()}),
                workers  :: queue:queue(client()),
                requests :: dict:dict(binary(), client())}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc wait for a new task
next() ->
    gen_server:call(?MODULE, next, infinity).

%% @doc respond with successfully processed response
response_ok(ReqId, Response) ->
    gen_server:call(?MODULE, {response, ReqId, ok, Response}).

%% @doc respond with failure
response_error(ReqId, Response) ->
    gen_server:call(?MODULE, {response, ReqId, error, Response}).

%% @doc synchronously execute function and return result
new_task(Task) ->
    gen_server:call(?MODULE, {new_task, Task}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init([]) ->
    process_flag(trap_exit, true),
    State = #state{tasks = queue:new(), workers = queue:new(),
                   requests = dict:new()},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} |
                         {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
                         {reply, Reply :: term(), NewState :: term(), hibernate} |
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_call(next, From, #state{tasks = Tasks, workers = Workers, requests = Requests} = State) ->
    case queue:out(Tasks) of
        {empty, Tasks1} ->
            {noreply, State#state{workers = queue:in(From, Workers), tasks = Tasks1}};
        {{value, {Requestor, Task}}, Tasks1} ->
            ReqId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            Requests1 = dict:store(ReqId, Requestor, Requests),
            {reply, {ReqId, Task}, State#state{tasks = Tasks1, requests = Requests1}}
    end;
handle_call({new_task, Task}, From,
            #state{tasks = Tasks, workers = Workers, requests = Requests} = State) ->
    case queue:out(Workers) of
        {empty, Workers1} ->
            Tasks1 = queue:in({From, Task}, Tasks),
            {noreply, State#state{tasks = Tasks1, workers = Workers1}};
        {{value, Worker}, Workers1} ->
            ReqId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            Requests1 = dict:store(ReqId, From, Requests),
            gen_server:reply(Worker, {ReqId, Task}),
            {noreply, State#state{workers = Workers1, requests = Requests1}}
    end;
handle_call({response, ReqId, Status, Response}, _From, #state{requests = Requests} = State) ->
    {Requestor, Requests1} = dict:take(ReqId, Requests),
    gen_server:reply(Requestor, {Status, Response}),
    {reply, ok, State#state{requests = Requests1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
