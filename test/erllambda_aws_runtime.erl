-module(erllambda_aws_runtime).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, start_phase/3, stop/1, prep_stop/1,
         config_change/3]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the application
start() ->
    application:ensure_all_started(?MODULE).

%% @doc Stops the applicationn
stop() ->
  application:stop(?MODULE).

%% @doc Port on which http server listens for incoming requests
http_port() ->
    {ok, Port} = application:get_env(?MODULE, port),
    Port.

%% @doc synchronously execute function and return result
call(Task) ->
    erllambda_aws_runtime_srv:new_task(Task).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType :: normal |
                         {takeover, Node :: node()} |
                         {failover, Node :: node()},
            StartArgs :: term()) ->
                   {ok, Pid :: pid()} |
                   {ok, Pid :: pid(), State :: term()} |
                   {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% top supervisor of the tree.
%% Starts an application with included applications, when
%% synchronization is needed between processes in the different
%% applications during startup.%% @end
%%--------------------------------------------------------------------
-spec start_phase(Phase :: atom(),
                  StartType :: normal |
                               {takeover, Node :: node()} |
                               {failover, Node :: node()},
                  PhaseArgs :: term()) -> ok | {error, Reason :: term()}.
start_phase(http_server, _StartType, _PhaseArgs) ->
    HostMatch = '_',
    Paths = [{"/:version/runtime/invocation/next",
              erllambda_aws_runtime_next_http, []},
             {"/:version/runtime/invocation/:req_id/:operation",
              erllambda_aws_runtime_response_http, []}],
    Dispatch = cowboy_router:compile([{HostMatch, Paths}]),
    Env = #{env => #{dispatch => Dispatch}},
    Port = http_port(),
    TransportOpts = [{port, Port}],
    {ok, _} = cowboy:start_clear(?MODULE, TransportOpts, Env),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> any().
stop(_State) ->
    cowboy:stop_listener(?MODULE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
%% @end
%%--------------------------------------------------------------------
-spec prep_stop(State :: term()) -> NewState :: term().
prep_stop(State) ->
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by an application after a code replacement,
%% if the configuration parameters have changed.
%% @end
%%--------------------------------------------------------------------
-spec config_change(Changed :: [{Par :: atom(), Val :: term()}],
                    New :: [{Par :: atom(), Val :: term()}],
                    Removed :: [Par :: atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  {ok, {SupFlags :: supervisor:sup_flags(),
                        [ChildSpec :: supervisor:child_spec()]}} |
                  ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    RuntimeSrv = #{id => erllambda_aws_runtime_srv,
                   start => {erllambda_aws_runtime_srv, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erllambda_aws_runtime_srv]},

    {ok, {SupFlags, [RuntimeSrv]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
