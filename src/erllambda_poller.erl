%%%-------------------------------------------------------------------
%% @doc erllambda_poller - AWS Lambda for Erlang Interface
%%
%% This module implmentes polling for event from AWS Lambda Runtime API
%% module uses erlang:error/1 on purpose to kill VM asap.
%% to let AWS Lambda clean it up. See Runtime API dociumentation.
%%
%% @copyright 2018 Alert Logic, Inc.
%%%-------------------------------------------------------------------
-module(erllambda_poller).

-behaviour(gen_server).

-export([
    spec/0,
    start_link/0,
    runtime_address/0,
    handler_module/0
]).

-export([handle/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% API
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Runtime API version
-define(API_VERSION, <<"2018-06-01">>).
-define(INVOKE_NEXT_PATH, <<"/runtime/invocation/next">>).
-define(INVOKE_REPLAY_SUCCESS_PATH(ReqId), <<"/runtime/invocation/",ReqId/binary, "/response">>).
-define(INVOKE_REPLAY_ERROR_PATH(ReqId), <<"/runtime/invocation/", ReqId/binary, "/error">>).

%%******************************************************************************
%% API functions
%%******************************************************************************
spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => permanent, shutdown => (5 * 1000), type => worker,
      modules => [?MODULE]
    }.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% get Address and port of runtime API
runtime_address() ->
    case os:getenv("AWS_LAMBDA_RUNTIME_API") of
        false -> undefined;
        V -> list_to_binary(V)
    end.

handler_module() ->
    case application:get_env(erllambda, handler_module, undefined) of
        undefined ->
            list_to_atom(os:getenv("_HANDLER", "erllambda_poller"));
        Module when is_atom(Module) ->
            Module;
        Module when is_list(Module) ->
            list_to_atom(Module);
        Module when is_binary(Module) ->
            binary_to_atom(Module, latin1)
    end.

-record(state, {
    runtime_addr  = undefined :: binary(),
    handler       = undefined :: atom(),
    timer_ref     = undefined :: reference(),
%%    aws_cfg       = #aws_config{http_client = hackney}
    aws_cfg       = undefined
}).

%%******************************************************************************
%% gen_server callbacks
%%******************************************************************************
%% @private
init([]) ->
    Addr = runtime_address(),
    Handler = handler_module(),
    erllambda:print_env(),
    erllambda:message("initializing ~p for handler ~p", [?MODULE, Handler]),
    {ok, #state{
          runtime_addr = Addr,
          handler = Handler,
          %% give it some time to start off
          timer_ref = erlang:send_after(100, self(), poll)
    }}.

%% @private
handle_call(What, _From, _State) ->
    erllambda:message("unknown msg ~p", [What]),
    {stop, "unknown msg"}.

%% @private
handle_cast(Info, State) ->
    erllambda:message("Unknown gen_server:cast: ~p", [Info]),
    {noreply, State}.

%% @private
handle_info(poll, #state{runtime_addr = undefined} = State) ->
    erllambda:message("Runtime API address not set - not in AWS?"),
    {noreply, State};
handle_info(poll, #state{handler = Handler} = State) ->
    %% SYNC to RUNTIME
    %% container freeze/thaw happens here
    {ok, ReqId, Hdrs, Body} = invoke_next(State),
    %% container thaw happens here
    erllambda:message("Next returns, in invoke ~p", [os:system_time(millisecond)]),
    case erllambda:invoke(Handler, Body, Hdrs) of
        {ok, Json} ->
            invoke_success(State, ReqId, Json);
        {Error, ErrJson}
                when Error == handled orelse Error == unhandled ->
            invoke_error(State, ReqId, ErrJson)
    end,
    {noreply, State#state{timer_ref = erlang:send_after(0, self(), poll)}};
handle_info(Info, State) ->
    erllambda:message("Unknown info: ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, #state{timer_ref = undefined}) ->
    ok;
terminate(_Reason, #state{timer_ref = Ref}) ->
    erlang:cancel_timer(Ref),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%******************************************************************************
%% Internal functions
%%******************************************************************************
% dummy basic handler
handle(Event, Context) ->
    erllambda:message("I'm Erlang noob event ~p ~p", [Event, Context]),
    {ok, #{noob => pass}}.

%%******************************************************************************
%% Internal functions
%%******************************************************************************
invoke_next(#state{runtime_addr = Addr, aws_cfg = AwsCfg}) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        ?INVOKE_NEXT_PATH/binary>>),
    erllambda:message("Invoke Next path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    case request(FullPath, get, [], "", infinity, AwsCfg) of
        {ok, {{200, _}, Hdrs, Body}} ->
            AwsReqId = erllambda:get_aws_request_id(Hdrs),
            set_context(AwsReqId),
            {ok, AwsReqId, Hdrs, Body};
        {ok, {{Other, _}, _Hdrs, Body}} ->
            % error from Runtime API
            erllambda:message("Error from runtime API ~p ~p ", [Other, Body]),
            erlang:error({Other, Body});
        {error, _} = Err ->
            erlang:error(Err)
    end.

invoke_success(#state{runtime_addr = Addr, aws_cfg = AwsCfg}, AwsReqId, Body) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        (?INVOKE_REPLAY_SUCCESS_PATH(AwsReqId))/binary>>),
    erllambda:message("Invoke Success path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    case request(FullPath, post, [], encode_body(Body), infinity, AwsCfg) of
        {ok, {{202, _}, _Hdrs, _Body}} ->
            ok;
        {ok, {{413, _}, _Hdrs, Body}} ->
            % we've sent too much
            % just logs it as it's actually returned to the caller as error
            erllambda:message("Payload too Large Error from runtime API ~p", [Body]),
            ok;
        {ok, {{Other, _}, _Hdrs, Body}} ->
            % error from Runtime API
            erllambda:message("Error form runtime API ~p ~p ", [Other, Body]),
            erlang:error({Other, Body});
        {error, _} = Err ->
            erlang:error(Err)
    end.

invoke_error(#state{runtime_addr = Addr, aws_cfg = AwsCfg}, AwsReqId, Body) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        (?INVOKE_REPLAY_ERROR_PATH(AwsReqId))/binary>>),
    erllambda:message("Invoke Error path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    case request(FullPath, post, [], encode_body(Body), infinity, AwsCfg) of
        {ok, {{202, _}, _Hdrs, _Body}} ->
            ok;
        {ok, {{Other, _}, _Hdrs, Body}} ->
            % error from Runtime API
            erllambda:message("Error from runtime API ~p ~p ", [Other, Body]),
            erlang:error({Other, Body});
        {error, _} = Err ->
            erlang:error(Err)
    end.

request(URL, Method, Hdrs, Body, Timeout, undefined) ->
    lhttpc:request(URL, Method, Hdrs, Body, Timeout);
request(URL, Method, Hdrs, Body, Timeout, AwsCfg) ->
    erlcloud_httpc:request(URL, Method, Hdrs, Body, Timeout, AwsCfg).

set_context(ReqId) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    erllambda:message_ctx(ReqId,
        "127.0.0.1 - - [~2..0b/~s/~4..0b:~2..0b:~2..0b:~2..0b -0000] "
        "Invoke Next",
        [Day, month(Month), Year, Hour, Min, Sec]
    ).

encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jiffy:encode(Body).


month(1) -> 'Jan';
month(2) -> 'Feb';
month(3) -> 'Mar';
month(4) -> 'Apr';
month(5) -> 'May';
month(6) -> 'Jun';
month(7) -> 'Jul';
month(8) -> 'Aug';
month(9) -> 'Sep';
month(10) -> 'Oct';
month(11) -> 'Nov';
month(12) -> 'Dec'.