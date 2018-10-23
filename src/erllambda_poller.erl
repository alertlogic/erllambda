%%%-------------------------------------------------------------------
%%% @author ebob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2018 00:11
%%%-------------------------------------------------------------------
-module(erllambda_poller).
-author("ebob").

-behaviour(gen_server).

-export([
    spec/0,
    start_link/0,
    runtime_address/0,
    handler_module/0,
    os_env2map/0,
    hide_secret/1
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

% BYOL API version
-define(API_VERSION, <<"2018-06-01">>).
-define(INVOKE_NEXT_PATH, <<"/runtime/invocation/next">>).
-define(INVOKE_REPLAY_SUCCESS_PATH(ReqId), <<"/runtime/invocation/",ReqId/binary, "/response">>).
-define(INVOKE_REPLAY_ERROR_PATH(ReqId), <<"/runtime/invocation/", ReqId/binary, "/error">>).
-define(INIT_ERROR_PATH, <<"/runtime/init/error">>).

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
    erllambda:message("Runtime Addressed not set - not BYOL?"),
    {noreply, State};
handle_info(poll, #state{handler = Handler} = State) ->
    %% SYNC to RUNTIME
    %% container freeze/thaw happens here
    {ok, ReqId, MHdrs, Body} = invoke_next(State),
    %% container thaw happens here
    OSMap = os_env2map(),
    erllambda:message("Next returns, in invoke ~p", [os:system_time(millisecond)]),
    case erllambda:invoke(Handler, Body, maps:merge(OSMap, MHdrs)) of
        {ok, Json} ->
            invoke_success(State, ReqId, Json),
            {noreply, State#state{timer_ref = erlang:send_after(0, self(), poll)}};
        {handled, ErrJson} ->
            invoke_success(State, ReqId, ErrJson),
            {noreply, State#state{timer_ref = erlang:send_after(0, self(), poll)}};
        {unhandled, ErrJson} ->
            invoke_error(State, ReqId, ErrJson),
            {stop, {error, ErrJson} , State}
    end;
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

invoke_next(#state{runtime_addr = Addr, aws_cfg = AwsCfg}) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        ?INVOKE_NEXT_PATH/binary>>),
    erllambda:message("Invoke Next path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    case request(FullPath, get, [], "", infinity, AwsCfg) of
        {ok, {{200, _}, Hdrs, Body}} ->
            #{<<"x-amz-aws-request-id">> := AwsReqId} = MapHdrs = hdr2map(Hdrs),
            set_context(AwsReqId),
            MBody = jiffy:decode(Body, [return_maps]),
            {ok, AwsReqId, MapHdrs, MBody};
        {error, _} = Err ->
            throw(Err)
    end.

invoke_success(#state{runtime_addr = Addr, aws_cfg = AwsCfg}, AwsReqId, Body) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        (?INVOKE_REPLAY_SUCCESS_PATH(AwsReqId))/binary>>),
    erllambda:message("Invoke Success path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    case request(FullPath, post, [], Body, infinity, AwsCfg) of
        {ok, {{202, _}, _Hdrs, _Body}} ->
            ok;
        {error, _} = Err ->
            erllambda:message("Runtime replied badly for success ~p", [Err]),
            throw(Err)
    end.

invoke_error(#state{runtime_addr = Addr, aws_cfg = AwsCfg}, AwsReqId, Body) ->
    FullPath = binary_to_list(<<"http://", Addr/binary,
        "/", ?API_VERSION/binary,
        (?INVOKE_REPLAY_ERROR_PATH(AwsReqId))/binary>>),
    erllambda:message("Invoke Error path ~p ~s", [os:system_time(millisecond), FullPath]),
    %% infinity due to container Freeze/thaw behaviour
    %% TODO remove double encode
    case request(FullPath, post, [], Body, infinity, AwsCfg) of
        {ok, {{202, _}, _Hdrs, _Body}} ->
            ok;
        {error, _} = Err ->
            erllambda:message("Runtime replied badly for error ~p", [Err]),
            throw(Err)
    end.

request(URL, Method, Hdrs, Body, Timeout, undefined) ->
    lhttpc:request(URL, Method, Hdrs, Body, Timeout);
request(URL, Method, Hdrs, Body, Timeout, AwsCfg) ->
    erlcloud_httpc:request(URL, Method, Hdrs, Body, Timeout, AwsCfg).

set_context(ReqId) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    erllambda:message_ctx(ReqId,
        "127.0.0.1 - - [~2..0b/~s/~4..0b:~2..0b:~2..0b:~2..0b -0000] "
        "\"Invoke Next",
        [Day, month(Month), Year, Hour, Min, Sec]
    ).

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

os_env2map() ->
    maps:from_list(lists:map(
        fun(S) ->
            [K, V] = string:split(S, "="),
            {list_to_binary(K), list_to_binary(V)}
        end,
        os:getenv()
    )).

hide_secret(#{<<"AWS_SECRET_ACCESS_KEY">> := _} = Map) ->
    maps:without([<<"AWS_SECRET_ACCESS_KEY">>], Map);
hide_secret(Map) ->
    Map.


hdr2map(Hdrs) ->
    maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K,V} <- Hdrs]).