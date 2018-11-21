%%%---------------------------------------------------------------------------
%% @doc erllambda - AWS Lambda for Erlang Interface
%%
%% This module contains the public programming interface used by AWS Lambda
%% function implemented in Erlang using the <code>erllambda</code> framework.
%%
%% Eventually, we should take care of the following to avoid the overhead
%% starting and terminating the Erlang VM for each request.
%%
%%  https://aws.amazon.com/blogs/compute/container-reuse-in-lambda/
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda).

%% public - high-level migration orchestration endpoints
-export([succeed/1, succeed/2, fail/1, fail/2]).
-export([message/1, message/2, message_ctx/2, message_ctx/3]).
-export([metric/1, metric/2, metric/3, metric/4]).
-export([get_remaining_ms/1, get_aws_request_id/1]).
-export([region/0, environ/0, accountid/0, config/0, config/1, config/2]).
-export([print_env/0]).

%% private - handler invocation entry point, used by http api
-export([invoke/3]).

-include("include/erllambda.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(AL_CTX_SD_ID, "context@36312").

%%============================================================================
%% Callback Interface Definition
%%============================================================================
%%%---------------------------------------------------------------------------
-callback init( Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.

-callback handle( Event :: map(), Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.

%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec succeed(Value :: iolist() | map()) -> none().
%%%---------------------------------------------------------------------------
%% @doc Complete processing with success
%%
succeed( Map ) when is_map(Map)  ->
    complete( #{success => Map} );
succeed( Value ) when is_binary(Value); is_list(Value) ->
    succeed( "~s", [Value] ).

succeed( Format, Values ) ->
    complete( #{success => format( Format, Values )} ).


%%%---------------------------------------------------------------------------
-spec fail( Message :: iolist() ) -> none().
%%%---------------------------------------------------------------------------
%% @doc Complete a processing with failure
%%
fail( Message ) ->
    fail( "~s", [Message] ).

fail( Format, Values ) ->
    complete( #{errorType => 'Handled',
                stackTrace => [],
                errorMessage => format( Format, Values )} ).


%%%---------------------------------------------------------------------------
-spec message( Message :: iolist() ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will ensure the message is output in the log for the lambda
%% invocation.
%%
message( Message ) ->
    message_send( Message ).


%%%---------------------------------------------------------------------------
-spec message( Format :: string(), Values :: [any()] ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will format the message using <code>io_lib:format/2</code>
%% and then ensure it is output in the log for the lambda invocation.
%%
message( Format, Values ) ->
    message_send( Format, Values ).


%%%---------------------------------------------------------------------------
-spec message_ctx( ReqId :: binary() | map(), Message :: iolist() ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will ensure the message is output in the log for the lambda
%% invocation.
%%
message_ctx( ReqId, Message ) ->
    message_ctx(ReqId, Message, []).


%%%---------------------------------------------------------------------------
-spec message_ctx( ReqId :: binary() | map(), Format :: string(),
        Values :: [any()] ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will format the message using <code>io_lib:format/2</code>
%% and then ensure it is output in the log for the lambda invocation.
%%
message_ctx( Ctx, Format, Values ) when is_map (Ctx) ->
    message_ctx(get_aws_request_id(Ctx), Format, Values);
message_ctx( ReqId, Format, Values ) when is_binary(ReqId) ->
    message_send("[" ?AL_CTX_SD_ID " aid=\"~s\"] " ++ Format,
        [ReqId  | Values] ).

-spec metric(MetricName :: string(), MetricValue :: integer(),
             Type :: string(), Tags :: list()) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send custom metrics to Datadog
%%
%% This function will send log message from lambda using following format
%% MONITORING|unix_epoch_timestamp|metric_value|metric_type|my.metric.name|#tag1:value,tag2
%% where metric_type :: "count" | "gauge" | "histogram"
%%
metric(MName) ->
    metric(MName, 1).
metric(MName, Val) when is_integer(Val) ->
    metric(MName, Val, "count").
metric(MName, Val, Type) ->
    metric(MName, Val, Type, []).
metric(MName, Val, Type, Tags)
        when is_list(MName)
        andalso (Type == "count" orelse Type == "gauge" orelse Type == "histogram")
        andalso is_number(Val) ->
    NewTags = "#" ++
        string:join(
            lists:map(
                fun ({T,V}) ->
                        erllambda_util:to_list(T) ++ ":" ++ erllambda_util:to_list(V);
                    (OtherTag) ->
                        erllambda_util:to_list(OtherTag)
                end, Tags
            ),
            ","
        ),
    Ts = os:system_time(second),
    Msg = string:join([
        "MONITORING",
        erllambda_util:to_list(Ts),
        erllambda_util:to_list(Val),
        Type,
        MName,
        NewTags
    ], "|"),
    message(Msg).



%%%---------------------------------------------------------------------------
-spec get_remaining_ms(map()) -> pos_integer().
%%%---------------------------------------------------------------------------
%% @doc The time remaining in our invoke
%%
%% This function will return the time remaining in ms in this given Lambda function
%% invocation.
%%
%% @see JS style Context Function
%% http://docs.aws.amazon.com/lambda/latest/dg/nodejs-prog-model-context.html
%%
get_remaining_ms(#{<<"lambda-runtime-deadline-ms">> := Deadline}) ->
    CurrentTs = os:system_time(millisecond),
    Deadline - CurrentTs.

%%%---------------------------------------------------------------------------
-spec get_aws_request_id(map()) -> binary() | undefined.
%%%---------------------------------------------------------------------------
%% @doc Extract the request Id
%%
%% This function will return the Req ID of the invoke
%% contains several alternatives from new to old
%% to support backward compatibility
%%
get_aws_request_id(Hdrs) when is_list(Hdrs) ->
    get_aws_request_id(hdr2map(Hdrs));
get_aws_request_id(#{<<"awsRequestId">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"lambda-runtime-aws-request-id">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"x-amz-aws-request-id">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"x-amzn-requestid">> := ReqId}) -> ReqId.

%%%---------------------------------------------------------------------------
-spec region() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the default region in which the Lambda function
%% is running.
%%
%% @see erllambda_util:region/0
%%
region() ->
    erllambda_util:region().


%%%---------------------------------------------------------------------------
-spec config() -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Default <code>erlcloud</code> config
%%
%% This function will return a default AWS configuration record for use by
%% the <code>erlcloud</code> application in calling AWS services from lambda
%% functions using the default execution role.  It is expected that the
%% application calls this function every time an <code>erlcloud
%% config</code> is needed, and the implementation will ensure that the
%% credentials contained in the configuration are valid for at least 60
%% seconds.
%%
%% If the application needs to assume an alternative role, it should call
%% the {@link erllambda_util:config/1,2} functions directly.
%%
%% @see erllambda_util:config/0
%%
config() ->
    erllambda_util:config().

-spec config( Services :: [service()] ) -> #aws_config{}.
config(Services) ->
    erllambda_util:config(Services).

-spec config( Region :: region(), Options :: [option()] ) -> #aws_config{}.
config(Region, Options) ->
    erllambda_util:config(Region, Options).

%%%---------------------------------------------------------------------------
-spec environ() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the erllambba context
%%
%% @see erllambda_util:environ/0
%%
environ() ->
    erllambda_util:environ().

%%%---------------------------------------------------------------------------
-spec accountid() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the account id in which we run
%%
%% @see erllambda_util:environ/0
%%
accountid() ->
    erllambda_util:accountid().

%%============================================================================
%% Private API Function
%%============================================================================

%%%---------------------------------------------------------------------------
-spec invoke( Handler :: module(), Event :: binary(),
              EventHdrs :: list() ) -> {ok, term()} | {error, term()}.
%%%---------------------------------------------------------------------------
%%
%%
invoke( Handler, Event, EventHdrs )  ->
    application:set_env( erllambda, handler, Handler ),
    Parent = self(),
    InvokeFun =
        fun () ->
            % construct the contexts on the fly
            % binaries all the way down
            Context = maps:merge(os_env2map(), hdr2map(EventHdrs)),
            invoke_credentials( Context, application:get_env( erllambda, config )),
            Res = invoke_exec(Handler, Event, Context),
            Parent ! {handle, Res}
        end,
    % each handler should leave and die in it's own process
    Opts = application:get_env(erllambda, handler_spawn_opts, []),
    {_, MonRef} = erlang:spawn_opt( InvokeFun , [monitor | Opts]),
    receive
        {handle, Res} ->
            erlang:demonitor(MonRef, [flush, info]),
            Res;
        {'DOWN', MonRef, process, _Pid, Result} ->
            Message = format( "Handler terminated with ~p", [Result] ),
            Response = #{errorType => 'Unhandled',
                errorMessage => Message,
                stackTrace => []},
            {error, Response}
    end.

invoke_exec( Handler, Event, Context ) ->
    try
        case Handler:handle(jiffy:decode(Event, [return_maps]), Context ) of
            ok -> succeed( "completed successfully" );
            {ok, Result} -> succeed( Result );
            {error, ErrResult} -> fail( ErrResult );
            _Anything ->
                %% if handler returns anything else, then it did not call
                %% fail/succeed/retry, or return ok, so it is assumed to fail
                fail( "did not invoke succeed/1,2 or fail/1,2" )
        end
    catch
        % both top level handler and we can can call success/fail
        throw:{?MODULE, result, JsonMap} -> {ok, JsonMap};
        throw:{?MODULE, failure, JsonMap} -> {error, JsonMap};
        Type:Reason ->
            Trace = erlang:get_stacktrace(),
            Message = format( "terminated with exception {~p, ~p}", [Type, Reason] ),
            message_send( format( "~s with trace ~p", [Message, Trace] ) ),
            Response = #{errorType => 'Unhandled',
                errorMessage => Message,
                stackTrace => format("~p", [Trace])},
            {error, Response}
    end.

invoke_credentials( #{<<"AWS_SESSION_TOKEN">> := Token} = _Context ,
                    {ok, #aws_config{security_token = Token}}) ->
    ok;
invoke_credentials( #{<<"AWS_SECRET_ACCESS_KEY">> := Key} = _Context ,
                    {ok, #aws_config{secret_access_key = Key}}) ->
    ok;
invoke_credentials( Context, _ ) ->
    invoke_update_credentials( Context ).

invoke_update_credentials( #{<<"AWS_ACCESS_KEY_ID">> := Id,
                             <<"AWS_SECRET_ACCESS_KEY">> := Key,
                             <<"AWS_SESSION_TOKEN">> := Token }) ->
%%TODO need to define expiration
%%                             <<"x-amz-deadline-ns">> := Expire}) ->
%%                             <<"AWS_CREDENTIAL_EXPIRE_TIME">> := Expire

    Config = #aws_config{ access_key_id = erllambda_util:to_list(Id),
                          secret_access_key = erllambda_util:to_list(Key),
                          security_token = erllambda_util:to_list(Token),
                          expiration = undefined },
    application:set_env( erllambda, config, Config ).

%%============================================================================
%% Internal Functions
%%============================================================================
format( Format, Values ) ->
    iolist_to_binary( io_lib:format( Format, Values ) ).

% in success case we care only about the body
complete( #{success := Response}) ->
    complete( result, Response );
% in error we care about the entire error object
complete( #{errorType := _} = Response) ->
    complete( failure, Response).

complete( Type, Response ) ->
    throw( {?MODULE, Type, Response} ).


message_send( Message ) ->
    error_logger:info_msg( "~s", [Message] ).

message_send( Format , Values) ->
    error_logger:info_msg( Format, Values ).

print_env() ->
    case application:get_env(erllambda, print_env, false) of
        true ->
            EnvWihtoutSecret = hide_secret(os_env2map()),
            erllambda:message("Erllambda Starting at ~p with Env ~p",
                [os:system_time(millisecond), EnvWihtoutSecret]);
        _ -> ok
    end.

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
    maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Hdrs]).

%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).


-endif.
