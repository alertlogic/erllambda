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
%% @copyright 2017 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(erllambda).
-author('Paul Fisher <pfisher@alertlogic.com>').


%% public - high-level migration orchestration endpoints
-export([succeed/1, succeed/2, fail/1, fail/2]).
-export([message/1, message/2, message_ctx/2, message_ctx/3]).
-export([metric/1, metric/2, metric/3, metric/4]).
-export([get_remaining_ms/1]).
-export([region/0, config/0]).

%% private - handler invocation entry point, used by http api
-export([invoke/3]).

-export([to_binary/1, to_list/1]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%============================================================================
%% Type Definitions
%% @see src/erllambda.hrl
%%============================================================================

-define(AL_CTX_SD_ID, "context@36312").

%%============================================================================
%% Callback Interface Definition
%%============================================================================
%%%---------------------------------------------------------------------------
-callback handle( Event :: map(), Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.

%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
    

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
    complete( #{errorType => 'HandlerFailure',
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
    Message = format( Format, Values ),
    message_send( Message ).


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
message_ctx( #{<<"x-amz-aws-request-Id">> := ReqId}, Format, Values ) ->
    message_ctx(ReqId, Format, Values);
message_ctx( ReqId, Format, Values ) when is_binary(ReqId) ->
    Message = format_reqid( ReqId, Format, Values ),
    message_send( Message );
message_ctx( _ReqId, Format, Values) ->
    message_ctx(<<"illegal_request_id">>, Format, Values).


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
                fun ({T,V}) -> to_list(T) ++ ":" ++ to_list(V);
                    (OtherTag) -> to_list(OtherTag)
                end, Tags
            ),
            ","
        ),
    Ts = os:system_time(second),
    Msg = string:join([
        "MONITORING",
        to_list(Ts),
        to_list(Val),
        Type,
        MName,
        NewTags
    ], "|"),
    message(Msg).



%%%---------------------------------------------------------------------------
-spec get_remaining_ms(map()) -> pos_integer() | undefined.
%%%---------------------------------------------------------------------------
%% @doc The time remaining in our invoke
%%
%% This function will return the time remaining in ms in this given Lambda function
%% invocation.
%% it will return `undefined` is EEE case.
%%
%% @see JS style Context Function
%% http://docs.aws.amazon.com/lambda/latest/dg/nodejs-prog-model-context.html
%%
get_remaining_ms(#{<<"TIME_STARTED_MS">>   := StartTs,
                   <<"TIME_REMAINING_MS">> := RemTime}) ->
    CurrentTs = os:system_time(millisecond),
    RemTime - (CurrentTs - StartTs);
get_remaining_ms(_) ->
    undefined.


%%%---------------------------------------------------------------------------
-spec region() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the default region in which the Lambda function
%% is running.
%%
%% @see iwsutil:region/0
%%
region() ->
    iwsutil:region().


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
%% the {@link iwsutil:config/1,2} functions directly.
%%
%% @see iwsutil:config/0
%%
config() ->
    iwsutil:config().


%%============================================================================
%% Private API Function
%%============================================================================

%%%---------------------------------------------------------------------------
-spec invoke( Handler :: module(), Event :: binary(),
              Context :: binary() ) -> ok.
%%%---------------------------------------------------------------------------
%%
%%
invoke( Handler, Event, Context ) ->
    application:set_env( erllambda, handler, Handler ),
    invoke_credentials( Context, application:get_env( iwsutils, config )),
    try 
        invoke_exec( Handler, Event, Context )
    catch
        throw:{?MODULE, result, JsonMap} -> {ok, JsonMap};
        throw:{?MODULE, failure, JsonMap} -> {handled, JsonMap};
        Type:Reason ->
            Trace = erlang:get_stacktrace(),
            Message = iolist_to_binary(
                        io_lib:format( "terminated with exception {~p, ~p}",
                                       [Type, Reason] ) ),
            message_send( format( "~s with trace ~p", [Message, Trace] ) ),
            Response = #{errorType => 'HandlerFailure',
                         errorMessage => Message,
                         stackTrace => format("~p", [Trace])},
            {unhandled, Response}
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
%%                             <<"x-amz-deadline-ns">> := Expire}) ->
%%                             <<"AWS_CREDENTIAL_EXPIRE_TIME">> := Expire

    Config = #aws_config{ access_key_id = to_list(Id),
                          secret_access_key = to_list(Key),
                          security_token = to_list(Token),
                          expiration = undefined },
    application:set_env( iwsutil, config, Config ).

to_list( V ) when is_list(V) -> V;
to_list( V ) when is_binary(V) -> binary_to_list(V);
to_list( V ) when is_atom(V) -> atom_to_list(V);
to_list( V ) when is_integer(V) -> integer_to_list(V);
to_list( V ) when is_float(V) -> float_to_list(V);
to_list( V ) -> V.

to_binary(T) when is_pid(T) ->
    iolist_to_binary(pid_to_list(T));
to_binary(T) when is_binary(T) ->
    T;
to_binary(T) when is_list(T) ->
    list_to_binary(T);
to_binary(T) when is_atom(T) ->
    atom_to_binary(T, latin1).

invoke_exec( Handler, Event, Context ) ->
    case Handler:handle( Event, Context ) of
        ok -> succeed( "completed successfully" );
        {ok, Result} -> succeed( Result );
        {error, Reason} -> fail( Reason );
        _Anything ->
            %% if handler returns anything else, then it did not call
            %% fail/succeed/retry, or return ok, so it is assumed to fail
            fail( "did not invoke succeed/1,2 or fail/1,2 or retry/2,3" )
    end.

%%============================================================================
%% Internal Functions
%%============================================================================
format_reqid( ReqId, Format, Values ) ->
    format( "[" ?AL_CTX_SD_ID " aid=\"~s\"] " ++ Format,
            [ReqId  | Values] ).

format( Format, Values ) ->
    iolist_to_binary( io_lib:format( Format, Values ) ).

complete( #{success := Response} ) ->
    complete( result, Response );
complete( #{errorType := Response} ) ->
    complete( failure, Response ).

complete( Type, Response ) ->
    throw( {?MODULE, Type, Response} ).


message_send( Message ) ->
    error_logger:info_msg( "~s", [Message] ).


%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).


-endif.
