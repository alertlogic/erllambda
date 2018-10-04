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
-export([retry/2, retry/3]).
-export([message/1, message/2, message_ctx/2, message_ctx/3]).
-export([metric/1, metric/2, metric/3, metric/4]).
-export([get_remaining_ms/1]).
-export([region/0, config/0]).
-export([ddb_init/1, ddb_init/3]).
-export([checkpoint_init/5, checkpoint_todo/1, checkpoint_complete/2]).

%% private - handler invocation entry point, used by http api
-export([invoke/3]).

-include("erllambda.hrl").

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
-spec retry(Message :: iolist(), Checkpoint :: iolist() ) -> none().
%%%---------------------------------------------------------------------------
%% @doc Complete processing with success
%%
retry( Message, Checkpoint ) ->
    retry( "~s", [Message], Checkpoint ).

retry( Format, Values, Checkpoint ) ->
    complete( #{errorType => 'HandlerCheckpoint',
                errorMessage => format( Format, Values ),
                checkpoint => Checkpoint} ).


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
    NewMessage = format( Message, [] ),
    message_send( NewMessage ).


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
message_ctx( #{<<"erllambda_request_id">> := ReqId}, Format, Values ) ->
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


%%%---------------------------------------------------------------------------
-spec ddb_init( Tables :: [binary()], Region :: binary(),
                Config :: #aws_config{} ) -> #aws_config{} | none().
%%%---------------------------------------------------------------------------
%% @doc Generate config and validate access to a list of tables
%%
%% This function will generate a new configuration to access the AWS
%% DynamoDB service in the specified region, based on the a config
%% containing valid credentials. In addition, this function will validate
%% access to a list of tables, and if any cannot be described, will call
%% {@link fail/2} under the assumption that the lambda function cannot
%% continue without access to these tables.
%%
ddb_init( Tables, Config, Region ) ->
    DDBConfig = erlcloud_aws:service_config( <<"dynamodb">>, Region, Config ),
    case lists:foldl( fun ddb_init_table/2, {ok, DDBConfig}, Tables ) of
        {ok, _} -> DDBConfig;
        {error, {table_not_available, Table, Reason}} ->
            fail( "ddb_init failed for ~s, because ~p", [Table, Reason] )
    end.

ddb_init_table( Table, {ok, DDBConfig} = Acc ) ->
    case erlcloud_ddb2:describe_table( Table, [], DDBConfig ) of
        {ok, _Description} -> Acc;
        Otherwise -> {error, {table_not_available, Table, Otherwise}}
    end;
ddb_init_table( _Table, Error ) -> Error.

ddb_init( Tables ) ->
    ddb_init( Tables, config(), region() ).


%%%---------------------------------------------------------------------------
-spec checkpoint_init( Table :: binary(), Function :: binary(),
                       RequestId :: binary(), Records :: [term()],
                       Config :: #aws_config{} ) ->
                             erllambda_checkpoint:checkpoint() | none().
%%%---------------------------------------------------------------------------
%%% @doc {@link erllambda_checkpoint:init/5}
checkpoint_init(Table, Function, RequestId, Records, Config) ->
    erllambda_checkpoint:init(Table, Function, RequestId, Records, Config).

%%%---------------------------------------------------------------------------
-spec checkpoint_todo( Checkpoint :: erllambda_checkpoint:checkpoint() ) ->
    erllambd_checkpoint:todo_list().
%%%---------------------------------------------------------------------------
%%% @doc {@link erllambda_checkpoint:todo/1}
checkpoint_todo(Checkpoint) -> erllambda_checkpoint:todo(Checkpoint).

%%%---------------------------------------------------------------------------
-spec checkpoint_complete( Complete :: record_id_list(),
                           Checkpoint :: erllambda_checkpoint:checkpoint() ) -> ok | none().
%%%---------------------------------------------------------------------------
%%% @doc {@link erllambda_checkpoint:complete/2}
checkpoint_complete(Complete, Checkpoint) ->
    erllambda_checkpoint:complete(Complete, Checkpoint).

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
    invoke_credentials( Context ),
    try 
        invoke_exec( Handler, Event, Context )
    catch
        throw:{?MODULE, result, Json} -> {ok, Json};
        throw:{?MODULE, failure, Json} -> {error, {500, Json}};
        throw:{?MODULE, retry, Json} -> {error, {503, Json}};
        Type:Reason ->
            Trace = erlang:get_stacktrace(),
            Message = iolist_to_binary(
                        io_lib:format( "terminated with exception {~w,~w}",
                                       [Type, Reason] ) ),
            message_send( format( "~s with trace ~n~p", [Message, Trace] ) ),
            Response = jiffy:encode( #{errorType => 'HandlerFailure',
                                       errorMessage => Message} ),
            {error, {500, Response}}
    after
        lhttpc_restart()
    end.

invoke_credentials( #{<<"AWS_SECURITY_TOKEN">> := Token} = Context )
  when Token =/= null ->
    case application:get_env( iwsutils, config ) of
        {ok, #aws_config{security_token = Token}} -> ok;
        _ -> invoke_update_credentials( Context )
    end;
invoke_credentials( #{<<"AWS_SECRET_ACCESS_KEY">> := Key} = Context  ) ->
    case application:get_env( iwsutils, config ) of
        {ok, #aws_config{secret_access_key = Key}} -> ok;
        _ -> invoke_update_credentials( Context )
    end.

invoke_update_credentials( #{<<"AWS_ACCESS_KEY_ID">> := Id,
                             <<"AWS_SECRET_ACCESS_KEY">> := Key,
                             <<"AWS_SECURITY_TOKEN">> := Token,
                             <<"AWS_CREDENTIAL_EXPIRE_TIME">> := Expire} ) ->
    Config = #aws_config{ access_key_id = to_list(Id),
                          secret_access_key = to_list(Key),
                          security_token = to_list(Token),
                          expiration = expiration(Expire) },
    application:set_env( iwsutil, config, Config ).

to_list( V ) when is_list(V) -> V;
to_list( V ) when is_binary(V) -> binary_to_list(V);
to_list( V ) when is_atom(V) -> atom_to_list(V);
to_list( V ) when is_integer(V) -> integer_to_list(V);
to_list( V ) when is_float(V) -> float_to_list(V);
to_list( V ) -> V.
    
expiration( V ) when is_integer(V) -> V;
expiration( null ) -> undefined;
expiration( undefined ) -> undefined.

invoke_exec( Handler, Event, Context ) ->
    case Handler:handle( Event, Context ) of
        ok -> succeed( "completed successfully" );
        {ok, Result} -> succeed( Result );
        {error, Reason} -> fail( Reason );
        % This last clause is a return only supported by eee for the moment;
        % one could eventually tie this with erllambda_checkpoint (provided
        % we have a stable way to identify if we are called from EEE or
        % AWS lambda; maybe look for the absence of awsRequestId in Context
        % meaning we're called from EEE)
        {checkpoint, {Reason, Checkpoint}} -> retry(Reason, Checkpoint);
        _Anything ->
            %% if handler returns anything else, then it did not call
            %% fail/succeed/retry, or return ok, so it is assumed to fail
            fail( "did not invoke succeed/1,2 or fail/1,2 or retry/2,3" )
    end.

% in Lambda environment ErlVM outlives single invoke
% need to reset lhttpC pool state since all sockets are dead next time.
% atm it's enough to reset default pool. (used by erllambda, erlcloud, alstore)
lhttpc_restart() ->
    case application:get_env(erllambda, reset_lhttpc_pools) of
        {ok, Pools} when is_list(Pools) ->
            lists:foreach(fun(P) ->
                    message("reseting ~p lhttpc pool", [P]),
                    lhttpc:delete_pool(P), lhttpc:add_pool(P)
                end,
                Pools);
        _ -> ok
    end.


%%============================================================================
%% Internal Functions
%%============================================================================
format_reqid( ReqId, Format, Values ) ->
    format( "[" ?AL_CTX_SD_ID " aid=\"~s\"] " ++ Format,
            [ReqId  | Values] ).

format( Format, Values ) ->
    iolist_to_binary( io_lib:format( Format, Values ) ).

complete( #{success := _} = Response ) ->
    complete( result, Response );
complete( #{checkpoint := _} = Response ) ->
    complete( retry, Response );
complete( #{errorType := _} = Response ) ->
    complete( failure, Response ).

complete( Type, Response ) ->
    Message = jiffy:encode( Response ),
    throw( {?MODULE, Type, Message} ).


message_send( Message ) ->
    error_logger:info_msg( "~s", [Message] ).


%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).


-endif.
