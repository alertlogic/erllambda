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
-export([succeed/1, succeed/2, fail/1, fail/2, message/1, message/2]).
-export([metric/1, metric/2, metric/3, metric/4]).
-export([get_remaining_ms/1]).
-export([region/0, config/0]).
-export([ddb_init/1, ddb_init/3]).
-export([checkpoint_init/5, checkpoint_todo/1, checkpoint_complete/2]).

%% private - handler invocation entry point, used by http api
-export([invoke/3]).


-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%============================================================================
%% Type Definitions
%%============================================================================
-type record_id() :: pos_integer().
-type record_id_list() :: [record_id()].
-type todo_list() :: record_id_list().
-type checkpoint() :: #{table => binary(), key => binary(),
                        function => binary(), requestid => binary(),
                        config => #aws_config{}, todo => todo_list(),
                        written => boolean()}.


%%============================================================================
%% Callback Interface Definition
%%============================================================================
%%%---------------------------------------------------------------------------
-callback handle( Event :: map(), Context :: map() ) -> ok | none().
%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
    

%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec succeed( Message :: iolist() ) -> none().
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
    NewMessage = format( Message ),
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


-spec metric(MetricName :: string(), MetricValue :: integer(),
             Type :: string(), Tags :: list()) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send custom metrics to Datadog
%%
%% This function will send log message from lambda using following format
%% MONITORING|unix_epoch_timestamp|metric_value|metric_type|my.metric.name|#tag1:value,tag2
%% where metric_type :: "count" | "gauge"
%%
metric(MName) ->
    metric(MName, 1).
metric(MName, Val) ->
    metric(MName, Val, "count").
metric(MName, Val, Type) ->
    metric(MName, Val, Type, []).
metric(MName, Val, Type, Tags)
        when is_list(MName)
        andalso (Type == "count" orelse Type == "gauge")
        andalso is_integer(Val) ->
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
                             checkpoint() | none().
%%%---------------------------------------------------------------------------
%% @doc Generate config and validate access to a list of tables
%%
%% This function will retrieve an existing checkpoint record if one exists,
%% and make the {@link checkpoint_todo/1. todo list} available for the
%% lambda invocation.  If this is the first time the function has been
%% invoked, then no checkpoint will exist, and the <em>todo list</em> will
%% contains a list of all records.  The lambda function is expected to
%% process just the records corresponding to the <em>todo list</code>, and
%% then when finished call {@link checkpoint_complete/2} with the list of
%% record that it was able to complete.
%%
checkpoint_init( Table, Function, RequestId, Records, Config ) ->
    RecordCount = length(Records),
    KeyVal = <<Function/binary, $:, RequestId/binary>>,
    Key = [{<<"ChkPointId">>, KeyVal}],
    Options = [consistent_read, {projection_expression, <<"ToDo">>}],
    case erlcloud_ddb2:get_item( Table, Key, Options, Config ) of
        {ok, []} ->
            %% no checkpoint found, which is normal operation w/no failures
            Todo = ordsets:from_list( lists:seq( 1, RecordCount ) ),
            #{table => Table, key => KeyVal, function => Function,
              requestid => RequestId, config => Config,
              todo => Todo, written => false};
        {ok, [{<<"ToDo">>, ListTodo}]} ->
            Todo = ordsets:from_list(ListTodo),
            #{table => Table, key => KeyVal, function => Function,
              requestid => RequestId, config => Config,
              todo => Todo, written => true};
        Otherwise ->
            fail( "checkpoint_init failed, because ~p", [Otherwise] )
    end.
    

%%%---------------------------------------------------------------------------
-spec checkpoint_todo( Checkpoint :: checkpoint() ) -> todo_list().
%%%---------------------------------------------------------------------------
%% @doc Retrieve the {@link todo_list/1. todo list} from the checkpoint
%%
checkpoint_todo( #{todo := Todo} ) -> ordsets:to_list(Todo);
checkpoint_todo( #{} ) -> undefined.


%%%---------------------------------------------------------------------------
-spec checkpoint_complete( Complete :: record_id_list(),
                           Checkpoint :: checkpoint() ) -> ok | none().
%%%---------------------------------------------------------------------------
%% @doc Complete the checkpoint based on the list provided.
%%
%% This function will update a {@link checkpoint(). Checkpoint} based on the
%% {@link record_id_list(). record id list} representing the records
%% successfully processed by the lambda function.  If this is not the same
%% as the {@link todo_list(). todo list} retrieved from the
%% <code>checkpoint</code>, then this function will write (or update) a
%% checkpoint record in the DynamoDB table.
%%
%% If this is the first invocation of the lambda function for a request id,
%% and the <code>Complete</code> list matches the list of all records in the
%% invocation, then no record will ever be written to DyanmoDB.  This is the
%% normal operating case, so writes should not occur.
%%
%% If this function does write/update a checkpoint record, it will ternmiate
%% the lambda invocation as failed, so that it will be rescheduled,
%% presuming that whatever issue existed which prevented some of the record
%% from being successfully processed will be cleared up.
%%
checkpoint_complete( Complete, #{todo := Complete, written := false} ) -> ok;
checkpoint_complete( Complete, #{todo := Complete, written := true,
                                 table := Table, key := KeyVal,
                                 config := Config} ) ->
    %% completed everything, but have previous checkpoint record to delete
    Key = [{<<"ChkPointId">>, KeyVal}],
    case erlcloud_ddb2:delete_item( Table, Key, [], Config ) of
        {ok, []} -> ok;
        Otherwise ->
            fail( "checkpoint_complete delete failed, because ~p", [Otherwise] )
    end;
checkpoint_complete( Complete, #{todo := Todo, table := Table,
                                 key := Key, config := Config} ) ->
    %% complete some of the remaining record, write (update) checkpoint
    NewTodo = ordsets:subtract( Todo, Complete ),
    Item = [{<<"ChkPointId">>, Key},
            {<<"ToDo">>, {ns, ordsets:to_list(NewTodo)}}],
    case erlcloud_ddb2:put_item( Table, Item, [], Config ) of
        {ok, []} ->
            %% we succeeded writing the checkpoint record, but since we did
            %% not complete all records, we fail the lambda so it will retry
            fail( "checkpoint, written for key ~p, failing to "
                  "retry records ~w", [Key, Complete] );
        Otherwise ->
            fail( "checkpoint_complete write failed, because ~p", [Otherwise] )
    end.


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
        message_send( "EOF: flush stdout" ),
        application:set_env( erllambda, handler, undefined )
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
to_list( V ) -> V.
    
expiration( V ) when is_integer(V) -> V;
expiration( null ) -> undefined;
expiration( undefined ) -> undefined.

invoke_exec( Handler, Event, Context ) ->
    case Handler:handle( Event, Context ) of
        ok -> succeed( "completed successfully" );
        {ok, Result} -> succeed( Result );
        {error, Reason} -> fail( Reason );
        _Anything ->
            %% if handler returns anything else, then it did not call
            %% fail/succeed, or return ok, so it is assumed to fail
            fail( "did not invoke succeed/1,2 or fail/1,2" )
    end.


%%============================================================================
%% Internal Functions
%%============================================================================
format( Message ) ->
    format( Message, [] ).

format( Format, Values ) ->
    iolist_to_binary( io_lib:format( Format, Values ) ).
    

complete( #{success := _} = Response ) ->
    complete( result, Response );
complete( #{errorType := _} = Response ) ->
    complete( failure, Response ).

complete( Type, Response ) ->
    Message = jiffy:encode( Response ),
    throw( {?MODULE, Type, Message} ).


message_send( Message ) ->
    io:fwrite( "~s\n", [Message] ).

%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).


-endif.
