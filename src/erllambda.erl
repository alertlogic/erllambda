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
%% @copyright 2016 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(erllambda).
-author('Paul Fisher <pfisher@alertlogic.com>').


%% public - high-level migration orchestration endpoints
-export([succeed/1, succeed/2, fail/1, fail/2, message/1, message/2, pterm/1]).
-export([region/0, config/0, service_host/2, service_config/3]).
-export([ddb_init/3]).
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
succeed( Error ) ->
    complete( success, Error ).

succeed( Format, Values ) ->
    complete( success, Format, Values ).


%%%---------------------------------------------------------------------------
-spec fail( Error :: iolist() ) -> none().
%%%---------------------------------------------------------------------------
%% @doc Complete a processing with failure
%%
fail( Error ) ->
    complete( error, Error ).

fail( Format, Values ) ->
    complete( error, Format, Values ).


%%%---------------------------------------------------------------------------
-spec message( Error :: iolist() ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will ensure the message is output in the log for the lambda
%% invocation.
%%
message( Message ) ->
    NewMessage = format( Message ),
    messages_send( NewMessage ).


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
    messages_send( Message ).


%%%---------------------------------------------------------------------------
-spec pterm( Term :: term() ) -> binary().
%%%---------------------------------------------------------------------------
%% @doc Print an erlang term, truncating result size
%%
%% This function will print an erlang term, limiting the overall size of
%% binary to 500 bytes.
%%
pterm( Term ) ->
    {P, _} = trunc_io:print( Term, 500 ),
    iolist_to_binary( P ).
    

%%%---------------------------------------------------------------------------
-spec region() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the default region in which the Lambda function
%% is running (e.g. AWS_DEFAULT_REGION environment variable), and if for
%% whatever reason this is not set, it will use the 'region' application
%% configuration value.
%%
region() ->
    {ok, Default} = application:get_env( erllambda, region ),
    os:getenv("AWS_DEFAULT_REGION", Default).


%%%---------------------------------------------------------------------------
-spec config() -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Default <code>erlcloud</code> config
%%
%% This function will return a default AWS configuration record for use by
%% the <code>erlcloud</code> application in calling AWS services from lambda
%% functions.
%%
config() ->
    Id = os:getenv( "AWS_ACCESS_KEY_ID" ),
    Secret = os:getenv( "AWS_SECRET_ACCESS_KEY", undefined ),
    Token = os:getenv( "AWS_SECURITY_TOKEN", undefined ),
    #aws_config{ access_key_id = Id,
                 secret_access_key = Secret,
                 security_token = Token }.


%%%---------------------------------------------------------------------------
-spec service_host( Service :: atom() | binary(),
                    Region :: binary() ) -> string().
%%%---------------------------------------------------------------------------
%% @doc Host name for the specified AWS service & region
%%
service_host( Service, Region ) when is_binary(Service) ->
    binary_to_list( <<Service/binary, $., Region/binary, "amazonaws.com">> );
service_host( ListService, Region ) when is_list(ListService) ->
    service_host( list_to_binary(ListService), Region ).


%%%---------------------------------------------------------------------------
-spec service_config( Service :: atom() | binary(), Region :: binary(),
                      Config :: #aws_config{} ) -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Generate config updated to work with specified AWS service & region
%%
%% This function will generate a new configuration to access an AWS service in
%% the specified region, based on the a config containing valid credentials.
%%
service_config( <<"kinesis">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ kinesis_host = Host, kinesis_port = 443 };
service_config( <<"dynamodb">> = Service, Region, Config ) ->
    Host = service_host( Service, Region ),
    Config#aws_config{ ddb_host = Host, ddb_port = 443 };
service_config( ddb, Region, Config ) ->
    service_config( <<"dynamodb">>, Region, Config );
service_config( Service, Region, Config ) when is_atom(Service) ->
    service_config( atom_to_binary(Service, latin1), Region, Config ).
    

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
    DDBConfig = service_config( <<"dynamodb">>, Region, Config ),
    case lists:foldl( fun ddb_init_table/2, {ok, DDBConfig}, Tables ) of
        {ok, _} -> DDBConfig;
        {error, Reason} ->
            erllambda:fail( "failed to init ddb tables, because ~s", [Reason] )
    end.

ddb_init_table( Table, {ok, DDBConfig} = Acc ) ->
    case erlcloud_ddb2:describe_table( Table, [], DDBConfig ) of
        {ok, _Description} -> Acc;
        {error, Reason} -> {error, pterm(Reason)}
    end;
ddb_init_table( _Table, Error ) -> Error.


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
    Key = [{<<"ChkpointId">>, <<Function/binary, $:, RequestId/binary>>}],
    Options = [consistent_read, {projection_expression, <<"ToDo">>}],
    case erlcloud_ddb2:get_item( Table, Key, Options, Config ) of
        {ok, []} ->
            %% no checkpoint found, which is normal operation w/no failures
            Todo = ordsets:from_list( lists:seq( 1, RecordCount ) ),
            #{table => Table, key => Key, function => Function,
              requestid => RequestId, config => Config,
              todo => Todo, written => false};
        {ok, [{<<"ToDo">>, ListTodo}]} ->
            Todo = ordsets:from_list(ListTodo),
            #{table => Table, key => Key, function => Function,
              requestid => RequestId, config => Config,
              todo => Todo, written => true};
        {error, Reason} ->
            erllambda:fail( "ddb checkpoint read for key ~p table ~p failed, "
                            "because ~s", [Key, Table, pterm(Reason)] )
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
                                 table := Table, key := Key,
                                 config := Config} ) ->
    %% completed everything, but have previous checkpoint record to delete
    case erlcloud_ddb2:delete_item( Table, Key, [], Config ) of
        {ok, []} -> ok;
        {error, Reason} ->
            erllambda:fail( "ddb checkpoint delete for key ~p table ~p "
                            "records ~w failed, because ~s",
                            [Key, Table, Complete, pterm(Reason)] )
    end;
checkpoint_complete( Complete, #{todo := Todo, function := Function,
                                 requestid := RequestId, table := Table,
                                 key := Key, config := Config} ) ->
    %% complete some of the remaining record, write (update) checkpoint
    NewTodo = ordsets:subtract( Todo, Complete ),
    Item = [{<<"ChkpointId">>, Key},
            {s, <<"Function">>, Function}, {s, <<"RequestId">>, RequestId},
            {ns, <<"Todo">>, ordsets:to_list(NewTodo)}],
    case erlcloud_ddb2:put_item( Table, Item, [], Config ) of
        {ok, []} ->
            %% we succeeded writing the checkpoint record, but since we did
            %% not complete all records, we fail the lambda so it will retry
            erllambda:fail( "checkpoint, written for key ~p, failing to "
                            "retry records ~w", [Key, Complete] );
        {error, Reason} ->
            erllambda:fail( "ddb checkpoint write for key ~p table ~p "
                            "records ~w failed, because ~s",
                            [Key, Table, Complete, pterm(Reason)] )
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
    application:set_env( erllambda, message_pid, self() ),
    try 
        invoke_( Handler, Event, Context )
    catch
        throw:{result, Json} -> Json;
        Type:Reason ->  
            Trace = erlang:get_stacktrace(),
            fail( "terminated with exception {~w,~w} with trace ~n~p",
                  [Type, Reason, Trace] )
    after
        application:set_env( erllambda, handler, undefined ),
        application:set_env( erllambda, message_pid, undefined )
    end.

invoke_( Handler, Event, Context ) ->
    case Handler:handle( Event, Context ) of
        ok ->
            %% if the handler returns ok, we assume success
            succeed( "completed successfully" );
        _Anything ->
            %% if handler returns anything else, then it did not call
            %% fail/succeed, or return ok, so it is assumed to fail
            fail( Handler, "did not invoke succeed/1,2 or fail/1,2" )
    end.


%%============================================================================
%% Internal Functions
%%============================================================================
format( Message ) ->
    format( Message, [] ).

format( Format, Values ) ->
    {ok, Handler} = application:get_env( erllambda, handler ),
    NewFormat = "~s: " ++ Format,
    NewValues = [Handler] ++ Values,
    Message = iolist_to_binary( io_lib:format( NewFormat, NewValues ) ),
    binary:replace( Message, <<"\"">>, <<"\\\"">>, [global] ).
    

complete( Field, Message ) ->
    complete( Field, Message, [] ).

complete( Field, Format, Values ) ->
    Message = format( Format, Values ),
    AdditionalMessages = messages(),
    Json = jiffy:encode( #{Field => Message, messages => AdditionalMessages} ),
    throw( {result, Json} ).


messages() ->
    messages( [] ).

messages( Acc ) ->
    receive
        {erllambda_message, Message} ->
            NewAcc = [Message | Acc],
            messages( NewAcc )
    after 0 ->
            lists:reverse( Acc )
    end.
                   

messages_send( Message ) ->
    error_logger:info_msg( Message, [] ).


%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).


-endif.
