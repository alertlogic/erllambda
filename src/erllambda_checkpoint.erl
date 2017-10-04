%%%---------------------------------------------------------------------------
%% @doc erllambda_checkpoint - Internal(?) API for checkpoint management.
%%
%% This module contains the core functions associated with checkpoint.
%%
%% @copyright 2017 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(erllambda_checkpoint).

-export([init/5, todo/1, complete/2]).

-include("erllambda.hrl").

-type todo_list() :: record_id_list().
-type checkpoint() :: #{table => binary(), key => binary(),
                        function => binary(), requestid => binary(),
                        config => #aws_config{}, todo => todo_list(),
                        written => boolean()}.
-export_type([todo_list/0, checkpoint/0]).

%%%---------------------------------------------------------------------------
-spec init( Table :: binary(), Function :: binary(),
                       RequestId :: binary(), Records :: [term()],
                       Config :: #aws_config{} ) ->
                             checkpoint() | none().
%%%---------------------------------------------------------------------------
%% @doc Generate config and validate access to a list of tables
%%
%% This function will retrieve an existing checkpoint record if one exists,
%% and make the {@link todo/1. todo list} available for the
%% lambda invocation.  If this is the first time the function has been
%% invoked, then no checkpoint will exist, and the <em>todo list</em> will
%% contains a list of all records.  The lambda function is expected to
%% process just the records corresponding to the <em>todo list</code>, and
%% then when finished call {@link complete/2} with the list of
%% record that it was able to complete.
%%
init( Table, Function, RequestId, Records, Config ) ->
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
            erllambda:fail( "checkpoint_init failed, because ~p", [Otherwise] )
    end.
    

%%%---------------------------------------------------------------------------
-spec todo( Checkpoint :: checkpoint() ) -> todo_list().
%%%---------------------------------------------------------------------------
%% @doc Retrieve the {@link todo_list/1. todo list} from the checkpoint
%%
todo( #{todo := Todo} ) -> ordsets:to_list(Todo);
todo( #{} ) -> undefined.


%%%---------------------------------------------------------------------------
-spec complete( Complete :: record_id_list(),
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
complete( Complete, #{todo := Complete, written := false} ) -> ok;
complete( Complete, #{todo := Complete, written := true,
                                 table := Table, key := KeyVal,
                                 config := Config} ) ->
    %% completed everything, but have previous checkpoint record to delete
    Key = [{<<"ChkPointId">>, KeyVal}],
    case erlcloud_ddb2:delete_item( Table, Key, [], Config ) of
        {ok, []} -> ok;
        Otherwise ->
            erllambda:fail( "checkpoint_complete delete failed, because ~p", [Otherwise] )
    end;
complete( Complete, #{todo := Todo, table := Table,
                                 key := Key, config := Config} ) ->
    %% complete some of the remaining record, write (update) checkpoint
    NewTodo = ordsets:subtract( Todo, Complete ),
    Item = [{<<"ChkPointId">>, Key},
            {<<"ToDo">>, {ns, ordsets:to_list(NewTodo)}}],
    case erlcloud_ddb2:put_item( Table, Item, [], Config ) of
        {ok, []} ->
            %% we succeeded writing the checkpoint record, but since we did
            %% not complete all records, we fail the lambda so it will retry
            erllambda:fail( "checkpoint, written for key ~p, failing to "
                  "retry records ~w", [Key, Complete] );
        Otherwise ->
            erllambda:fail( "checkpoint_complete write failed, because ~p", [Otherwise] )
    end.

