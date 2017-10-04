%%-------------------------------------------------------------------
%% @author michaelcoles
%% @copyright 2017 AlertLogic, Inc.
%% @doc
%%
%% @end
%% Created : 2017-09-21 15:59:16.202226
%%-------------------------------------------------------------------
-module(erllambda_checkpoint_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_ok/1,
         t_allbad/1,
         t_partial_fail_recover/1,
         t_clean_partial_fail_recover/1,
         t_tail_partial_fail_recover/1,
         t_intermittent_partial_fail_recover/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     {group, basic}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {basic, [],
      [
       t_ok,
       t_allbad,
       t_partial_fail_recover,
       t_clean_partial_fail_recover,
       t_tail_partial_fail_recover,
       t_intermittent_partial_fail_recover
      ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) -> 
    [{handler, "erllambda_test"} | Config].

end_per_suite(_Config) -> ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    erllambda_ct:setup_service( [{transport, tcp} | Config ]).

end_per_group(_Groupname, Config) ->
    erllambda_ct:cleanup_service( Config ).


%%%===================================================================
%%% Testcase setup/teardown
%%%===================================================================
init_per_testcase(TestCase, Config) ->
    Region = iwsutil:region(),
    Environ = iwsutil:environ(),
    [{ddb_table, <<Region/binary, "-", Environ/binary, "-erllambda-checkpoint">>},
     {lambda_fun, atom_to_binary(TestCase, utf8)},
     %% Since lambda_fun is primarily the key, request_id can be constant
     {request_id, <<"00000001">>},
     %% For simplicity our records are always a list of integers: 1-10
     {records, lists:seq(1, 10)}
     | Config ].

%% t_allbad is the only testcase we expect to leave any data in DDB
end_per_testcase(t_allbad, Config) ->
    Init = init_test_checkpoint(Config),
    clean_test_checkpoint(Init, Config),
    ok;
end_per_testcase(_TestCase, Config) ->
    Init = init_test_checkpoint(Config),
    case Init of
        %% New init, unwritten to DDB: no data in DDB: Good.
        #{ written := false } -> ok;
        #{ written := true, todo := Todo } ->
            %% There was checkpoint data in DDB, this is a problem.
            %% None of the other test cases here should be leaving 
            %% anything in DDB as it will cause issues with future 
            %% test runs.  Fail the test case if anything was found.
            _ = clean_test_checkpoint(Init, Config),
            ct:fail("test checkpoint not cleared, todo: ~p~n", [Todo])
    end.
    
%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

% Checkpointing edge conditions should be validated including:
%  - all pass
%  - all fail
%  - first record fails
%  - last record fails
%  - gaps in between pass/fail records. 

t_ok(Config) ->
    Init = init_test_checkpoint(Config),
    Records = ?config(records, Config),
    ?assertEqual(ok, erllambda_checkpoint:complete(Records, Init)).

t_allbad(Config) ->
    Init = init_test_checkpoint(Config),
    ?assertThrow({erllambda, failure, _}, erllambda_checkpoint:complete([], Init)).

t_partial_fail_recover(Config) ->
    Init0 = init_test_checkpoint(Config),
    Records = ?config(records, Config),
    ?assertThrow({erllambda, failure, _}, erllambda_checkpoint:complete([], Init0)),
    Init1 = init_test_checkpoint(Config),
    ?assertEqual(ok, erllambda_checkpoint:complete(Records, Init1)).

%% For the purposes of these tests, there are several flavours of partial failure,
%%  - the first N records succeed, rest fail
%%  - the first N records fail, the rest succeed
%%  - some random subset of records succeeds
t_clean_partial_fail_recover(Config) ->
    %% Only the first 5 records succeed
    Success = lists:seq(1, 5),
    partial_fail_recover(Success, Config).

t_tail_partial_fail_recover(Config) ->
    %% Only the last 5 records succeed
    Success = lists:seq(6, 10),
    partial_fail_recover(Success, Config).

t_intermittent_partial_fail_recover(Config) ->
    %% Only some "random" records succeed
    Success = lists:seq(1, 10, 3),
    partial_fail_recover(Success, Config).

%%%===================================================================
%%% Test helper functions
%%%===================================================================

init_test_checkpoint(Config) ->
    Table = ?config(ddb_table, Config),
    Function = ?config(lambda_fun, Config),
    RequestId = ?config(request_id, Config),
    Records = ?config(records, Config),
    AWSConfig = erllambda:ddb_init([ Table ]),
    erllambda_checkpoint:init(Table, Function, RequestId, Records, AWSConfig).

clean_test_checkpoint(#{ key := KeyVal }, Config) ->
    Key = [{<<"ChkPointId">>, KeyVal}],
    Table = ?config(ddb_table, Config),
    AWSConfig = erllambda:ddb_init([ Table ]),
    {ok, []} = erlcloud_ddb2:delete_item( Table, Key, [], AWSConfig ).

partial_fail_recover(Success, Config) ->
    Records = ?config(records, Config),
    % This would be bad testing data for the purpose of this function
    % if Success contained records not in Records...
    case Success -- Records of
        [] -> ok;
        _ -> ct:fail({success_not_subset_of_records, Success, Records})
    end,
    Init0 = init_test_checkpoint(Config),
    ?assertEqual(Records, erllambda_checkpoint:todo(Init0)),
    ?assertThrow({erllambda, failure, _}, erllambda_checkpoint:complete(Success, Init0)),
    %% We should be able to recover, and complete the checkpoint
    Init1 = init_test_checkpoint(Config),
    Todo = erllambda_checkpoint:todo(Init1),
    %% The remaining records need processing...
    ?assertEqual(Records -- Success, Todo),
    ?assertEqual(ok, erllambda_checkpoint:complete(Todo, Init1)).
