-module(erllambda_config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    AWSConfig = erllambda_ct:init_aws_env(),
    application:load(erllambda),
    application:set_env(erllambda, handler_module, erllambda_inspect_handler),
    application:set_env(erllambda, print_env, false),
    ErlcloudConfig =
        [{ddb_host, "localhost"},
         {ddb_scheme, "http://"}],
    OldAppEnv = erllambda_ct:set_app_env([{erlcloud, aws_config, ErlcloudConfig}]),
    {ok, _} = application:ensure_all_started(erllambda),
    lists:append(
      [AWSConfig,
       [{old_app_env, OldAppEnv}],
       ErlcloudConfig,
       Config]).


%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    application:stop(erllambda),
    application:unload(erllambda),
    erllambda_ct:destruct_aws_env(Config),
    OldAppEnv = ?config(old_app_env, Config),
    erllambda_ct:set_app_env(OldAppEnv).

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(new_access_key_id, Config) ->
    NewKeyId = "c4f806e027874b93",
    OldKeyId = ?config(access_key_id, Config),
    ?assertNotEqual(OldKeyId, NewKeyId),
    EnvConf = erllambda_ct:putenv([{"AWS_ACCESS_KEY_ID", NewKeyId}]),
    EnvConf ++ Config;
init_per_group(aws_config_cached, Config) ->
    [{access_key_id, erllambda_ct:test_key_id()} | Config];
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(new_access_key_id, Config) ->
    erllambda_ct:restore_env(Config);
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{aws_config_cached, [sequence],
      [test_aws_config_access_key_id,
       {group, new_access_key_id}]},
     {new_access_key_id, [sequence],
      [test_aws_config_access_key_id]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, aws_config_cached},
     test_ddb_config_from_app_config].

%%%===================================================================
%%% TestCases
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_aws_config_access_key_id(Config) ->
    KeyId = list_to_binary(?config(access_key_id, Config)),
    {ok, #{<<"access_key_id">> := KeyId}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}).

test_ddb_config_from_app_config(Config) ->
    DDBHost = list_to_binary(?config(ddb_host, Config)),
    DDBScheme = list_to_binary(?config(ddb_scheme, Config)),
    {ok, #{<<"ddb_host">> := DDBHost, <<"ddb_scheme">> := DDBScheme}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
