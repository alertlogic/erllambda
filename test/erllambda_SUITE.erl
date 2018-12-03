-module(erllambda_SUITE).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

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
    application:set_env(erllambda, handler_module, erllambda_proxy_handler),
    application:set_env(erllambda, print_env, false),
    {ok, _} = application:ensure_all_started(erllambda),
    AWSConfig ++ Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    application:stop(erllambda),
    application:unload(erllambda),
    erllambda_ct:destruct_aws_env(Config).

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
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
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [test_fibonachi,
     test_aws_config_preserved,
     test_aws_config_updated].

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
test_fibonachi(_Config) ->
    erllambda_proxy_handler:delegate_to(erllambda_fibonachi_handler),
    {ok, #{<<"sequence">> := [0, 1, 1, 2, 3, 5]}} =
        lists:foldl(
          fun(_, {ok, Params}) -> erllambda_aws_runtime:call(Params) end,
          {ok, #{<<"sequence">> => []}},
          lists:seq(0, 5)).


test_aws_config_preserved(_Config) ->
    erllambda_proxy_handler:delegate_to(erllambda_inspect_handler),
    {ok, #{<<"expiration">> := <<"undefined">>}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}),
    {ok, AWSConfig} = application:get_env(erllambda, config),
    Expiration = 1800,
    AWSConfig1 = AWSConfig#aws_config{expiration = Expiration},
    application:set_env(erllambda, config, AWSConfig1),
    {ok, #{<<"expiration">> := Expiration}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}).


test_aws_config_updated(_Config) ->
    erllambda_proxy_handler:delegate_to(erllambda_inspect_handler),
    {ok, #{<<"security_token">> := SecurityToken}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}),
    NewToken = <<"7956547a4caa4759b30ae9f977c374a5">>,
    ?assertNotEqual(SecurityToken, NewToken),
    os:putenv("AWS_SESSION_TOKEN", binary_to_list(NewToken)),
    {ok, #{<<"security_token">> := NewToken}} =
        erllambda_aws_runtime:call(#{<<"what">> => <<"erllambda_config">>}).
%%%===================================================================
%%% Internal functions
%%%===================================================================
