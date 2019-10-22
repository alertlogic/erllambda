-module(erllambda_logging_SUITE).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 10}}].

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
    application:set_env(erllambda, handler_module, erllambda_logging_handler),
    application:set_env(erllambda, print_env, false),
    {ok, _} = application:ensure_all_started(erllambda),
    erllambda_io_srv:start(),
    AWSConfig ++ Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(Config) ->
    erllambda_io_srv:stop(),
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
    erllambda_io_srv:flush(),
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
    [test_no_args_messages,
     test_bad_format_do_not_remove_error_handler].

%%%===================================================================
%%% TestCases
%%%===================================================================

-define(invoke_handler, invoke_handler(?FUNCTION_NAME)).

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_no_args_messages(_Config) ->
    ?invoke_handler,
    ErrorLoggerHandlers = gen_event:which_handlers(error_logger),
    true = lists:member(erllambda_error_handler, ErrorLoggerHandlers),
    {ok,
     ["Starting logs test\n",
      "This line should not break logging\n",
      "This and any further logs should be printed\n",
      "erllambda_logging_handler:20 Using LOG macro should give module and line numbers in the logs\n",
      "erllambda_logging_handler:21 LOG macro using format 42\n"]}
        = erllambda_io_srv:list_requests().

test_bad_format_do_not_remove_error_handler(_Config) ->
    ?invoke_handler,
    ErrorLoggerHandlers = gen_event:which_handlers(error_logger),
    true = lists:member(erllambda_error_handler, ErrorLoggerHandlers),
    {ok, ["Good format\n",
          "INFO: \"Bad format\" - [any]\n",
          "Good format\n"]}
        = erllambda_io_srv:list_requests().

%%%===================================================================
%%% Internal functions
%%%===================================================================
invoke_handler(Type) ->
    {ok, _} = erllambda_aws_runtime:call(#{type => Type}),
    %% make sure that error handler processed the event
    gen_event:sync_notify(error_logger, sync).
