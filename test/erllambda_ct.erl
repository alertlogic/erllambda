-module(erllambda_ct).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

init_aws_env() ->
    {ok, _} = erllambda_aws_runtime:start(),
    Port = erllambda_aws_runtime:http_port(),
    RunTimeAddress = lists:flatten(io_lib:format("127.0.0.1:~w", [Port])),
    OldEnv =
        putenv([{"AWS_LAMBDA_RUNTIME_API", RunTimeAddress},
                {"AWS_ACCESS_KEY_ID", test_key_id()},
                {"AWS_SECRET_ACCESS_KEY", test_access_key()},
                {"REGION", "us-east-1"}]),
    [{old_env, OldEnv}].

destruct_aws_env(Config) ->
    ok = erllambda_aws_runtime:stop(),
    restore_env(Config).

restore_env(Config) ->
    Env = proplists:get_value(old_env, Config),
    putenv(Env).

putenv([{Key, Value} | Tail]) ->
    OldValue = os:getenv(Key, undefined),
    putenv(Key, Value),
    [{Key, OldValue} | putenv(Tail)];
putenv([]) ->
    [].

putenv(Key, undefined) ->
    os:unsetenv(Key);
putenv(Key, Value) ->
    os:putenv(Key, Value).

set_app_env([{App, Key, NewValue} | Tail]) ->
    OldValue = application:get_env(App, Key, '__erllambda_ct_undefined'),
    set_app_env(App, Key, NewValue),
    [{App, Key, OldValue} | set_app_env(Tail)];
set_app_env([]) ->
    [].

set_app_env(App, Key, '__erllambda_ct_undefined') ->
    application:unset_env(App, Key);
set_app_env(App, Key, Value) ->
    application:set_env(App, Key, Value).

test_key_id() ->
    "10560ff7be594e0c".

test_access_key() ->
    "71b5110832454df0ba9a85073d60bb8b".

record_to_map(R) when is_record(R, aws_config) ->
    Ks = record_info(fields, aws_config),
    record_to_map(R, Ks);
record_to_map(R) when is_record(R, aws_assume_role) ->
    Ks = record_info(fields, aws_assume_role),
    record_to_map(R, Ks);
record_to_map(R) when is_record(R, hackney_client_options) ->
    Ks = record_info(fields, hackney_client_options),
    record_to_map(R, Ks).

record_to_map(Record, Ks) ->
    [_ | Vs] = tuple_to_list(Record),
    maps:from_list(lists:zip(Ks, Vs)).

map_to_record(M, aws_config) ->
    Vs = lists:map(
        fun(K) ->
            maps:get(K, M)
        end, record_info(fields, aws_config)
    ),
    list_to_tuple([aws_config | Vs]).
