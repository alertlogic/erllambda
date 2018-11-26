-module(erllambda_ct).

init_aws_env() ->
    {ok, _} = erllambda_aws_runtime:start(),
    Port = erllambda_aws_runtime:http_port(),
    RunTimeAddress = lists:flatten(io_lib:format("127.0.0.1:~w", [Port])),
    OldEnv =
        putenv([{"AWS_LAMBDA_RUNTIME_API", RunTimeAddress},
                {"AWS_ACCESS_KEY_ID", "10560ff7be594e0c"},
                {"AWS_SECRET_ACCESS_KEY", "71b5110832454df0ba9a85073d60bb8b"},
                {"AWS_SESSION_TOKEN", "c14c5ae0d56242cbb2d85a8cf1433ece"}]),
    [{old_env, OldEnv}].

destruct_aws_env(Config) ->
    ok = erllambda_aws_runtime:stop(),
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
