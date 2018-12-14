-module(erllambda_inspect_handler).

%% API
-export([handle/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%%%===================================================================
%%% API
%%%===================================================================

handle(#{<<"what">> := <<"erllambda_config">>}, _Context) ->
    AWSConfig = erllambda:config(),
    AWSMap = erllambda_ct:record_to_map(AWSConfig),
    SerializableMap = serializable_map(AWSMap),
    {ok, SerializableMap}.


%%%===================================================================
%%% Private functions
%%%===================================================================
serializable_map(Map) ->
    maps:map(fun serializable_map/2, Map).

serializable_map(_Key, String) when is_list(String) ->
    list_to_binary(String);
serializable_map(_Key, Fun) when is_function(Fun) ->
    list_to_binary(erlang:fun_to_list(Fun));
serializable_map(_Key, Tuple) when is_tuple(Tuple) ->
    serializable_map(erllambda_ct:record_to_map(Tuple));
serializable_map(_Key, Value) ->
    Value.
