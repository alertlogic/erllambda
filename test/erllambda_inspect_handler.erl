-module(erllambda_inspect_handler).

%% API
-export([handle/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

%%%===================================================================
%%% API
%%%===================================================================

handle(#{<<"what">> := <<"erllambda_config">>}, _Context) ->
    {ok, AWSConfig} = application:get_env( erllambda, config ),
    ConfigMap =
        #{access_key_id     => list_to_binary(AWSConfig#aws_config.access_key_id),
          secret_access_key => list_to_binary(AWSConfig#aws_config.secret_access_key),
          security_token    => list_to_binary(AWSConfig#aws_config.security_token),
          expiration        => AWSConfig#aws_config.expiration},
    {ok, ConfigMap}.


%%%===================================================================
%%% Private functions
%%%===================================================================
