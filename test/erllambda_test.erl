-module(erllambda_test).
-behavior(erllambda).

-export([handle/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").


%%%---------------------------------------------------------------------------
-spec handle( Event :: map(), Context :: map() ) -> ok | none().
%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( _Event, _Context ) ->
    case {erllambda:region(), erllambda:config()} of
        {Region, #aws_config{}} when is_binary(Region) -> ok;
        {BadRegion, BadConfig} ->
            erllambda:fail( "failed to resolve region: ~p config ~p",
                            [BadRegion, BadConfig] )
    end.
