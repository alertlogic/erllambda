-module(erllambda_test).
-behavior(erllambda).

-export([handle/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").


%%%---------------------------------------------------------------------------
-spec handle( Event :: map(), Context :: map() ) -> ok | none().
%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( #{<<"test">> := <<"test_success">>, <<"message">> := Message}, _Ctx ) ->
    erllambda:succeed( Message );
handle( #{<<"test">> := <<"test_success_ok">>,
          <<"message">> := Message}, _Ctx ) ->
    {ok, Message};
handle( #{<<"test">> := <<"test_fail">>, <<"message">> := Message}, _Ctx ) ->
    erllambda:fail( Message );
handle( #{<<"test">> := <<"test_fail_error">>,
          <<"message">> := Message}, _Ctx ) ->
    {error, Message};
handle( #{<<"test">> := <<"test_except_error_undef">>}, _Ctx ) ->
    erllmabda:fail( "never works" );
handle( _Event, _Context ) ->
    case {erllambda:region(), erllambda:config()} of
        {Region, #aws_config{}} when is_binary(Region) -> ok;
        {BadRegion, BadConfig} ->
            erllambda:fail( "failed to resolve region: ~p config ~p",
                            [BadRegion, BadConfig] )
    end.
