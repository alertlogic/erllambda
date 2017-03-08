%%%-----------------------------------------------------------------------------
%% @doc Validate basic operation
%%
%% Common Tests that verify the basic operation of defining a erllambda
%% module, starting the application, and invoking it via the javascript
%% works.
%%
%%
%% @copyright 2017 AlertLogic, Inc.
%%%-----------------------------------------------------------------------------
-module(erllambda_SUITE).
-author('Paul Fisher <pfisher@alertlogic.com>').

%% common test suite callbacks
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

%% just export all functions so we don't have to do each test individually
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%******************************************************************************
%% Test SUITE callbacks
%%******************************************************************************
suite() ->
    [
     {timetrap, {seconds, (5 * 60)}}
    ].


all() ->
    [
     {group, tcp},
     {group, unix}
    ].


groups() ->
    [
     {tcp, [],
      [
       test_verify,
       test_invoke
      ]},
     {unix, [],
      [
       test_verify,
       test_invoke
      ]}
    ].


init_per_suite( Config ) ->
    [{handler, "erllambda_test"} | Config].

end_per_suite( Config ) -> Config.
    

init_per_group( Transport, Config ) ->
    erllambda_ct:setup_service( [{transport, Transport}, Config] ).
     
end_per_group( _Transport, Config ) ->
    erllambda_ct:cleanup_service( Config ).


%%******************************************************************************
%% Test SUITE callbacks
%%******************************************************************************
test_verify( Config ) ->
    ?assertMatch( {ok, 200, _, undefined}, erllambda_ct:verify( Config ) ).

test_invoke( Config ) ->
    Context = #{<<"AWS_ACCESS_KEY_ID">> => "ID",
                <<"AWS_SECRET_ACCESS_KEY">> => "KEY",
                <<"AWS_SECURITY_TOKEN">> => "TOKEN",
                <<"AWS_CREDENTIAL_EXPIRE_TIME">> => 123456},
    ?assertMatch( {ok, 200, _, #{<<"success">> := _}},
                  erllambda_ct:invoke( #{result => ok}, Context, Config ) ).


%%******************************************************************************
%% Internal Functions
%%******************************************************************************

