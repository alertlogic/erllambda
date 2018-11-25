%%%-----------------------------------------------------------------------------
%% @doc Verification of all erllambda_util library functions
%%
%% @copyright 2018 Alert Logic, Inc.
%%%-----------------------------------------------------------------------------
-module(erllambda_util_SUITE).
-author('Paul Fisher <pfisher@alertlogic.com>').

%% common test suite callbacks
-export([all/0]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% just export all functions so we don't have to do each test individually
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%=============================================================================
%% Constant Definitions
%%%=============================================================================


%%%=============================================================================
%% Test SUITE callbacks
%%%=============================================================================
suite() ->
    [
     {timetrap, {seconds, 120}}
    ].

all() ->
    [
     {group, region},
     {group, accountid},
     {group, environ}
    ].

groups() ->
    [
     {region, [test_region_devenv, test_region_config, test_region_env,
               test_region_instancedoc]},
     {accountid, [test_accountid_config, test_accountid_instancedoc,
                  test_accountid_sts]},
     {environ, [test_environ_devenv, test_environ_config,
                test_environ_file]}
    ].

init_per_group( Group, Config ) ->
    ?assertMatch( {ok, _}, application:ensure_all_started( lhttpc ) ),
    NewConfig = mock_group( Group, Config ),
    [{group, Group} | NewConfig].
end_per_group( Group, Config ) ->
    NewConfig = unmock_group( Group, Config ),
    ?assertEqual( ok, application:stop( lhttpc ) ),
    ?assertEqual( ok, application:unload( lhttpc ) ),
    NewConfig.

init_per_testcase( Test, Config ) ->
    clear( proplists:get_value( group, Config ) ),
    application:load( erllambda ),
    application:set_env( erllambda, print_env, false ),
    ?assertMatch( {ok, _}, application:ensure_all_started( erllambda ) ),
    application:set_env( erllambda, '_test', Test ),
    Config.

end_per_testcase( _Test, Config ) ->
    application:unset_env( erllambba, '_test' ),
    ?assertEqual( ok, application:stop( erllambda ) ),
    ?assertEqual( ok, application:unload( erllambda ) ),
    Config.


%%%=============================================================================
%% Test Functions - Region Group
%%%=============================================================================
test_region_devenv( _Config ) ->
    os:putenv( "REGION", "us-north-1" ),
    ?assertEqual( <<"us-north-1">>, erllambda:region() ),
    %% reset env to verify original value cached
    os:putenv( "REGION", "us-wackadoodle-10" ),
    ?assertEqual( <<"us-north-1">>, erllambda:region() ).

test_region_config( _Config ) ->
    Region = <<"us-south-9">>,
    ok = application:set_env( erllambda, region, Region ),
    ?assertEqual( Region, erllambda:region() ),
    %% reset config to verify original value cached
    ok = application:set_env( erllambda, region, <<"us-deepsouth-99">> ),
    ?assertEqual( Region, erllambda:region() ).

test_region_env( _Config ) ->
    os:putenv( "AWS_REGION", "us-farnorth-1" ),
    ?assertEqual( <<"us-farnorth-1">>, erllambda:region() ),
    %% reset env to verify original value cached
    os:putenv( "AWS_REGION", "us-wackadoodle-10" ),
    ?assertEqual( <<"us-farnorth-1">>, erllambda:region() ).

test_region_instancedoc( _Config ) ->
    ?assertEqual( <<"us-fareast-1">>, erllambda:region() ).


%%%=============================================================================
%% Test Functions - AccountId Group
%%%=============================================================================
test_accountid_config( _Config ) ->
    AccountId = <<"123456789012">>,
    ok = application:set_env( erllambda, accountid, AccountId ),
    ?assertEqual( AccountId, erllambda:accountid() ),
    %% reset config to verify original value cached
    ok = application:set_env( erllambda, accountid, <<"987654321098">> ),
    ?assertEqual( AccountId, erllambda:accountid() ).

test_accountid_instancedoc( _Config ) ->
    ?assertEqual( <<"123456789012">>, erllambda:accountid() ).

test_accountid_sts( _Config ) ->
    ?assertEqual( <<"912345678901">>, erllambda:accountid() ).


%%%=============================================================================
%% Test Functions - Environ Group
%%%=============================================================================
test_environ_devenv( _Config ) ->
    os:putenv( "ENVIRON", "fudder" ),
    ?assertEqual( <<"fudder">>, erllambda:environ() ),
    %% reset env to verify original value cached
    os:putenv( "ENVIRON", "budder" ),
    ?assertEqual( <<"fudder">>, erllambda:environ() ).

test_environ_config( _Config ) ->
    Environ = <<"jaba-wat">>,
    ok = application:set_env( erllambda, environ, Environ ),
    ?assertEqual( Environ, erllambda:environ() ),
    %% reset config to verify original value cached
    ok = application:set_env( erllambda, environ, <<"faba-wat">> ),
    ?assertEqual( Environ, erllambda:environ() ).

test_environ_file( _Config ) ->
    ok = meck:new( file, [unstick, passthrough] ),
    ok = meck:expect( file, read_file, fun mock_file_read/1 ),
    ?assertEqual( <<"foo-environ">>, erllambda:environ() ),
    meck:unload( file ).


%%%=============================================================================
%% Internal Functions
%%%=============================================================================
mock_group( Group, Config ) ->
    Modules = mock_group(Group),
    [{mocked_modules, Modules} | Config].

mock_group( region ) ->
    ok = meck:new( lhttpc, [passthrough, no_link] ),
    ok = meck:expect( lhttpc, request, fun mock_lhttpc_request/6 ),
    [lhttpc];
mock_group( accountid ) ->
    ok = meck:new( lhttpc, [passthrough, no_link] ),
    ok = meck:expect( lhttpc, request, fun mock_lhttpc_request/6 ),
    ok = meck:new( erlcloud_sts, [passthrough, no_link] ),
    GetCallerIdentityResult =
        [{account, "912345678901"},
         {userId, "AROAIARCB7OKPBVOAAAAA:AWS-CLI-session-1489174314"},
         {arn, "arn:aws:sts::123456789012:assumed-role/lalalala/AWS-CLI-session-1489174314"}],
    meck:expect( erlcloud_sts, get_caller_identity,
                 fun(_Config) -> {ok, GetCallerIdentityResult} end ),
    [lhttpc, erlcloud_sts];
mock_group( environ ) ->
    [].

unmock_group( _Group, Config ) ->
    Modules = proplists:get_value( mocked_modules, Config ),
    meck:unload(Modules),
    Config.


mock_lhttpc_request( Uri, Method, Headers, Body, Timeout, Options ) ->
    {ok, Test} = application:get_env( erllambda, '_test' ),
    mock_lhttpc_request( Test, Uri, Method, Headers, Body, Timeout, Options ).

mock_lhttpc_request(
  Test, "http://169.254.169.254/latest/dynamic/instance-identity/document",
  get, _, <<>>, _Timeout, _Options )
  when Test =:= test_region_instancedoc; Test =:= test_accountid_instancedoc ->
    Body = jiffy:encode( #{devpayProductCodes => null,
                         availabilityZone => <<"us-east-1d">>,
                         privateIp => <<"10.158.112.84">>,
                         version => <<"2010-08-31">>,
                         region => <<"us-fareast-1">>,
                         instanceId => <<"i-1234567890abcdef0">>,
                         billingProducts => null,
                         instanceType => <<"t1.micro">>,
                         accountId => <<"123456789012">>,
                         pendingTime => <<"2015-11-19T16:32:11Z">>,
                         imageId => <<"ami-5fb8c835">>,
                         kernelId => <<"aki-919dcaf8">>,
                         ramdiskId => null,
                         architecture => <<"x86_64">>
                        } ),
    RespHeaders = [{"content-type", "application/json"},
                   {"content-length", integer_to_list( byte_size(Body) )}],
    {ok, {{200, "OK"}, RespHeaders, Body}};
mock_lhttpc_request(
  test_accountid_sts,
  "http://169.254.169.254/latest/dynamic/instance-identity/document",
  get, _, <<>>, _Timeout, _Options ) ->
    {error, econnrefused};
mock_lhttpc_request( _Test, Uri, Method, Headers, Body, Timeout, Options ) ->
    meck:passthrough( [Uri, Method, Headers, Body, Timeout, Options] ).


mock_file_read( "/var/alertlogic/data/base-stack-name" ) ->
    {ok, <<"foo-environ\n">>};
mock_file_read( Filename ) ->
    ?debugHere,
    meck:passthrough( [Filename] ).


clear( region ) ->
    true = os:unsetenv( "REGION" ),
    ok = application:unset_env( erllambda, region ),
    true = os:unsetenv( "AWS_REGION" ),
    ok;
clear( accountid ) ->
    ok = application:unset_env( erllambda, accountid );
clear( environ ) ->
    true = os:unsetenv( "ENVIRON" ),
    ok = application:unset_env( erllambda, environ ),
    ok.
