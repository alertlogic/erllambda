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
     {tcp, [], tests()},
     {unix, [], tests()}
    ].

tests() ->
    [
     test_verify,
     test_process,
     test_success,
     test_success_ok,
     test_fail,
     test_fail_error,
     test_except_error_undef
    ].

init_per_suite( Config ) ->
    [{handler, "erllambda_test"} | Config].

end_per_suite( Config ) -> Config.
    

init_per_group( Transport, Config ) ->
    erllambda_ct:setup_service( [{transport, Transport}, Config] ).
     
end_per_group( _Transport, Config ) ->
    erllambda_ct:cleanup_service( Config ).


%%******************************************************************************
%% Test functions
%%******************************************************************************
test_verify( Config ) ->
    ?assertMatch( {ok, undefined}, erllambda_ct:verify( Config ) ).

test_process( Config ) ->
    Context = #{<<"AWS_ACCESS_KEY_ID">> => "ID",
                <<"AWS_SECRET_ACCESS_KEY">> => "KEY",
                <<"AWS_SECURITY_TOKEN">> => "TOKEN",
                <<"AWS_CREDENTIAL_EXPIRE_TIME">> => 123456},
    ?assertMatch( {ok, #{<<"success">> := _}},
                  erllambda_ct:process( #{result => ok}, Context, Config ) ).

test_success( Config ) ->
    Message = <<"wild success">>,
    RespMessage = <<"erllambda_test: ", Message/binary>>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {ok, #{<<"success">> := RespMessage}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_success_ok( Config ) ->
    Message = <<"wild success">>,
    RespMessage = <<"erllambda_test: ", Message/binary>>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {ok, #{<<"success">> := RespMessage}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_fail( Config ) ->
    Message = <<"failure message">>,
    RespMessage = <<"erllambda_test: ", Message/binary>>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {error, {response, #{<<"error">> := RespMessage}}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_fail_error( Config ) ->
    Message = <<"failure message">>,
    RespMessage = <<"erllambda_test: ", Message/binary>>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {error, {response, #{<<"error">> := RespMessage}}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_except_error_undef( Config ) ->
    Event = #{test => ?FUNCTION_NAME},
    ?assertMatch(
       {error,
        {response,
         #{<<"error">> :=
               <<"erllambda_test: terminated with exception {error,undef}",
                 _Rest/binary>>}}},
                  erllambda_ct:process( Event, #{}, Config ) ).


%%******************************************************************************
%% Internal Functions
%%******************************************************************************

