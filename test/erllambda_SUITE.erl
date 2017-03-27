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
     test_except_error_undef,
     test_unknown_handler,
     test_invalid_request,
     test_invalid_request_no_event,
     test_invalid_request_no_context
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
    Context = #{<<"AWS_ACCESS_KEY_ID">> => <<"ID">>,
                <<"AWS_SECRET_ACCESS_KEY">> => <<"KEY">>,
                <<"AWS_SECURITY_TOKEN">> => <<"TOKEN">>,
                <<"AWS_CREDENTIAL_EXPIRE_TIME">> => 123456},
    ?assertMatch( {ok, #{<<"success">> := _}},
                  erllambda_ct:process( #{result => ok}, Context, Config ) ).

test_success( Config ) ->
    Message = <<"wild success">>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {ok, #{<<"success">> := Message}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_success_ok( Config ) ->
    Message = <<"wild success">>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch( {ok, #{<<"success">> := Message}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_fail( Config ) ->
    Message = <<"failure message">>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"HandlerFailure">>,
                            <<"errorMessage">> := Message}}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_fail_error( Config ) ->
    Message = <<"failure message">>,
    Event = #{test => ?FUNCTION_NAME, message => Message},
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"HandlerFailure">>,
                            <<"errorMessage">> := Message}}},
                  erllambda_ct:process( Event, #{}, Config ) ).

test_except_error_undef( Config ) ->
    Message = <<"terminated with exception {error,undef}">>,
    Event = #{test => ?FUNCTION_NAME},
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"HandlerFailure">>,
                            <<"errorMessage">> := Message}}},
       erllambda_ct:process( Event, #{}, Config ) ).

test_unknown_handler( Config ) ->
    Transport = proplists:get_value( transport, Config ),
    Options = [{skip_module_check, true}, {transport, Transport}],
    EeeConfig = eee_ct:setup( erllambda_wtf, Options ),
    Event = #{test => fubar_function},
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"UnknownHandler">>}}},
       eee_ct:verify( EeeConfig ) ).

test_invalid_request( Config ) ->
    Body = <<"WAT?">>,
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"InvalidRequest">>}}},
       invalid_request( Body, proplists:get_value( eee_ct, Config ) ) ).

test_invalid_request_no_event( Config ) ->
    Body = jiffy:encode( #{context => #{}} ),
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"InvalidRequest">>}}},
       invalid_request( Body, proplists:get_value( eee_ct, Config ) ) ).

test_invalid_request_no_context( Config ) ->
    Body = jiffy:encode( #{event => #{}} ),
    ?assertMatch(
       {error, {response, #{<<"errorType">> := <<"InvalidRequest">>}}},
       invalid_request( Body, proplists:get_value( eee_ct, Config ) ) ).


%%******************************************************************************
%% Internal Functions
%%******************************************************************************
invalid_request( Body, #{transport := {Transport, _}} = Config ) ->
    Uri = eee_ct:uri( [], Config ),
    Headers = eee_ct:headers( Transport ),
    eee_ct:common_result( eee_ct:invoke( Uri, Headers, Body, Config ) ).
