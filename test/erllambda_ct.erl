%%%-----------------------------------------------------------------------------
%% @doc Utilities for erllambda common tests
%%
%%
%% @copyright 2017 Alertlogic, Inc.
%%%-----------------------------------------------------------------------------
-module(erllambda_ct).
-author('Paul Fisher <pfisher@alertlogic.com>').

%% test functions called for this suite
-export([setup_service/1, cleanup_service/1, verify/1, process/3]).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%=============================================================================
%% Public Functions
%%%=============================================================================
setup_service( Config ) ->
    Setups = [erllambda, eee_ct],
    NewConfig = lists:foldl( fun setup/2, Config, Setups ),
    {ok, _} = application:ensure_all_started( erllambda ),
    [{erllambda_cleanups, Setups} | NewConfig].

cleanup_service( Config ) ->
    Cleanups = proplists:get_value( erllambda_cleanups, Config ),
    lists:foldl( fun cleanup/2, Config, Cleanups ).
    

setup( erllambda, Config ) ->
    Transport = proplists:get_value( transport, Config ),
    os:putenv( "AWS_REGION", "us-west-2" ),
    os:putenv( "ERLLAMBDA_LISTEN_PROTOCOLS", atom_to_list(Transport) ),
    ok = application:load( erllambda ),
    handler_config( [{transport, Transport} | Config] );
setup( eee_ct, Config ) ->
    Handler = list_to_atom( proplists:get_value( handler, Config ) ),
    Transport = proplists:get_value( transport, Config ),
    EeeConfig = eee_ct:setup( Handler, [{transport, Transport}] ),
    [{eee_ct, EeeConfig} | Config].


cleanup( erllambda, Config ) ->
    application:stop( erllambda ),
    application:unload( erllambda ),
    Config;
cleanup( eee_ct, Config ) ->
    eee_ct:cleanup( proplists:get_value( eee_ct, Config ) ),
    Config.


verify( Config ) ->
    eee_ct:verify( proplists:get_value( eee_ct, Config ) ).

process( Event, Context, Config ) ->
    eee_ct:process( Event, Context, proplists:get_value( eee_ct, Config ) ).
    

%%%=============================================================================
%% Internal Functions
%%%=============================================================================
handler_config( Config ) ->
    Module = list_to_atom(
               proplists:get_value( handler, Config, "erllambda_test" ) ),
    {module, Module} = code:load_file( Module ),
    case proplists:is_defined( handler, Config ) of
        true -> Config;
        false -> [{handler, "erllambda_test"} | Config]
    end.
