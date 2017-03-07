%%%-----------------------------------------------------------------------------
%% @doc Utilities for erllambda common tests
%%
%%
%% @copyright 2017 Alertlogic, Inc.
%%%-----------------------------------------------------------------------------
-module(erllambda_ct).
-author('Paul Fisher <pfisher@alertlogic.com>').

%% test functions called for this suite
-export([setup_service/1, cleanup_service/1]).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%%=============================================================================
%% Public Functions
%%%=============================================================================
setup_service( Config ) ->
    NewConfig = lists:foldl( fun setup/2, Config, [erllambda, httpc] ),
    {ok, _} = application:ensure_all_started( erllambda ),
    NewConfig.

cleanup_service( Config ) ->
    lists:foldl( fun cleanup/2, Config, [httpc, erllambda] ).
    

setup( erllambda, Config ) ->
    Transport = proplists:get_value( transport, Config ),
    os:putenv( "ERLLAMBDA_LISTEN_PROTOCOLS", atom_to_list(Transport) ),
    ok = application:load( erllambda ),
    transport_config( Transport,
                      handler_config( Config ) );
setup( httpc, Config ) ->
    {ok, _} = application:ensure_all_started( hackney ),
    hackney_trace:enable( max, io ),
    Config.


cleanup( httpc, Config ) ->
    hackney_trace:disable(),
    application:stop( hackney ),
    application:unload( hackney ),
    Config;
cleanup( erllambda, Config ) ->
    application:stop( erllambda ),
    application:unload( erllambda ),
    Config.


verify( Config ) ->
    Uri = uri( Config ),
    Headers = headers(),
    case hackney:request( get, Uri, Headers, <<>>, [] ) of
        {ok, Status, RespHeaders, Ref} ->
            {ok, Body} = hackney:body( Ref ),
            DecodeBody = decode_body( Body ),
            {ok, Status, RespHeaders, DecodeBody};
        Otherwise -> Otherwise
    end.

invoke( Event, Context, Config ) ->
    Uri = uri( Config ),
    Headers = headers(),
    Body = jiffy:encode( #{event => Event, context => Context} ),
    case hackney:request( post, Uri, Headers, Body, [] ) of
        {ok, Status, RespHeaders, Ref} ->
            {ok, RespBody} = hackney:body( Ref ),
            DecodeRespBody = decode_body( RespBody ),
            {ok, Status, RespHeaders, DecodeRespBody};
        Otherwise -> Otherwise
    end.
    

%%%=============================================================================
%% Internal Functions
%%%=============================================================================
transport_config( tcp, Config ) ->
    Port = 9999,
    application:set_env( erllambda, tcp_port, Port ),
    [{port, Port} | Config];
transport_config( unix, Config ) ->
    {ok, SocketFile} = application:get_env( erllambda, socket_file ),
    [{socket_file, SocketFile} | Config].


handler_config( Config ) ->
    Module = list_to_atom(
               proplists:get_value( handler, Config, "erllambda_test" ) ),
    {module, Module} = code:load_file( Module ),
    case proplists:is_defined( handler, Config ) of
        true -> Config;
        false -> [{handler, "erllambda_test"} | Config]
    end.


uri( Config ) ->
    Module = proplists:get_value( handler, Config ),
    uri( ["eee/v1", $/, Module], Config ).
    

uri( Resource, Config ) ->
    case proplists:get_value( transport, Config ) of
        tcp ->
            Port = proplists:get_value( port, Config ),
            Prefix = ["http://localhost:", integer_to_list(Port), "/"];
        unix ->
            SocketFile = hackney_url:urlencode(
                           proplists:get_value( socket_file, Config ) ),
            Prefix = ["http+unix://", SocketFile, "/"]
    end,
    iolist_to_binary( [Prefix, Resource] ).


headers() ->
    %% the host header is overridden here because hackney gets it wrong
    %% without this PR:
    %%
    %%   https://github.com/benoitc/hackney/pull/383
    %%
    %% in addition cowboy gets this wrong not handling the url encoding of
    %% the host header value, as reported in:
    %%
    %%   https://github.com/ninenines/cowlib/issues/53
    %%
    %% so... we put the override in for now to have these tests work 
    [{<<"Host">>, <<"dummy">>},{<<"accept">>, <<"application/json">>}].
    
decode_body( <<>> ) -> undefined;
decode_body( Body ) -> 
    try
        jiffy:decode( Body, [return_maps] )
    catch
        error:badarg -> Body
    end.
