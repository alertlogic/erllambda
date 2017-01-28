%%%---------------------------------------------------------------------------
%% @doc erllambda_app - Erllambda Application behavior
%%
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2016 Alert Logic, Inc
%% @author Paul Fisher <pfisher@alertlogic.com>
%%%---------------------------------------------------------------------------
-module(erllambda_app).
-author('Paul Fisher <pfisher@alertlogic.com>').

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Routes = [
              {"/erllambda/[...]", erllambda_http, []}
             ],
    try cowboy_router:compile( [{'_', Routes}] ) of
        Dispatch ->
            Options = cowboy_options(),
            {ok, _} = cowboy:start_clear( http, 1, Options,
                                          #{env => #{dispatch => Dispatch}} ),
            erllambda_sup:start_link()
    catch
        Type:Reason ->
            throw( {route_compile_failed, {Type, Reason}, Routes} )
    end.


%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener( http ),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
cowboy_options() ->
    %% when we run in an ECS cluster this environment variable is specifed
    %% as "tcp", and if run as an AWS lambda function, then we default to
    %% "unix" to use unix domain sockets
    cowboy_options( os:getenv( "ERLLAMBDA_TRANSPORT", "unix" ) ).

cowboy_options( "unix" ) ->
    %% socket file configured needs to be cleared out if we are to
    %% start and the directory needs to exist
    {ok, File} = application:get_env( erllambda, socket_file ),
    _ = file:delete( File ),
    ok = filelib:ensure_dir( File ),
    %% the following sequence is neccessary because cowboy does not
    %% permite the ifaddr option to pass through to ranch so that it
    %% can establish the unix domain socket itself
    %%
    %% Options = [{ifaddr, {local, File}}],
    TcpOptions = [binary, {nodelay, true}, {backlog, 1024},
                  {send_timeout, 30000}, {send_timeout_close, true},
                  {active, false}, {packet, raw},
                  {reuseaddr, true}, {ifaddr, {local, File}}],
    {ok, Socket} = gen_tcp:listen( 0, TcpOptions ),
    [{socket, Socket}];
cowboy_options( "tcp" ) ->
    {ok, Port} = application:get_env( erllambda, tcp_port ),
    [{port, Port}].
