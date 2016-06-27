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
            Options = [{ip, {127,0,0,1}}, {port, 7181}],
            {ok, _} = cowboy:start_http( http, 1, Options,
                                         [{env, [{dispatch, Dispatch}]}] ),
            erllambda_sup:start_link()
    catch
        Type:Reason ->
            throw( {route_compile_failed, {Type, Reason}, Routes} )
    end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
