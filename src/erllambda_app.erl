%%%---------------------------------------------------------------------------
%% @doc erllambda_app - Erllambda Application behavior
%%
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda_app).

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Sup = erllambda_sup:start_link(),
    Sup.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
