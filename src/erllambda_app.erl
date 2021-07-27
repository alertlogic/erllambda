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
    case application:get_env(erllambda, enable_error_handler) of
        {ok, true} ->
            error_logger:tty(false),
            error_logger:add_report_handler(erllambda_error_handler);
        _ ->
            ok
    end,
    Sup.

%%--------------------------------------------------------------------
stop(_State) ->
    case application:get_env(erllambda, enable_error_handler) of
        {ok, true} ->
            error_logger:delete_report_handler(erllambda_error_handler);
        _ ->
            ok
    end,
    ok.
