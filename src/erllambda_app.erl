%%%---------------------------------------------------------------------------
%% @doc erllambda_app - Erllambda Application behavior
%%
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2018 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(erllambda_app).
-author('Paul Fisher <pfisher@alertlogic.com>').
-author('Evgeny Bob <ebob@alertlogic.com>').

-behavior(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    error_logger:tty(false),
    error_logger:add_report_handler(erllambda_error_handler),
    EnvWihtoutSecret = erllambda_poller:hide_secret(erllambda_poller:os_env2map()),
    erllambda:message("Erllambda Starting at ~p with Env ~p",
        [os:system_time(millisecond), EnvWihtoutSecret]),
    erllambda_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
    
