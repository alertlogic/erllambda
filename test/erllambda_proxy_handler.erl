-module(erllambda_proxy_handler).

%% erllambda callback
-export([handle/2]).

%% API
-export([delegate_to/1]).

%%%===================================================================
%%% API
%%%===================================================================

handle(Event, Context) ->
    {ok, Module} = application:get_env(erllambda, test_proxy_handler),
    Module:handle(Event, Context).

delegate_to(Module) ->
    application:set_env(erllambda, test_proxy_handler, Module).

%%%===================================================================
%%% Private functions
%%%===================================================================
