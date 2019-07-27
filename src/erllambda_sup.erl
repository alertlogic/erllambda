%%%---------------------------------------------------------------------------
%% @doc erllambda_sup - Erllambda Application supervisor
%%
%% This module implements the Erlang <code>supervisor</code> behavior, which
%% exists, but starts no server processes.
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Children = [
        erllambda_poller:spec(),
        server_spec( erllambda_config_srv, [] )
    ],
    % in AWS Lambda environment it's better to die fast
    {ok, {{one_for_one, 1, 5}, Children}}.

server_spec( Module, Args ) ->
    #{id => Module,
        start => {Module, start_link, Args},
        restart => permanent, shutdown => (15 * 1000), type => worker,
        modules => [Module]
    }.

%%====================================================================
%% Internal functions
%%====================================================================
