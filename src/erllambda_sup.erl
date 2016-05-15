%%%---------------------------------------------------------------------------
%% @doc erllambda_sup - Erllambda Application supervisor
%%
%% This module implements the Erlang <code>supervisor</code> behavior, which
%% exists, but starts no server processes.
%%
%%
%% @copyright 2016 Alert Logic, Inc
%% @author Paul Fisher <pfisher@alertlogic.com>
%%%---------------------------------------------------------------------------
-module(erllambda_sup).
-author('Paul Fisher <pfisher@alertlogic.com>').

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
    {ok, {{one_for_one, 10, 10}, []} }.


%%====================================================================
%% Internal functions
%%====================================================================
