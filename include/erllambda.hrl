%%%---------------------------------------------------------------------------
%% @doc erllambda - AWS Lambda for Erlang Interface
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-author('Evgeny Bob <ebob@alertlogic.com>').

%%******************************************************************************
%% Type Definitions
%%******************************************************************************
-export_type([region/0, environ/0]).

-type region() :: binary().
-type environ() :: binary().
-type accountid() :: binary().
-type service() :: atom().
-type option() :: {services, [service()]} | erlcloud_aws:profile_option().

%%******************************************************************************
%% Macro Definitions
%%******************************************************************************

-define(CACHE_NAME, erllambda_config_cache).
-define(CACHE_GRACE_MSECS, (30 * 1000)).
-define(CACHE_MSECS(EXPIRE_SECS), ((EXPIRE_SECS * 1000) - ?CACHE_GRACE_MSECS)).

-define(DAY_MSECS, 86400000).
-define(AWSEVICT_SECS, (application:get_env(erllambda, default_role_evict_sec, 60))).
-define(AWSEVICT_MSECS, (?AWSEVICT_SECS * 1000)).
