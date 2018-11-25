%%------------------------------------------------------------------------------
%% @doc erllambda_util
%%
%% Utilities for the Erllambda Environment
%%
%% @copyright 2018 Alert Logic, Inc.
%%------------------------------------------------------------------------------
-module(erllambda_util).

-export([region/0, environ/0, accountid/0, config/0, config/1, config/2]).

-export([to_list/1, to_binary/1]).
%%******************************************************************************
%% Includes
%%******************************************************************************
-include("erllambda.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%******************************************************************************
%% API Functions
%%******************************************************************************
%%------------------------------------------------------------------------------
-spec region() -> region() | undefined.
%%------------------------------------------------------------------------------
%% @doc Access current AWS region
%%
%% This function will resolve the region in which we are currently executing
%% for all IWS execution environments, including AWS EC2 instance, IWS ECS
%% Containers, AWS Lambda Functions, and Jenkins/development tests.  The
%% region value is located in the following locations, in order:
%%
%% <ul>
%%   <li>Environment Variable <code>REGION</code>
%%     <p>The <code>makeincl<code> development tools set the
%%     <code>REGION</code> environment variable when executing eunit or
%%     common_tests, to allow local workstation execution to use AWS
%%     resources during testing.</p>
%%   </li>
%%   <li>Application Config <code>erllambda.region</code>
%%     <p>In cases where it is necessary to override natural region
%%     detection from within an AWS execution environment, the region can be
%%     set as part of a deployment application config. While rarely needed,
%%     this option is available for when it is.</p>
%%   </li>
%%   <li>Environment Variable <code>AWS_REGION</code>
%%     <p>For both AWS Lambda as well as IWS ECS Container based services,
%%     the environment variable <code>AWS_REGION</code> is set to indicate
%%     the region of execution.</p>
%%   </li>
%%   <li>AWS EC2 Metadata - Instance Document
%%     <p>For both AWS EC2 instance the instance metadata is accessed to
%%     extract the region of exection from the dynamic instance
%%     document.</p>
%%   </li>
%% </ul>
%%
region() ->
    case erllambda_config_srv:get( region ) of
        {ok, Result} -> Result;
        {error, not_found} ->
            Generate = fun() -> region_() end,
            erllambda_config_srv:serialize( region, Generate )
    end.

region_() ->
    Functions = [fun region_devenv/0, fun region_config/0,
                 fun region_env/0, fun region_meta/0, fun region_cli/0],
    Region = lists:foldl( fun( Function, undefined ) -> Function();
                             ( _, Result ) -> Result
                          end, undefined, Functions ),
    erllambda_config_srv:set( region, Region, undefined ),
    Region.

region_devenv() ->
    case os:getenv( "REGION" ) of
        false -> undefined;
        Region -> list_to_binary(Region)
    end.

region_config() ->
    case application:get_env( erllambda, region ) of
        {ok, undefined} -> undefined;
        {ok, Region} when is_binary(Region) -> Region;
        {ok, Region} when is_list(Region) -> list_to_binary(Region)
    end.

region_env() ->
    case os:getenv( "AWS_REGION" ) of
        false -> undefined;
        Region -> list_to_binary(Region)
    end.

region_meta() ->
    case instance_doc() of
        {ok, #{<<"region">> := Region}} -> Region;
        Otherwise -> Otherwise
    end.
            
region_cli() ->
    case string:tokens( os:cmd( "aws configure get region" ), "\n" ) of
        [Region] -> list_to_binary(Region);
        _ -> undefined
    end.
            

%%------------------------------------------------------------------------------
-spec accountid() -> accountid() | undefined.
%%------------------------------------------------------------------------------
%% @doc Access current AWS accountid
%%
%% This function will resolve the AWS AccountId in which we are currently
%% executing for all IWS execution environments, including AWS EC2 instance,
%% IWS ECS Containers, AWS Lambda Functions, and Jenkins/development tests.
%% The accountid value is located in the following locations, in order:
%%
%% <ul>
%%   <li>Application Config <code>erllambda.accountid</code>
%%     <p>In cases where it is necessary to override natural accountid
%%     detection from within an AWS execution environment, the accountid can be
%%     set as part of a deployment application config. While rarely needed,
%%     this option is available for when it is.</p>
%%   </li>
%%   <li>AWS EC2 Metadata - Instance Document
%%     <p>For both AWS EC2 instance the instance metadata is accessed to
%%     extract the accountid of exection from the dynamic instance
%%     document.</p>
%%   </li>
%%   <li>AWS IAM User
%%     <p>As a last resort, the AWS Account Id is extracted from the current
%%     IAM user Arn for the credentials under which we are executing.</p>
%%   </li>
%% </ul>
%%
accountid() ->
    case erllambda_config_srv:get( accountid ) of
        {ok, Result} -> Result;
        {error, not_found} ->
            Generate = fun() -> accountid_() end,
            erllambda_config_srv:serialize( accountid, Generate )
    end.

accountid_() ->
    Functions = [fun accountid_config/0, fun accountid_meta/0,
                 fun accountid_sts/0],
    AccountId = lists:foldl( fun( Function, undefined ) -> Function();
                                ( _, Result ) -> Result
                             end, undefined, Functions ),
    erllambda_config_srv:set( accountid, AccountId, undefined ),
    AccountId.

accountid_config() ->
    case application:get_env( erllambda, accountid ) of
        {ok, undefined} -> undefined;
        {ok, Accountid} when is_binary(Accountid) -> Accountid;
        {ok, Accountid} when is_list(Accountid) -> list_to_binary(Accountid)
    end.

accountid_meta() ->
    case instance_doc() of
        {ok, #{<<"accountId">> := Accountid}} -> Accountid;
        Otherwise -> Otherwise
    end.
            
accountid_sts() -> accountid_sts( config([<<"sts">>]) ).

accountid_sts( undefined ) -> undefined;
accountid_sts( Config ) -> 
    case erlcloud_sts:get_caller_identity( Config ) of
        {ok, Proplists} ->
            to_binary( proplists:get_value( account, Proplists ) );
        _ -> undefined
    end.


%%------------------------------------------------------------------------------
-spec environ() -> environ() | undefined.
%%------------------------------------------------------------------------------
%% @doc Create a erllambba context.
%%
environ() ->
    case erllambda_config_srv:get( environ ) of
        {ok, Result} -> Result;
        {error, not_found} ->
            Generate = fun() -> environ_() end,
            erllambda_config_srv:serialize( environ, Generate )
    end.

environ_() ->
    Functions = [fun environ_env/0, fun environ_config/0, fun environ_file/0],
    Environ = lists:foldl( fun( Function, undefined ) -> Function();
                              ( _, Result ) -> Result
                           end, undefined, Functions ),
    erllambda_config_srv:set( environ, Environ, undefined ),
    Environ.

environ_env() ->
    case os:getenv( "ENVIRON" ) of
        false -> undefined;
        Environ -> list_to_binary(Environ)
    end.

environ_file() ->
    case file:read_file( "/var/alertlogic/data/base-stack-name" ) of
        {ok, Bin} ->
            [Environ|_] = binary:split(Bin, <<"\n">>, [trim]),
            Environ;
        {error, _} -> undefined
    end.

environ_config() ->
    case application:get_env( erllambda, environ ) of
        {ok, undefined} -> undefined;
        {ok, Environ} when is_binary(Environ) -> Environ;
        {ok, Environ} when is_list(Environ) -> list_to_binary(Environ)
    end.


%%------------------------------------------------------------------------------
-spec config() -> #aws_config{}.
%%------------------------------------------------------------------------------
%% @doc Generate and cache an erlcloud configuration
%%
%% This function will generate and cache an {@link
%% erlcloud_aws:config(). erlcloud <code>#aws_config{}</code> record} for
%% the current AWS execution by calling {@link region/0}.
%%
%% @see config/2
%%
config() -> config( region(), [] ).

    
%%------------------------------------------------------------------------------
-spec config( Services :: [service()] ) -> #aws_config{}.
%%------------------------------------------------------------------------------
%% @doc Generate and cache an erlcloud configuration
%%
%% This function will generate and cache an {@link
%% erlcloud_aws:config(). erlcloud <code>#aws_config{}</code> record} for
%% the current AWS execution by calling {@link region/0}, and will setup the
%% regional endpoints for the list of services provided in the
%% <code>Services</code> parameter.
%%
%% @see config/2
%%
config( Services ) -> config( region(), [{services, Services}] ).

    
%%------------------------------------------------------------------------------
-spec config( Region :: region(), Options :: [option()] ) -> #aws_config{}.
%%------------------------------------------------------------------------------
%% @doc Generate and cache an erlcloud configuration
%%
%% This function will generate and cache an {@link
%% erlcloud_aws:config(). erlcloud <code>#aws_config{}</code> record} for
%% the AWS <code>Region</code> specified. This value will be cached and
%% automatically refreshed in the background, so it is safe to call this
%% whenever a config is needed.
%%
%% Whether or not the config ultimately assumes an alternative role or not,
%% the base credential information is sourced from
%% <code>erlcloud_aws:auto_config/1</code>.  These base credentials are then
%% used directly, or as the basis for the assumed role.  This function will
%% conditionally source the profile name used from the environment variable
%% <code>PROFILE</code>, and if not present, will request the
%% <code>default</code> profile.
%%
%% The <code>Options</code> parameter is a property list containing tuple
%% pairs that can be used to influence the construction of the {@link
%% erlcloud_aws:config(). erlcloud <code>#aws_config{}</code> record}. The
%% possible properties are as follows:
%%
%% <ul>
%%  <li><code>'iam_role'</code>
%%    <p>The name of an IAM Role that should be assumed when generating the
%%    new config.</p>
%%  </li>
%%  <li><code>'iam_extid'</code>
%%    <p>An optional IAM External Id value that should be used when assuming
%%    an alternative role, specified using <code>'iam_role'</code>.</p>
%%  </li>
%%  <li><code>'services'</code>
%%    <p>The list of AWS services to configure to access the specified
%%    <code>Region</code>.  This service names are ultimately passed to the
%%    {@link erlcloud_aws:service_config/3} function to setup correct
%%    regional endpoints.</p>
%%  </li>
%%  <li><code>'role_session_name'</code>
%%    <p>The name that should be used
%%    for the <code>RoleSessionName</code> parameter.  If this option is not
%%    specified, then it will default to "erllambda"</p>
%%  </li>
%%  <li><code>'role_duration_secs'</code>
%%    <p>The number of seconds that
%%    should be used for the <code>DurationSeconds</code> parameter.  If
%%    this option is not specified, then it will default to
%%    <code>`default_role_duration`</code> application parameter (3600 sec).
%%    Default is configurable in </p>
%%  </li>
%% </ul>
%%
config( Region, Options ) ->
    Key = erlang:phash2( {Region, Options} ),
    case erllambda_config_srv:get( Key ) of
        {ok, #aws_config{expiration = Expiration} = Result} 
                when is_integer(Expiration) ->
            GraceExp = Expiration - ?AWSEVICT_SECS,
            case GraceExp > os:system_time(seconds) of
                true -> Result;
                false ->
                    erllambda_config_srv:evict(Key),
                    config(Region, Options)
            end;
        % unsure if config is expired
        {ok, Result} -> Result;
        {error, not_found} ->
            Generate = config_generate( Key, Region, Options ),
            erllambda_config_srv:serialize( Region, Generate );
        Otherwise -> Otherwise
    end.

config_generate( Key, Region, Options ) ->
    fun() ->
            Refresh = config_refresh( Key, Region, Options ),
            config_generate( Key, Region, Options, Refresh )
    end.

config_refresh( Key, Region, Options ) ->
    fun Refresh() ->
            config_generate( Key, Region, Options, Refresh )
    end.

config_generate( Key, Region, Options, Refresh ) ->
    Profile = list_to_atom( os:getenv( "PROFILE", "default" ) ),
    {ok, Config} = case application:get_env( erllambda, config ) of
                       {ok, undefined} ->
                           erlcloud_aws:auto_config( [{profile, Profile}] );
                       {ok, _} = Predefined -> Predefined
                   end,
    config_finalize( Key, Region, Config, Options, Refresh ).

config_finalize( Key, Region, Config, Options, Refresh ) ->
    case {proplists:get_value( iam_role, Options ),
          proplists:get_value( iam_extid, Options )} of
        {undefined, undefined} ->
            config_finalize_( Key, Region, Config, Options, Refresh );
        {undefined, _ExtId} -> {error, iam_role_required};
        {Role, ExtId} ->
            config_assume( Role, ExtId, Key, Region, Config, Options, Refresh )
    end.

config_assume( Role, ExtId, Key, Region, Config, Options, Refresh ) ->
    Name = proplists:get_value( role_session_name, Options, "erllambda" ),
    {ok, DefaultDur} = application:get_env(erllambda, default_role_duration_sec),
    Secs = proplists:get_value( role_duration_secs, Options, DefaultDur),
    StsConfig = erlcloud_aws:service_config(<<"sts">>, Region, Config),
    try erlcloud_sts:assume_role(
          StsConfig, to_list(Role), Name, Secs, to_list(ExtId) ) of
        {NewConfig, _Creds} ->
            config_finalize_( Key, Region, NewConfig, Options, Refresh )
    catch
        error:{aws_error, Reason} -> {error, Reason}
    end.

config_finalize_( Key, Region, Config, Options, Refresh ) ->
    Services = proplists:get_value( services, Options, [] ),
    NewConfig = config_regionalize_(Config, Region, Services),
    ExpireTsSec = context_config_expire( Config ),
    RefreshTsMs = (ExpireTsSec * 1000) - ?AWSEVICT_MSECS,
    erllambda_config_srv:set( Key, NewConfig, ExpireTsSec ),
    erllambda_config_srv:schedule( RefreshTsMs, Refresh ),
    NewConfig.

config_regionalize_(Config, Region, []) ->
    erlcloud_aws:default_config_region(Config, Region);
config_regionalize_(Config, Region, Services) ->
    lists:foldl( fun( Service, Cfg ) ->
                    erlcloud_aws:service_config( Service, Region, Cfg )
                 end,
                 Config, Services ).

context_config_expire( #aws_config{expiration = undefined} ) ->
    {ok, DefaultDur} = application:get_env(erllambda, default_role_duration_sec),
    erlang:system_time( second ) + DefaultDur;
context_config_expire( #aws_config{expiration = ExpirationTs} ) ->
    ExpirationTs.


%%******************************************************************************
%% Internal Functions
%%******************************************************************************
instance_doc() ->
    case erllambda_config_srv:get( instance_doc ) of
        {ok, Result} -> Result;
        {error, not_found} ->
            Generate = instance_doc_generate(),
            erllambda_config_srv:serialize( instance_doc, Generate )
    end.

instance_doc_generate() ->
    fun() ->
            case erllambda_config_srv:get( instance_doc ) of
                {ok, Result} -> Result;
                {error, not_found} ->
                    instance_doc_()
            end
    end.

instance_doc_refresh() ->
    fun() -> instance_doc_() end.

instance_doc_() ->
    Url = "http://169.254.169.254/latest/dynamic/instance-identity/document",
    Headers = [{"accept", "*/*"}],
    case lhttpc:request( Url, get, Headers, <<>>, (10 * 1000), [] ) of
        {ok, {{200, _}, _RespHeaders, Body}} -> instance_doc( Body );
        _Otherwise -> undefined
    end.

instance_doc( Body ) ->
    try jiffy:decode( Body, [return_maps] ) of
        Json ->
            Result = {ok, Json},
            ExpireTsMsec = erlang:system_time( millisecond ) + (900 * 1000),
            RefreshTsMsec = ExpireTsMsec - ?AWSEVICT_MSECS,
            erllambda_config_srv:set( instance_doc, Result, ExpireTsMsec ),
            erllambda_config_srv:schedule( RefreshTsMsec, instance_doc_refresh() ),
            Result
    catch
        _:_ -> undefined
    end.

to_list( V ) when is_list(V) -> V;
to_list( V ) when is_binary(V) -> binary_to_list(V);
to_list( V ) when is_atom(V) -> atom_to_list(V);
to_list( V ) when is_integer(V) -> integer_to_list(V);
to_list( V ) when is_float(V) -> float_to_list(V);
to_list( V ) -> V.

to_binary(T) when is_pid(T) ->
    iolist_to_binary(pid_to_list(T));
to_binary(T) when is_binary(T) ->
    T;
to_binary(T) when is_list(T) ->
    list_to_binary(T);
to_binary(T) when is_atom(T) ->
    atom_to_binary(T, latin1).
%%******************************************************************************
%% Test functions
%%******************************************************************************
-ifdef(TEST).

-endif.
