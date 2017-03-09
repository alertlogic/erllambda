erllambda How-To
================

This document is intended to provide a list of short recipes that can be
used to accomplish specific tasks when implementing AWS Lambda functions in
Erlang using the `erllambda` framework.

## General

- [Basic erllcloud Configs](#basic-erlcloud-configs)
- [Assuming Alternative Roles](#assuming-alternative-roles)


## Development

- [Testing via Erlang Shell](#testing-via-erlang-shell)
- [Testing via Common Tests](#testing-via-common-tests)


# General Topics

This section convers topics that are generally applicable to all `erllambda`
implementations.


## Basic erlcloud Configs

The `erllambda` framework ensures that the AWS credentials using
the Lambda execution role are available for use with the
[`erlcloud` AWS SDK](https://github.com/erlcloud/erlcloud).

The basic config is available directly via the `erllambda:config/0` and
shown here being used to check if a DynamoDB table is accessible and exists
in the default region in which the lambda is executing:

```
ddb_table_exists( Table ) ->
    Region = erllambda:region(),
    Config = erllambda:config(),
    DDBConfig = erlcloud_aws:service_config( ddb, Region, Config ),
    case erlcloud_ddb2:describe_table( Table, [], DDBConfig ) of
        {ok, _Description} -> ok;
        {error, {<<"ResourceNotFoundException">>, _}} -> undefined;
        Error -> Error
    end;
```

`erlcloud` configs generated via this interface are cached and refreshed
when on any change to underlying credentials.  Therefore `erllambda`
implementations should not simply retrieve the config every time it is
needed, and not try to cache it.

**NOTE**: This method of accessing the credentials is the only method
guaranteed to work in all execution environments (e.g. local development,
AWS Lambda, and
[EEE Functions/Containers](https://algithub.pd.alertlogic.net/alertlogic/eee).


## Assuming Alternative Roles

If the Lambda implementation needs to assume an IAM Role, other than the
execution role configured for the Lambda function, this should be
accomplished by using the `iwsutil:config/1,2` functions.

```
ddb_with_role( Role, ExternalId ) ->
    Region = erllambda:region(),
    Options = [{iam_role, Role}, {iam_extid, ExternalId}, {services, [ddb]}],
    Config = iwsutil:config( Region, Options ),
    ...
```

Like basic `erlcloud` configs, assumed roles based configs are cached, so
`erllambda` implementations should call this interface when such a config is
needed.


# Development

This section convers topics that address issues encountered during
development of `erllambda` implementations, and in particular topics that
enable such implementations to be validated in the local development
and build environments.

**NOTE**: Many of the topics in this section assume that you have generated
your `erllambda` project using the
[`rebar3_erllambda`](https://algithub.pd.alertlogic.net/alertlogic/rebar3_erllambda)
plugin, or are otherwise working in an environment based on
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl).


## Testing via Erlang Shell

It is very simple to test your `erllambda` implementation, from your local
development environment, using the Erlang shell.  Simple run:

```
make shell
```

and the `rebar3` node will boot into the shell. Once booted, you can
interact directly in the shell calling the `handle/2` function in the lambda
module directly.  Using the other guidance in this document to access
`erlcloud` configs should allow the code to access AWS resources. All that
is required is for the `[default]` profile in `~/.aws/credentials` to be
capable of operating on the AWS resources accessed from the Lambda
implementation.


## Testing via Common Test

