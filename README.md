erllambda
=========

Enable AWS Lambda functions to be written in Erlang


# Overview

The `erllambda` library provides all functionality needed to build and
deploy fully functional [AWS Lambda](https://aws.amazon.com/lambda/)
functions, written entirely in Erlang.

Erlang Lambda functions implement a simple two function behavior:

```
-module(hello_lambda).

-behavior(erllambda).
-export([init/1]).
-export([handle/2]).

-spec init( Context :: map() ) -> ok | none().
init( Context ) ->
    erllambda:succeed( "Hello AWS Runtime!" ).
    
-spec handle( Event :: map(), Context :: map() ) -> {ok, Body} | {error, ErrorBody}.
handle( Event, Context ) ->
    erllambda:succeed( "Hello Event ~p", [Event] ).
```

There are really two ways to get started.  First you can checkout and review
the
[`erllambda_example`](https://github.com/alertlogic/erllambda_example/blob/master/README.md)
project.  This is a complete working AWS Lambda written in Erlang, with the
goal of demonstrating the capabilities available as part of the framework,
and how to write Lambda functions for different purposes in AWS.

The second path is to use the
[`rebar3_erllambda`](https://github.com/alertlogic/rebar3_erllambda/blob/master/README.md)
plugin for `rebar3`.  This will produce a fully working `erllambda` project
that can be used as a starting point for any new development.  This plugin
also implements additional `rebar3 erllambda zip` building and AWS Lambda
function packaging that simplify development.

More detailed information about developing Lambda functions using
`erllambda` can be found in:

- [Writing AWS Lambda Functions in Erlang](doc/tutorial.md)
- [Deploying AWS Lambda Functions in Erlang](doc/deployment.md)
- [`erllambda` How-To](doc/howto.md)


# Ownership

The `erllambda` application and it's supporting libraries are primarily owned by
[motobob](https://github.com/motobob) and [velimir0xff](https://github.com/velimir0xff)  

# Dependencies

The `erllambda` application is built using [`rebar3`](http://www.rebar3.org), 
and all other dependencies are automatically pulled in when `erllambda` is used in other projects
`rebar.config`.

As part of the Erlang/Elixir package for AWS Lambda following are used:
 - [`erllambda`](https://github.com/alertlogic/erllambda) - this repo. 
  Core integration point with AWS Lambda Runtime API
 - [`rebar3_erllambda`](https://github.com/alertlogic/rebar3_erllambda) - 
  `rebar3`/`relx` plugin to build & package your application in Erlang
 - [`erllambda_docker`](https://github.com/alertlogic/erllambda_docker) - docker images.
  Used for packaging your application with proper native libraries set.
 - [`mix_erllambda`](https://github.com/alertlogic/mix_erllambda) - 
  `mix`/`distillery` plugin to build & package your application in Elixir
 - [`erllambda_example`](https://github.com/alertlogic/erllambda_example) - 
  basic Erlang generated example
 - [`erllambda_elixir_example`](https://github.com/alertlogic/erllambda_elixir_example) - 
  basic Elixir generated example


## Initial setup, compilation and testing

*TLDR;* as long as your basic Erlang environment is setup, getting started
developing `erllambda` should be as easy as forking the repo, and then:

```
git clone git@github.com:${USER}/erllambda.git
cd erllambda
git remote add upstream git@github.com:alertlogic/erllambda.git
rebar3 compile 
rebar3 ct
rebar3 erllambda zip
```

## Packaging and Deployment

There are two key points about running Erlang AWS Lambda functions:

### OpenSSL version
**Important notice**: at this moment AWS Lambda native runtime has `openssl 1.0.1k` 
while latest linux systems have `1.0.2` or even `1.1.1.`. 
See [AWS Lambda](https://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html)

To be able to run Erlang Lambda functions in AWS Lambda it is vital to package
your SW with Erlang built against openssl `1.0.1`. 
You will want to setup DockerMachine and utilize the
[erllambda_docker](https://github.com/alertlogic/erllambda_docker)
repo for this purpose. This will allow you to perform release builds for `erllambda` based
components directly from the command line.

Users can have their own Erlang dockers and have those with statically linked preferred openssl versions. 
However statically linking for base libraries is discouraged. 

### Lambda Memory Size

Current testing has shown that it does not make sense to run Erlang 
on AWS Lambda functions with less then 256MB RAM.
Having 512-1024MB is optimal for most of the use cases.

### Basic Deployment

See [Erllambda Example](https://github.com/alertlogic/erllambda_example) for step by step procedure to deploy your Lambda.
but it basically boils down to following steps:
- compile and build a prod profile release of your application.

```
rebar3 erllambda zip
```

- create your AWS Lambda function 

```
aws --profile default --region <region> \ 
 lambda create-function \
 --function-name <your_function> \
 --memory-size 1024 \
 --handler <your_function_module_name> \
 --zip-file fileb://_build/prod/<your_function>-0.0.0.zip \ 
 --runtime provided \
 --role <role-arn>
```

- or update your previously deployed AWS Lambda function

```
aws --profile default --region <region> \ 
 lambda update-function-code /
 --function-name <your_function> \
 --zip-file fileb://_build/prod/<your_function>-0.0.0.zip
```

- and invoke it

```
aws --profile default --region <region> \
 lambda invoke  --function-name <your_function> \
  --log-type Tail \
  --payload '{"msg": "hello"}' \
  outputfile.txt
```

It is however recommended to use CloudFormation based approach described in `rebar3_erllambda`


# How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
[github issue](https://github.com/alertlogic/erllambda/issues),
or simply submit a PR directly that implements your improvement.

For complex changes, or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


# How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a
[github issue](https://github.com/alertlogic/erllambda/issues).


<!--- vim: sw=4 et ts=4 -->
