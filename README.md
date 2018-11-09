erllambda
=========

Enable AWS Lambda functions to be written in Erlang


# Overview

The `erllambda` library provides all functionality needed to build and
deploy fully functional [AWS Lambda](https://aws.amazon.com/lambda/)
functions, written entirely in Erlang.

Erlang Lambda functions implement a simple one function, behavior:

```
-module(hello_lambda).

-behavior(erllambda).
-export([init/1]).
-export([handle/2]).

-spec init( Context :: map() ) -> ok | none().
handle( Context ) ->
    erllambda:succeed( "Hello BYOL!" ).
    
-spec handle( Event :: map(), Context :: map() ) -> {ok, Body} | {error, ErrorBody}.
handle( Event, Context ) ->
    erllambda:succeed( "Hello Event ~p", [Event] ).
```

There are really two ways to get started.  First you can checkout and review
the
[`erllambda_example`](https://algithub.pd.alertlogic.net/alertlogic/erllambda_example)
project.  This is a complete working AWS Lambda written in Erlang, with the
goal of demonstrating the capabilities available as part of the framework,
and how to write Lambda functions for different purposes in AWS.

The second path is to use the
[`rebar3_erllambda`](https://algithub.pd.alertlogic.net/alertlogic/rebar3_erllambda)
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

The `erllambda` application owned by the
[Data Processing Team](https://alertlogic.atlassian.net/wiki/display/DPT).


# Dependencies

The `erllambda` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `erllambda` is used in other projects
`rebar.config`.

In addition makefile support is available in
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl) with
makes builds and pipeline integration trivial.


# How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a
[github issue](https://algithub.pd.alertlogic.net/alertlogic/erllambda/issues).


# How to contribute

Contributions to this repo are always welcome.  If you have an idea for
improving the this or related components, please submit a
[github issue](https://algithub.pd.alertlogic.net/alertlogic/erllambda/issues),
or simply submit a PR directly that implements your improvement.

For complex changes, or the introduction of a major feature, it is
beneficial to discuss ideas before implementing them, so that your efforts
can focus on pull requests that will be accepted more easily.

As you prepare your pull request, make sure that you follow the coding
conventions that exist in the files, and always make sure that all unit and
common tests run.  Please ensure that your contribution always adds to the
coverage percentage, and does not decrease it.


## Initial setup, compilation and testing

*TLDR;* as long as your basic environment is setup, getting started
developing `erllambda` should be as easy as forking the repo, and then:

```
git clone git@algithub.pd.alertlogic.net:${USER}/erllambda.git
cd erllambda
git remote add upstream git@algithub.pd.alertlogic.net:alertlogic/erllambda.git
make env
. .setenv
make deps compile test
```

If this is the first time that you are setting up to develop on a repo that
uses the utilizes the
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl) system,
you will need to perform the setup instructions detailed in the
[makeincl README.md](https://algithub.pd.alertlogic.net/alertlogic/makeincl/blob/master/README.md).

In addition, if you are developing on a non-linux system, you will want to
setup DockerMachine and utilize the
[docker-image-makeincl](https://algithub.pd.alertlogic.net/alertlogic/docker-image-makeincl)
repo. This will allow you to perform release builds for `erllambda` based
components directly from the command line first by deploying your personal
stack:

```
dsh make stack-create
```

and then subsequently updating the function:

```
dsh make function-update
```


## Makefile Targets

After initial setup, and in future shell sessions, only do the following is
needed configure the environment for developement:

```sh
cd erllambda
. .setenv
```

The main `makefile` targets for development and test are as follows:

- `make` will compile, execute the eunit tests, and generate a coverage
  report.
- `make unit` same as plain `make`
- `make ct` will compile, execute the common tests, and generate a coverage
  report.
- `make test` will compile, execute both the eunit and common tests, and
  generate a consolidated coverage report.

Full documentation of the makefile targets available and how to customize
`allib` makefiles can be found in
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl).



<!--- vim: sw=4 et ts=4 -->
