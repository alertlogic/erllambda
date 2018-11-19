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
init( Context ) ->
    erllambda:succeed( "Hello BYOL!" ).
    
-spec handle( Event :: map(), Context :: map() ) -> {ok, Body} | {error, ErrorBody}.
handle( Event, Context ) ->
    erllambda:succeed( "Hello Event ~p", [Event] ).
```

There are really two ways to get started.  First you can checkout and review
the
[`erllambda_example`](https://github.com/alertlogic/erllambda_example)
project.  This is a complete working AWS Lambda written in Erlang, with the
goal of demonstrating the capabilities available as part of the framework,
and how to write Lambda functions for different purposes in AWS.

The second path is to use the
[`rebar3_erllambda`](https://github.com/alertlogic/rebar3_erllambda)
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

## Roadmap / TODO

- Finish Tutorials
- more common tests
- support Runtime Init error
- remote GIT links in rebar and make it https
- make public
- add travis when public
- Erlang 21.X


# Dependencies

The `erllambda` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `erllambda` is used in other projects
`rebar.config`.


# How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a
[github issue](https://github.com/alertlogic/erllambda/issues).


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


## Initial setup, compilation and testing

*TLDR;* as long as your basic environment is setup, getting started
developing `erllambda` should be as easy as forking the repo, and then:

```
git clone git@github.com:${USER}/erllambda.git
cd erllambda
git remote add upstream git@github.com:alertlogic/erllambda.git
rebar3 compile 
rebar3 ct
```

## Packaging and Deployment
In addition, if you are developing on a non-linux system, you will want to
setup DockerMachine and utilize the
[erllambda_docker](https://github.com/alertlogic/erllambda_docker)
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

<!--- vim: sw=4 et ts=4 -->
