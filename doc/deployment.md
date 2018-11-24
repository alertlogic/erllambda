erllambda Deployment Tutorial
=============================

To create AWS Lambda function, it's required to provide a zip package with
function code.

# Bootstrapping

AWS Lambda functions with a provided runtime expects that package contains
executable `bootstrap` file in the root of the zip archive. `bootstrap` is an
entry point for a provided runtime. When AWS Lambda container boots, `bootstrap`
file is executed to start a provided with a package runtime.

In the current implementation `erllambda` provides a
[script](../priv/erlang-start), which can be used as an entry point to boot OTP
release.

It might be used by other projects (like [rebar3 erllambda plugin](https://github.com/alertlogic/rebar3_erllambda)) as a `bootstrap` file. Other projects (like [mix erllambda plugin](https://github.com/alertlogic/mix_erllambda)) might have a custom `bootstrap` file.
`erllambda` does not require runtime to be started using the start script in the repository.

# Packaging

`erllambda` doesn't know how to build a zip package.

There are currently 2 projects that help with packaging:

* Plugin for `rebar3` [rebar3_erllambda](https://github.com/alertlogic/rebar3_erllambda)
* `mix` task [mix_erllambda](https://github.com/alertlogic/mix_erllambda)

Created packages should be suitable for deployment as AWS Lambda functions with provided environment.

For further details please see documentations of mentioned projects.

# Deployment

See basic deployment [example](../README.md#basic-deployment).
