erllambda
===========

Enable AWS Lambda function to be written in Erlang


## Overview

The `erllambda` library provides all functionality needed to build and
deploy fully functional [AWS Lambda](https://aws.amazon.com/lambda/)
functions, written entirely in Erlang.

Erlang Lambda functions implement a simple one function, behavior:

```
-module(hello_lambda).

-behavior(erllambda).
-export([handle/2]).

-spec handle( Event :: map(), Context :: map() ) -> ok | none().
handle( Event, Context ) ->
    erllambda:succeed( "Hello World!" ).
```

This function is then built into an Erlang application, and packaged as
a [rebar3 release](http://www.rebar3.org/docs/releases) along with any other
applications/libraries needed to implement the intended functionality.

The [makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl)
project provides support for developing and deploying AWS Lambda functions
written using `erllambda` or simply with `node.js`.

A more detailed tutorial is located at
[Writing AWS Lambda Functions in Erlang](doc/tutorial.md).


## Ownership

The `erllambda` application owned by the
[Data Processing Team](https://alertlogic.atlassian.net/wiki/display/DPT).


## Dependencies

The `erllambda` application is built using
[`rebar3`](http://www.rebar3.org), and all other dependencies are
automatically pulled in when `erllambda` is used in other projects
`rebar.config`.

In addition makefile support is available in
[makeincl](https://algithub.pd.alertlogic.net/alertlogic/makeincl) with
makes builds and pipeline integration trivial.


## How to contribute

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


## How to report defects

If you encounter an problem, or simply have a question about using this
repo, please submit a
[github issue](https://algithub.pd.alertlogic.net/alertlogic/erllambda/issues).


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
uses the utilizes the `setup.sh`/`. .setenv` infrastructure provided by the
[`dev_scripts`](https://algithub.pd.alertlogic.net/alertlogic/dev_scripts),
you will need to perform or verify the following components are present and
working on your machine:

- [Kerl Machine Install](#kerl-machine-install)

That should be all that is required for a Linux system.

While it is possible to develop and test your work entirely within the
native Mac OS X environment, you should also ensure that no incompatibility
exists on Linux systems.  To do this you will need to perform the following
one-time configuration on your system:

- [Install Virtual Box & Vagrant](#vagrant-and-virtualbox-machine-install)

Then, for this project you will need to perform the follow:

- [ssh-agent and github private keys](#ssh-agent-and-github-private-keys)


## Makefile Targets

=======
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


## Detailed Install & Configuration Steps

One various system, there are several things that are not yet integrated
into the `dev_scripts` project and automated, so you will need to perform
them manually.


### Kerl Machine Install

To install and setup [kerl](https://github.com/yrashk/kerl) do the
following:

```sh
cd ~/bin
curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
chmod a+x kerl
cd
```

This assumes that you use ~/bin for your personal executable scripts, so if
you have another place in which you keep these things, then use that
location.  All it requires is for the `kerl` script to be in the path.
Verify that kerl now works with the following command, checking the output:

```sh
[vagrant@localhost ~]$ kerl
kerl: build and install Erlang/OTP
usage: /home/vagrant/bin/kerl <command> [options ...]
\n  <command>       Command to be executed\n
Valid commands are:
  build    Build specified release or git repository
  install  Install the specified release at the given location
  deploy   Deploy the specified installation to the given host and location
  update   Update the list of available releases from erlang.org
  list     List releases, builds and installations
  delete   Delete builds and installations
  active   Print the path of the active installation
  status   Print available builds and installations
  prompt   Print a string suitable for insertion in prompt
  cleanup  Remove compilation artifacts (use after installation)
[vagrant@localhost ~]$ kerl update releases
Getting the available releases from erlang.org...
The available releases are:
R10B-0 R10B-10 R10B-1a R10B-2 R10B-3 R10B-4 R10B-5 R10B-6 R10B-7 R10B-8 R10B-9 R11B-0 R11B-1 R11B-2 R11B-3 R11B-4 R11B-5 R12B-0 R12B-1 R12B-2 R12B-3 R12B-4 R12B-5 R13A R13B01 R13B02-1 R13B02 R13B03 R13B04 R13B R14A R14B01 R14B02 R14B03 R14B04 R14B_erts-5.8.1.1 R14B R15B01 R15B02 R15B02_with_MSVCR100_installer_fix R15B03-1 R15B03 R15B R16A_RELEASE_CANDIDATE R16B01 R16B02 R16B03-1 R16B03 R16B 17.0-rc1 17.0-rc2 17.0 17.1 17.3 17.4 17.5 18.0 18.1 18.2.1 18.2
[vagrant@localhost ~]$ kerl update releases
```

Once you have `kerl` ready to run on your system, then you will need to
setup the ~/.kerlrc file for your system.  The most important part of this
is to create a directory in which all erlang releases will be installed.
This needs to be writable by your user for the `dev_scripts` setup.sh
framework to function correctly.  A minimal ~/.kerlrc file should look as
follows:

```sh
KERL_DEFAULT_INSTALL_DIR=/home/${USER}/erlang
KERL_INSTALL_MANPAGES=yes
KERL_ENABLE_PROMPT=yes
export CFLAGS="-DOPENSSL_NO_EC=1"
```

For native Mac OSX, you will need to add the following:

```sh
KERL_CONFIGURE_OPTIONS="--enable-smp-support --enable-threads
                        --enable-kernel-poll  --enable-darwin-64bit"
export CC=gcc-4.9
```

### Vagrant and VirtualBox Machine Install

To install and configure your system to be able to run a CentOS7 vm, perform
the following once on your machine:

- Download and install [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
- Download and install [Vagrant](https://www.vagrantup.com/downloads.html)
- Import the CentOS7 box onto your system, and get the `vagrant-scp` plugin
  installed:

```sh
(r18)nova-storm:erllambda pfisher$ vagrant box add centos7 https://github.com/holms/vagrant-centos7-box/releases/download/7.1.1503.001/CentOS-7.1.1503-x86_64-netboot.box
==> box: Box file was not detected as metadata. Adding it directly...
==> box: Adding box 'centos7' (v0) for provider: 
    box: Downloading: https://github.com/holms/vagrant-centos7-box/releases/download/7.1.1503.001/CentOS-7.1.1503-x86_64-netboot.box
==> box: Successfully added box 'centos7' (v0) for 'virtualbox'!
(r18)nova-storm:erllambda pfisher$ vagrant plugin install vagrant-scp
Installing the 'vagrant-scp' plugin. This can take a few minutes...
Installed the plugin 'vagrant-scp (0.5.6)'!
(r18)nova-storm:erllambda pfisher$ 
```

### ssh-agent and github private keys


Once you have the vagrant machine running the first time, you will need to
get your github private key moved into the `/home/vagrant/.ssh/`
directory. To accomplishing this, do the following:

```sh
nova-storm:gp pfisher$ vagrant scp ~/.ssh/id_dsa :.ssh/
Warning: Permanently added '[127.0.0.1]:2222' (RSA) to the list of known hosts.
id_dsa                                        100%  736     0.7KB/s   00:00    
nova-storm:gp pfisher$ 
```

You will want to add the following to the end of your `~/.bash_profile` with
so that `ssh-agent` will start when you login, and you will be able to use
your ssh key to access github:

```sh
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

#Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi
```

Now when you login to the vagrant instance, it will prompt you to unlock
your ssh key and add it to ssh-agent running on the instance.


<!--- vim: sw=4 et ts=4 -->
