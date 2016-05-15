-module(erllambda_rebar3).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Options =
        [
         {name, erllambda},
         {module, ?MODULE},
         {bare, true},
         {deps, [release]},
         {example, "rebar3 erllambda"},
         {opts,
          [
          ]},              % list of options understood by the plugin
         {short_desc, "AWS Lambda support for Erlang"},
         {desc,
          "Rebar3 plugin that allows AWS Lambda functions to be implemented"
          "in Erlang in a way that is simple to write and appears to execute"
          "as a natively supported language."}
        ],
    Provider = providers:create( Options ),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
