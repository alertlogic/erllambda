-module(erllambda_logging_handler).

-include("erllambda.hrl").

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

handle(Event, _Context) ->
    group_leader(whereis(erllambda_io_srv), self()),
    handle_event(Event).

handle_event(#{<<"type">> := <<"test_no_args_messages">>}) ->
    erllambda:message("Starting logs test"),
    erllambda:message("This line should not break logging", []),
    erllambda:message("This and any further logs should be printed"),
    ?LOG("Using LOG macro should give module and line numbers in the logs"),
    ?LOG("~s ~p", ["LOG macro using format", 42]),
    {ok, "done"};

handle_event(#{<<"type">> := <<"test_bad_format_do_not_remove_error_handler">>}) ->
    erllambda:message("Good ~s", [<<"format">>]),
    erllambda:message("Bad format", [any]),
    erllambda:message("Good ~s", [<<"format">>]),
    {ok, "done"}.
