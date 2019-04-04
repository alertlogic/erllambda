-module(erllambda_logging_handler).

-include("erllambda.hrl").

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

handle(_Event, _Context) ->
    erllambda:message("Starting logs test"),
    erllambda:message("This line should not break logging", []),
    erllambda:message("This and any further logs should be printed"),
    ?LOG("Using LOG macro should give module and line numbers in the logs"),
    ?LOG("~s ~p", ["LOG macro using format", 42]),
    {ok, "done"}.
