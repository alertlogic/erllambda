-module(erllambda_logging_handler).

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

handle(_Event, _Context) ->
    erllambda:message("Starting logs test"),
    erllambda:message("This line should not brake logging", []),
    erllambda:message("This and any further logs should be printed"),
    {ok, "done"}.
