-module(erllambda_fibonachi_handler).

%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================

handle(#{<<"sequence">> := Sequence}, _Context) ->
    {ok, #{<<"sequence">> => sequence(Sequence)}}.


%%%===================================================================
%%% Private functions
%%%===================================================================

sequence([])         -> [0];
sequence([0])        -> [0, 1];
sequence([X, Y])     -> [X, Y, X + Y];
sequence([X | Tail]) -> [X | sequence(Tail)].
