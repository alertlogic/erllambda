-module(erllambda_aws_runtime_next_http).

-behavior(cowboy_handler).

%% API
-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

init(#{method := <<"GET">>} = Req0, Opts) ->
    {Id, Item} = erllambda_aws_runtime_srv:next(),
    Headers = #{<<"lambda-runtime-aws-request-id">> => Id,
                <<"lambda-runtime-deadline-ms">> =>
                    integer_to_binary(os:system_time(millisecond) + 10000),
                <<"content-type">> => <<"application/json">>},
    Response = jiffy:encode(Item),
    Req = cowboy_req:reply(200, Headers, Response, Req0),
    {ok, Req, Opts};
init(Req0, Opts) ->
    Req = cowboy_req:reply(405, Req0),
    {ok, Req, Opts}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
