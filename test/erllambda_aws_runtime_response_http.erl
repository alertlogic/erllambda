-module(erllambda_aws_runtime_response_http).

-behavior(cowboy_handler).

%% API
-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

init(#{method := <<"POST">>} = Req, Opts) ->
    ReqId = cowboy_req:binding(req_id, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    ResponseResult = maybe_decode_json(Body),
    case cowboy_req:binding(operation, Req1) of
        <<"response">> ->
            ok = erllambda_aws_runtime_srv:response_ok(ReqId, ResponseResult);
        <<"error">> ->
            ok = erllambda_aws_runtime_srv:response_error(ReqId, ResponseResult)
    end,
    Headers = #{<<"content-type">> => <<"application/json">>},
    Response = jiffy:encode(#{<<"status">> => <<"Accepted">>}),
    Req2 = cowboy_req:reply(202, Headers, Response, Req1),
    {ok, Req2, Opts};
init(Req0, Opts) ->
    Req = cowboy_req:reply(405, Req0),
    {ok, Req, Opts}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_decode_json(Body) ->
    try
        jiffy:decode(Body, [return_maps])
    catch
        _:_ ->
            Body
    end.
