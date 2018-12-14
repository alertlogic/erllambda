%%%---------------------------------------------------------------------------
%% @doc erllambda - AWS Lambda for Erlang Interface
%%
%% This module contains the public programming interface used by AWS Lambda
%% function implemented in Erlang using the <code>erllambda</code> framework.
%%
%% Eventually, we should take care of the following to avoid the overhead
%% starting and terminating the Erlang VM for each request.
%%
%%  https://aws.amazon.com/blogs/compute/container-reuse-in-lambda/
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda).

%% public - high-level migration orchestration endpoints
-export([succeed/1, succeed/2, fail/1, fail/2]).
-export([message/1, message/2, message_ctx/2, message_ctx/3]).
-export([metric/1, metric/2, metric/3, metric/4]).
-export([get_remaining_ms/1, get_aws_request_id/1]).
-export([region/0, environ/0, accountid/0, config/0, config/1, config/2]).
-export([print_env/0, line_format/2]).

%% private - handler invocation entry point, used by http api
-export([invoke/3]).

-include("erllambda.hrl").
-include("exception.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(AL_CTX_SD_ID, "context@36312").

%%============================================================================
%% Callback Interface Definition
%%============================================================================
%%%---------------------------------------------------------------------------
-callback handle( Event :: map(), Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.

%%============================================================================
%% API Functions
%%============================================================================
%%%---------------------------------------------------------------------------
-spec succeed(Value :: iolist() | map()) -> no_return().
%%%---------------------------------------------------------------------------
%% @doc Complete processing with success
%%
succeed( Map ) when is_map(Map)  ->
    complete( #{success => Map} );
succeed( Value ) when is_binary(Value); is_list(Value) ->
    succeed( "~s", [Value] ).

succeed( Format, Values ) ->
    complete( #{success => format( Format, Values )} ).


%%%---------------------------------------------------------------------------
-spec fail( Message :: iolist() ) -> no_return().
%%%---------------------------------------------------------------------------
%% @doc Complete a processing with failure
%%
fail( Message ) ->
    fail( "~s", [Message] ).

fail( Format, Values ) ->
    complete( #{errorType => 'HandlerFailure',
                errorMessage => format( Format, Values )} ).


%%%---------------------------------------------------------------------------
-spec message( Message :: iolist() ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will ensure the message is output in the log for the lambda
%% invocation.
%%
message( Message ) ->
    message_send( Message ).


%%%---------------------------------------------------------------------------
-spec message( Format :: string(), Values :: [any()] ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will format the message using <code>io_lib:format/2</code>
%% and then ensure it is output in the log for the lambda invocation.
%%
message( Format, Values ) ->
    message_send( Format, Values ).


%%%---------------------------------------------------------------------------
-spec message_ctx( ReqId :: binary() | map(), Message :: iolist() ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will ensure the message is output in the log for the lambda
%% invocation.
%%
message_ctx( ReqId, Message ) ->
    message_ctx(ReqId, Message, []).


%%%---------------------------------------------------------------------------
-spec message_ctx( ReqId :: binary() | map(), Format :: string(),
        Values :: [any()] ) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send an informational message to be logged
%%
%% This function will format the message using <code>io_lib:format/2</code>
%% and then ensure it is output in the log for the lambda invocation.
%%
message_ctx( Ctx, Format, Values ) when is_map (Ctx) ->
    message_ctx(get_aws_request_id(Ctx), Format, Values);
message_ctx( ReqId, Format, Values ) when is_binary(ReqId) ->
    message_send("[" ?AL_CTX_SD_ID " aid=\"~s\"] " ++ Format,
        [ReqId  | Values] ).

-spec metric(MetricName :: string(), MetricValue :: integer(),
             Type :: string(), Tags :: list()) -> ok.
%%%---------------------------------------------------------------------------
%% @doc Send custom metrics to Datadog
%%
%% This function will send log message from lambda using following format
%% MONITORING|unix_epoch_timestamp|metric_value|metric_type|my.metric.name|#tag1:value,tag2
%% where metric_type :: "count" | "gauge" | "histogram"
%%
metric(MName) ->
    metric(MName, 1).
metric(MName, Val) when is_integer(Val) ->
    metric(MName, Val, "count").
metric(MName, Val, Type) ->
    metric(MName, Val, Type, []).
metric(MName, Val, Type, Tags)
        when is_list(MName)
        andalso (Type == "count" orelse Type == "gauge" orelse Type == "histogram")
        andalso is_number(Val) ->
    NewTags = "#" ++
        string:join(
            lists:map(
                fun ({T,V}) ->
                        erllambda_util:to_list(T) ++ ":" ++ erllambda_util:to_list(V);
                    (OtherTag) ->
                        erllambda_util:to_list(OtherTag)
                end, Tags
            ),
            ","
        ),
    Ts = os:system_time(second),
    Msg = string:join([
        "MONITORING",
        erllambda_util:to_list(Ts),
        erllambda_util:to_list(Val),
        Type,
        MName,
        NewTags
    ], "|"),
    message(Msg).



%%%---------------------------------------------------------------------------
-spec get_remaining_ms(map()) -> pos_integer().
%%%---------------------------------------------------------------------------
%% @doc The time remaining in our invoke
%%
%% This function will return the time remaining in ms in this given Lambda function
%% invocation.
%%
%% @see JS style Context Function
%% http://docs.aws.amazon.com/lambda/latest/dg/nodejs-prog-model-context.html
%%
get_remaining_ms(#{<<"lambda-runtime-deadline-ms">> := Deadline}) ->
    CurrentTs = os:system_time(millisecond),
    Deadline - CurrentTs.

%%%---------------------------------------------------------------------------
-spec get_aws_request_id(list() | map()) -> binary() | undefined.
%%%---------------------------------------------------------------------------
%% @doc Extract the request Id
%%
%% This function will return the Req ID of the invoke
%% contains several alternatives from new to old
%% to support backward compatibility
%%
get_aws_request_id(Hdrs) when is_list(Hdrs) ->
    get_aws_request_id(hdr2map(Hdrs));
get_aws_request_id(#{<<"awsRequestId">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"lambda-runtime-aws-request-id">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"x-amz-aws-request-id">> := ReqId}) -> ReqId;
get_aws_request_id(#{<<"x-amzn-requestid">> := ReqId}) -> ReqId.

%%%---------------------------------------------------------------------------
-spec region() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the default region in which the Lambda function
%% is running.
%%
%% @see erllambda_util:region/0
%%
region() ->
    erllambda_util:region().


%%%---------------------------------------------------------------------------
-spec config() -> #aws_config{}.
%%%---------------------------------------------------------------------------
%% @doc Default <code>erlcloud</code> config
%%
%% This function will return a default AWS configuration record for use by
%% the <code>erlcloud</code> application in calling AWS services from lambda
%% functions using the default execution role.  It is expected that the
%% application calls this function every time an <code>erlcloud
%% config</code> is needed, and the implementation will ensure that the
%% credentials contained in the configuration are valid for at least 60
%% seconds.
%%
%% If the application needs to assume an alternative role, it should call
%% the {@link erllambda_util:config/1,2} functions directly.
%%
%% @see erllambda_util:config/0
%%
config() ->
    erllambda_util:config().

-spec config( Services :: [service()] ) -> #aws_config{}.
config(Services) ->
    erllambda_util:config(Services).

-spec config( Region :: region(), Options :: [option()] ) -> #aws_config{}.
config(Region, Options) ->
    erllambda_util:config(Region, Options).

%%%---------------------------------------------------------------------------
-spec environ() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the erllambba context
%%
%% @see erllambda_util:environ/0
%%
environ() ->
    erllambda_util:environ().

%%%---------------------------------------------------------------------------
-spec accountid() -> binary().
%%%---------------------------------------------------------------------------
%% @doc The default region
%%
%% This function will return the account id in which we run
%%
%% @see erllambda_util:environ/0
%%
accountid() ->
    erllambda_util:accountid().

%%============================================================================
%% Private API Function
%%============================================================================

%%%---------------------------------------------------------------------------
-spec invoke( Handler :: atom(), Event :: any(),
              EventHdrs :: list() ) -> {ok, term()} | {handled|unhandled, term()}.
%%%---------------------------------------------------------------------------
%%
%%
invoke( Handler, Event, EventHdrs )  ->
    application:set_env( erllambda, handler, Handler ),
    Parent = self(),
    InvokeFun =
        fun () ->
            % construct the contexts on the fly
            % binaries all the way down
            Context = maps:merge(os_env2map(), hdr2map(EventHdrs)),
            Res = invoke_exec(Handler, Event, Context),
            Parent ! {handle, Res}
        end,
    % each handler should leave and die in it's own process
    Opts = application:get_env(erllambda, handler_spawn_opts, []),
    {_, MonRef} = erlang:spawn_opt( InvokeFun , [monitor | Opts]),
    receive
        {handle, Res} ->
            erlang:demonitor(MonRef, [flush, info]),
            Res;
        {'DOWN', MonRef, process, _Pid, Result} ->
            Message = format( "Handler terminated with ~p", [Result] ),
            Response = #{errorType => 'HandlerFailure',
                errorMessage => Message,
                stackTrace => []},
            {unhandled, Response}
    end.

invoke_exec( Handler, Event, Context ) ->
    try
        case Handler:handle(jsone:decode(Event), Context ) of
            ok -> succeed( "completed successfully" );
            {ok, Result} -> succeed( Result );
            {error, ErrResult} -> fail( ErrResult );
            _Anything ->
                %% if handler returns anything else, then it did not call
                %% fail/succeed/retry, or return ok, so it is assumed to fail
                fail( "did not invoke succeed/1,2 or fail/1,2" )
        end
    catch
        % both top level handler and we can can call success/fail
        throw:{?MODULE, result, JsonMap} -> {ok, JsonMap};
        throw:{?MODULE, failure, JsonMap} -> {handled, JsonMap};
        ?EXCEPTION(Type, Reason, Stacktrace) ->
            Trace = ?GET_STACK(Stacktrace),
            Message = format( "terminated with exception {~p, ~p}", [Type, Reason] ),
            message_send( format( "~s with trace ~p", [Message, Trace] ) ),
            Response = #{errorType => 'HandlerFailure',
                errorMessage => Message,
                stackTrace => format_stack(Trace)},
            {unhandled, Response}
    end.

-spec line_format(io:format(), [term()]) -> iodata() | unicode:charlist().
line_format(Format, Data) ->
    Format1 = io_lib:scan_format(Format, Data),
    Format2 = reformat(Format1),
    Text = io_lib:build_text(Format2),
    one_line_it(Text).

%%============================================================================
%% Internal Functions
%%============================================================================
format( Format, Values ) ->
    iolist_to_binary( io_lib:format( Format, Values ) ).

% in success case we care only about the body
complete( #{success := Response}) ->
    complete( result, Response );
% in error we care about the entire error object
complete( #{errorType := _} = Response) ->
    complete( failure, Response).

-spec complete(Type:: failure | result, Response :: map()) -> no_return().
complete( Type, Response ) ->
    throw( {?MODULE, Type, Response} ).


message_send( Message ) ->
    error_logger:info_msg( "~s", [Message] ).

message_send( Format , Values) ->
    error_logger:info_msg( Format, Values ).

print_env() ->
    case application:get_env(erllambda, print_env, false) of
        true ->
            EnvWihtoutSecret = hide_secret(os_env2map()),
            erllambda:message("Erllambda Starting at ~p with Env ~p",
                [os:system_time(millisecond), EnvWihtoutSecret]);
        _ -> ok
    end.

os_env2map() ->
    maps:from_list(lists:map(
        fun(S) ->
            [K, V] = string:split(S, "="),
            {list_to_binary(K), list_to_binary(V)}
        end,
        os:getenv()
    )).

hide_secret(#{<<"AWS_SECRET_ACCESS_KEY">> := _} = Map) ->
    maps:without([<<"AWS_SECRET_ACCESS_KEY">>], Map);
hide_secret(Map) ->
    Map.

hdr2map(Hdrs) ->
    maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Hdrs]).

format_stack([StackItem | Tail]) ->
    ItemBin = iolist_to_binary(line_format("~p", [StackItem])),
    [ItemBin | format_stack(Tail)];
format_stack([]) ->
    [].

reformat(Format) ->
    reformat(Format, _Width = 134217721).

reformat([#{control_char := C} = M | T], Width) when C =:= $p; C =:= $P ->
    [M#{width => Width} | reformat(T, Width)];
reformat([H | T], Width) ->
    [H | reformat(T, Width)];
reformat([], _Width) ->
    [].

one_line_it(Text) ->
    re:replace(string:trim(Text), "\r?\n\s*", " ", [{return,list},global,unicode]).


%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).

line_format_test_() ->
    MultilineJson =
        <<"[\n"
          "  {\n"
          "    \"_id\": \"5be7f5f8e8c1ed9241898c1a\",\n"
          "    \"index\": 0,\n"
          "    \"guid\": \"581599e9-4f5d-45c7-9c5c-2bd5611f59fd\",\n"
          "    \"isActive\": false,\n"
          "    \"balance\": \"$3,819.73\",\n"
          "    \"picture\": \"http://placehold.it/32x32\",\n"
          "    \"age\": 21,\n"
          "    \"eyeColor\": \"blue\",\n"
          "    \"name\": {\n"
          "      \"first\": \"Marcie\",\n"
          "      \"last\": \"Byrd\"\n"
          "    }\n"
          "  }\n"
          "]\n">>,
    [
     ?_assertEqual("List [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,"
                   "21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,"
                   "41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,"
                   "61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,"
                   "81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]",
                   line_format("List ~p", [lists:seq(1, 100)])),
     ?_assertEqual("Multiline Json [ { \"_id\": \"5be7f5f8e8c1ed9241898c1a\", \"index\": 0, \"guid\": \"581599e9-4f5d-45c7-9c5c-2bd5611f59fd\", \"isActive\": false, \"balance\": \"$3,819.73\", \"picture\": \"http://placehold.it/32x32\", \"age\": 21, \"eyeColor\": \"blue\", \"name\": { \"first\": \"Marcie\", \"last\": \"Byrd\" } } ]",
                   line_format("Multiline Json ~s", [MultilineJson]))
    ].

format_stack_test_() ->
    [
     ?_assertEqual(
        [<<"{module,function,2,[{file,\"file.erl\"},{line,11}]}">>,
         <<"{module2,function2,3,[{file,\"file2.erl\"},{line,305}]}">>,
         <<"{module3,function3,4,[{file,\"file3.erl\"},{line,285}]}">>],
        format_stack([{module,function,2, [{file,"file.erl"}, {line,11}]},
                      {module2,function2, 3, [{file,"file2.erl"}, {line,305}]},
                      {module3,function3, 4, [{file,"file3.erl"}, {line,285}]}]))
    ].

-endif.
