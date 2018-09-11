%%%---------------------------------------------------------------------------
%% @doc erllambda_v1_http - Erllambda Application HTTP endpoint
%%
%% This module implements the Cowboy request handler, used by the javascript
%% driver to interface with the Erlang node.
%%
%%
%% @copyright 2017 Alert Logic, Inc
%% @author Paul Fisher <pfisher@alertlogic.com>
%%%---------------------------------------------------------------------------
-module(erllambda_v1_http).
-author('Paul Fisher <pfisher@alertlogic.com>').

-export([init/2, terminate/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%====================================================================
%% Cowboy handler functions
%%====================================================================
%%%--------------------------------------------------------------------
-spec init( Request :: cowboy_req:req(), [] ) ->
          {ok, Req :: cowboy_req:req(), State :: undefined}.
%%%--------------------------------------------------------------------
%% @private
%%
%% @doc Callback to process the cowboy http request
%%
%% This function is called for each request, and blocks until complete.
%%
init( Request, [] ) ->
    RequestId = cowboy_req:header(<<"x-request-id">>, Request,
            al_uuid:gen_v4()),
    RawMethod = cowboy_req:method( Request ),
    Method = method( RawMethod ),
    Finalizer = make_handler_finalizer(RequestId, Method),
    <<"/eee/v1/", Module/binary>> = Path = cowboy_req:path( Request ),
    QueryString = cowboy_req:qs( Request ),
    NewRequest =
        try request( RequestId, Method, Module, Request ) of
            {ok, Status, MethodRequest} ->
                NewReq0 = cowboy_req:reply( Status, MethodRequest ),
                access_log( RequestId, RawMethod, Path, QueryString, Status ),
                NewReq0;
            {ok, Status, Headers, Body, MethodRequest} ->
                NewReq0 = cowboy_req:reply( Status, Headers, Body, MethodRequest ),
                access_log( RequestId, RawMethod, Path, QueryString, Status,
                            byte_size(Body) ),
                NewReq0
        catch
            Type:Reason ->
                Trace = erlang:get_stacktrace(),
                error_logger:error_report(
                  {request_failed, {Type, Reason}, Trace} ),
                Body = jiffy:encode( #{error => #{type => Type,
                                                  reason => Reason}} ),
                cowboy_req:reply( 200, json_headers(), Body, Request )  
        end,
    finalize_handler(Finalizer),
    {ok, NewRequest, undefined}.

%%%--------------------------------------------------------------------
-spec terminate( Reason :: any(), Request :: cowboy_req:req(),
                 State :: undefined ) -> ok.
%%%--------------------------------------------------------------------
%% @private
%%
%% @doc Callback to process the termination of a cowboy http request
%%
%% This function should cleanup any resources allocated to this process
%% during other callback processing (which should be accounted for in the
%% state), in order to allow the process to be reused to for the next
%% pipelined request.  If this cleanup cannot be accomplished with
%% certainty, then this callback should crash, which will terminate the
%% process.
%%
terminate( _Reason, _Request, undefined ) -> ok.



%%===========================================================================
%% Internal functions
%%===========================================================================
make_handler_finalizer(_RequestId, get) ->
    undefined;
make_handler_finalizer(RequestId, post) ->
    HandlerProcess = self(),
    spawn(
        fun() ->
            MonitorRef = monitor(process, HandlerProcess),
            receive
                {'DOWN', MonitorRef, process, HandlerProcess, _Reason} ->
                    ok;
                {finalize_erllambda, HandlerProcess} ->
                    ok
            end,
            application:set_env(erllambda, handler, undefined),
            erllambda:message("EOF: flush stdout")
        end
    ).

finalize_handler(undefined) ->
    ok;
finalize_handler(FinalizerPid) ->
    FinalizerPid ! {finalize_erllambda, self()}.

method( <<"GET">> ) -> get;
method( <<"POST">> ) -> post.


access_log( RequestId, Method, Path, QueryString, Status ) ->
    access_log( RequestId, Method, Path, QueryString, Status, 0 ).

access_log( RequestId, Method, Path, QueryString, Status, Size ) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    erllambda:message_ctx(RequestId,
        "127.0.0.1 - - [~2..0b/~s/~4..0b:~2..0b:~2..0b:~2..0b -0000] "
        "\"~s ~s?~s HTTP/1.1\" ~b ~b~n",
        [Day, month(Month), Year, Hour, Min, Sec,
            Method, Path, QueryString, Status, Size] ).

month(1) -> 'Jan';
month(2) -> 'Feb';
month(3) -> 'Mar';
month(4) -> 'Apr';
month(5) -> 'May';
month(6) -> 'Jun';
month(7) -> 'Jul';
month(8) -> 'Aug';
month(9) -> 'Sep';
month(10) -> 'Oct';
month(11) -> 'Nov';
month(12) -> 'Dec'.
    
    
request( _RequestId, get, BinModule, Request ) ->
    try binary_to_existing_atom( BinModule, latin1 ) of
        Module ->
            case code:ensure_loaded( Module ) of
                {module, Module} ->
                    {ok, 200, Request};
                {error, _} ->
                % {error, embedded} has a whiff of potential to bite us later.
                % TODO Investigate {error, embedded} implications from code:ensure_loaded/1
                    unknown_handler( BinModule, Request )
            end
    catch
        error:badarg -> unknown_handler( BinModule, Request )
    end;
request( RequestId, post, BinModule, Request ) ->
    try binary_to_existing_atom( BinModule, latin1 ) of
        Module ->
            case cowboy_req:has_body( Request ) of
                true -> post_body( RequestId, Module, Request );
                false -> invalid_request( "Missing request body", Request )
            end
    catch
        error:badarg -> unknown_handler( BinModule, Request )
    end.

post_body( RequestId, Module, Request ) ->
    case post_body_read( Request, <<>> ) of
        {ok, Body, NewRequest} ->
            post_process( RequestId, Module, Body, NewRequest );
        Otherwise -> Otherwise
    end.

post_body_read( Request, Body ) ->
    case cowboy_req:read_body( Request ) of
        {ok, Data, NewRequest} ->
            NewBody = <<Body/binary, Data/binary>>,
            {ok, NewBody, NewRequest};
        {more, Data, NewRequest} ->
            NewBody = <<Body/binary, Data/binary>>,
            post_body_read( NewRequest, NewBody );
        Otherwise -> Otherwise
    end.

post_process( RequestId, Module, Body, Request ) ->
    try jiffy:decode( Body, [return_maps] ) of
        #{<<"event">> := Event, <<"context">> := Context} ->
            Context0 = Context#{<<"erllambda_request_id">> => RequestId},
            Response = erllambda:invoke( Module, Event, Context0 ),
            post_response( Response, Request );
        #{<<"context">> := _} ->
            invalid_request( "Event object missing from body", Request );
        _Invalid ->
            invalid_request( "Context object missing from body", Request )
    catch
        throw:{error, {_, invalid_json}} ->
            invalid_request( "Invalid JSON document", Request )
    end.            

post_response( {ok, Response}, Request ) ->
    {ok, 200, json_headers(), Response, Request};
post_response( {error, {Status, Response}}, Request ) ->
    {ok, Status, json_headers(), Response, Request}.



unknown_handler( Handler, Request ) ->
    {ok, 404, json_headers(),
     error( "UnknownHandler", "The handler ~s cannot be found", [Handler] ),
     Request}.


invalid_request( Reason, Request ) ->
    {ok, 400, json_headers(), error( "InvalidRequest", Reason, [] ), Request}.


json_headers() ->
    #{<<"content-type">> => <<"application/json">>}.

error( Type, Format, Values ) ->
    NewFormat = "{\"errorType\": \"~s\", \"errorMessage\": \""
        ++ Format ++ "\"}",
    NewValues = [Type | Values],
    iolist_to_binary( io_lib:format( NewFormat, NewValues ) ).
