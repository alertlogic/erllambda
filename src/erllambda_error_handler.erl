%%%---------------------------------------------------------------------------
%% @doc erllambda_error_handler - error_logger's handler for friendly
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda_error_handler).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_event).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #{}}.

%general cae of logs
handle_event({EventType, _Gleader,
             {_Pid, Format, Data}}, State)
        when EventType =:= error;
             EventType =:= warning_msg;
             EventType =:= info_msg ->
    output(Format, Data),
    {ok, State};
handle_event({EventType, _Gleader,
             {_Pid, _Type, Report}}, State)
        when EventType =:= error_report;
             EventType =:= warning_report;
             EventType =:= info_report ->
    output("~p", [Report]),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(Reason, State) ->
    io:fwrite("~w terminated with reason ~w and state ~w~n",
              [?MODULE, Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

output(Format, Data) ->
    io:fwrite(string:concat(erllambda:line_format(Format, Data), "\n")).

%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).

output_test_() ->
    [
     ?_assertEqual(ok, output("~s", ["arg"])),
     ?_assertEqual(ok, output("message ~s", ["arg"])),
     ?_assertEqual(ok, output("message", []))
    ].

-endif.
