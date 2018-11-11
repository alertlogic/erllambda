%%%---------------------------------------------------------------------------
%% @doc erllambda_error_handler - error_logger's handler for friendly
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2018 Alert Logic, Inc.
%%%---------------------------------------------------------------------------
-module(erllambda_error_handler).
-author('Anton Zaets <anton.zaets@alertlogic.com>').
-author('Evgeny Bob <ebob@alertlogic.com>').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_event).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_event:start_link({local, ?MODULE}).

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
output(Format, Data) ->
    io:fwrite(format(Format, Data)).

format(Format, Data) ->
    Format1 = io_lib:scan_format(Format, Data),
    Format2 = reformat(Format1),
    Text = io_lib:build_text(Format2),
    one_line_it(Text).

reformat(Format) ->
    reformat(Format, _Width = 134217721).

reformat([#{control_char := C} = M | T], Width) when C =:= $p; C =:= $P ->
    [M#{width => Width} | reformat(T, Width)];
reformat([H | T], Width) ->
    [H | reformat(T, Width)];
reformat([], Width) ->
    [].

one_line_it(Text) ->
    re:replace(string:trim(Text), "\r?\n\s*", " ", [{return,list},global,unicode]).


%%====================================================================
%% Test Functions
%%====================================================================
-ifdef(TEST).

format_test_() ->
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
                   format("List ~p", [lists:seq(1, 100)])),
     ?_assertEqual("Multiline Json [ { \"_id\": \"5be7f5f8e8c1ed9241898c1a\", \"index\": 0, \"guid\": \"581599e9-4f5d-45c7-9c5c-2bd5611f59fd\", \"isActive\": false, \"balance\": \"$3,819.73\", \"picture\": \"http://placehold.it/32x32\", \"age\": 21, \"eyeColor\": \"blue\", \"name\": { \"first\": \"Marcie\", \"last\": \"Byrd\" } } ]",
                   format("Multiline Json ~s", [MultilineJson]))
    ].

-endif.
