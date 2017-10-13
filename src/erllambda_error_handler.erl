%%%---------------------------------------------------------------------------
%% @doc erllambda_error_handler - error_logger's handler for friendly
%% CloudWatch logging from erllambda
%%%---------------------------------------------------------------------------
-module(erllambda_error_handler).
-author('Anton Zaets <anton.zaets@alertlogic.com>').
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

-define(MSG_END, <<"\t\n">>).
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

handle_event({EventType, _Gleader, {_Pid, Format, Data}}, State)
  when EventType =:= error;
       EventType =:= warning_msg;
       EventType =:= info_msg ->
    StrBin = iolist_to_binary(io_lib:format(Format, Data)),
    output(StrBin),
    {ok, State};
handle_event({EventType, _Gleader, {_Pid, _Type, Report}}, State)
  when EventType =:= error_report;
       EventType =:= warning_report;
       EventType =:= info_report ->
    StrBin = iolist_to_binary(io_lib:format("~p", [Report])),
    output(StrBin),
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

output(Str) ->
    io:fwrite("~s~s", [Str, ?MSG_END]).
