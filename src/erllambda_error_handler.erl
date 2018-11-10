%%%---------------------------------------------------------------------------
%% @doc erllambda_error_handler - error_logger's handler for friendly
%% This module implements the Erlang <code>application</code> behavior, and
%% starts the simple http server endpoint used by the javascript driver.
%%
%%
%% @copyright 2018 Alert Logic, Inc
%%%---------------------------------------------------------------------------
-module(erllambda_error_handler).
-author('Anton Zaets <anton.zaets@alertlogic.com>').
-author('Evgeny Bob <ebob@alertlogic.com>').

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
    output(io_lib:format(Format, Data)),
    {ok, State};
handle_event({EventType, _Gleader,
             {_Pid, _Type, Report}}, State)
        when EventType =:= error_report;
             EventType =:= warning_report;
             EventType =:= info_report ->
    output(io_lib:format("~p", [Report])),
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
    io:fwrite("~s~n", [nonl(Str)]).

%% replace all '\n' with space and consume all the whitespace after '\n'
nonl(S) ->
  nonl(S, []).

nonl([], Accu) ->
  lists:reverse(Accu);

nonl([$\n|T], Accu) ->
  nonl(nows(T), [$\  | Accu]);

nonl([H|T], Accu) ->
  nonl(T, [H | Accu]).

%% remove all the leading whitespace
nows([]) -> [];
nows([$\  | T]) -> nows(T);
nows(T) -> T.
