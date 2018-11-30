%%------------------------------------------------------------------------------
%% @doc erllambda_config_srv
%%
%% Erllambda configs gen_server
%%
%% @copyright 2018 Alert Logic, Inc.
%%------------------------------------------------------------------------------
-module(erllambda_config_srv).

%% private - for serializing and refreshing caches
-export([get/1, set/3, evict/1, serialize/2, schedule/2]).

%% gen_server callbacks
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([start_link/0]).

-include("erllambda.hrl").
-include("exception.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%******************************************************************************
%% Record definitions
%%******************************************************************************
-record(cv, {
          key :: any(),
          value :: any(),
          expire :: undefined | pos_integer()
}).

%%******************************************************************************
%% API functions
%%******************************************************************************
get( Key ) ->
    case ets:lookup( ?CACHE_NAME, Key ) of
        [#cv{value=Value}] -> {ok, Value};
        [] -> {error, not_found}
    end.

set( Key, Value, Expire ) ->
    ets:insert( ?CACHE_NAME, #cv{key=Key, value=Value, expire=Expire} ).


evict(Key) ->
    true = ets:delete(?CACHE_NAME, Key).


serialize( Key, Generate ) ->
    gen_server:call( ?MODULE, {serialize, Key, Generate}, infinity ).


schedule( TimestampMs, Run ) ->
    erlang:send_after( TimestampMs, ?MODULE, {run, Run}, [{abs, true}] ).


%%%--------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, any()}.
%%%--------------------------------------------------------------------------
%% @doc Start server and link to caller
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%******************************************************************************
%% gen_server callbacks
%%******************************************************************************
%% @private
init( [] ) ->
    ets:new( ?CACHE_NAME, [public, named_table, {keypos, #cv.key},
                           {read_concurrency, true}] ),
    {ok, #{waiters => dict:new(), processes => dict:new()}}.


%% @private
handle_call({serialize, Key, Generate}, From, State) ->
    NewState = serialize_start( Key, Generate, From, State ),
    {noreply, NewState};
handle_call( _Request, _From, State ) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast( {complete, Pid, Key, Result}, State ) ->
    NewState = serialize_complete( Pid, Key, Result, State ),
    {noreply, NewState};
handle_cast( _Info, State ) -> {noreply, State}.

%% @private
handle_info( {run, Run}, #{} = State ) ->
    %% just fire it and forget it
    spawn( Run ),
    {noreply, State};
handle_info( {'DOWN', _MonitorRef, process, Pid, Reason}, #{} = State ) ->
    NewState = serialize_fail( Pid, Reason, State ),
    {noreply, NewState};
handle_info( _Info, State ) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%******************************************************************************
%% Internal functions
%%******************************************************************************
serialize_start( Key, Generate, From,
                 #{waiters := Waiters, processes := Processes} = State ) ->
    case dict:find( Key, Waiters ) of
        {ok, FromList} ->
            NewWaiters = dict:store( Key, [From | FromList], Waiters ),
            State#{waiters := NewWaiters};
        error ->
            {Pid, Monitor} = serialize_run( Key, Generate ),
            Process = #{monitor => Monitor, key => Key},
            NewProcesses = dict:store( Pid, Process, Processes ),
            NewWaiters = dict:store( Key, [From], Waiters ),
            State#{waiters := NewWaiters, processes := NewProcesses}
    end.

            
serialize_complete( Pid, Key, Result,
                    #{waiters := Waiters, processes := Processes} = State ) ->
    FromList = dict:fetch( Key, Waiters ),
    %% disable the monitor we had on the process
    #{monitor := Monitor} = dict:fetch( Pid, Processes ),
    erlang:demonitor( Monitor, [flush] ),
    NewProcesses = dict:erase( Pid, Processes ),
    %% reply to all waiters
    [gen_server:reply( From, Result ) || From <- FromList],
    NewWaiters = dict:erase( Key, Waiters ),
    State#{waiters := NewWaiters, processes := NewProcesses}.


serialize_fail( Pid, Reason,
                #{waiters := Waiters, processes := Processes} = State ) ->
    #{key := Key} = dict:fetch( Pid, Processes ),
    FromList = dict:fetch( Key, Waiters ),
    [gen_server:reply( From, Reason ) || From <- FromList],
    NewWaiters = dict:erase( Key, Waiters ),
    NewProcesses = dict:erase( Pid, Processes ),
    State#{waiters := NewWaiters, processes := NewProcesses}.
            
            
serialize_run( Key, Generate ) ->
    Parent = self(),
    spawn_monitor( fun() ->
                           Result =
                               try
                                   Generate()
                               catch
                                   ?EXCEPTION(Type, Reason, Stacktrace) ->
                                       {error, {exception, Type, Reason, ?GET_STACK(Stacktrace)}}
                               end,
                           Message = {complete, self(), Key, Result},
                           gen_server:cast( Parent, Message )
                   end ).

%%******************************************************************************
%% Test functions
%%******************************************************************************
-ifdef(TEST).
cache_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [
         {"get_cold",
          fun() ->
              ?assertMatch( {error,not_found}, erllambda_config_srv:get( foo ) )
          end},

         {"get_warm",
          fun() ->
              ?assertMatch( true, erllambda_config_srv:set(foo, bar, 1000)),
              ?assertMatch( {ok, bar}, erllambda_config_srv:get( foo ) )
          end}
     ]}.

setup() ->
    init([]).

cleanup(_) ->
    ok.

-endif.
