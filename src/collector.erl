%% A worker that keeps values for a single metric for fixed time period.
-module(collector).
-behaviour(gen_server).
-export([start_link/1, stop/1, average/1, record/2]).
-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, code_change/3, terminate/2]).

-define(PERIOD, 60000). % 1 minute = 60000 ms
-record(measure, {value, timestamp}).

%% Our state is just a stack (list) of measures, i.e. {value, timestamp} pairs.
%% New values are just pushed on top of the stack and old values (older than 1 minute)
%% get discarded on a timeout (about every minute).
%% This scheme should work good enough if we receive not too many measures per second
%% (say, less than a 1000). With high rate of incoming messages we'll get skewed
%% average and might not be able to timely process all incoming messages
%% (average and timeout will incure long pauses).
%% Alternative approach: employ a heap structure sorted on a timestamp.

start_link(Value) ->
  gen_server:start_link(?MODULE, [mkmeasure(Value)], []).
 
stop(Pid) ->
  gen_server:call(Pid, stop).


average(Pid) ->
  gen_server:call(Pid, average).

record(Pid, Value) ->
  gen_server:cast(Pid, {record, Value}).


%% private functions

mkmeasure(Value) ->
  #measure{value = Value, timestamp = erlang:monotonic_time(millisecond)}.


%% gen_server API

init(State) ->
  erlang:send_after(?PERIOD, self(), timeout),
  {ok, State}.

% Returning 0 when we had no measures for the last minute.
handle_call(average, _From, []) ->
  {reply, 0, []};

handle_call(average, _From, State) ->
  {Sum, Count} = lists:foldl(fun(#measure{value = V}, {S, C}) -> {S + V, C + 1} end, {0, 0}, State),
  {reply, Sum/Count, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({record, Value}, State) ->
  M = mkmeasure(Value),
  {noreply, [M | State]};

handle_cast(_Msg, State) ->
  {noreply, State}.

% Looks like we haven't got any measures for two PERIODs,
% shut self down to save some resources. Dispatcher will start
% fresh one when will get new measure for the same id.
handle_info(timeout, []) ->
  {stop, normal, ok, []};

handle_info(timeout, State) ->
  Now = erlang:monotonic_time(millisecond),
  State1 = lists:takewhile(fun(#measure{timestamp = T}) -> Now - T < ?PERIOD end, State),
  erlang:send_after(?PERIOD, self(), timeout),
  {noreply, State1};

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
