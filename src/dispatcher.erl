%% A server that keeps track of what worker collects values for which metric.
-module(dispatcher).
-behaviour(gen_server).
-export([start_link/0, stop/0, average/1, record/2]).
-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, code_change/3, terminate/2]).

-define(NAME, ecollectd_dispatcher).
-define(SPEC(Id, Value), #{id => Id, start => {collector, start_link, [Value]}, restart => temporary}).

start_link() ->
  gen_server:start_link({local, ?NAME}, ?MODULE, gb_trees:empty(), []).
 
stop() ->
  gen_server:call(?NAME, stop).

average(Id) ->
  gen_server:call(?NAME, {average, Id}).

record(Id, Value) ->
  gen_server:cast(?NAME, {record, Id, Value}).


%% private

new_collector(Id, Value) ->
  {ok, Pid} = supervisor:start_child(collector_sup, ?SPEC(Id, Value)),
  Pid.


%% gen_server API

init(State) ->
  {ok, State}.

handle_call({average, Id}, _From, State) ->
  Avg = case gb_trees:lookup(Id, State) of
    {value, Pid} ->
      collector:average(Pid);
    none ->
      0 % kinda default value. FIXME: should I just crash?
    end,
  {reply, Avg, State};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({record, Id, Value}, State) ->
  State1 = case gb_trees:lookup(Id, State) of
    {value, Pid} ->
      collector:record(Pid, Value),
      State;
    none ->
      Pid = new_collector(Id, Value),
      _ = erlang:monitor(process, Pid),
      gb_trees:insert(Id, Pid, State)
    end,
  {noreply, State1};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
  {value, {Key, _}} = lists:search(fun({_, P}) -> P == Pid end, gb_trees:to_list(State)),
  {noreply, gb_trees:delete(Key, State)};

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
