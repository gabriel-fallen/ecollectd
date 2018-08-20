%%%-------------------------------------------------------------------
%% @doc ecollectd public API
%% @end
%%%-------------------------------------------------------------------

-module(ecollectd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([report/2, average/1]).

-define(SPEC(Id, Value), #{id => Id, start => {collector, start_link, [Value]}, restart => temporary}).


%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ecollectd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  case whereis(ecollectd_sup) of
    P when is_pid(P) -> exit(P, kill);
    _ -> ok
  end.

report(Id, Value) ->
  case ets:lookup(ecollectd_collectors, Id) of
    [{Id, Pid}] ->
      case is_process_alive(Pid) of
        true  -> collector:record(Pid, Value);
        false ->
          Pid = new_collector(Id, Value),
          ets:insert(ecollectd_collectors, {Id, Pid})
      end;
    [] ->
      Pid = new_collector(Id, Value),
      ets:insert(ecollectd_collectors, {Id, Pid})
  end,
  ok.

average(Id) ->
  case ets:lookup(ecollectd_collectors, Id) of
    [{Id, Pid}] ->
      case is_process_alive(Pid) of
        true  -> collector:average(Pid);
        false -> 0
      end;
    [] ->
      0 % kinda default value. FIXME: should I just crash?
  end.

%%====================================================================
%% Internal functions
%%====================================================================

new_collector(Id, Value) ->
  case supervisor:start_child(ecollectd_sup, ?SPEC(Id, Value)) of
    {ok, Pid} -> Pid;
    {error, {already_started, Pid}} -> % FIXME: why is this happening at all?
      collector:record(Pid, Value),
      Pid
  end.
