-module(ecollectd_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([load/1, read/1]).

-define(N, 12000).
-define(TIMES, 12000). % 12000 * 100ms = 2 minutes
-define(TIMEOUT, 100). % 100 ms


all() -> [{group, stress_test}].
groups() -> [
  {stress_test, [parallel], [load, read]}
].

init_per_group(stress_test, Config) ->
  ok = application:ensure_started(ecollectd),
  Config;

init_per_group(_, Config) ->
  Config.

end_per_group(stress_test, _Config) ->
  application:stop(ecollectd),
  ok;

end_per_group(_, _Config) ->
  ok.

load(_Config) -> start_testers(?N).

read(_Config) -> read_loop(<<"test", 22>>, ?TIMES).

%% generate individual tests

start_testers(0) -> ok;
start_testers(N) ->
  Name = <<"test", N>>,
  spawn_link(tester(Name)),
  start_testers(N-1).

tester(Name) -> fun() -> write_loop(Name, ?TIMES) end.

write_loop(_, 0) -> ok;
write_loop(Name, N) ->
  ecollectd_app:report(Name, 42), % whatever value
  timer:sleep(?TIMEOUT),
  write_loop(Name, N-1).

read_loop(_, 1) -> ok;
read_loop(Name, N) ->
  timer:sleep(?TIMEOUT), % let writer time to write
  42.0 = ecollectd_app:average(Name), % the same value
  read_loop(Name, N-1).
