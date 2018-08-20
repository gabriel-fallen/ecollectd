-module(ecollectd_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([avg_empty/1, avg_immediate/1, avg_after_min/1, load/1, read/1]).

-define(N, 12000).
-define(TIMES, 12000). % 12000 * 100ms = 2 minutes
-define(TIMEOUT, 100). % 100 ms


all() -> [{group, correctness}, {group, stress_test}].
groups() -> [
  {correctness, [], [avg_empty, avg_immediate, avg_after_min]},
  {stress_test, [parallel], [load, read]}
].

init_per_group(correctness, Config) ->
  ok = application:ensure_started(ecollectd),
  Config;

init_per_group(stress_test, Config) ->
  ok = application:ensure_started(ecollectd),
  Config;

init_per_group(_, Config) ->
  Config.

end_per_group(correctness, _Config) ->
  application:stop(ecollectd),
  ok;

end_per_group(stress_test, _Config) ->
  application:stop(ecollectd),
  ok;

end_per_group(_, _Config) ->
  ok.

% correctness group

avg_empty(_Config) ->
  0 = ecollectd_app:average(<<"correctness", 11>>).

avg_immediate(_Config) ->
  Name = <<"correctness", 22>>,
  ecollectd_app:report(Name, 1.1),
  ecollectd_app:report(Name, 2.2),
  ecollectd_app:report(Name, 3.3),
  Avg = ecollectd_app:average(Name),
  if
    abs(Avg - 2.2) < 0.0001 -> ok
  end.

avg_after_min(_Config) ->
  Name = <<"correctness", 33>>,
  ecollectd_app:report(Name, 1.1),
  ecollectd_app:report(Name, 2.2),
  ecollectd_app:report(Name, 3.3),
  timer:sleep(60000), % 1 minute
  ecollectd_app:report(Name, 21),
  ecollectd_app:report(Name, 22),
  ecollectd_app:report(Name, 23),
  Avg = ecollectd_app:average(Name),
  if
    abs(Avg - 22) < 0.0001 -> ok
  end.

%% stress group

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
  ecollectd_app:average(Name), % whatever the value, it should not fail
  read_loop(Name, N-1).
