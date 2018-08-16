-module(collector_sup).
-export([start_link/0, init/1]).
-behaviour(supervisor).

-define(NAME, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?NAME}, ?MODULE, []).
 
init([]) ->
  MaxRestart = 5,
  MaxTime = 3600,
  {ok, { {one_for_one, MaxRestart, MaxTime}, [] } }.
