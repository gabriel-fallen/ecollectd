%%%-------------------------------------------------------------------
%% @doc ecollectd public API
%% @end
%%%-------------------------------------------------------------------

-module(ecollectd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([report/2, average/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ecollectd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

report(Id, Value) ->
  dispatcher:record(Id, Value).

average(Id) ->
  dispatcher:average(Id).

%%====================================================================
%% Internal functions
%%====================================================================
