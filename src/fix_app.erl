-module(fix_app).
-include("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  fix_sup:start_link().

stop(_State) ->
    ok.

