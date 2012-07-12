-module(fix_app).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([restart/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

restart() ->
  application:stop(fix),
  application:unload(fix),
  application:start(fix).

start(_StartType, _StartArgs) ->
  case file:path_consult(["."], "fix.conf") of
    {ok, Env, Path} ->
      error_logger:info_msg("Load FIX config from ~s~n", [Path]),
      application:set_env(fix, config, Env);
    {error, enoent} ->
      ok
  end,  
  fix_sup:start_link().

stop(_State) ->
    ok.

