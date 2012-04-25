-module(fix_app).
-include("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  case file:path_consult("fix.conf", ["."]) of
    {ok, Env, _} ->
      application:set_env(fix, config, Env);
    _ ->
      ok
  end,  
  fix_sup:start_link().

stop(_State) ->
    ok.

