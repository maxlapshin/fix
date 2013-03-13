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
  {ok, Pid} = fix_sup:start_link(),
  case fix:get_value(fix_port, undefined) of
    undefined -> ok;
    Port -> fix:start_listener()
  end,
  {ok, Pid}.
  

stop(_State) ->
    ok.

