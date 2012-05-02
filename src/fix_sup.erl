-module(fix_sup).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(supervisor).

%% API
-export([start_link/0, start_session/2]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Name, Desc) ->
  supervisor:start_child(fix_session_sup, {
    Name, 
    {fix_session, start_link, [Desc]},
    permanent,
    2000,
    worker,
    [fix_session]
  }).


init([fix_session]) ->
  {ok, {{one_for_one, 5, 10}, []}};

init([]) ->
  Supervisors = [
    {fix_session_sup,
      {supervisor,start_link,[{local, fix_session_sup}, ?MODULE, [fix_session]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.
