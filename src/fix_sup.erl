-module(fix_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_connection/2]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(Name, Desc) ->
  supervisor:start_child(fix_connection_sup, {
    Name, 
    {fix_connection, start_link, [Desc]},
    permanent,
    2000,
    worker,
    [fix_connection]
  }).


init([fix_connection]) ->
  {ok, {{one_for_one, 5, 10}, []}};

init([]) ->
  Supervisors = [
    {fix_connection_sup,
      {supervisor,start_link,[{local, fix_connection_sup}, ?MODULE, [fix_connection]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.
