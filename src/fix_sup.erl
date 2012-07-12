-module(fix_sup).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(supervisor).

%% API
-export([start_link/0, start_connection/2, start_server/2]).
-export([start_reader/1, start_stock/2, stop_reader/1]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(Consumer, Options) ->
  supervisor:start_child(fix_connection_sup, [Consumer, Options]).

stop_reader(Name) when is_atom(Name) ->
  SupName = list_to_atom(atom_to_list(Name) ++ "_sup"),
  supervisor:terminate_child(fix_readers_sup, SupName),
  supervisor:delete_child(fix_readers_sup, SupName).

start_reader(Name) when is_atom(Name) ->
  SupName = list_to_atom(atom_to_list(Name) ++ "_sup"),
  supervisor:start_child(fix_readers_sup, {
    SupName,
    {supervisor,start_link,[{local, SupName}, ?MODULE, [reader]]},
    temporary,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
  }),
  
  supervisor:start_child(SupName, {
    fix_reader,
    {fix_reader,start_link,[Name]},
    permanent,
    2000,
    worker,
    []
  }).

start_stock(Reader, Stock) ->
  SupName = list_to_atom(atom_to_list(Reader) ++ "_sup"),
  Reply = supervisor:start_child(SupName, {
    {stock, Stock},
    {gen_event,start_link,[{local, Stock}]},
    permanent,
    2000,
    worker,
    []
  }),
  case Reply of
    {ok, Pid} -> {ok, Pid};
    {error, {{already_started,Pid}, _}} -> {ok, Pid};
    {error, {already_started,Pid}} -> {ok, Pid}
  end.


start_server(Socket, Opts) ->
  supervisor:start_child(fix_server_sup, [Socket, Opts]).



init([fix_server]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
    {   undefined,                               % Id       = internal id
      {fix_server,start_link,[]},             % StartFun = {M, F, A}
            temporary,                               % Restart  = permanent | transient | temporary
            2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                                  % Type     = worker | supervisor
            []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };

init([fix_connection]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
    {   undefined,                               % Id       = internal id
      {fix_connection,start_link,[]},             % StartFun = {M, F, A}
            temporary,                               % Restart  = permanent | transient | temporary
            2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                                  % Type     = worker | supervisor
            []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };


init([readers]) ->
  {ok, {{one_for_one, 0, 60}, []}};

init([reader]) ->
  {ok, {{one_for_all, 2, 60}, []}};

init([]) ->
  Supervisors = [
    {fix_server_sup,
      {supervisor,start_link,[{local, fix_server_sup}, ?MODULE, [fix_server]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {fix_connection_sup,
      {supervisor,start_link,[{local, fix_connection_sup}, ?MODULE, [fix_connection]]},
      temporary,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {fix_readers_sup,
      {supervisor,start_link,[{local, fix_readers_sup}, ?MODULE, [readers]]},
      temporary,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.
