-module(fix_sup).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(supervisor).

%% API
-export([start_link/0, start_exec_conn/2, start_worker/3]).
-export([start_read_conn/2, stop_read_conn/1, stocks/1]).
-export([start_reader/1, start_stock/2, stop_stock/2, stop_reader/1]).

%% Supervisor callbacks
-export([init/1]).

%% Public helpers
-export([read_sup/1]).


%% Reader supervisor ID
read_sup(Reader) when is_atom(Reader) ->
  list_to_atom(atom_to_list(Reader) ++ "_sup").

%% Given reader's stocks supervisor
stocks_sup(Reader) when is_atom(Reader) ->
  list_to_atom(atom_to_list(Reader) ++ "_stocks").


%% Public API
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_read_conn(Name, Options) when is_atom(Name) ->
  SupName = read_sup(Name),
  supervisor:start_child(SupName, {connection,
      {fix_read_conn, start_link, [Name, Options]},
      temporary, 3000, worker, []}).

start_exec_conn(Name, Options) when is_atom(Name) ->
  supervisor:start_child(fix_exec_conn_sup, [Name, Options]).


stop_read_conn(Name) when is_atom(Name) ->
  SupName = read_sup(Name),
  % No need to delete temporary child
  supervisor:terminate_child(SupName, connection).

stop_reader(Name) when is_atom(Name) ->
  supervisor:terminate_child(fix_readers_sup, Name),
  supervisor:delete_child(fix_readers_sup, Name).

start_reader(Name) when is_atom(Name) ->
  SupName = read_sup(Name),
  supervisor:start_child(fix_readers_sup, {
      Name,
      {supervisor,start_link,[{local, SupName}, ?MODULE, [reader, Name]]},
      temporary,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }),
  [Manager] = [Pid || {manager, Pid, _, _} <- supervisor:which_children(SupName)],
  {ok, Manager}.


start_stock(Reader, Stock) ->
  Reply = supervisor:start_child(stocks_sup(Reader),
    {Stock, {gen_event,start_link,[{local, Stock}]},
      permanent, 2000, worker, []}),
  case Reply of
    {ok, Pid} -> {ok, Pid};
    {error, {{already_started,Pid}, _}} -> {ok, Pid};
    {error, {already_started,Pid}} -> {ok, Pid}
  end.

stop_stock(Reader, Stock) ->
  SupName = stocks_sup(Reader),
  supervisor:terminate_child(SupName, Stock),
  supervisor:delete_child(SupName, Stock).

stocks(Reader) ->
  SupName = stocks_sup(Reader),
  [Stock || {Stock, _Pid, _Type, _M} <- supervisor:which_children(SupName)].
  

start_worker(ListenerPid, Socket, Opts) ->
  supervisor:start_child(fix_server_sup, [ListenerPid, Socket, Opts]).



%%% Internals

%% Fix server (used for MD proxying)
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

%% Execution connection.
init([fix_exec_conn_sup]) ->
  {ok, {{simple_one_for_one, 5, 60}, [
    {   undefined,                               % Id       = internal id
      {fix_exec_conn,start_link,[]},             % StartFun = {M, F, A}
            temporary,                               % Restart  = permanent | transient | temporary
            2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
            worker,                                  % Type     = worker | supervisor
            []                            % Modules  = [Module] | dynamic
        }
      ]
    }
  };


%% Supervisor for all readers
init([readers]) ->
  {ok, {{one_for_one, 0, 60}, []}};


%% Single reader infrastructure
init([reader, Name]) ->
  MgrSpec = {manager, {fix_read_manager, start_link, [Name]},
    permanent, 5000, worker, []},
  StocksSpec = {stocks, {supervisor, start_link, [{local, stocks_sup(Name)}, ?MODULE, [stocks]]},
    permanent, infinity, supervisor, []},

  {ok, {{rest_for_one, 2, 60}, [MgrSpec, StocksSpec]}};


%% Supervisor for reader's stocks
init([stocks]) ->
  {ok, {{one_for_one, 0, 60}, []}};


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
      {supervisor,start_link,[{local, fix_exec_conn_sup}, ?MODULE, [fix_exec_conn_sup]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    },
    {fix_readers_sup,
      {supervisor,start_link,[{local, fix_readers_sup}, ?MODULE, [readers]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
    }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.
