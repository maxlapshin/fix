-module(fix_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-export([subscribe/2, subscribe/3, subscribe/4]).
-export([stock/2, working_stocks/1]).
-export([autostart/1]).
-export([ensure_saving/2, ensure_saving/3]).

-include("log.hrl").

subscribe(Name, Stock) ->
  subscribe(Name, Stock, {fix_msg_sender, [self()]}).

subscribe(Name, Stock, Subscribe) ->
  subscribe(Name, Stock, Subscribe, []).

subscribe(Name, Stock, {M, F, A}, Options) when is_atom(Stock) ->
  subscribe(Name, Stock, {fix_msg_sender, [{M,F,A}]}, Options);

subscribe(Name, Stock, {Module, Args}, Options) when is_atom(Stock) ->
  case stock(Name, Stock) of
    {ok, Pid} ->
      case proplists:get_bool(no_sup, Options) of
        true -> gen_event:add_handler(Pid, Module, Args);
        false -> gen_event:add_sup_handler(Pid, Module, Args)
      end,
      {ok, Pid};
    {error, _} = Error ->
      Error
  end.


stock(Name, Stock) ->
  autostart(Name),
  case lists:member(Stock, working_stocks(Name)) of
    true ->
      {ok, erlang:whereis(Stock)};
    false ->
      case fix_read_manager:new_stock(Name, Stock) of
        ok ->
          fix_market_data:attach(Stock),
          {ok, erlang:whereis(Stock)};
        {error, _} = Error ->
          Error
      end
  end.


% Ensure there is installed stockdb_saver in specified stock event handler
ensure_saving(Name, Stock) ->
  ensure_saving(Name, Stock, []).

ensure_saving(Name, Stock, Options) ->
  case stock(Name, Stock) of
    {ok, StockPid} -> do_add_save_handler(StockPid, Stock, Options);
    Other -> Other
  end.

do_add_save_handler(StockPid, Stock, Options) ->
  HandlerOptions = [
      {id, fix_reader}, % Logically connect saver to this function
      {transform, {fix_market_data, to_stockdb, []}} ],
  case stockdb_saver:add_handler(StockPid, Stock, HandlerOptions ++ Options) of
    ok -> {ok, StockPid};
    Other -> Other
  end.


autostart(Name) when is_atom(Name) ->
  case erlang:whereis(Name) of
    undefined -> 
      case fix_sup:start_reader(Name) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
      end;
    Pid -> Pid
  end.


working_stocks(Name) ->
  fix_sup:stocks(Name).

