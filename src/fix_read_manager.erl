-module(fix_read_manager).
-author({"Danil Zagoskin", 'z@gosk.in'}).

-include("log.hrl").
-include("../include/fix.hrl").

% Public API
-export([start_link/1, new_stock/2]).
-export([status/1]).

% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, terminate/2]).

% exported utility for testability
-export([connect_after/1]).

-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_DELAY, 5000).

-record(mgr, {
    name,
    reconnect_timer,
    conn,
    requests = [],
    failcount = 0
  }).


start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

new_stock(Name, Stock) ->
  gen_server:call(Name, {new_stock, Stock}).

status(Name) ->
  gen_server:call(Name, status).

% mockable utility function
connect_after(Timeout) ->
  erlang:start_timer(Timeout, self(), connect).


init([Name]) ->
  Timer = ?MODULE:connect_after(0),
  {ok, #mgr{
      name = Name,
      reconnect_timer = Timer}}.


% Reconnect timeout
handle_info({timeout, Ref, connect}, #mgr{reconnect_timer = Ref} = Mgr) when is_reference(Ref) ->
  {noreply, try_connect(Mgr)};

% parasite timeout. Ignore.
handle_info({timeout, _Ref, connect}, #mgr{} = Mgr) ->
  {noreply, Mgr};

% Termination of working connection
handle_info({'DOWN', _, _, Conn, Reason}, #mgr{name = _Name, conn = Conn} = Mgr) ->
  ?D({connection_down, _Name, get_error(Reason)}),
  {noreply, try_connect(Mgr)};

handle_info({stock_available, Stock}, #mgr{requests = Requests} = Mgr) ->
  {Send, Save} = lists:partition(fun({S,_}) -> S == Stock end, Requests),
  [gen_server:reply(From, ok) || {_,From} <- Send],
  {noreply, Mgr#mgr{requests = Save}};

handle_info({reject, Stock, Reject}, #mgr{name = Name, requests = Requests} = Mgr) ->
  ?D({reject, Name, Stock, Reject}),
  % Stop stock. It will respawn if needed
  fix_sup:stop_stock(Name, Stock),
  {Send, Save} = lists:partition(fun({S,_}) -> S == Stock end, Requests),
  [gen_server:reply(From, {error, rejected}) || {_,From} <- Send],
  {noreply, Mgr#mgr{requests = Save}};
  
handle_info(#fix{} = Fix, #mgr{name = _Name} = Mgr) ->
  ?D({unknown_fix, _Name, Fix}),
  {noreply, Mgr}.

handle_call(status, _From, #mgr{conn = Conn} = Mgr) ->
  Status = case Conn of
    undefined -> connecting;
    _ -> connected
  end,
  {reply, Status, Mgr};

handle_call({new_stock, Stock}, From, #mgr{requests = Requests, conn = Conn, name = Name} = Mgr) ->
  case lists:member(Stock, fix_sup:stocks(Name)) of
    true ->
      {reply, ok, Mgr};
    false ->
      {ok, _Pid} = fix_sup:start_stock(Name, Stock),
      maybe_subscribe(Conn, Stock),
      {noreply, Mgr#mgr{requests = [{Stock,From}|Requests]}}
  end;

handle_call(_, _, #mgr{} = Mgr) ->
  {reply, {error, not_implemented}, Mgr}.


terminate(_, _) ->
  ok.


% Connection logic
try_connect(#mgr{name = Name} = Mgr) ->
  {ok, Conn} = fix_sup:start_read_conn(Name, []),
  case (catch fix_read_conn:connect(Conn, ?CONNECT_TIMEOUT)) of
    ok ->
      erlang:monitor(process, Conn),
      resubscribe_stocks(Mgr#mgr{conn = Conn, reconnect_timer = undefined, failcount = 0});
    Error ->
      Timer = ?MODULE:connect_after(?RECONNECT_DELAY),
      fix_sup:stop_read_conn(Name),
      log_error(Error, Mgr#mgr{reconnect_timer = Timer})
  end.

get_error({'EXIT', {Reason, _Call}}) ->
  {conn_died, get_error(Reason)};
get_error({shutdown, Reason}) ->
  get_error(Reason);
get_error({error, Reason}) ->
  Reason;
get_error(Error) ->
  Error.


log_error(Error, #mgr{name = Name, failcount = FC} = Mgr) ->
  case log_needed(FC) of
    true ->
      ?D({connection_failed, Name, get_error(Error), FC});
    false ->
      ok
  end,
  Mgr#mgr{failcount = FC + 1}.


log_needed(FC) when FC < 10 ->
  true;
log_needed(FC) when FC < 100 ->
  FC rem 10 == 0;
log_needed(FC) ->
  FC rem 50 == 0.

resubscribe_stocks(#mgr{name = Name, conn = Conn} = Mgr) ->
  Stocks = fix_sup:stocks(Name),
  [fix_read_conn:subscribe(Conn, Stock) || Stock <- Stocks],
  Mgr.



maybe_subscribe(undefined, _Stock) ->
  ok;
maybe_subscribe(Conn, Stock) ->
  % Use catch to avoid crash when connection suddenly dies
  (catch fix_read_conn:subscribe(Conn, Stock)).
