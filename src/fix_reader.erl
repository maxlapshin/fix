-module(fix_reader).
-author('Max Lapshin <max@maxidoors.ru>').

-export([start_link/0, start_link/1]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).

-export([subscribe/2, subscribe/3, subscribe/4, stock_sender/2]).
-export([order_status/2]).

-export([working_stocks/1]).

-include("log.hrl").
-include("../include/business.hrl").



start_link() ->
  gen_server:start_link(?MODULE, [], []).


start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

subscribe(Name, Stock) ->
  subscribe(Name, Stock, {fix_msg_sender, [self()]}).

subscribe(Name, Stock, {Module, Args}) ->
  subscribe(Name, Stock, {Module, Args}, []).

subscribe(Name, Stock, {Module, Args}, Options) ->
  {ok, Pid} = stock_sender(Name, Stock),
  case proplists:get_bool(no_sup, Options) of
    true -> gen_event:add_handler(Pid, Module, Args);
    false -> gen_event:add_sup_handler(Pid, Module, Args)
  end.


stock_sender(Name, Stock) ->
  gen_server:call(autostart(Name), {get_stock, Stock}).


order_status(Side, OrderId) ->
  order_status(fix_write, Side, OrderId).

order_status(Pid, Side, OrderId) ->
  gen_server:call(autostart(Pid), {order_status, Side, OrderId}).



autostart(Pid) when is_pid(Pid) ->
  Pid;

autostart(Name) when is_atom(Name) ->
  case erlang:whereis(Name) of
    undefined -> 
      case fix_sup:start_reader(Name) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
      end;
    Pid -> Pid
  end.


-record(feed, {
  fix,
  name,
  stocks = []
}).

-record(stock, {
  req_id,
  stock,
  pid
}).

init([Name]) ->
  self() ! resubscribe_on_restart,
  {ok, #feed{name = Name}}.

working_stocks(Name) ->
  [Stock || {{stock,Stock}, _, _, _} <- supervisor:which_children(list_to_atom(atom_to_list(Name) ++ "_sup"))].

handle_info(resubscribe_on_restart, #feed{name = Name} = Feed) ->
  Feed1 = lists:foldl(fun(Stock, Feed_) ->
    ?D({reload_stock,Stock}),
    {ok, _, Feed1_} = get_stock1(Stock, Feed_),
    Feed1_
  end, Feed, working_stocks(Name)),
  {noreply, Feed1};

handle_info({fix, Fix, #market_data_snapshot_full_refresh{md_req_id = MsgReqId} = Msg},
  #feed{stocks = Stocks, fix = Fix} = Feed)
->
  Workers = [Stock || #stock{req_id = ReqId} = Stock <- Stocks, ReqId == MsgReqId],
  % Such cryptic way to find required pid is only for detecting that there is only one
  % #stock with required MsgReqId
  case Workers of
    [] ->
      % Parasite stream, need to unsubscribe
      ?D({parasite, MsgReqId, Msg#market_data_snapshot_full_refresh.symbol}),
      ok;
    [Worker] ->
      send_data(Worker, Msg)
  end,
  {noreply, Feed};


handle_info({fix, Fix, #market_data_request_reject{md_req_id = MsgReqId} = Msg}, #feed{stocks = Stocks, fix = Fix} = Feed) ->
  [Worker] = [Stock || #stock{req_id = ReqId} = Stock <- Stocks, ReqId == MsgReqId],
  send_reject(Worker, Msg),
  {noreply, Feed};

handle_info({fix, Fix, Msg}, #feed{fix = Fix} = Feed) ->
  ?D({unknown_fix,Msg}),
  {noreply, Feed};
  
handle_info({'DOWN', _, process, Fix, _}, #feed{fix = Fix} = Feed) ->
  {stop, {fix_dead, Fix}, Feed};


% This code should not ever work due to supervisor tree
%
% handle_info({'DOWN', _, process, DeadPid, _}, #feed{fix = Fix, stocks = Stocks} = Feed) ->
%   {DeadInstr, NewInstrList} = lists:partition(fun(#stock{pid = Pid}) -> Pid == DeadPid end, Stocks),
%   lists:foreach(
%     fun(#stock{req_id = ReqId, stock = Stock}) ->
%         case [x || #stock{req_id = ReqId_} <- NewInstrList, ReqId_ == ReqId] of
%           [] ->
%             {Exchange, Symbol} = fix:stock_to_instrument(Stock),
%             % No one uses that req_id any more
%             fix_connection:unsubscribe(Fix, ReqId, Exchange, Symbol);
%           [_] ->
%             ok
%         end
%     end, DeadInstr),
%   {noreply, Feed#feed{stocks = NewInstrList}};
%   

handle_info(Msg, #feed{} = Feed) ->
  ?D({unknown,Msg}),
  % {noreply, Feed}.
  {stop, {unknown_msg, Msg}, Feed}.

handle_call({order_status, Side, OrderId}, _From, #feed{} = Feed) ->
  Feed1 = autoconnect(Feed),
  fix_connection:order_status(Feed1#feed.fix, Side, OrderId),
  {reply, ok, Feed1};

handle_call({get_stock, Stock}, _From, #feed{} = Feed) ->
  {ok, Pid, Feed1} = get_stock1(Stock, Feed), 
  {reply, {ok, Pid}, Feed1};
      
handle_call(Call, _From, #feed{} = Feed) ->
  {reply, {error1, Call}, Feed}.



get_stock1(Stock, #feed{stocks = Stocks, name = Name} = Feed) ->
  case lists:keyfind(Stock, #stock.stock, Stocks) of
    false ->
      {Exchange, Symbol} = fix:stock_to_instrument(Stock),
      % No known instr, start and register one
      {ok, NewInstrPid} = fix_sup:start_stock(Name, Stock),
      % {ok, NewInstrPid} = gen_event:start_link(),
      erlang:monitor(process, NewInstrPid),
      ConnFeed = autoconnect(Feed),
      {ok, ReqId} = fix_connection:subscribe(ConnFeed#feed.fix, Exchange, Symbol),
      Entry = #stock{pid = NewInstrPid, stock = Stock, req_id = ReqId},
      ?D({new_instr, Stock}),
      {ok, NewInstrPid, ConnFeed#feed{stocks = [Entry|Stocks]}};

    #stock{pid = KnownPid} ->
      {ok, KnownPid, Feed}
  end.



send_data(#stock{pid = Pid} = Instr, Msg) ->
  % ?D({fix, Instr}),
  gen_event:notify(Pid, Msg),
  Instr.

send_reject(#stock{pid = Pid} = Instr, Msg) ->
  gen_event:notify(Pid, Msg),
  Instr.


autoconnect(#feed{fix = undefined, name = Name} = Feed) ->
  {ok, F} = fix_sup:start_connection(self(), fix_connection:options(Name)),
  erlang:monitor(process, F),
  link(F),
  ok = fix_connection:logon(F),
  Feed#feed{fix = F};

autoconnect(Feed) ->
  Feed.


terminate(_, _) -> ok.

-include_lib("eunit/include/eunit.hrl").

subscribe_test() ->
  {spawn,
  {setup,
    fun() ->
      meck:new(?MODULE),
      meck:expect(?MODULE, autoconnect, fun(Feed) -> Feed#feed{fix = fix_pid} end),
      start_link()
    end,
    fun({ok, _Reader}) ->
      ok
    end,
    fun({ok, Reader}) ->
      Reader ! stop,
      meck:unload(?MODULE)
    end
  }
  }.












