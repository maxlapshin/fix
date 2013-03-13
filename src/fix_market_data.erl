-module(fix_market_data).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_event).

% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
% API
-export([get/1, get/2, get/3, attach/1, attach/2]).
-export([to_stockdb/1, to_stockdb/2, transform/1]).
-export([best_price/2]).

-include("../include/business.hrl").
-include("log.hrl").

-compile({no_auto_import,[get/1]}).

attach(Handler) ->
  attach(Handler, []).

attach(Handler, Args) ->
  case lists:member(fix_market_data, gen_event:which_handlers(Handler)) of
    true ->
      ok;
    false ->
      gen_event:add_handler(Handler, fix_market_data, Args)
  end.


get(Stock) when is_atom(Stock) ->
  case erlang:whereis(Stock) of
    undefined -> [];
    Pid -> case get(Pid) of
      Reply when is_list(Reply) -> Reply;
      {error, bad_module} ->
        attach(Stock),
        case get(Pid) of
          {error, Error} -> error({fix_market_data,Stock,Error});
          Reply -> Reply
        end;
      {error, Error} -> error({fix_market_data,Stock,Error})
    end
  end;

get(Pid) when is_pid(Pid) ->
  case gen_event:call(Pid, ?MODULE, info) of
    Info when is_list(Info) ->
      parse_info(Info);
    Else ->
      Else
  end.

parse_info(Info) ->
  Now = fix:utc_ms(),
  UTC = proplists:get_value(utc, Info, 0),
  Delay = Now - UTC,
  [{delay,Delay}|Info].


get(Stock, Field) ->
  get(Stock, Field, undefined).

get(Stock, Field, Default) ->
  case get(Stock) of
    [_|_] = Info ->
      proplists:get_value(Field, Info, Default);
    _ ->
      Default
  end.

-spec best_price(Stock::atom(), Side::buy|sell) -> float()|undefined.
best_price(Stock, buy) ->
  get(Stock, ask);
best_price(Stock, sell) ->
  get(Stock, bid).

transform(#market_data_snapshot_full_refresh{symbol = Stock} = FR) ->
  case to_stockdb(FR) of
    {md, UTC, [{BestBid, _}|_] = Bid, [{BestAsk, _}|_] = Ask} ->
      Depth = erlang:max(erlang:length(Bid), erlang:length(Ask)),
      [{bid,BestBid}, {ask,BestAsk}, {stock,Stock}, {utc,UTC}, {depth, Depth}];
    _ ->
      undefined
  end;

transform(_) ->
  undefined.

% Parse single MD entry
-record(md_entry, {type, px, size}).

parse_md_entry(Fields) ->
  parse_md_entry(Fields, #md_entry{}).

parse_md_entry([], Entry) ->
  Entry;
parse_md_entry([{md_entry_type, Type}|Fields], #md_entry{} = Entry) ->
  parse_md_entry(Fields, Entry#md_entry{type = Type});
parse_md_entry([{md_entry_px, Px}|Fields], #md_entry{} = Entry) ->
  parse_md_entry(Fields, Entry#md_entry{px = Px});
parse_md_entry([{md_entry_size, Size}|Fields], #md_entry{} = Entry) ->
  parse_md_entry(Fields, Entry#md_entry{size = Size});
parse_md_entry([_|Fields], Entry) ->
  parse_md_entry(Fields, Entry).

% Date to millisecond timestamp convertion
date_to_ms({{_Y,_Mon,_D} = Day,{H,Min,S}}) ->
  date_to_ms({Day, {H,Min,S, 0}});
date_to_ms({{_Y,_Mon,_D} = Day,{H,Min,S, Milli}}) ->
  GregSeconds_Zero = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
  GregSeconds_Now = calendar:datetime_to_gregorian_seconds({Day,{H,Min,S}}),
  (GregSeconds_Now - GregSeconds_Zero)*1000 + Milli.

% Convert incoming market data packet to stockdb event
to_stockdb(FR) ->
  case to_stockdb(FR, []) of
    [Evt|_] -> Evt;
    _ -> undefined
  end.

% Extract all possible stockdb events from market data
to_stockdb(#market_data_snapshot_full_refresh{sending_time = BrokerTime, md_entries = MdEntries}, _Options) ->
  % Extract timestamp
  Date = fix_splitter:parse_date(BrokerTime),
  UTC = date_to_ms(Date),

  % Parse all md entries
  ParsedEntries = [parse_md_entry(E) || E <- MdEntries],

  % Sort entries by type in single run
  {RevBid, RevAsk, Trade, Left} = lists:foldl(fun
      (#md_entry{type = bid, px = Price, size = Volume}, {RevBid, RevAsk, Trade, Left}) ->
        {[{Price, Volume}|RevBid], RevAsk, Trade, Left};
      (#md_entry{type = offer, px = Price, size = Volume}, {RevBid, RevAsk, Trade, Left}) ->
        {RevBid, [{Price, Volume}|RevAsk], Trade, Left};
      (#md_entry{type = trade, px = Price, size = Volume}, {RevBid, RevAsk, Trade, Left}) ->
        {RevBid, RevAsk, [{Price, Volume}|Trade], Left};
      (#md_entry{} = Unknown, {RevBid, RevAsk, Trade, Left}) ->
        {RevBid, RevAsk, Trade, [Unknown|Left]}
    end, {[], [], [], []}, ParsedEntries),

  % Print warning if there are unknown entries
  Left == [] orelse ?D({unparsed, Left}),

  % Back to original order
  Bid = lists:reverse(RevBid),
  Ask = lists:reverse(RevAsk),

  % Make an event from each trade entry (usually only one)
  TradeEvents = [{trade, UTC, Price, Volume} || {Price, Volume} <- Trade],

  % If there is non-empty bid or ask, also return md event
  case {Bid, Ask} of
    {[],[]} ->
      TradeEvents;
    {_, _} ->
      MD = {md, UTC, Bid, Ask},
      [MD | TradeEvents]
  end;

to_stockdb(_, _) -> undefined.


init([]) ->
  MFA = {?MODULE, transform, []},
  init([MFA]);

init([{M,F,A}]) ->
  {ok, {{M,F,A}, []}}.


handle_event(Event, {{M,F,A} = MFA, _OldInfo} = State) ->
  case erlang:apply(M,F,[Event|A]) of
    undefined -> {ok, State};
    Info when is_list(Info) -> {ok, {MFA, Info}}
  end.


handle_call(info, {_MFA, Info} = State) ->
  {ok, Info, State};

handle_call(_Call, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.



terminate(_,_) ->  ok.
code_change(_,State,_) -> {ok, State}.

