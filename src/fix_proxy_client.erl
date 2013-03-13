-module(fix_proxy_client).

-compile(export_all).
-include("../include/business.hrl").
-include("../include/fix.hrl").


run(Exchange, Symbol) ->
  {ok, Pid} = fix_connection:start_link(self(), fix:get_value(fix_proxy)),
  fix_connection:logon(Pid),
  fix_connection:subscribe(Pid, Exchange, Symbol),

  loop(Pid).


loop(Pid) ->
  receive
    #fix{pid = Pid, message = #market_data_snapshot_full_refresh{md_entries = MdEntries}} ->
      Bid = [Entry || Entry <- MdEntries, proplists:get_value(md_entry_type,Entry) == bid],
      Ask = [Entry || Entry <- MdEntries, proplists:get_value(md_entry_type,Entry) == offer],
      if
        length(Bid) > 0 andalso length(Ask) > 0 ->
          [BestBid|_] = Bid,
          BestBidSize = proplists:get_value(md_entry_size, BestBid),
          BestBidPrice = proplists:get_value(md_entry_px, BestBid),
          [BestAsk|_] = Ask,
          BestAskSize = proplists:get_value(md_entry_size, BestAsk),
          BestAskPrice = proplists:get_value(md_entry_px, BestAsk),
          io:format("~6.. B@~.3f    ~6.. B@~.3f   (~B deep)~n", [BestBidSize, BestBidPrice*1.0, BestAskSize, BestAskPrice*1.0, length(MdEntries)]),
          ok;
        true ->
          io:format("Trade~n")
      end,
      ok;
    Else ->
      io:format("Else: ~p~n", [Else])
  end,
  loop(Pid).
  