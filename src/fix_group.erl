-module(fix_group).
-author('Max Lapshin <max@maxidoors.ru>').


-include("../include/business.hrl").
-include("log.hrl").

-export([postprocess/1]).


fetch_md_entries(Fields) ->
  fetch_md_entries(Fields, []).

fetch_md_entries([], []) ->
  {[], []};

fetch_md_entries([{no_md_entries,0}|Fields], Acc) ->
  {[], []};

fetch_md_entries([{no_md_entries,_Count},{md_entry_type,Type}|Fields], Acc) ->
  {Result, Fields1} = fetch_md_entries(Fields, [{md_entry_type,Type}], []),
  {Result, lists:reverse(Acc) ++ Fields1};

fetch_md_entries([Field|Fields], Acc) ->
  fetch_md_entries(Fields, [Field|Acc]).

fetch_md_entries([{md_entry_type,Type}|Fields], Group, Groups) ->
  fetch_md_entries(Fields, [{md_entry_type,Type}], [lists:reverse(Group)|Groups]);

fetch_md_entries([{md_entry_px,Value}|Fields], Group, Groups) ->
  fetch_md_entries(Fields, [{md_entry_px,Value}|Group], Groups);

fetch_md_entries([{md_entry_size,Value}|Fields], Group, Groups) ->
  fetch_md_entries(Fields, [{md_entry_size,Value}|Group], Groups);

fetch_md_entries(Fields, Group, Groups) ->
  {lists:reverse([lists:reverse(Group)|Groups]), Fields}.

postprocess(#market_data_snapshot_full_refresh{fields = Fields} = Record) ->
  {MdEntries, Fields1} = fetch_md_entries(Fields),
  Record#market_data_snapshot_full_refresh{md_entries = MdEntries, fields = Fields1};

postprocess(#execution_report{cl_ord_id = ClOrdId, orig_cl_ord_id = OrigClOrdId} = Record) ->
  Record#execution_report{cl_ord_id = to_i(ClOrdId), orig_cl_ord_id = to_i(OrigClOrdId)};

postprocess(#order_cancel_reject{cl_ord_id = ClOrdId, orig_cl_ord_id = OrigClOrdId} = Record) ->
  Record#order_cancel_reject{cl_ord_id = to_i(ClOrdId), orig_cl_ord_id = to_i(OrigClOrdId)};

postprocess(Record) ->
  Record.


to_i(undefined) -> undefined;
to_i(Bin) when is_binary(Bin) ->
  try list_to_integer(binary_to_list(Bin)) of
    Result -> Result
  catch
    _Error:_ -> Bin
  end.
  