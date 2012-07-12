-module(fix_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("../include/admin.hrl").
-include("../include/business.hrl").

-compile(export_all).

encode_test() ->
  ?assertEqual(fix:sample_fix(), iolist_to_binary(fix:encode(fix_splitter:split(fix:sample_fix())))).

decode_test() ->
  ?assertMatch([{msg_type,market_data_snapshot_full_refresh}, {msg_seq_num,3}, 
  {sending_time,<<"20120426-06:33:03.516">>}, {symbol,<<"URKA">>}, {md_req_id,<<"42">>}, 
  {no_md_entries,2}, {md_entry_type,bid}, {md_entry_px,218.87}, {md_entry_size,20}, 
  {md_entry_type,offer}, {md_entry_px,219.03}, {md_entry_size,140}], fix_splitter:split(fix:sample_fix())).

pack_test() ->
  Out = fix:pack(market_data_request, [{sending_time,"20120502-13:08:35"}, {md_req_id,42},{subscription_request_type,1},{market_depth,0},{md_update_type,0},{no_md_entry_types,2},
  {md_entry_type,bid},{md_entry_type,offer},{no_related_sym,1},{symbol,"URKA"},{cfi_code,"EXXXXX"},{security_exchange,"MICEX"}], 31, "SENDER", "TARGET"),  
  Fix = <<"8=FIX.4.4|9=135|35=V|49=SENDER|56=TARGET|34=31|43=N|52=20120502-13:08:35|262=42|263=1|264=0|265=0|267=2|269=0|269=1|146=1|55=URKA|461=EXXXXX|207=MICEX|10=158|">>,

  ?assertEqual(Fix, fix:dump(iolist_to_binary(Out))).


% fix_decode_1_test() ->
%   ?assertMatch({ok, #heartbeat{signature = <<"A",1,"89=234">>}, <<>>}, fix:decode(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).


fix_decode_2_test() ->
  {ok, Record, <<>>} =
  fix:decode(<<"8=FIX.4.4",1,"9=135",1,"35=V",1,"49=SENDER",1,"56=TARGET",1,"34=31",1,"43=N",1,"52=20120502-13:08:35",1,"262=42",1,"263=1",1,"264=0",1,"265=0",1,"267=2",1,"269=0",1,"269=1",1,"146=1",1,"55=URKA",1,"461=EXXXXX",1,"207=MICEX",1,"10=158",1,"">>),
  ?assertMatch(#market_data_request{
    % sender_comp_id = <<"SENDER">>,
    % target_comp_id = <<"TARGET">>,
    % msg_seq_num = 31,
    % md_req_id = <<"42">>,
    % sending_time = <<"20120502-13:08:35">>,
    % subscription_request_type = snapshotupdate,
    % market_depth = 0,
    % md_update_type = 0,
    fields = [
      {poss_dup_flag,false},
      {no_md_entry_types,2},
      {md_entry_type, bid},
      {md_entry_type, offer},
      {no_related_sym,1},
      {symbol,<<"URKA">>},
      {cfi_code,<<"EXXXXX">>},
      {security_exchange,<<"MICEX">>}
    ]
  }, Record).
  
sample_md() ->
  <<"8=FIX.4.4",1,"9=1084",1,"35=W",1,"34=2",1,"49=TARGET_FEED_UAT",1,"52=20120504-08:03:28.693",1,
  "56=SENDER1_FEED_UAT",1,"55=APPL",1,"262=1",1,"268=40",1,"269=0",1,"270=215.34",1,"271=320",1,"269=0",1,"270=215.24",1,"271=3200",1,"269=0",1,"270=215.17",1,
  "271=2190",1,"269=0",1,"270=215.14",1,"271=60",1,"269=0",1,"270=215.11",1,"271=80",1,"269=0",1,"270=215.02",1,"271=150",1,"269=0",1,"270=215.01",1,"271=130",1,
  "269=0",1,"270=215",1,"271=8970",1,"269=0",1,"270=214.98",1,"271=30",1,"269=0",1,"270=214.97",1,"271=20",1,"269=0",1,"270=214.95",1,"271=790",1,"269=0",1,
  "270=214.9",1,"271=110",1,"269=0",1,"270=214.85",1,"271=20",1,"269=0",1,"270=214.8",1,"271=20",1,"269=0",1,"270=214.77",1,"271=160",1,"269=0",1,"270=214.7",1,
  "271=460",1,"269=0",1,"270=214.6",1,"271=10",1,"269=0",1,"270=214.57",1,"271=10",1,"269=0",1,"270=214.56",1,"271=60",1,"269=0",1,"270=214.53",1,"271=1560",1,
  "269=1",1,"270=215.45",1,"271=150",1,"269=1",1,"270=215.6",1,"271=140",1,"269=1",1,"270=215.61",1,"271=2790",1,"269=1",1,"270=215.82",1,"271=150",1,"269=1",1,
  "270=215.83",1,"271=890",1,"269=1",1,"270=215.85",1,"271=990",1,"269=1",1,"270=215.86",1,"271=520",1,"269=1",1,"270=215.88",1,"271=500",1,"269=1",1,"270=215.94",1,
  "271=290",1,"269=1",1,"270=215.95",1,"271=250",1,"269=1",1,"270=215.96",1,"271=2500",1,"269=1",1,"270=215.97",1,"271=8810",1,"269=1",1,"270=216",1,"271=2050",1,
  "269=1",1,"270=216.17",1,"271=60",1,"269=1",1,"270=216.24",1,"271=1760",1,"269=1",1,"270=216.25",1,"271=1200",1,"269=1",1,"270=216.3",1,"271=1000",1,"269=1",1,
  "270=216.45",1,"271=410",1,"269=1",1,"270=216.47",1,"271=500",1,"269=1",1,"270=216.48",1,"271=250",1,"10=156",1>>.
  
fix_decode_3_test() ->
  {ok, Record, <<>>} = fix:decode(sample_md()),
  ?assertMatch(#market_data_snapshot_full_refresh{
    md_entries = [[{md_entry_type,bid},{md_entry_px,215.34},{md_entry_size,320}]|_]
  }, Record).
  