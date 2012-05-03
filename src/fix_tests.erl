-module(fix_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("../include/admin.hrl").
-include("../include/business.hrl").


parse_num_test() ->
  ?assertEqual(123, fix:parse_num(<<"123">>)),
  ?assertEqual(0, fix:parse_num(<<"">>)),
  ?assertMatch(123.4, fix:parse_num(<<"123.4">>)),
  ?assertEqual(123.0, fix:parse_num(<<"123.">>)),
  ok.

encode_test() ->
  ?assertEqual(fix:sample_fix(), iolist_to_binary(fix:encode(fix:decode_fields(fix:sample_fix())))).

decode_test() ->
  ?assertMatch([{msg_type,market_data_snapshot_full_refresh}, {msg_seq_num,3}, 
  {sending_time,<<"20120426-06:33:03.516">>}, {symbol,<<"URKA">>}, {md_req_id,<<"42">>}, 
  {no_md_entries,2}, {md_entry_type,bid}, {md_entry_px,218.87}, {md_entry_size,20}, 
  {md_entry_type,offer}, {md_entry_px,219.03}, {md_entry_size,140}], fix:decode_fields(fix:sample_fix())).

pack_test() ->
  Out = fix:pack(market_data_request, [{sending_time,"20120502-13:08:35"}, {md_req_id,42},{subscription_request_type,1},{market_depth,0},{md_update_type,0},{no_md_entry_types,2},
  {md_entry_type,bid},{md_entry_type,offer},{no_related_sym,1},{symbol,"URKA"},{cfi_code,"EXXXXX"},{security_exchange,"MICEX"}], 31, "SENDER", "TARGET"),  
  Fix = <<"8=FIX.4.4|9=135|35=V|49=SENDER|56=TARGET|34=31|43=N|52=20120502-13:08:35|262=42|263=1|264=0|265=0|267=2|269=0|269=1|146=1|55=URKA|461=EXXXXX|207=MICEX|10=158|">>,

  ?assertEqual(Fix, fix:dump(iolist_to_binary(Out))).


fix_decode_1_test() ->
  ?assertMatch({ok, #heartbeat{signature = <<"A",1,"89=234">>}, <<>>}, fix:decode(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).


fix_decode_2_test() ->
  {ok, Record, <<>>} =
  fix:decode(<<"8=FIX.4.4",1,"9=135",1,"35=V",1,"49=SENDER",1,"56=TARGET",1,"34=31",1,"43=N",1,"52=20120502-13:08:35",1,"262=42",1,"263=1",1,"264=0",1,"265=0",1,"267=2",1,"269=0",1,"269=1",1,"146=1",1,"55=URKA",1,"461=EXXXXX",1,"207=MICEX",1,"10=158",1,"">>),
  ?assertMatch(#market_data_request{
    sender_comp_id = <<"SENDER">>,
    target_comp_id = <<"TARGET">>,
    msg_seq_num = 31,
    md_req_id = <<"42">>,
    sending_time = <<"20120502-13:08:35">>,
    subscription_request_type = snapshotupdate,
    market_depth = 0,
    md_update_type = 0,
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
  
  