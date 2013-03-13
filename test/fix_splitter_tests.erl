-module(fix_splitter_tests).

-include_lib("eunit/include/eunit.hrl").


split_1_test() ->
  ?assertEqual([{msg_type,heartbeat}], fix_splitter:split(<<"35=0",1>>)).


split_2_test() ->
  List = fix_splitter:split(fix_tests:sample_md()),
  ?assertMatch([{begin_string, <<"FIX.4.4">>},{body_length,1084},{msg_type,market_data_snapshot_full_refresh},{msg_seq_num,2}|_], List).

split_3_test() ->
  ?assertMatch([{begin_string,<<"FIX.4.4">>},{body_length,22},{msg_type,heartbeat},{signature,<<"A",1,"89=234">>},{check_sum,<<"999">>}], 
  fix_splitter:split(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).

split_float_test() ->
  ?assertMatch([{md_entry_px,218.87}], fix_splitter:split(<<"270=218.87",1>>)).


split_4_test() ->
  ?assertEqual([{poss_dup_flag,false}], fix_splitter:split(<<"43=N",1>>)).

split_5_test() ->
  ?assertEqual([{poss_dup_flag,false},{sending_time,<<"20120502-13:08:35">>}], fix_splitter:split(<<"43=N",1,"52=20120502-13:08:35",1>>)).

split_6_test() ->
  ?assertEqual([{order_qty,-287},{ord_status,pendingnew}], fix_splitter:split(<<"38=-287",1, "39=A",1>>)).

dont_fail_on_unknown_code_test() ->
  ?assertEqual(10001, fix_splitter:field_by_number(10001)),
  ?assertEqual([{10001,<<"Y">>}], fix_splitter:split(<<"10001=Y",1>>)).

good_field_names_test() ->
  Pairs = [{N,fix_parser:field_by_number(list_to_binary(integer_to_list(N))), fix_splitter:field_by_number(N)} || N <- lists:seq(1, 956)],
  % SplitterPairs = [{Number, } || {Number, _Name} <- Pairs],
  % ?assertEqual(Pairs, SplitterPairs).
  [?assertEqual({Number,Field1}, {Number,Field2}) || {Number, Field1, Field2} <- Pairs],
  ok.



