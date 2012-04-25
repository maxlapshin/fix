-module(fix).
-include("log.hrl").
% -include("../include/admin.hrl").
% -include("../include/business.hrl").
-compile(export_all).


-type fix_message() :: any().

login() ->
  {ok, Env} = application:get_env(fix, config),
  {ip, IP} = lists:keyfind(ip, 1, Env),
  {port, Port} = lists:keyfind(port, 1, Env),
  {password, Password} = lists:keyfind(password, 1, Env),
  {target, TargetCompID} = lists:keyfind(target, 1, Env),
  put(target_id, TargetCompID),
  {sender, SenderCompID} = lists:keyfind(sender, 1, Env),
  put(sender_id, SenderCompID),
  {heartbeat, HeartBTInt} = lists:keyfind(heartbeat, 1, Env),
  
  {ok, Sock} = gen_tcp:connect(IP, Port, [binary, {packet,raw}, {active, false}]),
  put(seq_num, 1),
  
  Bin1 = pack(logon, [{'EncryptMethod', 0},{'HeartBTInt', HeartBTInt},{'ResetSeqNumFlag', "Y"},{'Password', Password}]),
  ok = gen_tcp:send(Sock, Bin1),
  {ok, R1} = gen_tcp:recv(Sock, 0, 4000),
  ?D(decode(R1)),
  
  Bin2 = pack(market_data, [{'MDReqID', 42}, {'SubscriptionRequestType', 1}, {'MarketDepth', 1}, {'MDUpdateType', 0},
    {'NoMDEntryTypes',2},{'MDEntryType',0},{'MDEntryType',1},{'NoRelatedSym',1},
    {'Symbol', proplists:get_value(symbol, Env)},{'CFICode', "EXXXXX"},{'SecurityExchange', proplists:get_value(security, Env)}]),
  gen_tcp:send(Sock, Bin2),

  % timer:send_after(HeartBTInt*1000, hearbeat),

  {ok, R2} = gen_tcp:recv(Sock, 0, 4000),
  ?D(decode(R2)),

  {ok, R3} = gen_tcp:recv(Sock, 0, 4000),
  ?D(decode(R3)),
  
  Bin10 = pack(logout, [{'Text', "closing"}]),
  ok = gen_tcp:send(Sock, Bin10),
  gen_tcp:close(Sock),
  ok.
  % Bin.

pack(MessageType, Body) ->
  Seq = get(seq_num),
  Bin = pack(MessageType, Body, Seq, get(sender_id), get(target_id)),
  put(seq_num, Seq+1),
  Bin.

pack(MessageType, Body, SeqNum, Sender, Target) ->
  Header2 = [{msg_type, MessageType},{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum},
  {poss_dup_flag, "N"},{sending_time, sending_time()}],
  Body1 = encode(Header2 ++ Body),
  BodyLength = iolist_size(Body1),
  Body2 = iolist_to_binary([encode([{begin_string, "FIX.4.4"}, {body_length, BodyLength}]), Body1]),
  CheckSum = checksum(Body2),
  Body3 = [Body2, encode([{check_sum, CheckSum}])],
  ?D({out,dump(Body3)}),
  Body3.

checksum(Packet) ->
  lists:flatten(io_lib:format("~3..0B", [lists:sum([Char || <<Char>> <=iolist_to_binary(encode(Packet))]) rem 256])).

encode(Packet) when is_binary(Packet) -> Packet;
encode([{_K,_V}|_] = Packet) ->
  [[fix_parser:number_by_field(Key), "=", fix_parser:encode_typed_field(Key, Value), 1] || {Key, Value} <- Packet].

encode_value(Value) when is_number(Value) -> integer_to_list(Value);
encode_value(Value) when is_float(Value) -> io_lib:format("~.2f", [Value]);
encode_value(Value) when is_list(Value) -> Value;
encode_value(Value) when is_binary(Value) -> Value.


sending_time() ->
  {{YY,MM,DD},{H,M,S}} = calendar:universal_time(),
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B", [YY,MM,DD,H,M,S])).
  
dump(Bin) ->
  re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).


-spec decode(binary()) -> {ok, fix_message(), binary()} | {more, non_neg_integer()} | error.
decode(<<"8=FIX.4.4",1,"9=", Bin/binary>>) ->
  case binary:split(Bin, <<1>>) of
    [BinLen, Rest1] ->
      BodyLength = list_to_integer(binary_to_list(BinLen)),
      case Rest1 of
        <<Message:BodyLength/binary, "10=", _CheckSum:3/binary, 1, Rest2/binary>> ->
          {ok, decode_message(Message), Rest2};
        _ ->
          {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
      end;
    _ ->
      {more, 1}
  end;

decode(<<"8", Rest/binary>>) when length(Rest) < 14 ->
  {more, 14 - size(Rest)};

decode(<<"8", _/binary>>) ->
  {more, 1};
          
decode(<<_/binary>>) ->
  error.

decode_message(Message) ->
  ?D({in, dump(Message)}),
  fix_parser:decode_message(Message).
  
decode_fields(Message) ->
  [begin
    [K,V] = binary:split(Field, <<"=">>),
    Tag = fix_parser:field_by_number(K),
    {Tag, fix_parser:decode_typed_field(Tag, V)}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0].


parse_num(Bin) -> parse_num_erl(Bin).

parse_num_erl(Bin) -> parse_num(Bin, 0, 0).
parse_num(<<$., Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*1.0, 0.1);
parse_num(<<X, Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*10 + X - $0, 0);
parse_num(<<X, Bin/binary>>, Acc, Coeff) -> parse_num(Bin, Acc + (X - $0)*Coeff, Coeff*0.1);
parse_num(<<>>, Acc, _) -> Acc.

  


sample_fix() ->
  <<51,53,61,87,1,51,52,61,51,1,53,50,61,50,48,49,50,48,52,50,54,45,48,54,58,51,
    51,58,48,51,46,53,49,54,1,53,53,61,85,82,75,65,1,50,54,50,61,52,50,1,50,54,
    56,61,50,1,50,54,57,61,48,1,50,55,48,61,50,49,56,46,56,55,48,1,50,55,49,61,
    50,48,1,50,54,57,61,49,1,50,55,48,61,50,49,57,46,48,51,48,1,50,55,49,61,49,
    52,48,1>>.

profile() ->
  FIX = sample_fix(),
  Num = 1000,
  Nums = lists:seq(1, Num),
  fprof:start(),
  T1 = erlang:now(),
  fprof:apply(fun() ->
    [fix_parser:decode_message(FIX) || _N <- Nums]
  end, []),
  T2 = erlang:now(),
  fprof:profile(),
  fprof:analyse(),  
  ?D({Num, timer:now_diff(T2,T1), round(timer:now_diff(T2,T1) / Num)}),
  ok.

bench() ->
  FIX = sample_fix(),
  Num = 100000,
  Nums = lists:seq(1, Num),
  T1 = erlang:now(),
  [fix_parser:decode_message(FIX) || _N <- Nums],
  T2 = erlang:now(),
  ?D({Num, timer:now_diff(T2,T1), round(timer:now_diff(T2,T1) / Num)}),
  ok.

measure(Fun) ->
  T1 = erlang:now(),
  Fun(),
  T2 = erlang:now(),
  timer:now_diff(T2,T1).
  
  
bench1() ->
  Num = 10*1000,
  Nums = lists:seq(1, Num),
  Bins = [
    {small_int, <<"123">>, list_to_integer},
    {medium_int, <<"1234567890">>, list_to_integer},
    {small_float, <<"123.45">>, list_to_float},
    {medium_float, <<"123.4567890">>, list_to_float}
  ],
  Results = [begin
    T1 = measure(fun() -> [erlang:F(binary_to_list(Bin)) || _N <- Nums] end),
    T2 = measure(fun() -> [parse_num(Bin) || _N <- Nums] end),
    T3 = measure(fun() -> [parse_num_erl(Bin) || _N <- Nums] end),
    {Tag, T1, T2, T3}
  end || {Tag, Bin, F} <- Bins],
  ?D(Results).
  
-include_lib("eunit/include/eunit.hrl").

parse_num_test() ->
  ?assertEqual(123, parse_num(<<"123">>)),
  ?assertEqual(0, parse_num(<<"">>)),
  ?assertMatch(123.4, parse_num(<<"123.4">>)),
  ?assertEqual(123.0, parse_num(<<"123.">>)),
  ok.

encode_test() ->
  ?assertEqual(sample_fix(), iolist_to_binary(fix:encode(fix:decode_fields(sample_fix())))).

decode_test() ->
  ?assertMatch([{msg_type,market_data_snapshot_full_refresh}, {msg_seq_num,3}, 
  {sending_time,<<"20120426-06:33:03.516">>}, {symbol,<<"URKA">>}, {md_req_id,<<"42">>}, 
  {no_md_entries,2}, {md_entry_type,<<"0">>}, {md_entry_px,218.87}, {md_entry_size,20}, 
  {md_entry_type,<<"1">>}, {md_entry_px,219.03}, {md_entry_size,140}], fix:decode_fields(sample_fix())).

