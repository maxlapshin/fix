%% @author Max Lapshin <max@maxidoors.ru>
%% @copyright 2012 Max Lapshin
%% @doc Main module for fix usage.
%%
-module(fix).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
% -include("../include/admin.hrl").
% -include("../include/business.hrl").
-compile(export_all).


start_listener() ->
  application:start(ranch),
  ranch:start_listener(fix_listener, 10,
    ranch_tcp, [{port, 8102}],
    fix_server, []
  ).
  

-type fix_message() :: any().

now() ->
  {{YY,MM,DD},{H,M,S}} = calendar:universal_time(),
  % 20120529-10:40:17.578
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B", [YY, MM, DD, H, M, S])).

pack(MessageType, Body) ->
  Seq = get(seq_num),
  Bin = pack(MessageType, Body, Seq, get(sender_id), get(target_id)),
  put(seq_num, Seq+1),
  Bin.

pack(MessageType, Body, SeqNum, Sender, Target) ->
  Header2 = [{msg_type, MessageType},{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum},
  {poss_dup_flag, "N"}] ++ case proplists:get_value(sending_time, Body) of
    undefined -> [{sending_time, fix:now()}];
    _ -> []
  end,
  Body1 = encode(Header2 ++ Body),
  BodyLength = iolist_size(Body1),
  Body2 = iolist_to_binary([encode([{begin_string, "FIX.4.4"}, {body_length, BodyLength}]), Body1]),
  CheckSum = checksum(Body2),
  Body3 = [Body2, encode([{check_sum, CheckSum}])],
  % ?D({out,Header2, dump(Body3)}),
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


dump(Bin) ->
  re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).


-spec decode(binary()) -> {ok, fix_message(), binary()} | {more, non_neg_integer()} | error.
decode(Bin) ->
  case decode_fields(Bin) of
    {ok, Fields, Rest} ->
      {ok, fix_group:postprocess(fix_parser:decode_message(Fields)), Rest};
    Else ->
      Else
  end.  

decode_fields(<<"8=FIX.4.4",1,"9=", Bin/binary>>) ->
  case binary:split(Bin, <<1>>) of
    [BinLen, Rest1] ->
      BodyLength = list_to_integer(binary_to_list(BinLen)),
      case Rest1 of
        <<Message:BodyLength/binary, "10=", _CheckSum:3/binary, 1, Rest2/binary>> ->
          {ok, fix_splitter:split(Message), Rest2};
        _ ->
          {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
      end;
    _ ->
      {more, 1}
  end;

decode_fields(<<"8", Rest/binary>>) when length(Rest) < 14 ->
  {more, 14 - size(Rest)};

decode_fields(<<"8", _/binary>>) ->
  {more, 1};

decode_fields(<<>>) ->
  {more, 14};
          
decode_fields(<<_/binary>>) ->
  error.

  
stock_to_instrument(Stock) when is_atom(Stock) ->
  case binary:split(atom_to_binary(Stock,latin1), <<".">>) of
    [Sym] -> {undefined, Sym};
    [Ex, Sym] -> {Ex, Sym}
  end.




sample_fix() ->
  <<51,53,61,87,1,51,52,61,51,1,53,50,61,50,48,49,50,48,52,50,54,45,48,54,58,51,
    51,58,48,51,46,53,49,54,1,53,53,61,85,82,75,65,1,50,54,50,61,52,50,1,50,54,
    56,61,50,1,50,54,57,61,48,1,50,55,48,61,50,49,56,46,56,55,48,1,50,55,49,61,
    50,48,1,50,54,57,61,49,1,50,55,48,61,50,49,57,46,48,51,48,1,50,55,49,61,49,
    52,48,1>>.
  

profile() ->
  _FIX = sample_fix(),
  Num = 1000,
  Nums = lists:seq(1, Num),
  fprof:start(),
  T1 = erlang:now(),
  fprof:apply(fun() ->
    % [fix_parser:decode_message(FIX) || _N <- Nums]
    [decode(fix_tests:sample_md()) || _N <- Nums]
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
  [decode_fields(FIX) || _N <- Nums],
  T2 = erlang:now(),
  ?D({Num, timer:now_diff(T2,T1), round(timer:now_diff(T2,T1) / Num)}),
  ok.

measure(Fun) ->
  T1 = erlang:now(),
  Fun(),
  T2 = erlang:now(),
  timer:now_diff(T2,T1).
  
 
bench2() ->
  FIX = fix_tests:sample_md(),
  Num = 1000,
  Nums = lists:seq(1, Num),

  T3 = erlang:now(),
  [fix_splitter:split(FIX) || _N <- Nums],
  T4 = erlang:now(),
  ?D({Num, timer:now_diff(T4,T3), round(timer:now_diff(T4,T3) / Num)}),
  ok.
  
-include_lib("eunit/include/eunit.hrl").


  
  
  
  