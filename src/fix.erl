%% @author Max Lapshin <max@maxidoors.ru>
%% @copyright 2012 Max Lapshin
%% @doc Main module for fix usage.
%%
-module(fix).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
% -include("../include/admin.hrl").
-include("../include/business.hrl").
-export([start_listener/0,
         get_value/2,
         get_value/1,
         start_exec_conn/1,
         now/0,
         timestamp/1,
         utc_ms/0,
         pack/5,
         checksum/1,
         encode/1,
         encode_value/1,
         dump/1,
         decode/1,
         decode_fields/1,
         stock_to_instrument/1,
         instrument_to_stock/1,
         get_stock/1,
         cfi_code/1,
         stock_to_instrument_block/1,
         pretty_print/1]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-export([sample_fix/0]).

-endif.


%% @doc Start acceptor with `ranch' on port, specified in application environment under fix_port
%%
-spec start_listener() -> {ok, pid()}.
start_listener() ->
  application:start(ranch),
  Spec = ranch:child_spec(fix_listener,
    ranch_tcp, [{port, fix:get_value(fix_port)}],
    fix_server, []
  ),
  {ok, Pid} = supervisor:start_child(fix_sup, Spec),
  error_logger:info_msg("Starting FIX server on port ~p~n", [fix:get_value(fix_port)]),
  {ok, Pid}.


%% @doc returns value from application environment, or default value
-spec get_value(any(), any()) -> any().
get_value(Key, Default) ->
  case application:get_env(fix, Key) of
    {ok, Value} -> Value;
    undefined -> Default
  end.

%% @doc returns value from application environment, or raise error
-spec get_value(any()) -> any().
get_value(Key) ->
  case application:get_env(fix, Key) of
    {ok, Value} -> Value;
    undefined -> erlang:error({no_key,Key})
  end.
  


%% @doc starts fix connection by its well known name
-spec start_exec_conn(Name :: term()) -> {ok, Pid :: pid()}.
start_exec_conn(Name) ->
  Options = fix:get_value(Name),
  {ok, Fix} = fix_sup:start_exec_conn(Name, Options),
  {ok, Fix}.


%% @doc fix local reimplementation of UTC as a string 
-spec now() -> string().
now() ->
  timestamp(to_date_ms(erlang:timestamp())).

-spec timestamp({calendar:date(), {0..23, 0..59, 0..59, non_neg_integer()}}) -> string().
timestamp({{YY,MM,DD},{H,M,S,Milli}}) ->
  % 20120529-10:40:17.578
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B.~3..0B", [YY, MM, DD, H, M, S, Milli])).

-spec to_date_ms(erlang:timestamp()) -> {calendar:date(), {0..23, 0..59, 0..59, non_neg_integer()}}.
to_date_ms({Mega, Sec, Micro}) ->
  Seconds = Mega*1000000 + Sec,
  Milli = Micro div 1000,
  {Date, {H,M,S}} = calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
  {Date, {H,M,S,Milli}}.


-spec utc_ms() -> non_neg_integer().
utc_ms() ->
  utc_ms(erlang:timestamp()).

-spec utc_ms(erlang:timestamp()) -> non_neg_integer().
utc_ms({Mega, Sec, Micro}) ->
  (Mega*1000000+Sec)*1000 + Micro div 1000.



%% @doc packs fix message into binary
-spec pack(atom(), list(), non_neg_integer(), any(), any()) -> iolist().
pack(MessageType, Body, SeqNum, Sender, Target) when 
MessageType =/= undefined, is_list(Body), is_integer(SeqNum), Sender =/= undefined, Target =/= undefined ->
  Header2 = [{msg_type, MessageType},{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}
  % ,{poss_dup_flag, "N"}
  ] ++ case proplists:get_value(sending_time, Body) of
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

-spec checksum(binary()) -> string().
checksum(Packet) ->
  lists:flatten(io_lib:format("~3..0B", [lists:sum([Char || <<Char>> <=iolist_to_binary(encode(Packet))]) rem 256])).

-spec encode(binary() | proplists:proplist()) -> iolist().
encode(Packet) when is_binary(Packet) -> Packet;
encode([{_K,_V}|_] = Packet) ->
  [[fix_parser:number_by_field(Key), "=", fix_parser:encode_typed_field(Key, Value), 1] || {Key, Value} <- Packet].

-spec encode_value(float() | integer() | string() | binary()) -> string().
encode_value(Value) when is_float(Value) -> io_lib:format("~.2f", [Value]);
encode_value(Value) when is_number(Value) -> integer_to_list(Value);
encode_value(Value) when is_list(Value) -> Value;
encode_value(Value) when is_binary(Value) -> Value.


-spec dump(iolist()) -> binary().
dump(Bin) ->
  re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).


-spec decode(binary()) -> {ok, fix_splitter:decoded_message(), binary(), binary()}
                          | {more, non_neg_integer()}
                          | error.
decode(Bin) ->
  try decode0(Bin) of
    Result -> Result
  catch
    error:Error:StackTrace ->
      ?DBG("Failed to decode fix '~s': ~p~n~p~n", [fix:dump(Bin), Error, StackTrace]),
      error(invalid_fix)
  end.

decode0(Bin) ->
  case decode_fields(Bin) of
    {ok, Fields, MessageBin, Rest} ->
      {ok, fix_group:postprocess(fix_parser:decode_message(Fields)), MessageBin, Rest};
    Else ->
      Else
  end.  

-spec decode_fields(binary()) -> {'ok',fix_splitter:decode_message(),binary(),binary()}
                                 | {'more',integer()}
                                 | error.
decode_fields(<<"8=FIX.4.4",1,"9=", Bin/binary>> = FullBin) ->
  case binary:split(Bin, <<1>>) of
    [BinLen, Rest1] ->
      BodyLength = list_to_integer(binary_to_list(BinLen)),
      case Rest1 of
        <<Message:BodyLength/binary, "10=", _CheckSum:3/binary, 1, Rest2/binary>> ->
          MessageLength = size(FullBin) - size(Rest2),
          <<MessageBin:MessageLength/binary, _/binary>> = FullBin,
          {ok, fix_splitter:split(Message), MessageBin, Rest2};
        _ ->
          {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
      end;
    _ ->
      {more, 1}
  end;

decode_fields(<<"8", Rest/binary>>) when byte_size(Rest) < 14 ->
  {more, 14 - size(Rest)};

decode_fields(<<"8", _/binary>>) ->
  {more, 1};

decode_fields(<<>>) ->
  {more, 14};
          
decode_fields(<<_/binary>>) ->
  error.

  
-spec stock_to_instrument(atom() | binary()) -> {'undefined' | binary(),binary()}
                                                | {binary(),binary(),binary()}.
stock_to_instrument(Stock) when is_atom(Stock) ->
  stock_to_instrument(atom_to_binary(Stock,latin1));

stock_to_instrument(Stock) when is_binary(Stock) ->
  case binary:split(Stock, <<".">>, [global]) of
    [<<Currency1:3/binary, Currency2:3/binary>>] -> {undefined, <<Currency1/binary, "/", Currency2/binary>>};
    [Sym] -> {undefined, Sym};
    [<<"FX_", _/binary>> = Ex, <<Currency1:3/binary, Currency2:3/binary>>] -> {Ex, <<Currency1/binary, "/", Currency2/binary>>};
    [Ex, Sym] -> {Ex, Sym};
    [Ex, Sym, Date] -> {Ex, Sym, Date}
  end.


-spec instrument_to_stock({'undefined' | binary(),binary()} | {binary(),binary(),binary()}) -> atom().
instrument_to_stock({undefined, <<Currency1:3/binary, "/", Currency2:3/binary>>}) ->
  binary_to_atom(<<Currency1/binary, Currency2/binary>>, latin1);

instrument_to_stock({<<"FX_", _/binary>> = Exchange, <<Currency1:3/binary, "/", Currency2:3/binary>>}) ->
  binary_to_atom(<<Exchange/binary, ".", Currency1/binary, Currency2/binary>>, latin1);

instrument_to_stock({Exchange, Symbol, Maturity}) when is_binary(Exchange) andalso is_binary(Symbol) 
  andalso is_binary(Maturity) ->
  binary_to_atom(<<Exchange/binary, ".", Symbol/binary, ".", Maturity/binary>>, latin1);

instrument_to_stock({Exchange, Symbol}) when is_binary(Exchange) andalso is_binary(Symbol) ->
  binary_to_atom(<<Exchange/binary, ".", Symbol/binary>>, latin1).

-spec get_stock(#execution_report{}) -> atom().
get_stock(#execution_report{security_exchange = Exchange, symbol = Symbol}) ->
  instrument_to_stock({Exchange, Symbol}).

-spec cfi_code(_) -> string().
cfi_code(futures) -> "F*****";
cfi_code(undefined) -> "MRCXXX";
cfi_code(<<"FX_TOD">>) -> "MRCXXX";
cfi_code(<<"FX_TOM">>) -> "MRCXXX";
cfi_code(_) -> "EXXXXX".

-spec stock_to_instrument_block(atom() | binary()) -> proplists:proplist().
stock_to_instrument_block(Stock) ->
  case stock_to_instrument(Stock) of
    {Exchange, Symbol} ->
      [{symbol, Symbol}, {cfi_code, cfi_code(Exchange)}, {security_exchange, Exchange}];
    {Exchange, Symbol, Date} ->
      [{symbol, Symbol}, {cfi_code, cfi_code(futures)}, {maturity_month_year, Date}, {security_exchange, Exchange}]
  end.

-spec pretty_print(iolist()) -> ok.
pretty_print(Encoded) ->
    io:format("~s~n", [binary:replace(erlang:iolist_to_binary(Encoded), <<1>>, <<"|">>, [global])]).

-ifdef(TEST).

sample_fix() ->
  <<51,53,61,87,1,51,52,61,51,1,53,50,61,50,48,49,50,48,52,50,54,45,48,54,58,51,
    51,58,48,51,46,53,49,54,1,53,53,61,85,82,75,65,1,50,54,50,61,52,50,1,50,54,
    56,61,50,1,50,54,57,61,48,1,50,55,48,61,50,49,56,46,56,55,48,1,50,55,49,61,
    50,48,1,50,54,57,61,49,1,50,55,48,61,50,49,57,46,48,51,48,1,50,55,49,61,49,
    52,48,1>>.
  

print_time_diff(T1, T2, Num) ->
  ?D({Num, T2 - T1, round(erlang:convert_time_unit(T2 - T1, native, microsecond) / Num)}).

profile() ->
  _FIX = sample_fix(),
  Num = 1000,
  Nums = lists:seq(1, Num),
  fprof:start(),
  T1 = erlang:monotonic_time(),
  fprof:apply(fun() ->
    % [fix_parser:decode_message(FIX) || _N <- Nums]
    [decode(fix_tests:sample_md()) || _N <- Nums]
  end, []),
  T2 = erlang:monotonic_time(),
  fprof:profile(),
  fprof:analyse(),
  print_time_diff(T1, T2, Num),
  ok.

bench() ->
  FIX = sample_fix(),
  Num = 100000,
  Nums = lists:seq(1, Num),
  T1 = erlang:monotonic_time(),
  [decode_fields(FIX) || _N <- Nums],
  T2 = erlang:monotonic_time(),
  print_time_diff(T1, T2, Num),
  ok.

measure(Fun) ->
  T1 = erlang:monotonic_time(),
  Fun(),
  T2 = erlang:monotonic_time(),
  erlang:convert_time_unit(T2 - T1, native, microsecond).
  
 
bench2() ->
  FIX = fix_tests:sample_md(),
  Num = 1000,
  Nums = lists:seq(1, Num),

  T3 = erlang:monotonic_time(),
  [fix_splitter:split(FIX) || _N <- Nums],
  T4 = erlang:monotonic_time(),
  print_time_diff(T3, T4, Num),
  ok.
  
-endif.
