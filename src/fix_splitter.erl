-module(fix_splitter).

-on_load(init_nif/0).
-include("log.hrl").

-export([split/1, bench/0]).

init_nif() ->
  Path = filename:dirname(code:which(?MODULE)) ++ "/../priv",
  Load = erlang:load_nif(Path ++ "/fix_splitter", 0),
  case Load of
    ok -> ok;
    {error, {Reason,Text}} -> io:format("Load fix_splitter failed. ~p:~p~n", [Reason, Text])
  end,
  ok.

split(_Binary) ->
  erlang:error(not_implemented).


decode_fields_erl(Message) ->
  [begin
    [K,V] = binary:split(Field, <<"=">>),
    Tag = fix_parser:field_by_number(K),
    {Tag, fix_parser:decode_typed_field(Tag, V)}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0].

bench() ->
  ok.


-include_lib("eunit/include/eunit.hrl").


split_1_test() ->
  ?assertEqual([{msg_type,heartbeat}], split(<<"35=0",1>>)).


split_2_test() ->
  List = split(fix_tests:sample_md()),
  ?assertMatch([{begin_string, <<"FIX.4.4">>},{body_length,1084},{msg_type,marketdatasnapshotfullrefresh},{msg_seq_num,2}|_], List).

split_3_test() ->
  ?assertMatch([{begin_string,<<"FIX.4.4">>},{body_length,22},{msg_type,heartbeat},{signature,<<"A",1,"89=234">>},{check_sum,<<"999">>}], 
  split(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).

