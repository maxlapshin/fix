-module(fix_reader_5_0_test).
-include("fix_version.hrl").
-include_lib("eunit/include/eunit.hrl").

stop_normal_exitmsg(Pid) ->
  monitor(process, Pid),
  Pid ! {'EXIT', self(), normal},
  ?assertEqual(normal,
               receive
                   {'DOWN', _, _, P, Reason} when Pid == P -> Reason
               after 200 -> error
               end).


read_conn_connect_test() ->
  application:set_env(fix, fix_test_5_0_read, [
    {host,"127.0.0.1"},
    {port,6790},
    {password,no_password},
    {target,"TestTarget"},
    {sender,"TestSender"},
    {version, ?FIX_5_0_SP2},
    {heartbeat,30}
  ]),

  fix_test_server:start(6790, [{version, ?FIX_5_0_SP2}, {password, no_password}]),
  % start and check start is successful
  StartResult = fix_read_conn:start_link(fix_test_5_0_read, []),
  {ok, R} = StartResult,
  ?assertEqual(ok, fix_read_conn:connect(R, [])),
  stop_normal_exitmsg(R),
  fix_test_server:stop().

send_quote_message_test() ->
  application:set_env(fix, fix_test_5_0_read, [
    {host,"127.0.0.1"},
    {port,6790},
    {password,no_password},
    {target,"TestTarget"},
    {sender,"TestSender"},
    {version, ?FIX_5_0_SP2},
    {heartbeat,30}
  ]),

  fix_test_server:start(6790, [{version, ?FIX_5_0_SP2}, {password, no_password}, {forward_pid, self()}]),
  % start and check start is successful
  {ok, R} = fix_read_conn:start_link(fix_test_5_0_read, []),
  ok = fix_read_conn:connect(R),
  ok = fix_read_conn:send(R, quote, [
            {symbol, <<"USD/JPY">>},
            {quote_id, <<"q_id_USD/JPY">>},
            {quote_req_id, <<"QRS_11631014961606">>},
            {bid_px, <<"99.53816278537646">>},
            {offer_px, <<"105.48890454985164">>},
            {bid_size, 5000000},
            {offer_size, 5000000},
            {quote_type, tradeable},
            {quote_msg_id, <<"q_msg_id_USD/JPY_1">>}]),
  receive
      {message, quote, _, _, _, _} ->
          ok
  after 5_000 ->
    throw(sent_message_not_received)
  end,
  stop_normal_exitmsg(R),
  fix_test_server:stop().

