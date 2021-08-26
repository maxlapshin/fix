-module(fix_server_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


fix_start_stop_test() ->
  ?assertEqual({error, econnrefused}, gen_tcp:connect("127.0.0.1", 6555, [binary])),
  % error_logger:delete_report_handler(error_logger_tty_h),
  application:stop(fix),
  application:unload(fix),
  application:load(fix),
  application:set_env(fix, fix_port, 6555),
  application:start(fix),

  ?assertMatch({ok, _}, gen_tcp:connect("127.0.0.1", 6555, [binary])),
  application:stop(fix),
  ?assertEqual({error, econnrefused}, gen_tcp:connect("127.0.0.1", 6555, [binary])),
  ok.


fix_server_test_() ->
  {foreach, 
  fun() ->
    % error_logger:delete_report_handler(error_logger_tty_h),
    application:stop(fix),
    application:unload(fix),
    application:load(fix),
    application:set_env(fix, fix_port, 6555),
    application:set_env(fix, test_read, []),
    application:start(fix)
  end,
  fun(_) ->
    application:stop(fix)
  end,
  [{atom_to_list(F), fun ?MODULE:F/0} || {F,0} <- ?MODULE:module_info(exports), lists:prefix("test_", atom_to_list(F))]
  }.


test_login() ->
  {ok, Socket} = login(),

  {ok, Reply} = gen_tcp:recv(Socket, 0),
  {ok, Msg, _, <<>>} = fix:decode_fields(Reply),

  ?assertEqual(<<"TestClient">>, proplists:get_value(target_comp_id, Msg)),
  ?assertEqual(<<"TestServer">>, proplists:get_value(sender_comp_id, Msg)),

  ok.





test_subscribe_failed() ->
  {ok, Socket} = login(),
  {ok, _} = gen_tcp:recv(Socket, 0),

  ok = meck:new(fix_reader, [passthrough]),
  ok = meck:expect(fix_reader, subscribe, fun(_,'MICEX.URKA', _) -> {error, rejected} end),

  Request = [{md_req_id, 1}, {subscription_request_type, 1}, {market_depth, 0}, {md_update_type, 0}, {no_related_sym,1}]++
    fix:stock_to_instrument_block('MICEX.URKA') ++ fix_connection:entry_types('MICEX.URKA'),
  Subscribe = fix:pack(market_data_request, Request, 2, "TestClient", "TestServer"),
  ok = gen_tcp:send(Socket, Subscribe),

  timer:sleep(1000),
  {ok, Reply} = gen_tcp:recv(Socket, 0),
  {ok, Msg, _, <<>>} = fix:decode_fields(Reply),

  ?assertEqual(market_data_request_reject, proplists:get_value(msg_type, Msg)),
  ok.


login() ->
  {ok, Socket} = gen_tcp:connect("127.0.0.1", 6555, [binary, {send_timeout, 100}, {active,false}]),
  Password = "123",
  Heartbeat = 30,
  Sender = "TestClient",
  Target = "TestServer",
  Logon = fix:pack(logon, [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}], 1, Sender, Target),
  ok = gen_tcp:send(Socket, Logon),
  {ok, Socket}.




