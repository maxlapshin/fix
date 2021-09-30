-module(fix_reader_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("fix/include/business.hrl").
% -include_lib("trader/include/trader.hrl").

-compile(export_all).

% Needed to keep test order and to ensure broken test does not affect other tests with left meck
reader_test_() ->
  % Labels increase as line number increase. Use that fact for sorting
  {ok, {_, [{labeled_exports, LExports}]}} = beam_lib:chunks(code:which(?MODULE), [labeled_exports]),
  SLExports = lists:keysort(3, LExports),
  TestFunctions = [F || {F,0,_} <- SLExports,
    lists:prefix("test_", atom_to_list(F))],
  {foreach, fun setup/0, fun cleanup/1,
    [{atom_to_list(F), fun ?MODULE:F/0} || F <- TestFunctions] }.

setup() ->
  % prepare
  application:set_env(fix, fix_test_read, [
    {host,"127.0.0.1"},
    {port,6789},
    {password,"TestPw"},
    {target,"TestTarget"},
    {sender,"TestSender"},
    {heartbeat,30}
  ]),

  fix_test_server:start(6789, []),
  [].

cleanup(Mods) ->
  % error_logger:delete_report_handler(sasl_report_tty_h),
  % error_logger:delete_report_handler(error_logger_tty_h),
  fix_test_server:stop(),
  meck:unload(Mods).


stop_normal_exitmsg(Pid) ->
  monitor(process, Pid),
  Pid ! {'EXIT', self(), normal},
  ?assertEqual(normal, receive {'DOWN', _, _, Pid, Reason} -> Reason after 200 -> error end),
  ok.


test_read_conn_connect() ->
  % start and check start is successful
  StartResult = fix_read_conn:start_link(fix_test_read, []),
  ?assertMatch({ok, _}, StartResult),
  {ok, R} = StartResult,
  ?assertEqual(ok, fix_read_conn:connect(R, [])),
  stop_normal_exitmsg(R),
  ok.


test_read_conn_connect_error() ->
  application:set_env(fix, fix_test_read, [
    {host,"127.0.0.1"},
    {port,6788},
    {password,"TestPw"},
    {target,"TestTarget"},
    {sender,"TestSender"},
    {heartbeat,30}
  ]),

  {ok, R} = fix_read_conn:start_link(fix_test_read, []),
  ?assertEqual({error, {connect, econnrefused}}, fix_read_conn:connect(R, [])),
  stop_normal_exitmsg(R),
  ok.

test_read_conn_logon_timeout() ->
  fix_test_server:stop(),
  fix_test_server:start(6789, [{on_fix, fun
    (logon, _) -> timer:sleep(1000);
    (_,_) -> ok
  end}]),

  % We know that fix_read_conn uses gen_server timeout for logon
  % meck:expect(gen_tcp, send, fun(_S, _D) -> self() ! timeout, ok end),

  {ok, R} = fix_read_conn:start_link(fix_test_read, []),

  ?assertEqual({error, logon_timeout}, fix_read_conn:connect(R, [])),

  stop_normal_exitmsg(R),
  ok.



test_supervision_tree() ->
  % start supervisor where readers go
  ReadersSupStartResult = supervisor:start_link({local, fix_readers_sup}, fix_sup, [readers]),
  ?assertMatch({ok, _}, ReadersSupStartResult),
  {ok, ReadersSup} = ReadersSupStartResult,

  % Start reader and sync to ensure it completes startup
  ?assertMatch({ok, _}, fix_sup:start_reader(fix_test_read)),
  ?assertMatch([{fix_test_read, _, supervisor, _}], supervisor:which_children(fix_readers_sup)),
  timer:sleep(10),
  (catch gen_server:call(fix_test_read, sync)),

  % Check reader supervisor children list
  SupName = fix_sup:read_sup(fix_test_read),
  ?assertMatch([
      {connection, _, worker, _},
      {stocks, _, supervisor, _},
      {manager, _, worker, _} ], supervisor:which_children(SupName)),

  stop_normal_exitmsg(ReadersSup),
  ok.


test_manager_schedules_reconnect() ->
  Tester = self(),
  meck:new(fix_read_manager, [passthrough]),
  meck:expect(fix_read_manager, connect_after, fun (T) ->
        Ref = erlang:make_ref(),
        Msg = {timeout, Ref, connect},
        case T of
          0 -> self() ! Msg;
          _ -> Tester ! {connect_timer, T, self(), Msg}
        end,
        Ref
    end),

  % Make it fail for first time
  fix_test_server:stop(),

  {ok, ReadersSup} = supervisor:start_link({local, fix_readers_sup}, fix_sup, [readers]),
  {ok, _} = fix_sup:start_reader(fix_test_read),

  % Ensure manager set timeout for next connect
  Timers = receive_timers(100),
  ?assertMatch([_], Timers),

  % "Repair" connection and send timeout event
  fix_test_server:start(6789, []),

  {_, _, Mgr, TmoMsg} = hd(Timers),
  Mgr ! TmoMsg,

  % Sync to manager and ensure no more reconnects scheduled
  ?assertEqual(connected, fix_read_manager:status(Mgr)),
  ?assertEqual([], receive_timers(20)),

  % Ensure connect attempts were made from two different Pids (which means connection was restarted only once)
  % ?assertMatch([_, _], lists:ukeysort(1, meck:history(gen_tcp))),

  % Turn off verbose error logging

  % Now simulate error in connection
  erlang:exit(whereis(fix_test_server), normal),

  % Ensure new connection is started without timeout
  ?assertEqual([], receive_timers(40)),
  ?assertEqual(connected, fix_read_manager:status(Mgr)),


  stop_normal_exitmsg(ReadersSup),
  meck:unload(fix_read_manager),
  ok.

receive_timers(Timeout) ->
  receive
    {connect_timer, _, _, _} = Timer ->
      [Timer | receive_timers(10)]
  after
    Timeout -> []
  end.


test_stock_subscribe() ->

  Self = self(),
  fix_test_server:stop(),
  fix_test_server:start(6789, [{on_fix, fun
    (market_data_request,Msg) ->
      Exchange = proplists:get_value(security_exchange, Msg),
      Symbol = proplists:get_value(symbol, Msg),
      MdReqId = proplists:get_value(md_req_id, Msg),
      Self ! {subscribe, Exchange, Symbol, MdReqId};
    (_,_) -> ok
  end}]),

  % Pre-requisites
  ?assert(whereis('MICEX.TEST') == undefined),
  {ok, ReadersSup} = supervisor:start_link({local, fix_readers_sup}, fix_sup, [readers]),

  % Request reader's stock and ensure it is started
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST'),
  ?assertNot(whereis('MICEX.TEST') == undefined),

  ID1 = receive
    {subscribe, <<"MICEX">>, <<"TEST">>, ID1_} -> ID1_
  after
    5 -> error(havent_subscribed_micex_test)
  end,

  receive
    {subscribe, <<"MICEX">>, <<"TEST">>, _} -> error(double_subscribe_micex_test)
  after
    0 -> ok
  end,


  % Add one more stock. Now it uses already started reader
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST2'),

  ID2 = receive
    {subscribe, <<"MICEX">>, <<"TEST2">>, ID2_} -> ID2_
  after
    5 -> error(havent_subscribed_micex_test2)
  end,

  receive
    {subscribe, <<"MICEX">>, <<"TEST2">>, _} -> error(double_subscribe_micex_test2)
  after
    0 -> ok
  end,


  ?assertNotEqual(ID1, ID2),

  stop_normal_exitmsg(ReadersSup),
  ok.

test_stock_route() ->
  % Construct event proxy for monitoring stock events
  ok = meck:new(test_ev_proxy, [non_strict]),
  ok = meck:expect(test_ev_proxy, init, fun(Arg) -> {ok, Arg} end),
  ok = meck:expect(test_ev_proxy, handle_event, fun(Ev, [Dest, Self]) -> Dest ! {event, Self, Ev}, {ok, [Dest, Self]} end),
  ok = meck:expect(test_ev_proxy, handle_info,  fun(Ev, [Dest, Self]) -> Dest ! {info,  Self, Ev}, {ok, [Dest, Self]} end),

  % Pre-requisites
  {ok, ReadersSup} = supervisor:start_link({local, fix_readers_sup}, fix_sup, [readers]),

  % Start stocks
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST'),
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST2'),

  % Install proxy
  gen_event:add_handler('MICEX.TEST', test_ev_proxy, [self(), 'MICEX.TEST']),
  gen_event:add_handler('MICEX.TEST2', test_ev_proxy, [self(), 'MICEX.TEST2']),

  % Get IDs
  % SentRequests2 = [Msg || {_, {gen_tcp, send, [fake_socket, Packet]}, _} <- meck:history(gen_tcp),
  %   {ok, #market_data_request{} = Msg, _, _} <- [fix:decode(iolist_to_binary(Packet))]],
  % [ID1] = [ReqID || #market_data_request{md_req_id = ReqID, fields = Fields} <- SentRequests2,
  %   lists:member({symbol,<<"TEST">>}, Fields)],

  % Send some MD for first stock
  % Conn = fix_read_conn:id(fix_test_read),
  % MDEntries = [[{md_entry_type, trade}, {md_entry_px, 18.2}, {md_entry_size, 14}]],
  % FixMessage = #market_data_snapshot_full_refresh{md_req_id = ID1, md_entries = MDEntries},
  % Conn ! {test_messages, [FixMessage]},

  receive
    {event, 'MICEX.TEST', #market_data_snapshot_full_refresh{symbol = 'MICEX.TEST'}} -> ok
  after
    100 -> error(timeout_routing)
  end,

  stop_normal_exitmsg(ReadersSup),
  meck:unload(test_ev_proxy),
  ok.


test_invalid_instrument() ->
  {ok, ReadersSup} = supervisor:start_link({local, fix_readers_sup}, fix_sup, [readers]),

  % Start stocks
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST'),
  {ok, _} = fix_reader:stock(fix_test_read, 'MICEX.TEST2'),

  Manager = whereis(fix_test_read),
  Connection = whereis(fix_test_read_conn),
  ?assertMatch({fix_connection, P} when is_pid(P), {fix_connection, Connection}),
  ?assertEqual(ok, fix_read_conn:status(Connection)),
  ?assertEqual(connected, fix_read_manager:status(Manager)),


  ?assertEqual({error, rejected}, fix_reader:stock(fix_test_read, 'MICEX.INVALID')),

  ?assertEqual({error, rejected}, fix_reader:subscribe(fix_test_read, 'MICEX.INVALID')),

  % timer:sleep(1000),
  % ?assertEqual(connected, fix_read_manager:status(Manager)),

  % timer:sleep(1000),
  ?assertEqual(ok, fix_read_conn:status(Connection)),
  stop_normal_exitmsg(ReadersSup),
  ok.





receive_msgs(_, 0) -> [];

receive_msgs(Timeout, Count) ->
  receive
    Msg -> [Msg | receive_msgs(Timeout, Count - 1)]
  after
    Timeout -> []
  end.
