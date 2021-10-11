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
