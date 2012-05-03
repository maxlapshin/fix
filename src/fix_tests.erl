-module(fix_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("../include/admin.hrl").
-include("../include/business.hrl").


fix_decode_1_test() ->
  ?assertMatch({ok, #heartbeat{signature = <<"A",1,"89=234">>}, <<>>}, fix:decode(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).
