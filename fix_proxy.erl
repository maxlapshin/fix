#!/usr/bin/env escript

main(Args) ->
  Root = filename:dirname(escript:script_name()),
  code:add_pathz(Root ++ "/ebin"),
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/../../deps/*/ebin")],
  application:start(fix),
  Debug = lists:member("-d", Args),
  application:set_env(fix, debug, Debug),
  fix:start_listener(),
  receive
    stop -> ok
  end.

    