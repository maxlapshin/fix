#!/usr/bin/env escript

main([]) ->
  Root = filename:dirname(escript:script_name()),
  code:add_pathz(Root ++ "/ebin"),
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/../../deps/*/ebin")],
  application:start(fix),
  fix:start_listener(),
  receive
    stop -> ok
  end.

    