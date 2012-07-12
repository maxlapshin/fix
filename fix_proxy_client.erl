#!/usr/bin/env escript

-mode(compile).

main([]) ->
  io:format("./fix_proxy_client.erl [Exchange] SYMBOL~n"),
  erlang:exit(1);

main(Args) ->
  Root = filename:dirname(escript:script_name()),
  code:add_pathz(Root ++ "/ebin"),
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/../../deps/*/ebin")],
  application:start(fix),
  {Exchange, Symbol} = case Args of
    [Sym] -> {undefined, Sym};
    [Exch, Sym] -> {Exch, Sym}
  end,

  fix_proxy_client:run(Exchange, Symbol),
  ok.
  
