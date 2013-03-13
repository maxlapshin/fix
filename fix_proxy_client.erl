#!/usr/bin/env escript

-mode(compile).

main([]) ->
  io:format("./fix_proxy_client.erl [Exchange] SYMBOL~n"),
  erlang:exit(1);

main(Args) ->
  Root = filename:dirname(escript:script_name()),
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/../../apps/*/ebin")],
  [code:add_pathz(Path) || Path <- filelib:wildcard(Root ++ "/../../deps/*/ebin")],
  helper:load_conf(),
  application:start(fix),
  {Debug, Args1} = case Args of
    ["-d" | Arg_] -> {true, Arg_};
    _ -> {false, Args}
  end,
  
  application:set_env(fix, debug, Debug),
  {Exchange, Symbol} = case Args1 of
    [Sym] -> {undefined, Sym};
    [Exch, Sym] -> {Exch, Sym}
  end,

  fix_proxy_client:run(Exchange, Symbol),
  ok.
  
