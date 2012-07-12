#!/usr/bin/env escript
%%! -pa ebin

-mode(compile).

-compile(export_all).

% handle([_Evt, first, _Name]) ->
%   ok;
% 

handle(Tag, Name) ->
  handle(none, Tag, Name).
handle(Evt, Tag, Name) ->
  io:format("~s ~s~n", [Tag, Name]),
  ok.

main([]) ->
  Root = filename:dirname(escript:script_name()),
  [code:add_pathz(P) || P <- filelib:wildcard(Root ++ "/ebin")],
  
  application:start(fix),

  Pid1 = spawn(fun() ->
        fix_reader:subscribe(fix_proxy, {undefined, "USD/RUB"}, {?MODULE, handle, [first, <<"USDRUB">>]}),
        fix_reader:subscribe(fix_proxy, {"MICEX", "URKA"}, {?MODULE, handle, [first, <<"URKA.MM">>]}),
        receive
          stop ->
            ok
        end
    end),
  Pid2 = spawn(fun() ->
        fix_reader:subscribe(fix_proxy, {undefined, "USD/RUB"}, {?MODULE, handle, [second, <<"USDRUB">>]}),
        fix_reader:subscribe(fix_proxy, {"MICEX", "URKA"}, {?MODULE, handle, [second, <<"URKA.MM">>]}),
        receive
          stop ->
            ok
        end
    end),


  monitor(process, Pid1),
  monitor(process, Pid2),

  io:format(".......~n"),
  collect_exits(Pid1, Pid2).
  
collect_exits(dead, dead) ->
  ok;
collect_exits(Pid1, Pid2) ->
  receive
    {'DOWN', _, _, Pid1, Reason} ->
      io:format("first exited: ~p~n", [Reason]),
      collect_exits(dead, Pid2);
    {'DOWN', _, _, Pid2, Reason} ->
      io:format("second exited: ~p~n", [Reason]),
      collect_exits(Pid1, dead);
    stop -> ok
  end.
