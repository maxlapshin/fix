-module(fix_msg_sender).
-author('Max Lapshin <max@maxidoors.ru>').

-export([init/1, handle_event/2, terminate/2]).



init([Pid]) ->
  {ok, Pid}.

handle_event(Event, Pid) ->
  Pid ! Event,
  {ok, Pid}.


terminate(_,_) ->
  ok.

