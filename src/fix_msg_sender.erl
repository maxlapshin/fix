-module(fix_msg_sender).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2, code_change/3]).


init([{M,F,A}]) ->
  {ok, {M,F,A}};

init([Pid]) ->
  {ok, {Pid}}.

handle_event(Event, {M,F,A} = State) ->
  erlang:apply(M, F, [Event|A]),
  {ok, State};

handle_event(Event, {Pid}) ->
  Pid ! Event,
  {ok, {Pid}}.

handle_info(_,State) ->
  {ok, State}.


terminate(_,_) ->
  ok.

handle_call(Call,State) ->
  {ok, {error, Call}, State}.


code_change(_,State,_) -> {ok, State}.
