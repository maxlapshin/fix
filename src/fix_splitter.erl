-module(fix_splitter).

-on_load(init_nif/0).
-include("log.hrl").

-export([split/1, field_by_number/1]).

init_nif() ->
  Path = filename:dirname(code:which(?MODULE)) ++ "/../priv",
  Load = erlang:load_nif(Path ++ "/fix_splitter", 0),
  case Load of
    ok -> ok;
    {error, {Reason,Text}} -> io:format("Load fix_splitter failed. ~p:~p~n", [Reason, Text])
  end,
  ok.

split(_Binary) ->
  erlang:error(not_implemented).

field_by_number(_Field) ->
  erlang:error(not_implemented).


-include_lib("eunit/include/eunit.hrl").


