%%% @doc Fix log parser
%%% This module reads line-by-line log written by fix_connection,
%%% returning entry description

-module(fix_log_parser).
-author({"Danil Zagoskin", 'z@gosk.in'}).

-export([next_entry/1, read_file/1]).

read_file(Filename) ->
  {ok, F} = file:open(Filename, [read, binary, raw]),
  Entries = read_all_entries(F),
  ok = file:close(F),
  {ok, Entries}.

next_entry(File) ->
  case file:read_line(File) of
    {ok, Line} -> parse_line(Line);
    eof -> eof
  end.

parse_line(Line) ->
  % First, parse log-specific fields - space-separated direction, local timestamp, fix dump
  {match, [DirBin, ClientTime, Dump]} = re:run(Line, "^([^ ]+) +([^ ]+) +(.*)$", [{capture, [1, 2, 3], binary}]),

  Dir = erlang:binary_to_atom(DirBin, latin1),
  Dump1 = erlang:iolist_to_binary(re:replace(Dump, "\\|", "\1", [global])),

  MessageProps = case fix:decode_fields(Dump1) of
    {ok, Message, Dump1, <<>>} ->
      [{raw_fix, Dump1} | Message]
  end,

  {Dir, ClientTime, MessageProps}.

read_all_entries(File) ->
  read_all_entries(File, []).

read_all_entries(_File, [eof|Acc]) ->
  lists:reverse(Acc);
read_all_entries(File, Acc) ->
  NewAcc = [next_entry(File)|Acc],
  read_all_entries(File, NewAcc).
