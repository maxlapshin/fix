-module(fix_connection).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([open_next_log/1, decode_messages/3, entry_types/1]).


entry_types(_) ->
  EntryTypes = [{md_entry_type,bid},{md_entry_type,offer},{md_entry_type,trade}],
  [{no_md_entry_types,length(EntryTypes)}|EntryTypes].


decode_messages(Bin, Debug, Log) ->
  decode_messages(Bin, [], Debug, Log).

decode_messages(Bin, Acc, Debug, Log) ->
  case fix:decode(Bin) of
    {ok, Message, MessageBin, Rest} ->
      if Log =/= undefined andalso element(1,Message) =/= heartbeat ->
        file:write(Log, ["in  ", fix:now(), " ", fix:dump(MessageBin), "\n"]);
      true -> ok
      end,
      if Debug == true -> ?D({in, fix:dump(MessageBin)}); true -> ok end,
      decode_messages(Rest, [{Message,MessageBin}|Acc], Debug, Log);
    {more, _} ->
      {lists:reverse(Acc), Bin};
    error ->
      ?D({error, Bin}),
      erlang:error(broken_fix)
  end.


open_next_log(Profile) ->
  {{Y,M,D},_} = calendar:universal_time(),
  Timestamp = io_lib:format("~4..0B-~2..0B-~2..0B", [Y,M,D]),

  LogBase = filename:join([log, ["fix-", Profile, "-", Timestamp]]),
  LogSuffix = ".log",
  SymlinkPath = filename:join([log, ["fix-", Profile, "-current.log"]]),

  LogPath = increment_while_exists(LogBase, 1, LogSuffix),

  ?D({open_fix_log,LogPath}),
  filelib:ensure_dir(LogPath),
  {ok, LogFile} = file:open(LogPath, [binary,append,raw]),

  file:delete(SymlinkPath),
  file:make_symlink(filename:basename(LogPath), SymlinkPath),
  LogFile.

increment_while_exists(Base, N, Suffix) ->
  NStr = if
    N < 1000 -> io_lib:format("~3..0B", [N]);
    true -> io_lib:print(N) % Just in case
  end,
  CurrentFile = [Base, "-", NStr, Suffix],
  case filelib:is_file(CurrentFile) of
    true -> increment_while_exists(Base, N + 1, Suffix);
    false -> lists:flatten(CurrentFile)
  end.
