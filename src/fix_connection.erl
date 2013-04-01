-module(fix_connection).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([open_next_log/1, open_next_log/2, write_log/4]).
-export([decode_messages/3, entry_types/1]).


entry_types(_) ->
  EntryTypes = [{md_entry_type,bid},{md_entry_type,offer},{md_entry_type,trade}],
  [{no_md_entry_types,length(EntryTypes)}|EntryTypes].


decode_messages(Bin, Debug, Log) ->
  decode_messages(Bin, [], Debug, Log).

decode_messages(Bin, Acc, Debug, Log) ->
  case fix:decode(Bin) of
    {ok, Message, MessageBin, Rest} ->
      % Append new string to log
      NextLog = write_log(Log, in, element(1,Message), MessageBin),
      % Print debug if requested
      Debug == true andalso ?D({in, fix:dump(MessageBin)}),
      % Continue decoding
      decode_messages(Rest, [{Message,MessageBin}|Acc], Debug, NextLog);
    {more, _} ->
      return_decoded(lists:reverse(Acc), Bin, Log);
    error ->
      ?D({error, Bin}),
      erlang:error(broken_fix)
  end.

% When log is buffer, return log as third element of tuple
return_decoded(Messages, Rest, {buffer, _} = Log) ->
  {Messages, Rest, Log};
return_decoded(Messages, Rest, _Log) ->
  {Messages, Rest}.

% If log is defined, append to buffer (iolist) or plain file write
write_log(undefined, _Data) -> % no-op for undefined log
  undefined;
write_log({buffer, Buffer}, Data) ->
  {buffer, [Buffer, Data]};
write_log(File, Data) ->
  ok = file:write(File, Data),
  File.

% Construct log string from given direction, message type and message binary; write log
write_log(Log, _Direction, heartbeat, _Bin) ->
  Log; % We don't log heartbeats
write_log(undefined, _Direction, _Type, _Bin) ->
  undefined; % Avoid constructing log string when it will not be written
write_log(Log, Direction, _Type, Bin) ->
  LogString = [dir_str(Direction), fix:now(), " ", fix:dump(Bin), "\n"],
  write_log(Log, LogString).

dir_str(in)  -> "in  ";
dir_str(out) -> "out ".


% Find next unused log filename
open_next_log(undefined) -> undefined; % no-op for undefined profile
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

% Open log and flush buffer
open_next_log(Profile, {buffer, Buffer}) ->
  LogFile = open_next_log(Profile),
  write_log(LogFile, Buffer).


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
