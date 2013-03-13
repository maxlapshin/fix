-module(fix_connection).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([check_order/4, set_md_req_id/2]).
-export([logon/1, logon/2, subscribe/2, unsubscribe/3, status/1]).
-export([order_status/4, new_order_single/6, cancel_order/7]).

-export([open_next_log/1, decode_messages/3, entry_types/1]).

-record(fix_connection, {
  log,
  debug = false,
  socket,
  md_req_id = 20,
  host,
  port,
  target,
  sender,
  password,
  seq,
  heartbeat,
  consumer,
  ssl,
  transport,
  buffer = <<>>,
  properties = []
}).

-include("meta_access.hrl").
-include("../include/admin.hrl").
-include("../include/business.hrl").
-include("../include/fix.hrl").

start_link(Consumer, Options) ->
  gen_server:start_link(?MODULE, [Consumer, Options], []).



logon(Pid) ->
  logon(Pid, []).

logon(Pid, Options) ->
  Password = get(Pid, password),
  Heartbeat = get(Pid, heartbeat),
  CoD = case proplists:get_value(cancel_on_disconnect, Options) of
    true -> [{10001,"Y"}];
    _ -> []
  end,
  ok = gen_server:call(Pid, {msg, logon, [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}] ++ CoD}),
  receive
    #fix{pid = Pid, message = #logon{}} -> ok
  after
    5000 -> {error, timeout}
  end.

check_order(Pid, OrderId, Side, Stock) ->
  CheckOrder = [{cl_ord_id, OrderId}, {side, Side} | fix:stock_to_instrument_block(Stock)],
  ok = gen_server:call(Pid, {msg, order_status_request, CheckOrder}),
  receive
    #fix{pid = Pid, message = #execution_report{cl_ord_id = OrderId, exec_type = orderstatus, ord_status = filled}} -> {ok, filled};
    #fix{pid = Pid, message = #execution_report{cl_ord_id = OrderId, exec_type = orderstatus} = ER} -> {ok, ER}
  after
    1000 -> erlang:error({error, timeout})
  end.

new_order_single(Pid, OrderId, Stock, Side, Quantity, Options) ->  
  NewOrder = [{cl_ord_id, OrderId}] ++ fix:stock_to_instrument_block(Stock) ++ [{side, Side}, {transact_time, fix:now()}, {order_qty, Quantity} | Options],
  ok = gen_server:call(Pid, {msg, new_order_single, NewOrder}),
  {ok, OrderId}.


cancel_order(Pid, OrderId, CancelId, Stock, Side, Quantity, Options) ->
  OrderCancel = [{cl_ord_id, CancelId}, {orig_cl_ord_id, OrderId}] ++ fix:stock_to_instrument_block(Stock) ++
   [{side, Side}, {transact_time, fix:now()}, {order_qty, Quantity} | Options],
  ok = gen_server:call(Pid, {msg, order_cancel_request, OrderCancel}),
  ok.


entry_types(_) ->
  EntryTypes = [{md_entry_type,bid},{md_entry_type,offer},{md_entry_type,trade}],
  [{no_md_entry_types,length(EntryTypes)}|EntryTypes].

subscribe(Pid, Stock) ->
  MdReqId = next_md_req_id(Pid),
  Instrument = fix:stock_to_instrument_block(Stock),
  EntryTypes = entry_types(Instrument),
  Opts = [{md_req_id, MdReqId}, {subscription_request_type, 1}, {market_depth, 0}, {md_update_type, 0}]
    ++ EntryTypes ++ [{no_related_sym,1}] ++ Instrument,
  gen_server:call(Pid, {msg, market_data_request, Opts}),
  {ok, MdReqId}.

unsubscribe(Pid, MdReqId, Stock) ->
  Instrument = fix:stock_to_instrument_block(Stock),
  EntryTypes = entry_types(Instrument),
  Opts = [{md_req_id, MdReqId}, {subscription_request_type, 2}, {market_depth, 0}]++
    EntryTypes ++ [{no_related_sym,1}] ++ Instrument,
  gen_server:call(Pid, {msg, market_data_request, Opts}).

order_status(Pid, OrderId, Side, Stock) ->
  Opts = [{cl_ord_id, OrderId},{side, Side}] ++ fix:stock_to_instrument_block(Stock),
  gen_server:call(Pid, {msg, order_status_request, Opts}).

next_md_req_id(Pid) ->
  gen_server:call(Pid, next_md_req_id).

set_md_req_id(Pid, Id) ->
  gen_server:call(Pid, {set_md_req_id, Id}).

status(Pid) ->
  gen_server:call(Pid, status).

init([Consumer, Options]) ->
  {host, Host} = lists:keyfind(host, 1, Options),
  {port, Port} = lists:keyfind(port, 1, Options),
  {password, Password} = lists:keyfind(password, 1, Options),
  {sender, Sender} = lists:keyfind(sender, 1, Options),
  {target, Target} = lists:keyfind(target, 1, Options),
  SSL = proplists:get_value(ssl, Options),
  Heartbeat = proplists:get_value(heartbeat, Options, 30),
  Debug = case application:get_env(fix, debug) of
    {ok, true} -> true;
    _ -> false
  end,
  timer:send_interval(Heartbeat*1000, heartbeat),
  Log = case proplists:get_value(logging, Options) of
    undefined -> undefined;
    Profile -> open_next_log(Profile)
  end,
  erlang:monitor(process, Consumer),
  self() ! connect,
  {ok, #fix_connection{debug = Debug, host = Host, port = Port, target = Target, sender = Sender, heartbeat = Heartbeat, seq = 1, 
  consumer = Consumer, password = Password, ssl = SSL, log = Log}}.


handle_info(connect, #fix_connection{host = Host, port = Port, ssl = SSL} = Session) ->
  {Transport, Options} = case SSL of
    undefined ->
      {gen_tcp, [binary, {packet,raw}, {active, once}]};
    _ when is_list(SSL) ->
      ssl:start(),
      ?D({ssl, SSL}),
      {ssl, [binary, {active,once}|SSL]}
  end,

  case Transport:connect(Host, Port, Options) of
    {ok, Socket} ->
      {noreply, Session#fix_connection{socket = Socket, transport = Transport}};
    {error, _} = Error ->
      {stop, Error, Session}
  end;


handle_info(heartbeat, #fix_connection{} = Session) ->
  {noreply, send(heartbeat, [], Session)};

handle_info({'DOWN', _, _, Consumer, _}, #fix_connection{consumer = Consumer} = Session) ->
  ?D({session_exit}),
  {stop, normal, send(logout, [], Session)};

handle_info(stop, Session) ->
  {stop, normal, Session};

handle_info({Trans, Socket, Data}, #fix_connection{buffer = Buffer, debug = Debug, log = Log} = Session) when Trans == tcp orelse Trans == ssl ->
  Bin = case Buffer of
    <<>> -> Data;
    _ -> <<Buffer/binary, Data/binary>>
  end,
  case Trans of
    tcp -> inet:setopts(Socket, [{active,once}]);
    ssl -> ssl:setopts(Socket, [{active,once}])
  end,
  {Messages, Rest} = decode_messages(Bin, Debug, Log),
  handle_messages(Messages, Session#fix_connection{buffer = Rest});

handle_info({tcp_closed, Socket}, #fix_connection{socket = Socket, host = Host, port = Port} = Session) ->
  ?D({fix_connection,socket_closed, Host, Port}),
  {stop, normal, Session};

handle_info({ssl_closed, Socket}, #fix_connection{socket = Socket, host = Host, port = Port} = Session) ->
  ?D({fix_connection,socket_closed, Host, Port}),
  {stop, normal, Session};

handle_info(Msg, Session) ->
  ?D({unknown,Msg}),
  {noreply, Session}.

handle_call({get_field, Key}, _From, Session) ->
  {reply, get(Session, Key), Session};

handle_call({set_field, Key, Value}, _From, Session) ->
  {reply, ok, set(Session, Key, Value)};

handle_call(next_md_req_id, _From, #fix_connection{md_req_id = MdReqId} = Session) ->
  {reply, list_to_binary(integer_to_list(MdReqId)), Session#fix_connection{md_req_id = MdReqId + 1}};

handle_call({set_md_req_id, Id}, _From, #fix_connection{} = Session) ->
  {reply, ok, Session#fix_connection{md_req_id = Id}};

handle_call(status, _From, #fix_connection{} = Session) ->
  {reply, [], Session};

handle_call({msg, MessageType, Body}, _From, #fix_connection{} = Session) ->  
  {reply, ok, send(MessageType, Body, Session)}.


send(MessageType, Body, #fix_connection{seq = Seq, sender = Sender, target = Target, socket = Socket, debug = Debug, transport = Transport, log = Log} = Session) ->
  % if MessageType =/= heartbeat -> ?D({pack, MessageType, Body, Seq, Sender, Target}); true -> ok end,
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
  if MessageType =/= heartbeat andalso Log =/= undefined->
    catch file:write(Log, ["out ", fix:now(), " ", fix:dump(Bin), "\n"]);
  true -> ok
  end,
    
  if Debug == true andalso MessageType =/= heartbeat -> ?D({send, fix:dump(Bin)}); true -> ok end,
  Transport:send(Socket, Bin),
  Session#fix_connection{seq = Seq + 1}.

terminate(_,_) ->
  ok.

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

handle_messages([{#heartbeat{},_}|Messages], Session) ->
  % ?D(heartbeat),
  handle_messages(Messages, Session);

handle_messages([{Message,MessageBin}|Messages], #fix_connection{consumer = Consumer} = Session) ->
  % dump(Message),
  Consumer ! #fix{pid = self(), message = Message, bin = MessageBin},
  handle_messages(Messages, Session);

handle_messages([], #fix_connection{} = Session) ->
  {noreply, Session}.

% dump(#market_data_snapshot_full_refresh{fields = Fields, md_entry_type = Type, md_entry_px = Price, md_entry_size = Size}) ->
%   ?D({market_data, if Type == <<"1">> -> ask; true -> bid end, proplists:get_value(symbol, Fields), Price, Size});

open_next_log(Profile) ->
  {{Y,M,D},_} = calendar:universal_time(),
  Timestamp = io_lib:format("~4..0B-~2..0B-~2..0B", [Y,M,D]),

  LogBase = ["log/fix-", Profile, "-", Timestamp],
  LogSuffix = ".log",
  SymlinkPath = ["log/fix-", Profile, "-current.log"],

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
