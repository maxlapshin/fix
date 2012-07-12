-module(fix_connection).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([options/1, check_order/5, set_md_req_id/2]).
-export([logon/1, subscribe/3, unsubscribe/4, status/1]).
-export([order_status/3, new_order_single/7, cancel_order/8]).

-record(fix_connection, {
  socket,
  md_req_id = 20,
  host,
  port,
  target,
  sender,
  password,
  seq,
  hearbeat,
  consumer,
  buffer = <<>>,
  properties = []
}).

-include("meta_access.hrl").
-include("../include/admin.hrl").
-include("../include/business.hrl").

start_link(Consumer, Options) ->
  gen_server:start_link(?MODULE, [Consumer, Options], []).

options(Name) ->
  {ok, Env} = application:get_env(fix, config),
  case lists:keyfind(Name, 1, Env) of
    {Name, Options} -> Options;
    false -> erlang:error({missing_required_option,Name})
  end.



logon(Pid) ->
  Password = get(Pid, password),
  Heartbeat = get(Pid, hearbeat),
  gen_server:call(Pid, {msg, logon, [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}]}),
  receive
    {fix, Pid, #logon{}} -> ok
  after
    5000 -> {error, timeout}
  end.

check_order(Pid, OrderId, Side, Exchange, Symbol) ->
  CheckOrder = [{cl_ord_id, OrderId}, {security_exchange, Exchange}, {symbol, Symbol}, {cfi_code, "EXXXXX"},
  {side, Side}],
  gen_server:call(Pid, {msg, order_status_request, CheckOrder}),
  receive
    {fix, Pid, #execution_report{cl_ord_id = OrderId, exec_type = orderstatus, ord_status = filled}} -> {ok, filled};
    {fix, Pid, #execution_report{cl_ord_id = OrderId, exec_type = orderstatus} = ER} -> {ok, ER}
  after
    1000 -> erlang:error({error, timeout})
  end.

cfi_code(undefined) -> "EXXXXX";
cfi_code(_) -> "MRCXXX".

new_order_single(Pid, OrderId, Exchange, Symbol, Side, Quantity, Options) ->
  CFICode = cfi_code(Exchange),
  ExchangeOpts = case Exchange of
    undefined -> [];
    _ -> [{security_exchange, Exchange}]
  end,
  
  NewOrder = ExchangeOpts ++ [{cl_ord_id, OrderId}, {symbol, Symbol}, {cfi_code, CFICode}, 
  {side, Side}, {transact_time, fix:now()}, {order_qty, Quantity} | Options],
  gen_server:call(Pid, {msg, new_order_single, NewOrder}),
  {ok, OrderId}.


cancel_order(Pid, OrderId, CancelId, Exchange, Symbol, Side, Quantity, Options) ->
  OrderCancel = [{cl_ord_id, CancelId}, {orig_cl_ord_id, OrderId}, {security_exchange, Exchange}, {symbol, Symbol}, {cfi_code, "EXXXXX"}, 
  {side, Side}, {transact_time, fix:now()}, {order_qty, Quantity} | Options],
  gen_server:call(Pid, {msg, order_cancel_request, OrderCancel}),
  ok.


subscribe(Pid, Exchange, Symbol) ->
  CFICode = cfi_code(Exchange),
  ExchangeOpts = case Exchange of
    undefined -> [];
    _ -> [{security_exchange, Exchange}]
  end,
  MdReqId = next_md_req_id(Pid),
  Opts = [{md_req_id, MdReqId}, {subscription_request_type, 1}, {market_depth, 0}, {md_update_type, 0},
    {no_md_entry_types,3},{md_entry_type,bid},{md_entry_type,offer},{md_entry_type,trade},{no_related_sym,1},
    {symbol, Symbol},{cfi_code, CFICode}] ++ ExchangeOpts,
  gen_server:call(Pid, {msg, market_data_request, Opts}),
  {ok, MdReqId}.

unsubscribe(Pid, MdReqId, Exchange, Symbol) ->
  CFICode = cfi_code(Exchange),
  ExchangeOpts = case Exchange of
    undefined -> [];
    _ -> [{security_exchange, Exchange}]
  end,
  Opts = [{md_req_id, MdReqId}, {subscription_request_type, 2}, {market_depth, 0}, {no_md_entry_types, 2}, 
  {md_entry_type,bid},{md_entry_type,offer}, {no_related_sym, 1},
  {symbol, Symbol},{cfi_code, CFICode}] ++ ExchangeOpts,
  gen_server:call(Pid, {msg, market_data_request, Opts}).

order_status(Pid, Side, OrderId) ->
  SideCode = case Side of
    buy -> 1;
    sell -> 2
  end,
  Opts = [{cl_ord_id, OrderId},{side, SideCode}],
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
  Heartbeat = proplists:get_value(heartbeat, Options, 30),
  timer:send_interval(Heartbeat*1000, hearbeat),
  erlang:monitor(process, Consumer),
  self() ! connect,
  {ok, #fix_connection{host = Host, port = Port, target = Target, sender = Sender, hearbeat = Heartbeat, seq = 1, 
  consumer = Consumer, password = Password}}.


handle_info(connect, #fix_connection{host = Host, port = Port} = Session) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet,raw}, {active, once}]),
  {noreply, Session#fix_connection{socket = Socket}};

handle_info(hearbeat, #fix_connection{} = Session) ->
  {noreply, send(heartbeat, [], Session)};

handle_info({'DOWN', _, _, Consumer, _}, #fix_connection{consumer = Consumer} = Session) ->
  ?D({session_exit}),
  {stop, normal, send(logout, [], Session)};

handle_info(stop, Session) ->
  {stop, normal, Session};

handle_info({tcp, Socket, Data}, #fix_connection{buffer = Buffer} = Session) ->
  Bin = case Buffer of
    <<>> -> Data;
    _ -> <<Buffer/binary, Data/binary>>
  end,
  inet:setopts(Socket, [{active,once}]),
  {Messages, Rest} = decode_messages(Bin),
  handle_messages(Messages, Session#fix_connection{buffer = Rest});

handle_info({tcp_closed, Socket}, #fix_connection{socket = Socket, host = Host, port = Port} = Session) ->
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


send(MessageType, Body, #fix_connection{seq = Seq, sender = Sender, target = Target, socket = Socket} = Session) ->
  % if MessageType =/= heartbeat -> ?D({pack, MessageType, Body, Seq, Sender, Target}); true -> ok end,
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
  % if MessageType =/= heartbeat -> ?D({send, fix:dump(Bin)}); true -> ok end,
  gen_tcp:send(Socket, Bin),
  Session#fix_connection{seq = Seq + 1}.

terminate(_,_) ->
  ok.

decode_messages(Bin) ->
  decode_messages(Bin, []).

decode_messages(Bin, Acc) ->
  case fix:decode(Bin) of
    {ok, Message, Rest} ->
      % ?D({in, fix:dump(Bin)}),
      decode_messages(Rest, [Message|Acc]);
    {more, _} ->
      {lists:reverse(Acc), Bin};
    error ->
      ?D({error, Bin}),
      erlang:error(broken_fix)
  end.

handle_messages([#heartbeat{}|Messages], Session) ->
  % ?D(heartbeat),
  handle_messages(Messages, Session);

handle_messages([Message|Messages], #fix_connection{consumer = Consumer} = Session) ->
  % dump(Message),
  Consumer ! {fix, self(), Message},
  handle_messages(Messages, Session);

handle_messages([], #fix_connection{} = Session) ->
  {noreply, Session}.

% dump(#market_data_snapshot_full_refresh{fields = Fields, md_entry_type = Type, md_entry_px = Price, md_entry_size = Size}) ->
%   ?D({market_data, if Type == <<"1">> -> ask; true -> bid end, proplists:get_value(symbol, Fields), Price, Size});

dump(#logon{}) ->
  ?D(logged_in);

dump(Message) ->
  ?D({fix, Message}).

