-module(fix_exec_conn).

-author({"Danil Zagoskin", 'z@gosk.in'}).

-include("log.hrl").

-include("../include/admin.hrl").
-include("../include/business.hrl").
-include("../include/fix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NETWORK_TIMEOUT, 500).

% Public API
-export([id/1, start_link/2, connect/2]).
-export([attach/2, attach/3, mirror_to/2]).
-export([order_status/4, new_order_single/6, cancel_order/7]).

-export([status/1]).

% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3, terminate/2]).


-record(conn, {
    name,

    debug = false,
    log,

    socket,
    seq = 1,
    md_req_id = 20,

    host,
    port,
    ssl,
    transport,
    password,
    sender,
    target,

    heartbeat,
    consumer,
    logon_from,

    order_owners = [],
    status_requesters = [],
    mirror = [],

    buffer = <<>>
  }).


id(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_conn").


start_link(Name, Options) ->
  gen_server:start_link({local, id(Name)}, ?MODULE, [Name, Options], []).

connect(Pid, Timeout) when is_integer(Timeout) ->
  connect(Pid, [], Timeout);

connect(Pid, Options) when is_list(Options) ->
  gen_server:call(Pid, {connect_logon, Options}).

connect(Pid, Options, Timeout) when is_integer(Timeout), is_list(Options) ->
  gen_server:call(Pid, {connect_logon, Options}, Timeout).

status(Pid) ->
  gen_server:call(Pid, status).

attach(Fix, OrderId) ->
  attach(Fix, OrderId, self()).

attach(Fix, OrderId, Destination) ->
  gen_server:call(Fix, {attach, OrderId, Destination}).

mirror_to(Fix, Destination) ->
  gen_server:call(Fix, {mirror_to, Destination}).


% Request new order placement
new_order_single(Pid, OrderId, Stock, Side, Quantity, Options) ->
  NewOrder = [{transact_time, fix:now()}, {order_qty, Quantity} | Options],
  ok = gen_server:call(Pid, {msg, new_order_single, OrderId, Side, Stock, NewOrder}),
  {ok, OrderId}.

% Cancel an order placed before
cancel_order(Pid, OrderId, CancelId, Stock, Side, Quantity, Options) ->
  OrderCancel = [{orig_cl_ord_id, OrderId}, {transact_time, fix:now()}, {order_qty, Quantity} | Options],
  ok = gen_server:call(Pid, {msg, order_cancel_request, CancelId, Side, Stock, OrderCancel}),
  ok.

% Request order status
order_status(Pid, OrderId, Side, Stock) ->
  gen_server:call(Pid, {msg, order_status_request, OrderId, Side, Stock, []}).


% Initialize
init([Name, GivenOptions]) ->
  Options = GivenOptions ++ fix:get_value(Name),

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

  Log = case proplists:get_value(logging, Options) of
    undefined -> undefined;
    Profile -> fix_connection:open_next_log(Profile)
  end,

  {ok, #conn{
      name = Name,
      debug = Debug,
      log = Log,

      host = Host,
      port = Port,
      ssl = SSL,
      password = Password,

      sender = Sender,
      target = Target,
      heartbeat = Heartbeat
    }}.


handle_call(status, _From, #conn{status_requesters = SRs, order_owners = OOwners} = Conn) ->
  Result = [{order_owners, OOwners}, {status_requesters, SRs}],
  {reply, Result, Conn};

handle_call({attach, OrderId, Destination}, _From, #conn{} = Conn) ->
  NewConn = remember_request(attach, OrderId, Destination, Conn),
  {reply, ok, NewConn};

handle_call({mirror_to, Destination}, _From, #conn{mirror = Mirror0} = Conn) ->
  erlang:monitor(process, Destination),
  Mirror = lists:umerge([Destination], Mirror0),
  {reply, ok, Conn#conn{mirror = Mirror}};

handle_call({connect_logon, Options}, From, #conn{} = Conn) ->
  Connected = #conn{} = do_connect(Conn),
  Logon_sent = send_logon(Connected#conn{logon_from = From}, Options),
  {noreply, Logon_sent, ?NETWORK_TIMEOUT};

handle_call({msg, MessageType, ClOrdId, Side, Stock, Tail}, {Owner, _Ref}, #conn{} = Conn) ->
  Body = fix:stock_to_instrument_block(Stock) ++ [{side, Side}, {cl_ord_id, ClOrdId}|Tail],
  NewConn = remember_request(MessageType, ClOrdId, Owner, Conn),
  {reply, ok, send(MessageType, Body, NewConn)}.



handle_info(heartbeat, #conn{} = Conn) ->
  {noreply, send(heartbeat, [], Conn)};

handle_info(timeout, #conn{logon_from = {_, _} = From} = Conn) ->
  gen_server:reply(From, {error, logon_timeout}),
  {noreply, Conn#conn{logon_from = undefined}};

handle_info({Trans, Socket, Data}, #conn{buffer = PrevBuf, debug = Debug, log = Log} = Conn)
when Trans == tcp; Trans == ssl; Trans == test ->
  Buffer = <<PrevBuf/binary, Data/binary>>,
  {Messages, Rest} = fix_connection:decode_messages(Buffer, Debug, Log),

  case Trans of
    tcp -> inet:setopts(Socket, [{active,once}]);
    ssl -> ssl:setopts(Socket, [{active,once}]);
    test -> ok
  end,
  handle_messages(Messages, Conn#conn{buffer = Rest});

handle_info({test_messages, Messages}, #conn{} = Conn) ->
  MsgsBins = [{Msg, <<>>} || Msg <- Messages],
  handle_messages(MsgsBins, Conn);

handle_info({Closed, Socket}, #conn{socket = Socket, host = Host, port = Port} = Conn)
when Closed == tcp_closed; Closed == ssl_closed ->
  ?D({fix_connection,socket_closed, Host, Port}),
  {stop, socket_closed, Conn};

handle_info({'DOWN', _, _, Consumer, _}, #conn{consumer = Consumer} = Conn) ->
  {stop, consumer_dead, Conn};

handle_info({'DOWN', _, _, Owner, _}, #conn{} = Conn) when is_pid(Owner) ->
  {noreply, unsubscribe_owner(Owner, Conn)};

handle_info({'EXIT', _From, Reason}, #conn{} = Conn) ->
  {stop, Reason, Conn}.

do_connect(#conn{host = Host, port = Port, ssl = SSL} = Conn) ->
  {Transport, AddOptions} = case SSL of
    undefined ->
      {gen_tcp, [{packet,raw}]};
    _ when is_list(SSL) ->
      ssl:start(),
      ?D({ssl, SSL}),
      {ssl, SSL}
  end,
  Options = [binary, {active,once}, {send_timeout, ?NETWORK_TIMEOUT} | AddOptions],

  case Transport:connect(Host, Port, Options, ?NETWORK_TIMEOUT) of
    {ok, Socket} ->
      Conn#conn{socket = Socket, transport = Transport};
    {error, Error} ->
      erlang:throw({reply, {error, {connect, Error}}, Conn})
  end.

send_logon(#conn{password = Password, heartbeat = Heartbeat} = Conn, Options) ->
  CoD = case proplists:get_value(cancel_on_disconnect, Options) of
    true -> [{10001,"Y"}];
    _ -> []
  end,
  MsgBody = [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}] ++ CoD,

  timer:send_interval(Heartbeat*1000, heartbeat),
  send(logon, MsgBody, Conn).



send(MessageType, Body, #conn{seq = Seq, sender = Sender, target = Target,
    socket = Socket, debug = Debug, transport = Transport, log = Log} = Conn) ->
  % if MessageType =/= heartbeat -> ?D({pack, MessageType, Body, Seq, Sender, Target}); true -> ok end,
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
  if Debug == true andalso MessageType =/= heartbeat -> ?D({send, fix:dump(Bin)}); true -> ok end,
  Result = Transport:send(Socket, Bin),

  if % Log to file after data is sent
    MessageType =/= heartbeat andalso Log =/= undefined ->
      catch file:write(Log, ["out ", fix:now(), " ", fix:dump(Bin), "\n"]);
    true -> ok
  end,

  ok = Result, % Crash on send failure

  Conn#conn{seq = Seq + 1}.

terminate(_,_) ->
  ok.


remember_request(NewRequest, Id, Pid, #conn{order_owners = Owners} = Conn)
when NewRequest == order_cancel_request; NewRequest == new_order_single; NewRequest == attach ->
  Conn#conn{order_owners = do_remember(Id, Pid, Owners)};

remember_request(order_status_request, Id, Pid, #conn{status_requesters = Owners} = Conn) ->
  Conn#conn{status_requesters = do_remember(Id, Pid, Owners)}.

do_remember(Id, Pid, Mapping) ->
  case lists:member({Id, Pid}, Mapping) of
    true -> Mapping;
    false ->
      maybe_monitor_new(Pid, Mapping),
      [{Id, Pid}|Mapping]
  end.

maybe_monitor_new(Pid, Mapping) ->
  case lists:keymember(Pid, 1, Mapping) of
    true -> ok;
    false -> erlang:monitor(process, Pid)
  end.

unsubscribe_owner(Owner, #conn{status_requesters = SRs, order_owners = OOwners, mirror = Mirror} = Conn) ->
  Conn#conn{
    status_requesters = [SR || SR = {_id, Pid} <- SRs, Pid /= Owner],
    order_owners = [OO || OO = {_id, Pid} <- OOwners, Pid /= Owner],
    mirror = [M || M <- Mirror, M /= Owner] }.


handle_messages([{#heartbeat{},_}|Messages], #conn{} = Conn) ->
  % ?D(heartbeat),
  handle_messages(Messages, Conn);

% Reply to pending logon request
handle_messages([{#logon{},_}|Messages], #conn{logon_from = {Pid, _} = From} = Conn) ->
  gen_server:reply(From, ok),
  erlang:monitor(process, Pid),
  LoggedOn = Conn#conn{logon_from = undefined, consumer = Pid},
  handle_messages(Messages, LoggedOn);

handle_messages([{#logout{} = Logout, Bin}|_Messages], #conn{consumer = undefined, logon_from = {_, _} = From} = Conn) ->
  ?D(Logout),
  gen_server:reply(From, {error, {Logout, Bin}}),
  {stop, logged_out, Conn};

handle_messages([{#order_cancel_reject{cl_ord_id = ClOrdId} = Reject, Bin}|Messages], #conn{} = Conn) ->
  Handled = pass_message(Reject, Bin, ClOrdId, Conn, true),
  handle_messages(Messages, Handled);

handle_messages([{#execution_report{cl_ord_id = ClOrdId, orig_cl_ord_id = OrigOrdId, ord_status = Status} = ER, Bin}|Messages], #conn{} = Conn) ->
  Final = lists:member(Status, [filled, rejected, canceled]),
  Handled = pass_message(ER, Bin, [ClOrdId, OrigOrdId], Conn, Final),
  handle_messages(Messages, Handled);

handle_messages([{Unknown, Bin}|Messages], #conn{consumer = Consumer} = Conn) when Consumer /= undefined ->
  Consumer ! #fix{pid = self(), message = Unknown, bin = Bin},
  handle_messages(Messages, Conn);

handle_messages([], #conn{} = Conn) ->
  {noreply, Conn}.


% Perform message pass
pass_message(Message, Bin, ClOrdId, #conn{status_requesters = SRs, order_owners = OOwners, mirror = Mirror} = Conn, Final) when is_boolean(Final) ->
  {OPids, NewOwners} = which_pass(ClOrdId, OOwners, Final),
  {SPids, NewSRs} = which_pass(ClOrdId, SRs, true),

  ToPass = #fix{pid = self(), message = Message, bin = Bin},
  [Pid ! ToPass || Pid <- lists:umerge([OPids, SPids, Mirror])],

  Conn#conn{order_owners = NewOwners, status_requesters = NewSRs}.


which_pass(ClOrdId, Map, Final) when not is_list(ClOrdId) ->
  which_pass([ClOrdId], Map, Final);

which_pass(ClOrdIds, Map, false) ->
  Pids = lists:usort([Pid || {Id, Pid} <- Map, lists:member(Id, ClOrdIds)]),
  {Pids, Map};

which_pass(ClOrdIds, Map, true) ->
  {Matched, Kept} = lists:partition(fun({Id, _}) -> lists:member(Id, ClOrdIds) end, Map),
  Pids = lists:usort([Pid || {_Id, Pid} <- Matched]),
  {Pids, Kept}.

