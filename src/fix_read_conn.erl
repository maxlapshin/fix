-module(fix_read_conn).
-author({"Danil Zagoskin", 'z@gosk.in'}).

-include("log.hrl").

-include("../include/admin.hrl").
-include("../include/business.hrl").
-include("../include/fix.hrl").
-include("../include/fix_version.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NETWORK_TIMEOUT, 500).

% Public API
-export([id/1, start_link/2, connect/2, subscribe/2, unsubscribe/2]).
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
    version = ?FIX_4_4,

    heartbeat,
    consumer,
    logon_from,

    subscriptions = [],
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

subscribe(Pid, Stock) ->
  gen_server:call(Pid, {subscribe, Stock}).

unsubscribe(Pid, Stock) ->
  gen_server:call(Pid, {unsubscribe, Stock}).

status(Pid) ->
  gen_server:call(Pid, status).


init([Name, GivenOptions]) ->
  Options = GivenOptions ++ fix:get_value(Name),

  {host, Host} = lists:keyfind(host, 1, Options),
  {port, Port} = lists:keyfind(port, 1, Options),
  Password = proplists:get_value(password, Options, no_password),
  {sender, Sender} = lists:keyfind(sender, 1, Options),
  {target, Target} = lists:keyfind(target, 1, Options),
  Version = proplists:get_value(version, Options, ?FIX_4_4),

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

  % We want to terminate FIX gracefully if possible

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
      version = Version,
      heartbeat = Heartbeat
    }}.


handle_call(status, _From, #conn{} = Conn) ->
  {reply, ok, Conn};

handle_call({connect_logon, Options}, From, #conn{} = Conn) ->
  Connected = #conn{} = do_connect(Conn),
  Logon_sent = send_logon(Connected#conn{logon_from = From}, Options),
  {noreply, Logon_sent, ?NETWORK_TIMEOUT};


handle_call({subscribe, Stock}, _From, #conn{} = Conn) ->
  {ReqId, NewConn} = subscribe_stock(Stock, Conn),
  {reply, {ok, ReqId}, NewConn};


handle_call({unsubscribe, Stock}, _From, #conn{} = Conn) ->
  case unsubscribe_stock(Stock, Conn) of
    {ok, NewConn} ->
      {reply, ok, NewConn};
    {error, _} = Error ->
      {reply, Error, Conn}
  end.


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

handle_info({tcp_error, Socket, Reason}, #conn{socket = Socket, host = Host, port = Port} = Conn) ->
  ?D({fix_connection,socket_error, Host, Port, Reason}),
  {stop, socket_closed, Conn};

% We monitor only stocks, so try to unsubscribe
handle_info({'DOWN', _, _, {Stock, _}, _}, #conn{} = Conn) when is_atom(Stock) ->
  case unsubscribe_stock(Stock, Conn) of
    {ok, NewConn} ->
      ?D({stock_terminated, Stock}),
      {noreply, NewConn};
    {error, notfound} ->
      {noreply, Conn}
  end;

handle_info({'EXIT', _From, Reason}, #conn{} = Conn) ->
  {stop, Reason, Conn}.


terminate(_Reason, #conn{} = _Conn) -> ok.





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

send_logon(#conn{password = Password, version = Version, heartbeat = Heartbeat} = Conn, Options) ->
  CoD = case proplists:get_value(cancel_on_disconnect, Options) of
    true -> [{10001,"Y"}];
    _ -> []
  end,
  CommonParams = [{encrypt_method, 0},
                  {heart_bt_int, Heartbeat},
                  {reset_seq_num_flag, "Y"}],
  PasswordParams =
    case Password of
      no_password -> [];
      _ -> [{password, Password}]
    end,
  VersionParams =
    case Version of
      ?FIX_5_0_SP2 -> [{default_appl_ver_id, 9}]; % TODO: use something like fix_50_sp2
        _ -> []
    end,
  MsgBody = CommonParams ++ PasswordParams ++ VersionParams ++ CoD,

  timer:send_interval(Heartbeat*1000, heartbeat),
  send(logon, MsgBody, Conn).



send(MessageType, Body, #conn{seq = Seq, sender = Sender, target = Target, transport = Transport, socket = Socket,
    version = Version, debug = Debug, log = Log} = Conn) ->
  % if MessageType =/= heartbeat -> ?D({pack, MessageType, Body, Seq, Sender, Target}); true -> ok end,
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target, Version),
  % ?DBG("~s  ~s", [MessageType, fix:dump(Bin)]),
  if MessageType =/= heartbeat andalso Log =/= undefined->
    catch file:write(Log, ["out ", fix:now(), " ", fix:dump(Bin), "\n"]);
  true -> ok
  end,
    
  if Debug == true andalso MessageType =/= heartbeat -> ?D({send, fix:dump(Bin)}); true -> ok end,
  Transport:send(Socket, Bin),
  Conn#conn{seq = Seq + 1}.



handle_messages([{#heartbeat{},_}|Messages], #conn{} = Conn) ->
  % ?D(heartbeat),
  handle_messages(Messages, Conn);

% Reply to pending logon request
handle_messages([{#logon{},_}|Messages], #conn{logon_from = {Pid, _} = From} = Conn) ->
  gen_server:reply(From, ok),
  LoggedOn = Conn#conn{logon_from = undefined, consumer = Pid},
  handle_messages(Messages, LoggedOn);

% discard messages before logon
handle_messages([_Msg|Messages], #conn{consumer = undefined} = Conn) ->
  handle_messages(Messages, Conn);

handle_messages([{#market_data_snapshot_full_refresh{} = Msg,_}|Messages], #conn{} = Conn) ->
  handle_messages(Messages, route_md(Msg, Conn));

handle_messages([{#market_data_request_reject{} = Reject,_}|Messages], #conn{} = Conn) ->
  NewConn = handle_reject(Reject, Conn),
  handle_messages(Messages, NewConn);

handle_messages([{Message,MessageBin}|Messages], #conn{consumer = Consumer} = Conn) ->
  % dump(Message),
  Consumer ! #fix{pid = self(), message = Message, bin = MessageBin},
  handle_messages(Messages, Conn);

handle_messages([], #conn{} = Conn) ->
  {noreply, Conn}.


route_md(#market_data_snapshot_full_refresh{md_req_id = MdReqId, md_entries = MdEntries} = Msg, 
  #conn{subscriptions = Subs, consumer = Consumer} = Conn) ->
  case lists:keyfind(MdReqId, 1, Subs) of
    false ->
      ?D({parasite, MdReqId, Msg#market_data_snapshot_full_refresh.symbol}),
      Conn;
    {MdReqId, Stock, false} ->
      Consumer ! {stock_available, Stock},
      route_md(Msg, Conn#conn{subscriptions = lists:keystore(MdReqId, 1, Subs, {MdReqId, Stock})});
    {MdReqId, Stock} when length(MdEntries) > 0 ->
      Instrument1 = Msg#market_data_snapshot_full_refresh.symbol,
      Instrument2 = element(2, fix:stock_to_instrument(Stock)),
      Instrument1 == Instrument2 orelse error({MdReqId,Stock,Msg}),
      catch gen_event:notify(Stock, Msg#market_data_snapshot_full_refresh{symbol = Stock}),
      Conn;
    {MdReqId, Stock} when MdEntries == [] ->
      ?D({empty_md_snapshot, Stock}),
      Conn
  end.

handle_reject(#market_data_request_reject{md_req_id = RejReqId} = Reject, #conn{consumer = Consumer, subscriptions = Subs} = Conn) ->
  case lists:keyfind(RejReqId, 1, Subs) of
    false ->
      % Reject for unknown stock, ignore
      Conn;
    {RejReqId, Stock, false} ->
      % Let manager handle reject
      Consumer ! {reject, Stock, Reject},
      Conn#conn{subscriptions = lists:keyreplace(RejReqId, 1, Subs, {RejReqId, Stock})};
    {RejReqId, Stock} ->
      % Let manager handle reject
      Consumer ! {reject, Stock, Reject},
      Conn
  end.

subscribe_stock(Stock, #conn{md_req_id = ReqId, subscriptions = Subs} = Conn) ->
  % Construct request
  RqBody = [{md_req_id, ReqId},
    {subscription_request_type, 1}, {market_depth, 0}, {md_update_type, 0}, {no_related_sym,1} |
    fix:stock_to_instrument_block(Stock) ++ fix_connection:entry_types(Stock)],
  % We get binary ReqId from fix, so convert it now
  BinReqId = list_to_binary(integer_to_list(ReqId)),
  % Remember request
  ?DBG("subscribe ~s ~B, ~p", [Stock, ReqId, fix:stock_to_instrument_block(Stock)]),
  NewConn = Conn#conn{
    md_req_id = ReqId + 1,
    subscriptions = [{BinReqId, Stock, false}|Subs] },
  % Monitor to unsubscribe stock on death
  erlang:monitor(process, Stock),
  % Send request, report success
  {ReqId, send(market_data_request, RqBody, NewConn)}.


unsubscribe_stock(Stock, #conn{subscriptions = Subs} = Conn) ->
  case lists:keytake(Stock, 2, Subs) of
    false ->
      {error, notfound};
    {value, {ID, Stock}, NewSubs} ->
      NewConn = Conn#conn{subscriptions = NewSubs},
      {ok, do_unsubscribe_stock(ID, Stock, NewConn)}
  end.

do_unsubscribe_stock(ReqId, Stock, Conn) ->
  % Construct request
  RqBody = [{md_req_id, ReqId},
    {subscription_request_type, 2}, {market_depth, 0}, {no_related_sym,1} |
    fix:stock_to_instrument_block(Stock) ++ fix_connection:entry_types(Stock)],
  % Send request
  send(market_data_request, RqBody, Conn).
