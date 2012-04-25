-module(fix_session).
-include("log.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([open/0, logon/1, default_options/0]).

-record(fix_session, {
  socket,
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

start_link(Consumer, Options) ->
  gen_server:start_link(?MODULE, [Consumer, Options], []).

default_options() ->
  {ok, Env} = application:get_env(fix, config),
  Env.


open() ->
  Options = default_options(),
  {ok, Pid} = start_link(self(), Options),
  logon(Pid),
  subscribe(Pid, proplists:get_value(symbol, Options), proplists:get_value(security, Options)),
  {ok, Pid}.
  

logon(Pid) ->
  Password = get(Pid, password),
  Heartbeat = get(Pid, hearbeat),
  gen_server:call(Pid, {msg, logon, [{'EncryptMethod', 0},{'HeartBTInt', Heartbeat},{'ResetSeqNumFlag', "Y"},{'Password', Password}]}),
  ok.

subscribe(Pid, Symbol, Exchange) ->
  Opts = [{'MDReqID', 42}, {'SubscriptionRequestType', 1}, {'MarketDepth', 0}, {'MDUpdateType', 0},
    {'NoMDEntryTypes',2},{'MDEntryType',0},{'MDEntryType',1},{'NoRelatedSym',1},
    {'Symbol', Symbol},{'CFICode', "EXXXXX"},{'SecurityExchange', Exchange}],
  gen_server:call(Pid, {msg, market_data, Opts}),
  ok.

init([Consumer, Options]) ->
  {host, Host} = lists:keyfind(host, 1, Options),
  {port, Port} = lists:keyfind(port, 1, Options),
  {password, Password} = lists:keyfind(password, 1, Options),
  {sender, Sender} = lists:keyfind(sender, 1, Options),
  {target, Target} = lists:keyfind(target, 1, Options),
  {hearbeat, Heartbeat} = lists:keyfind(hearbeat, 1, Options),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet,raw}, {active, once}]),
  timer:send_interval(Heartbeat*1000, hearbeat),
  erlang:monitor(process, Consumer),
  {ok, #fix_session{host = Host, port = Port, target = Target, sender = Sender, hearbeat = Heartbeat, seq = 1, 
  socket = Socket, consumer = Consumer, password = Password}}.


handle_info(hearbeat, #fix_session{} = Session) ->
  {noreply, send(heartbeat, [], Session)};

handle_info({'DOWN', _, _, Consumer, _}, #fix_session{consumer = Consumer} = Session) ->
  ?D({session_exit}),
  {stop, normal, send(logout, [], Session)};

handle_info({tcp, Socket, Data}, #fix_session{buffer = Buffer, consumer = Consumer} = Session) ->
  Bin = case Buffer of
    <<>> -> Data;
    _ -> <<Buffer/binary, Data/binary>>
  end,
  inet:setopts(Socket, [{active,once}]),
  case fix:decode(Bin) of
    % FIXME: сделать парсинг нескольких сообщений в пакете
    {ok, Message, Rest} ->
      ?D({fix, Message}),
      Consumer ! {fix, self(), Message},
      {noreply, Session#fix_session{buffer = Rest}};
    {more, _} ->
      {noreply, Session#fix_session{buffer = Bin}};
    error ->
      {stop, {error, {invalid_fix, Bin}}, Session}
  end;    

handle_info(Msg, Session) ->
  ?D({unknown,Msg}),
  {noreply, Session}.

handle_call({get_field, Key}, _From, Media) ->
  {reply, get(Media, Key), Media};

handle_call({set_field, Key, Value}, _From, Media) ->
  {reply, ok, set(Media, Key, Value)};

handle_call({msg, MessageType, Body}, _From, #fix_session{} = Session) ->  
  {reply, ok, send(MessageType, Body, Session)}.


send(MessageType, Body, #fix_session{seq = Seq, sender = Sender, target = Target, socket = Socket} = Session) ->
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
  gen_tcp:send(Socket, Bin),
  Session#fix_session{seq = Seq + 1}.

terminate(_,_) ->
  ok.
