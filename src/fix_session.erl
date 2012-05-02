-module(fix_session).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").

-export([start_link/2]).
-export([init/1, handle_info/2, handle_call/3, terminate/2]).
-export([open/0, default_options/0]).
-export([logon/1, subscribe/3, status/1]).

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
  gen_server:call(Pid, {msg, logon, [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}]}),
  ok.

subscribe(Pid, Symbol, Exchange) ->
  Opts = [{md_req_id, 42}, {subscription_request_type, 1}, {market_depth, 0}, {md_update_type, 0},
    {no_md_entry_types,2},{md_entry_type,0},{md_entry_type,1},{no_related_sym,1},
    {symbol, Symbol},{cfi_code, "EXXXXX"},{security_exchange, Exchange}],
  gen_server:call(Pid, {msg, market_data_request, Opts}),
  ok.


status(Pid) ->
  gen_server:call(Pid, status).

init([Consumer, Options]) ->
  {host, Host} = lists:keyfind(host, 1, Options),
  {port, Port} = lists:keyfind(port, 1, Options),
  {password, Password} = lists:keyfind(password, 1, Options),
  {sender, Sender} = lists:keyfind(sender, 1, Options),
  {target, Target} = lists:keyfind(target, 1, Options),
  Heartbeat = proplists:get_value(heartbeat, Options, 30),
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

handle_info({tcp, Socket, Data}, #fix_session{buffer = Buffer} = Session) ->
  Bin = case Buffer of
    <<>> -> Data;
    _ -> <<Buffer/binary, Data/binary>>
  end,
  inet:setopts(Socket, [{active,once}]),
  {Messages, Rest} = decode_messages(Bin),
  handle_messages(Messages, Session#fix_session{buffer = Rest});

handle_info(Msg, Session) ->
  ?D({unknown,Msg}),
  {noreply, Session}.

handle_call({get_field, Key}, _From, Session) ->
  {reply, get(Session, Key), Session};

handle_call({set_field, Key, Value}, _From, Session) ->
  {reply, ok, set(Session, Key, Value)};

handle_call(status, _From, #fix_session{} = Session) ->
  {reply, [], Session};

handle_call({msg, MessageType, Body}, _From, #fix_session{} = Session) ->  
  {reply, ok, send(MessageType, Body, Session)}.


send(MessageType, Body, #fix_session{seq = Seq, sender = Sender, target = Target, socket = Socket} = Session) ->
  ?D({pack, MessageType, Body, Seq, Sender, Target}),
  Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
  gen_tcp:send(Socket, Bin),
  Session#fix_session{seq = Seq + 1}.

terminate(_,_) ->
  ok.

decode_messages(Bin) ->
  decode_messages(Bin, []).

decode_messages(Bin, Acc) ->
  case fix:decode(Bin) of
    {ok, Message, Rest} ->
      decode_messages(Rest, [Message|Acc]);
    {more, _} ->
      {lists:reverse(Acc), Bin};
    error ->
      ?D({error, Bin}),
      erlang:error(broken_fix)
  end.

handle_messages([Message|Messages], #fix_session{consumer = Consumer} = Session) ->
  ?D({fix, Message}),
  Consumer ! {fix, self(), Message},
  handle_messages(Messages, Session);

handle_messages([], #fix_session{} = Session) ->
  {noreply, Session}.
