-module(fix_server).
-author('Max Lapshin <max@maxidoors.ru>').
-include("log.hrl").
-include("../include/business.hrl").

-export([start_link/4]).
-export([start_link/2, init/1, handle_info/2, handle_call/3, terminate/2]).
-export([handle_md/3]).

start_link(_ListenerPid, Socket, _Transport, Opts) ->
  fix_sup:start_server(Socket, Opts).


start_link(Socket, Opts) ->
  gen_server:start_link(?MODULE, [Socket, Opts], []).


-record(message, {
  type,
  seq,
  sender,
  target,
  body
}).


-record(server, {
  socket,
  buffer = <<>>,
  sender,
  target,
  seq = 1,
  opts
}).

init([Socket, Opts]) ->
  {ok, Remote} = inet:peername(Socket),
  ?D({start,self(), Remote}),
  {ok, #server{socket = Socket, opts = Opts}}.

handle_info({shoot, _ListenerPid}, #server{socket = Socket} = Server) ->
  inet:setopts(Socket, [{packet,raw},{active,once}]),
  {noreply, Server};

handle_info(#market_data_snapshot_full_refresh{sending_time = Time, symbol = Symbol, md_req_id = MdReqId, md_entries = MdEntries},
  #server{} = Server)
->
  Body = [{sending_time,Time},{symbol,Symbol},{md_req_id,MdReqId},{no_md_entries,length(MdEntries)}|lists:flatten(MdEntries)],
  Msg = #message{type = market_data_snapshot_full_refresh, body = Body},
  Server1 = send(Msg, Server),
  % Server1 = Server,
  {noreply, Server1};
  
handle_info(#market_data_request_reject{sending_time = Time, md_req_id = MdReqId, md_req_rej_reason = Reason, text = Text},
  #server{} = Server)
->
  Body = [{sending_time,Time},{md_req_id,MdReqId}, {md_req_rej_reason,Reason}, {text,Text}],
  Msg = #message{type = market_data_request_reject, body = Body},
  Server1 = send(Msg, Server),
  % Server1 = Server,
  {noreply, Server1};

handle_info({tcp, Socket, Bin}, #server{buffer = Buffer} = Server) ->
  case decode(<<Buffer/binary, Bin/binary>>) of
    {ok, Msg, Rest} ->
      Server1 = handle_fix(Msg, Server#server{buffer = Rest}),
      handle_info({tcp, Socket, <<>>}, Server1);
    {more, _} ->
      inet:setopts(Socket, [{active,once}]),
      {noreply, Server};
    error ->
      {stop, fix_error, Server}
  end;

handle_info({tcp_closed, Socket}, #server{socket = Socket} = Server) ->
  {stop, normal, Server}.

handle_call(Call, _From, #server{} = Server) ->
  {reply, {error, Call}, Server}.

terminate(_,_) -> ?D({die,self()}), ok.


decode(Bin) ->
  case fix:decode_fields(Bin) of
    {ok, Msg1, Rest} ->
      {value, {msg_type, Type}, Msg2} = lists:keytake(msg_type, 1, Msg1),
      {value, {msg_seq_num, Seq}, Msg3} = lists:keytake(msg_seq_num, 1, Msg2),
      {value, {sender_comp_id, Sender}, Msg4} = lists:keytake(sender_comp_id, 1, Msg3),
      {value, {target_comp_id, Target}, Msg5} = lists:keytake(target_comp_id, 1, Msg4),
      {ok, #message{type = Type, seq = Seq, sender = Sender, target = Target, body = Msg5}, Rest};
    Else ->
      Else
  end.

encode(#message{type = Type, seq = Seq, sender = Sender, target = Target, body = Body}) ->
  fix:pack(Type, Body, Seq, Sender, Target).


send(#message{} = Msg, #server{seq = Seq, socket = Socket, sender = Sender, target = Target} = Server) ->
  gen_tcp:send(Socket, encode(Msg#message{seq = Seq, sender = Sender, target = Target})),
  Server#server{seq = Seq + 1}.

handle_fix(#message{type = heartbeat}, #server{} = Server) ->
  Server;

handle_fix(#message{type = logon, seq = _Seq, sender = Sender, target = Target} = Msg, #server{socket = _Socket} = Server) ->
  send(Msg, Server#server{seq = 1, sender = Sender, target = Target});

handle_fix(#message{type = market_data_request, body = Body}, #server{} = Server) ->
  {symbol, Symbol} = lists:keyfind(symbol, 1, Body),
  {md_req_id, RReqId} = lists:keyfind(md_req_id, 1, Body),
  Exchange = proplists:get_value(security_exchange, Body),
  fix_reader:async_subscribe({Exchange, Symbol}, {?MODULE, handle_md, [self(), RReqId]}),
  Server;

handle_fix(Msg, Server) ->
  ?D({fix, Msg}),
  Server.

handle_md(#market_data_snapshot_full_refresh{} = DataIn, Pid, ReqId) ->
  DataOut = DataIn#market_data_snapshot_full_refresh{md_req_id = ReqId},
  Pid ! DataOut.
