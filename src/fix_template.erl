-module(fix_template).
-author('Max Lapshin <max@maxidoors.ru>').

-include("log.hrl").
-compile(export_all).



-record(header, {
  fields = [],
  required = []
}).

-record(message, {
  name,
  code,
  type,
  admin = false,
  fields = [],
  required = [],
  components = [],
  groups = []
}).

-record(component, {
  name,
  code,
  required = false,
  fields = [],
  components = []
}).

-record(field, {
  name,
  code,
  number,
  type,
  raw_type,
  choices = []
}).

-record(group, {
  counter,
  name,
  fields = [],
  stack
}).

-record(parser, {
  state,
  header = #header{},
  message,
  messages = [],
  field,
  fields = [],
  component,
  inline = false,
  components = [],
  groups = []
}).


root() ->
  case code:lib_dir(fix) of
    {error, Error} -> erlang:error(Error);
    Root_ when is_list(Root_) -> Root_
  end.

parse() ->
  {ok, Xml} = file:read_file(root() ++"/spec/FIX44.xml"),
  {ok, #parser{messages = Messages} = State, _} = erlsom:parse_sax(Xml, #parser{}, fun handler/2, []),
  Prepend = ["sender_comp_id","target_comp_id","msg_seq_num","sending_time"],
  Trailer = ["signature"],
  State#parser{messages = [Message#message{fields = Prepend ++ Fields ++ Trailer, required = Prepend ++ Required} || 
  #message{fields = Fields, required = Required} = Message <- Messages]}.


generate_includes() ->
  #parser{messages = Messages, fields = Fields} = parse(),
  write_messages_to_header([Message || #message{admin = Admin} = Message <- Messages, Admin == true], Fields, root() ++ "/include/admin.hrl"),  
  write_messages_to_header([Message || #message{admin = Admin} = Message <- Messages, Admin == false], Fields, root() ++ "/include/business.hrl"),  
  ok.

generate_parser() ->
  file:write_file(root() ++ "/src/fix_parser.erl", parser_body()).

parser_body() ->
  #parser{messages = Messages, fields = Fields} = parse(),
  Header = ["-module(fix_parser).\n-include(\"../include/admin.hrl\").\n-include(\"../include/business.hrl\").\n\n",
  "-export([decode_message/1, field_by_number/1, number_by_field/1, decode_typed_field/2, encode_typed_field/2, message_by_number/1, number_by_message/1]).\n\n"],
  Body1 = generate_decode_message(Messages, Fields),
  Body2 = [],
  Body3 = generate_field_decoders(Fields),
  Body4 = generate_field_by_number(Fields),
  Body5 = generate_decode_typed_field(Fields),
  Body6 = generate_encode_typed_field(Fields),
  Body7 = generate_number_by_field(Fields),
  Body8 = generate_message_by_number(Messages),
  Body9 = generate_number_by_message(Messages),
  Body10 = add_parse_num(),
  iolist_to_binary([Header, Body1, Body2, Body3, Body4, Body5, Body6, Body7, Body8, Body9, Body10]).

generate_decode_message(Messages, Fields) ->
  Bodies1 = [
  ["decode_message(<<\"35=",Type,"\",1,Message/binary>>) -> % ", Code, "\n",
  "  decode_fields", "(Message, #",Name,"{}, ",Name,", ",integer_to_list(default_field_offset(Msg, Fields)),")"]
  || #message{name = Name, type = Type, code = Code} = Msg <- Messages],
  
  Bodies2_ = [begin
    UsedFields = message_fields(Msg, Fields),
    Indexes = lists:zip(UsedFields, lists:seq(2, 1+length(UsedFields))),
    [["field_index(",RecordName,", ",FieldName,") -> ",integer_to_list(Index),";\n"] || {FieldName,Index} <- Indexes]
  end|| #message{name = RecordName} = Msg <- Messages],
  Bodies2 = [Bodies2_, "field_index(_,_) -> undefined.\n\n"],
  
  [string:join(Bodies1, ";\n\n"), ".\n\n", Bodies2].

generate_field_by_number(Fields) ->
  [
  [["field_by_number(<<\"",Number,"\">>) -> ",Name,";\n"] || #field{name = Name, number = Number} <- Fields],
  "field_by_number(Key) -> Key.\n\n"
  ].

generate_number_by_field(Fields) ->
  [
  [["number_by_field(",Name,") -> <<\"",Number,"\">>;\n"] || #field{name = Name, number = Number} <- Fields],
  "number_by_field(Key) when is_binary(Key) -> Key.\n\n"
  ].

generate_message_by_number(Messages) ->
  [
  [["message_by_number(<<\"", Type,"\">>) -> ", Name,";\n"] || #message{name = Name, type = Type} <- Messages],
  "message_by_number(Type) when is_binary(Type) -> Type.\n\n"
  ].

generate_number_by_message(Messages) ->
  [
  [["number_by_message(", Name,") -> <<\"", Type,"\">>;\n"] || #message{name = Name, type = Type} <- Messages],
  "number_by_message(Type) when is_binary(Type) -> Type.\n\n"
  ].

generate_decode_typed_field(Fields) ->
  [
  lists:map(fun
    (#field{name = "msg_type"}) -> ["decode_typed_field(msg_type, V) -> message_by_number(V);\n"];
    (#field{name = Name, type = int}) -> ["decode_typed_field(",Name,", V) -> parse_num(V);\n"];
    (#field{name = Name, type = float}) -> ["decode_typed_field(",Name,", V) -> parse_num(V)*1.0;\n"];
    (#field{name = Name, type = bool}) -> ["decode_typed_field(",Name,", V) -> V == <<\"Y\">>;\n"];
    (#field{name = Name}) -> ["decode_typed_field(",Name,", V) -> V;\n"]
  end, Fields),
  "decode_typed_field(_Key, V) -> V.\n\n"
  ].

generate_encode_typed_field(Fields) ->
  [
  lists:map(fun
    (#field{name = "msg_type"}) -> ["encode_typed_field(msg_type, V) -> number_by_message(V);\n"];
    (#field{name = Name, type = int}) -> ["encode_typed_field(",Name,", V) when is_integer(V) -> list_to_binary(integer_to_list(V));\n"];
    (#field{name = Name, type = float}) -> ["encode_typed_field(",Name,", V) when is_float(V) -> iolist_to_binary(io_lib:format(\"~.3f\", [V*1.0]));\n"];
    (#field{name = Name, type = bool}) -> ["encode_typed_field(",Name,", true) -> <<\"Y\">>;\nencode_typed_field(",Name,",false) -> <<\"N\">>;\n"];
    (#field{}) -> []
  end, Fields),
  "encode_typed_field(_Key, V) when is_binary(V) -> V;\n"
  "encode_typed_field(_Key, V) when is_list(V) -> V;\n"
  "encode_typed_field(_Key, V) when is_integer(V) -> list_to_binary(integer_to_list(V));\n"
  "encode_typed_field(_Key, V) when is_float(V) -> iolist_to_binary(io_lib:format(\"~.3f\", [V*1.0])).\n"
  "\n"
  ].


generate_field_decoders(Fields) ->
  Bodies1 = [
  ["decode_fields(<<\"",Number,"=\", Message/binary>>, Record, RecordName, Default) ->\n",
  "  [RawValue, Rest] = binary:split(Message, <<1>>),\n",
  "  Value = ", case Type of
    int -> "parse_num(RawValue)";
    float -> "parse_num(RawValue)";
    bool -> "RawValue == <<\"Y\">>";
    _ -> "RawValue"
  end, ",\n",
  "  Record1 = case field_index(RecordName, ",Name,") of\n",
  "    undefined -> erlang:setelement(Default, Record, [{",Name,",Value}|erlang:element(Default,Record)]);\n",
  "    Index -> erlang:setelement(Index, Record, Value)\n",
  "  end,",
  "  decode_fields(Rest, Record1, RecordName, Default);\n\n"]
  || #field{number = Number, name = Name, raw_type = T, type = Type} <- Fields, T =/= "DATA" andalso T =/= "LENGTH"],
  
  Bodies2 = [
  ["decode_fields(<<\"",Number,"=\", Message/binary>>, Record, RecordName, Default) ->\n",
  "  [RawValue, Rest] = binary:split(Message, <<1>>),\n",
  "  DataLength = parse_num(RawValue),\n",
  "  decode_data_field(Rest, DataLength, Record, RecordName, Default);\n\n"
  ]
  || #field{number = Number, raw_type = T} <- Fields, T == "LENGTH"],
  
  Bodies3 = [
  "decode_fields(<<>>, Record, _RecordName, Default) ->\n",
  "  erlang:setelement(Default, Record, lists:reverse(erlang:element(Default,Record))).\n\n"
  ],
  
  Bodies4_ = [
  ["decode_data_field(<<\"",Number,"=\", Message/binary>>, DataLength, Record, RecordName, Default) ->\n",
  "  <<Value:DataLength/binary, 1, Rest/binary>> = Message,\n",
  "  Record1 = case field_index(RecordName, ",Name,") of\n",
  "    undefined -> erlang:setelement(Default, Record, [{",Name,",Value}|erlang:element(Default,Record)]);\n",
  "    Index -> erlang:setelement(Index, Record, Value)\n",
  "  end,",
  "  decode_fields(Rest, Record1, RecordName, Default)"]
  || #field{number = Number, name = Name, raw_type = T} <- Fields, T == "DATA"],
  Bodies4 = [string:join(Bodies4_, ";\n\n"), ".\n\n"],
  
  [Bodies1, Bodies2, Bodies3, Bodies4].
  

add_parse_num() ->
  "parse_num(Bin) -> parse_num_erl(Bin).\n"
  "\n"
  "parse_num_erl(Bin) -> parse_num(Bin, 0, 0).\n"
  "parse_num(<<$., Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*1.0, 0.1);\n"
  "parse_num(<<X, Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*10 + X - $0, 0);\n"
  "parse_num(<<X, Bin/binary>>, Acc, Coeff) -> parse_num(Bin, Acc + (X - $0)*Coeff, Coeff*0.1);\n"
  "parse_num(<<>>, Acc, _) -> Acc.\n\n".
  

message_fields(#message{fields = FieldNames}, Fields) ->
  [Name || #field{raw_type = T, name = Name} <- [lists:keyfind(F, #field.name, Fields) || F <- FieldNames], T =/= "LENGTH"].

default_field_offset(#message{groups = Groups} = Msg, Fields) ->
  length(message_fields(Msg, Fields)) + length(Groups) + 2. 

write_messages_to_header(Messages, Fields, Path) ->
  file:write_file(Path, lists:map(fun(#message{name = Name, groups = Groups} = Msg) ->
    Names = message_fields(Msg, Fields),
    ["-record(", Name, ", {\n",
      [["  ", N,",\n"] || N <- Names],
      [["  ", N," = [],\n"] || #group{name = N} <- Groups],
    "  fields = []\n",
    "}).\n\n"]
  end, Messages)).

underscore(String) ->
  underscore(String, []).

underscore("ID" ++ String, [$_|Acc]) when length(Acc) > 0 ->
  underscore(String, [$d,$i,$_|Acc]);

underscore("ID" ++ String, Acc) when length(Acc) > 0 ->
  underscore(String, [$d,$i,$_|Acc]);

underscore([Char,$_,Char2|String], Acc) when 
  (Char >= $A andalso Char =< $Z) andalso (Char2 >= $A andalso Char2 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char2|String], [$_, string:to_lower(Char)|Acc]);


underscore([Char,Char1,Char2|String], Acc) when 
  not (Char >= $A andalso Char =< $Z) andalso (Char1 >= $A andalso Char1 =< $Z) andalso (Char2 >= $A andalso Char2 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char1,Char2|String], [$_, Char|Acc]);

underscore([Char,Char1|String], Acc) when 
  Char >= $A andalso Char =< $Z andalso not (Char1 >= $A andalso Char1 =< $Z) andalso length(Acc) > 0 ->
  underscore([Char1|String], [string:to_lower(Char), $_|Acc]);

underscore([Char|String], Acc) when Char >= $A andalso Char =< $Z->
  underscore(String, [string:to_lower(Char)|Acc]);

underscore([Char|String], Acc) ->
  underscore(String, [Char|Acc]);

underscore([], Acc) ->
  lists:reverse(Acc).

parse_field({startElement, _, "field", _, Attributes}) ->
  Required = case lists:keyfind("required", 2, Attributes) of
    {attribute, "required", _, _, "Y"} -> true;
    _ -> false
  end,
  {attribute, "name", _, _, Name} = lists:keyfind("name", 2, Attributes),
  {underscore(Name), Required}.

handler({ignorableWhitespace, _}, State) ->
  State;

handler(startDocument, State) ->
  State;

handler(endDocument, State) ->
  State;

handler({startElement, _, "fix", _, _}, State) ->
  State;

handler({endElement, _, "fix", _}, State) ->
  State;

%%%%%%%%%%%%%   Header group start

handler({startElement, _, "header", _, _}, State) ->
  State#parser{state = header};

handler({startElement, _, "field", _, _} = F, #parser{state = header, header = #header{fields = Fields, required = Required} = Header} = State) ->
  {Name, Mandatory} = parse_field(F),
  Required1 = case Mandatory of
    true -> Required++[Name];
    false -> Required
  end,
  Fields1 = Fields++[Name],
  State#parser{header = Header#header{fields = Fields1, required = Required1}};

handler({endElement, _, "field", _}, #parser{state = header} = State) ->
  State;

handler({endElement, _, "header", _}, State) ->
  State#parser{state = undefined};

%%%%%%%%%%%%%   Header group end

%%%%%%%%%%%%%   Message group start

handler({startElement, _, "messages", _, _}, State) ->
  State#parser{state = messages};

handler({startElement, _, "message", _, Attributes}, #parser{state = messages} = State) ->
  {attribute, "msgtype", _, _, Type} = lists:keyfind("msgtype", 2, Attributes),
  {attribute, "msgcat", _, _, Category} = lists:keyfind("msgcat", 2, Attributes),
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  Name = underscore(Code),
  State#parser{message = #message{name = Name, code = Code, admin = Category == "admin", type = Type}};

handler({startElement, _, "field", _, _} = F, #parser{state = messages, message = #message{fields = Fields, required = Required} = Message} = State) ->
  {Name, Mandatory} = parse_field(F),
  Required1 = case Mandatory of
    true -> Required++[Name];
    false -> Required
  end,
  Fields1 = Fields++[Name],
  State#parser{message = Message#message{fields = Fields1, required = Required1}};

handler({endElement, _, "field", _}, #parser{state = messages} = State) ->
  State;

handler({startElement, _, "group", _, Attributes}, #parser{state = messages} = State) ->
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  Counter = "no_" ++ Name = underscore(Code),
  ?D({skip_group,Name}),
  #group{name = Name, counter = Counter, stack = State};


handler({startElement, _, "field", _, _} = F, #group{fields = Fields} = Group) ->
  {Name, _Mandatory} = parse_field(F),
  % Required1 = case Mandatory of
  %   true -> Required++[Name];
  %   false -> Required
  % end,
  Group#group{fields = Fields++[Name]};

handler({endElement, _, "field", _}, #group{} = State) ->
  State;

  
handler({endElement, _, "group", _}, #group{stack = #parser{groups = AllGroups, message = #message{groups = Groups} = Message} = State} = Group) ->
  Group1 = Group#group{stack = undefined},
  State#parser{message = Message#message{groups = Groups ++ [Group1]}, groups = AllGroups ++ [Group1]};

handler({startElement, _, "component", _, Attributes}, #parser{state = messages, message = #message{components = Components} = Message} = State) ->
  {attribute, "required", _, _, Required} = lists:keyfind("required", 2, Attributes),
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  Name = underscore(Code),
  Component = {Name, Required == "Y"},
  State#parser{message = Message#message{components = Components ++ [Component]}};

handler({endElement, _, "component", _}, #parser{state = messages} = State) ->
  State;

handler({endElement, _, "message", _}, #parser{state = messages, message = #message{} = Message, messages = Messages} = State) ->
  State#parser{messages = Messages ++ [Message], message = undefined};

handler({endElement, _, "messages", _}, #parser{state = messages} = State) ->
  State#parser{state = undefined};


%%%%%%%%%%%%%   Message group end


%%%%%%%%%%%%%   Component group start
handler({startElement, _, "components", _, _}, State) ->
  State#parser{state = components};

handler({startElement, _, "component", _, Attributes}, #parser{state = components, component = undefined} = State) ->
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  Name = underscore(Code),
  Component = #component{name = Name, code = Code},
  State#parser{component = Component};

handler({startElement, _, "component", _, Attributes}, #parser{state = components, component = #component{components = Components} = Component} = State) ->
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  {attribute, "required", _, _, Required} = lists:keyfind("required", 2, Attributes),
  Name = underscore(Code),
  State#parser{component = Component#component{components = Components ++ [{Name,Required== "Y"}]}, inline = true};

handler({startElement, _, "field", _, _} = F, #parser{state = components, component = #component{fields = Fields} = Component} = State) ->
  {Name, Mandatory} = parse_field(F),
  State#parser{component = Component#component{fields = Fields ++ [{Name, Mandatory}]}};

handler({endElement, _, "field", _}, #parser{state = components} = State) ->
  State;

handler({endElement, _, "component", _}, #parser{state = components, inline = true} = State) ->
  State#parser{inline = false};

handler({endElement, _, "component", _}, #parser{state = components, component = #component{} = Component, inline = false, components = Components} = State) ->
  State#parser{components = Components ++ [Component], component = undefined};

handler({endElement, _, "components", _}, State) ->
  State#parser{state = undefined};
  
%%%%%%%%%%%%%   Component group end


%%%%%%%%%%%%%   Fields group start

handler({startElement, _, "fields", _, _}, State) ->
  State#parser{state = fields};

handler({startElement, _, "field", _, Attributes}, #parser{state = fields} = State) ->
  {attribute, "number", _, _, Number} = lists:keyfind("number", 2, Attributes),
  {attribute, "name", _, _, Code} = lists:keyfind("name", 2, Attributes),
  {attribute, "type", _, _, RawType} = lists:keyfind("type", 2, Attributes),
  Name = underscore(Code),
  Type = case RawType of
    % "STRING" -> string;
    % "CHAR" -> char;
    "INT" -> int;
    "PRICE" -> float;
    "SEQNUM" -> int;
    "LENGTH" -> int;
    "QTY" -> int;
    "NUMINGROUP" -> int;
    "BOOLEAN" -> bool;
    _ -> string
    % "AMT" -> amt
  end,
  State#parser{field = #field{name = Name, code = Code, type = Type, raw_type = RawType, number = Number}};

handler({startElement, _, "value", _, Attributes}, #parser{state = fields, field = #field{choices = Choices} = Field} = State) ->
  {attribute, "description", _, _, Desc} = lists:keyfind("description", 2, Attributes),
  {attribute, "enum", _, _, Value} = lists:keyfind("enum", 2, Attributes),
  State#parser{field = Field#field{choices = Choices ++ [{Value,Desc}]}};

handler({endElement, _, "value", _}, #parser{state = fields} = State) ->
  State;

handler({endElement, _, "field", _}, #parser{state = fields, field = Field, fields = Fields} = State) ->
  State#parser{fields = Fields ++ [Field]};

handler({endElement, _, "fields", _}, State) ->
  State#parser{state = undefined};


%%%%%%%%%%%%%   Fields group end

handler({startElement, _, "group", _, _}, State) ->
  State;

handler({endElement, _, "group", _}, State) ->
  State;

handler(_Event, State) ->
  % ?D({State#parser.state, _Event}),
  State.

-include_lib("eunit/include/eunit.hrl").

underscore_test() ->
  ?assertEqual("logout", underscore("Logout")),
  ?assertEqual("logout", underscore("logout")),
  ?assertEqual("logout", underscore("LOGOUT")),
  ?assertEqual("test_request", underscore("TestRequest")),
  ?assertEqual("allocation_instruction_ack", underscore("AllocationInstructionAck")),
  ?assertEqual("ioi", underscore("IOI")),
  ?assertEqual("rfq_request", underscore("RFQRequest")),
  ?assertEqual("hop_comp_id", underscore("HopCompID")),
  ?assertEqual("id", underscore("ID")),
  ?assertEqual("id", underscore("Id")),
  ?assertEqual("no_md_entries", underscore("NoMDEntries")),
  ?assertEqual("ioi_id", underscore("IOIID")),
  ?assertEqual("leg_ioi_qty", underscore("LegIOIQty")),
  ?assertEqual("out_main_cntry_u_index", underscore("OutMainCntryUIndex")),
  ?assertEqual("alt_md_source_id", underscore("AltMDSourceID")),
  ?assertEqual("closing_price", underscore("CLOSING_PRICE")),
  % ?assertEqual("", underscore("")),
  ok.

