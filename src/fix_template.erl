-module(fix_template).
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
  components = []
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
  choices = []
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
  components = []
}).


root() ->
  case code:lib_dir(fix) of
    {error, Error} -> erlang:error(Error);
    Root_ when is_list(Root_) -> Root_
  end.

parse() ->
  {ok, Xml} = file:read_file(root() ++"/spec/FIX44.xml"),
  {ok, #parser{} = State, _} = erlsom:parse_sax(Xml, #parser{}, fun handler/2, []),
  State.


generate_includes() ->
  #parser{messages = Messages} = parse(),
  write_messages_to_header([Message || #message{admin = Admin} = Message <- Messages, Admin == true], root() ++ "/include/admin.hrl"),  
  write_messages_to_header([Message || #message{admin = Admin} = Message <- Messages, Admin == false], root() ++ "/include/business.hrl"),  
  ok.

generate_parser() ->
  file:write_file(root() ++ "/src/fix_parser.erl", parser_body()).

parser_body() ->
  #parser{messages = Messages, fields = Fields} = parse(),
  Header = ["-module(fix_parser).\n-include(\"../include/admin.hrl\").\n-include(\"../include/business.hrl\").\n\n",
  "-export([decode_message/1, field_by_number/1, number_by_field/1, decode_typed_field/2, encode_typed_field/2, message_by_number/1, number_by_message/1]).\n\n"],
  Body1 = generate_decode_message(Messages, []),
  Body2 = generate_message_decoders(Messages, Fields, []),
  Body3 = generate_field_by_number(Fields),
  Body4 = generate_decode_typed_field(Fields),
  Body5 = generate_encode_typed_field(Fields),
  Body6 = generate_number_by_field(Fields),
  Body7 = generate_message_by_number(Messages),
  Body8 = generate_number_by_message(Messages),
  iolist_to_binary([Header, Body1, Body2, Body3, Body4, Body5, Body6, Body7, Body8]).

generate_decode_message([#message{name = Name, type = Type, code = Code}|Messages], Acc) ->
  Body = [
  "decode_message(<<\"35=",Type,"\",1,Message/binary>>) -> % ", Code, "\n",
  "  decode_message_",Name, "(Message, #",Name,"{})",
  if length(Messages) == 0 -> "."; true -> ";" end,
  "\n\n"
  ],
  generate_decode_message(Messages, Acc ++ Body);

generate_decode_message([], Acc) ->
  Acc.

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
    (#field{name = Name, type = int}) -> ["decode_typed_field(",Name,", V) -> fix:parse_num(V);\n"];
    (#field{name = Name, type = float}) -> ["decode_typed_field(",Name,", V) -> fix:parse_num(V)*1.0;\n"];
    (#field{name = Name, type = bool}) -> ["decode_typed_field(",Name,", V) -> V == <<\"Y\">>;\n"];
    (#field{name = Name}) -> ["decode_typed_field(",Name,", V) -> V;\n"]
  end, Fields),
  "decode_typed_field(_Key, V) -> V.\n\n"
  ].

generate_encode_typed_field(Fields) ->
  [
  lists:map(fun
    (#field{name = "msg_type"}) -> ["encode_typed_field(msg_type, V) -> number_by_message(V);\n"];
    (#field{name = Name, type = int}) -> ["encode_typed_field(",Name,", V) -> list_to_binary(integer_to_list(V));\n"];
    (#field{name = Name, type = float}) -> ["encode_typed_field(",Name,", V) -> iolist_to_binary(io_lib:format(\"~.3f\", [V*1.0]));\n"];
    (#field{name = Name, type = bool}) -> ["encode_typed_field(",Name,", true) -> <<\"Y\">>;\nencode_typed_field(",Name,",false) -> <<\"N\">>;\n"];
    (#field{name = Name}) -> ["encode_typed_field(",Name,", V) -> V;\n"]
  end, Fields),
  "encode_typed_field(_Key, V) -> V.\n\n"
  ].


generate_message_decoders([#message{name = Name, fields = MsgFields}|Messages], Fields, Acc) ->
  % Choice 1
  %
  % Body = [lists:map(fun(FieldName) ->
  %   #field{number = Number} = lists:keyfind(FieldName, #field.name, Fields),
  %   ["decode_message_", Name, "(<<\"",Number,"=\", Message/binary>>, #",Name,"{} = Record) ->\n",
  %   "  [Value, Rest] = binary:split(Message, <<1>>),\n",
  %   "  decode_message_", Name, "(Rest, Record#",Name,"{",FieldName,"=Value});\n\n"
  %   ]
  % end, MsgFields),
  % 
  % "decode_message_",Name, "(<<>>, #",Name,"{fields = Fields} = Record) ->\n",
  % "  Record#",Name,"{fields = lists:reverse(Fields)};\n\n",
  % 
  % "decode_message_",Name, "(Message, #",Name,"{fields = Fields} = Record) ->\n",
  % "  [F, Rest] = binary:split(Message, <<1>>),\n",
  % "  [Key, Value] = binary:split(F, <<\"=\">>),\n",
  % "  decode_message_",Name,"(Rest, Record#",Name,"{fields = [{field_by_number(Key),Value}|Fields]}).\n\n"
  % 
  % ],
  
  % Choice 2
  Body = [
  "decode_message_",Name, "(Message, #",Name,"{} = Record) ->\n"
  "  Fields = [begin\n"
  "    [K,V] = binary:split(Field, <<\"=\">>),\n"
  "    {field_by_number(K),V}\n"
  "  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],\n"
  "  Record1 = #",Name,"{fields = F} = lists:foldl(fun\n",
  lists:map(fun(FieldName) ->
    Value = case lists:keyfind(FieldName, #field.name, Fields) of
      #field{type = float} -> "list_to_float(binary_to_list(V))";
      #field{type = int} -> "list_to_integer(binary_to_list(V))";
      #field{type = bool} -> "V == <<\"Y\">>";
      _ -> "V"
    end,
 ["    ({",FieldName,",V}, Rec) -> Rec#",Name,"{",FieldName," = ",Value,"};\n"]  
  end, MsgFields),
  "    ({K,V}, #",Name,"{fields = F} = Rec) -> Rec#",Name,"{fields = [{K,decode_typed_field(K,V)}|F]}\n"    
  "  end, Record, Fields),\n"
  "  Record1#",Name,"{fields = lists:reverse(F)}.\n\n"
  ],


  generate_message_decoders(Messages, Fields, Acc ++ Body);

generate_message_decoders([], _Fields, Acc) ->
  Acc.

write_messages_to_header(Messages, Path) ->
  file:write_file(Path, lists:map(fun(#message{name = Name, fields = Fields}) ->
    Names = [Field || Field <- Fields],
    ["-record(", Name, ", {\n",
      [["  ", N,",\n"] || N <- Names],
    "  fields = []\n",
    "}).\n\n"]
  end, Messages)).

underscore(String) ->
  underscore(String, []).

underscore("ID" ++ String, [$_|Acc]) when length(Acc) > 0 ->
  underscore(String, [$d,$i,$_|Acc]);

underscore("ID" ++ String, Acc) when length(Acc) > 0 ->
  underscore(String, [$d,$i,$_|Acc]);

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
  State#parser{field = #field{name = Name, code = Code, type = Type, number = Number}};

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
  % ?assertEqual("", underscore("")),
  % ?assertEqual("", underscore("")),
  ok.

