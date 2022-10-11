-module(fix_tests).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("eunit/include/eunit.hrl").
-include("admin.hrl").
-include("business.hrl").
-include("fix_version.hrl").

-export([sample_md/0]).

encode_test() ->
  ?assertEqual(fix:sample_fix(), iolist_to_binary(fix:encode(fix_splitter:split(fix:sample_fix())))).

decode_test() ->
  ?assertMatch([{msg_type,market_data_snapshot_full_refresh}, {msg_seq_num,3}, 
  {sending_time,<<"20120426-06:33:03.516">>}, {symbol,<<"URKA">>}, {md_req_id,<<"42">>}, 
  {no_md_entries,2}, {md_entry_type,bid}, {md_entry_px,218.87}, {md_entry_size,20}, 
  {md_entry_type,offer}, {md_entry_px,219.03}, {md_entry_size,140}], fix_splitter:split(fix:sample_fix())).

pack_test() ->
  Body = [{sending_time,"20120502-13:08:35"}, {md_req_id,42},{subscription_request_type,1},
    {market_depth,0},{md_update_type,0},{no_md_entry_types,2},
    {md_entry_type,bid},{md_entry_type,offer},{no_related_sym,1},
    {symbol,"URKA"},{cfi_code,"EXXXXX"},{security_exchange,"MICEX"}],
  Out = fix:pack(market_data_request, Body, 31, "SENDER", "TARGET"),  
  Fix = <<"8=FIX.4.4|9=130|35=V|49=SENDER|56=TARGET|34=31|52=20120502-13:08:35|262=42|263=1|264=0|265=0|267=2|269=0|269=1|146=1|55=URKA|461=EXXXXX|207=MICEX|10=166|">>,

  OutPD = fix:pack(market_data_request, [{poss_dup_flag, "N"}|Body], 31, "SENDER", "TARGET"),  
  FixPD = <<"8=FIX.4.4|9=135|35=V|49=SENDER|56=TARGET|34=31|43=N|52=20120502-13:08:35|262=42|263=1|264=0|265=0|267=2|269=0|269=1|146=1|55=URKA|461=EXXXXX|207=MICEX|10=158|">>,

  ?assertEqual(Fix, fix:dump(iolist_to_binary(Out))),
  ?assertEqual(FixPD, fix:dump(iolist_to_binary(OutPD))).


% fix_decode_1_test() ->
%   ?assertMatch({ok, #heartbeat{signature = <<"A",1,"89=234">>}, <<>>}, fix:decode(<<"8=FIX.4.4",1,"9=22",1,"35=0",1,"93=8",1,"89=A",1,"89=234",1,"10=999",1>>)).


fix_decode_2_test() ->
  Result = fix:decode(<<"8=FIX.4.4",1,"9=135",1,"35=V",1,"49=SENDER",1,"56=TARGET",1,"34=31",1,"43=N",1,"52=20120502-13:08:35",1,"262=42",1,"263=1",1,"264=0",1,"265=0",1,"267=2",1,"269=0",1,"269=1",1,"146=1",1,"55=URKA",1,"461=EXXXXX",1,"207=MICEX",1,"10=158",1,"">>),  
  ?assertMatch({ok, _Record, _Bin, <<>>}, Result),
  {ok, Record, _, <<>>} = Result,
  ?assertMatch(#market_data_request{
    % sender_comp_id = <<"SENDER">>,
    % target_comp_id = <<"TARGET">>,
    % msg_seq_num = 31,
    % md_req_id = <<"42">>,
    % sending_time = <<"20120502-13:08:35">>,
    % subscription_request_type = snapshotupdate,
    % market_depth = 0,
    % md_update_type = 0,
    fields = [
      {poss_dup_flag,false},
      {no_md_entry_types,2},
      {md_entry_type, bid},
      {md_entry_type, offer},
      {no_related_sym,1},
      {symbol,<<"URKA">>},
      {cfi_code,<<"EXXXXX">>},
      {security_exchange,<<"MICEX">>}
    ]
  }, Record).

sample_md() ->
  <<"8=FIX.4.4",1,"9=1084",1,"35=W",1,"34=2",1,"49=TARGET_FEED_UAT",1,"52=20120504-08:03:28.693",1,
  "56=SENDER1_FEED_UAT",1,"55=APPL",1,"262=1",1,"268=40",1,"269=0",1,"270=215.34",1,"271=320",1,"269=0",1,"270=215.24",1,"271=3200",1,"269=0",1,"270=215.17",1,
  "271=2190",1,"269=0",1,"270=215.14",1,"271=60",1,"269=0",1,"270=215.11",1,"271=80",1,"269=0",1,"270=215.02",1,"271=150",1,"269=0",1,"270=215.01",1,"271=130",1,
  "269=0",1,"270=215",1,"271=8970",1,"269=0",1,"270=214.98",1,"271=30",1,"269=0",1,"270=214.97",1,"271=20",1,"269=0",1,"270=214.95",1,"271=790",1,"269=0",1,
  "270=214.9",1,"271=110",1,"269=0",1,"270=214.85",1,"271=20",1,"269=0",1,"270=214.8",1,"271=20",1,"269=0",1,"270=214.77",1,"271=160",1,"269=0",1,"270=214.7",1,
  "271=460",1,"269=0",1,"270=214.6",1,"271=10",1,"269=0",1,"270=214.57",1,"271=10",1,"269=0",1,"270=214.56",1,"271=60",1,"269=0",1,"270=214.53",1,"271=1560",1,
  "269=1",1,"270=215.45",1,"271=150",1,"269=1",1,"270=215.6",1,"271=140",1,"269=1",1,"270=215.61",1,"271=2790",1,"269=1",1,"270=215.82",1,"271=150",1,"269=1",1,
  "270=215.83",1,"271=890",1,"269=1",1,"270=215.85",1,"271=990",1,"269=1",1,"270=215.86",1,"271=520",1,"269=1",1,"270=215.88",1,"271=500",1,"269=1",1,"270=215.94",1,
  "271=290",1,"269=1",1,"270=215.95",1,"271=250",1,"269=1",1,"270=215.96",1,"271=2500",1,"269=1",1,"270=215.97",1,"271=8810",1,"269=1",1,"270=216",1,"271=2050",1,
  "269=1",1,"270=216.17",1,"271=60",1,"269=1",1,"270=216.24",1,"271=1760",1,"269=1",1,"270=216.25",1,"271=1200",1,"269=1",1,"270=216.3",1,"271=1000",1,"269=1",1,
  "270=216.45",1,"271=410",1,"269=1",1,"270=216.47",1,"271=500",1,"269=1",1,"270=216.48",1,"271=250",1,"10=156",1>>.
  
fix_decode_3_test() ->
  {ok, Record, _, <<>>} = fix:decode(sample_md()),
  ?assertMatch(#market_data_snapshot_full_refresh{
    md_entries = [[{md_entry_type,bid},{md_entry_px,215.34},{md_entry_size,320}]|_]
  }, Record).

logon_50_test() ->
  SendingTime = <<"20210907-11:42:41.654">>,
  Encoded = fix:pack(logon,
                   [{sending_time, SendingTime},
                    {encrypt_method, 0},
                    {heart_bt_int, 30},
                    {reset_seq_num_flag, "Y"} ,
                    {appl_ver_id, fix_50_sp1},
                    {default_appl_ver_id, fix_50_sp1}],
                   2,
                   <<"Sender">>,
                   <<"Target">>,
                   ?FIX_5_0_SP2),
  Expected = "8=FIXT.1.1|9=87|35=A|49=Sender|56=Target|34=2|52=20210907-11:42:41.654|98=0|108=30|141=Y|1128=8|1137=8|10=206|",
  ?assertEqual(Expected, fix:convert_pretty(Encoded)),

  {ok, Decoded, _, <<>>} = fix:decode(iolist_to_binary(Encoded)),
  ?assertMatch(#logon{sending_time = SendingTime,
                      encrypt_method = 0,
                      heart_bt_int = 30,
                      reset_seq_num_flag = true,
                      fields = [{appl_ver_id,fix_50_sp1},
                                {default_appl_ver_id, fix_50_sp1}]},
               Decoded).

quote_encoding_test() ->
    Sender = <<"Crypto-LP-Q">>,
    Target = <<"Crypto-RFQ-Q">>,
    SendingTime = "20210907-11:42:41.654",
    Encoded = fix:pack(quote,
           [{sending_time, SendingTime},
            {symbol, <<"USD/JPY">>},
            {quote_id, <<"q_id_USD/JPY">>},
            {quote_req_id, <<"QRS_11631014961606">>},
            {bid_px, <<"99.53816278537646">>},
            {offer_px, <<"105.48890454985164">>},
            {bid_size, 5000000},
            {offer_size, 5000000},
            {settl_date, <<"2022-04-29">>},
            {quote_type, tradeable},
            {quote_msg_id, <<"q_msg_id_USD/JPY_1">>}],
           3,
           Sender,
           Target),
    Expected = "8=FIX.4.4|9=230|35=S|49=Crypto-LP-Q|56=Crypto-RFQ-Q|34=3|52=20210907-11:42:41.654|55=USD/JPY|117=q_id_USD/JPY|131=QRS_11631014961606|132=99.53816278537646|133=105.48890454985164|134=5000000|135=5000000|64=2022-04-29|537=1|1166=q_msg_id_USD/JPY_1|10=183|",
    compare(Expected, fix:convert_pretty(Encoded)),
    ?assertEqual(Expected, fix:convert_pretty(Encoded)).

quote_decoding_test() ->
    Quote1 = <<"8=FIXT.1.1|9=221|35=S|34=2|49=Crypto-LP-Q|52=20210907-11:42:41.653|56=Crypto-RFQ-Q|55=LTC/USD|117=q_id_LTC/USD|131=QRS_11631014961606|132=299.5514016087717|133=330.0882584096762|134=349|135=351|64=2022-04-29|537=1|1166=q_msg_id_LTC/USD_1|10=023|">>,
    {ok, DecodedQuote1, _, <<>>} = fix:decode_printable(Quote1),
    ?assertMatch(#quote{
                    symbol = <<"LTC/USD">>,
                    quote_id = <<"q_id_LTC/USD">>,
                    quote_req_id = <<"QRS_11631014961606">>,
                    bid_px = 299.5514016087717,
                    offer_px = _, %330.0882584096762, % float conversion is not precise!
                    bid_size = 349,
                    offer_size = 351,
                    quote_type = tradeable,
                    quote_msg_id = <<"q_msg_id_LTC/USD_1">>,
                    settl_date = <<"2022-04-29">>,
                    fields = []
                   }, DecodedQuote1).

security_list_request_encoding_test() ->
    Sender = <<"Crypto-LP-T">>,
    Target = <<"Crypto-RFQ-T">>,

    EncodedSecurityListRequest = fix:pack(security_list_request,
           [{sending_time, "20210907-11:42:41.468"},
            {subscription_request_type, snapshot},
            {security_req_id, <<"SLR_01631014961437">>},
            {security_list_request_type, allsecurities},
            {security_list_type, tradinglist}
           ],
           2,
           Target,
           Sender,
           ?FIX_5_0_SP2),
    ExpectedSecurityListRequest = "8=FIXT.1.1|9=108|35=x|49=Crypto-RFQ-T|56=Crypto-LP-T|34=2|52=20210907-11:42:41.468|263=0|320=SLR_01631014961437|559=4|1470=2|10=206|",
    ok = compare(ExpectedSecurityListRequest, fix:convert_pretty(EncodedSecurityListRequest)),
    ?assertEqual(ExpectedSecurityListRequest, fix:convert_pretty(EncodedSecurityListRequest)).

security_list_request_decoding_test() ->
    SecurityListRequest = <<"8=FIXT.1.1|9=108|35=x|34=2|49=Crypto-RFQ-T|52=20210907-11:42:41.468|56=Crypto-LP-T|263=0|320=SLR_01631014961437|559=4|1470=2|10=206|">>,

    {ok, DecodedSLR, _, <<>>} = fix:decode_printable(SecurityListRequest),
    ?assertMatch(#security_list_request{
                    subscription_request_type = snapshot,
                    security_req_id = <<"SLR_01631014961437">>,
                    security_list_request_type = allsecurities,
                    security_list_type = tradinglist}, DecodedSLR).

security_list_encoding_test() ->
    Sender = <<"Crypto-LP-T">>,
    Target = <<"Crypto-RFQ-T">>,

    EncodedSecurityList = fix:pack(security_list,
           [{sending_time, "20210907-11:42:41.582"},
            {security_req_id, <<"SLR_01631014961437">>},
            {security_request_result, val_idreq},
            {no_related_sym, 6},
            {symbol, <<"EUR/USD">>},
            {security_type, <<"FXSPOT">>},
            {symbol, <<"EUR/USD">>},
            {security_type, <<"FXFWD">>},
            {symbol, <<"USD/JPY">>},
            {security_type, <<"FXSPOT">>},
            {symbol, <<"USD/JPY">>},
            {security_type, <<"FXFWD">>},
            {symbol, <<"XRP/USD">>},
            {security_type, <<"CASH">>},
            {symbol, <<"LTC/USD">>},
            {security_type, <<"CASH">>}
           ],
           2,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedSecurityList = "8=FIXT.1.1|9=227|35=y|49=Crypto-LP-T|56=Crypto-RFQ-T|34=2|52=20210907-11:42:41.582|320=SLR_01631014961437|560=0|146=6|55=EUR/USD|167=FXSPOT|55=EUR/USD|167=FXFWD|55=USD/JPY|167=FXSPOT|55=USD/JPY|167=FXFWD|55=XRP/USD|167=CASH|55=LTC/USD|167=CASH|10=229|",
    ok = compare(ExpectedSecurityList, fix:convert_pretty(EncodedSecurityList)),
    ?assertEqual(ExpectedSecurityList, fix:convert_pretty(EncodedSecurityList)),

    RejectSecurityList = fix:pack(security_list,
           [{sending_time, "20210907-11:42:41.582"},
            {security_req_id, <<"SLR_01631014961437">>},
            {security_request_result, inval_idreq},
            {security_reject_reason, request_type_not_supported}
           ],
           2,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedReject = "8=FIXT.1.1|9=102|35=y|49=Crypto-LP-T|56=Crypto-RFQ-T|34=2|52=20210907-11:42:41.582|320=SLR_01631014961437|560=1|1607=3|10=181|",
    ok = compare(ExpectedReject, fix:convert_pretty(RejectSecurityList)),
    ?assertEqual(ExpectedReject, fix:convert_pretty(RejectSecurityList)).

security_list_decoding_test() ->
    SecurityList = <<"8=FIXT.1.1|9=227|35=y|34=2|49=Crypto-LP-T|52=20210907-11:42:41.582|56=Crypto-RFQ-T|320=SLR_01631014961437|560=0|146=6|55=EUR/USD|167=FXSPOT|55=EUR/USD|167=FXFWD|55=USD/JPY|167=FXSPOT|55=USD/JPY|167=FXFWD|55=XRP/USD|167=CASH|55=LTC/USD|167=CASH|10=229|">>,
    {ok, DecodedSL, _, <<>>} = fix:decode_printable(SecurityList),
    ?assertMatch(#security_list{
                    security_req_id = <<"SLR_01631014961437">>,
                    security_request_result = val_idreq,
                    no_related_sym = 6,
                    symbols = _,
                    fields = _
                   }, DecodedSL),

    RejectSecurityList = <<"8=FIXT.1.1|9=102|35=y|49=Crypto-LP-T|56=Crypto-RFQ-T|34=2|52=20210907-11:42:41.582|320=SLR_01631014961437|560=1|1607=3|10=181|">>,
    {ok, DecodedReject, _, <<>>} = fix:decode_printable(RejectSecurityList),
    ?assertMatch(#security_list{
                    security_req_id = <<"SLR_01631014961437">>,
                    security_request_result = inval_idreq,
                    security_reject_reason = request_type_not_supported,
                    symbols = _,
                    fields = _
                   }, DecodedReject).

quote_request_encoding_test() ->
    Sender = <<"Crypto-LP-Q">>,
    Target = <<"Crypto-RFQ-Q">>,

    EncodedQuoteRequest = fix:pack(quote_request,
           [{sending_time, "20210907-11:42:41.619"},
            {quote_req_id, <<"QRS_11631014961606">>},
            {private_quote, <<"N">>},
            {no_related_sym, 3},

            {symbol, <<"EUR/USD">>},
            {security_type, <<"FXSPOT">>},
            {quote_type, tradeable},
            {settl_type, regular},
            {settl_date, <<"20210909">>},
            {transact_time, <<"20210907-11:42:41.618">>},

            {symbol, <<"LTC/USD">>},
            {security_type, <<"CASH">>},
            {quote_type, tradeable},
            {settl_type, cash},
            {settl_date, <<"20210907">>},
            {transact_time, <<"20210907-11:42:41.619">>},

            {symbol, <<"USD/JPY">>},
            {security_type, <<"FXSPOT">>},
            {quote_type, tradeable},
            {settl_type, regular},
            {settl_date, <<"20210909">>},
            {transact_time, <<"20210907-11:42:41.619">>}
           ],
           2,
           Target,
           Sender,
           ?FIX_5_0_SP2),
    ExpectedQuoteRequest = "8=FIXT.1.1|9=310|35=R|49=Crypto-RFQ-Q|56=Crypto-LP-Q|34=2|52=20210907-11:42:41.619|131=QRS_11631014961606|1171=N|146=3|55=EUR/USD|167=FXSPOT|537=1|63=0|64=20210909|60=20210907-11:42:41.618|55=LTC/USD|167=CASH|537=1|63=1|64=20210907|60=20210907-11:42:41.619|55=USD/JPY|167=FXSPOT|537=1|63=0|64=20210909|60=20210907-11:42:41.619|10=022|",
    ?assertEqual(ExpectedQuoteRequest, fix:convert_pretty(EncodedQuoteRequest)).

quote_request_decoding_test() ->
    QuoteRequest = <<"8=FIXT.1.1|9=316|35=R|34=2|49=Crypto-RFQ-Q|52=20210907-11:42:41.619|56=Crypto-LP-Q|131=QRS_11631014961606|775=0|1171=N|146=3|55=EUR/USD|167=FXSPOT|537=1|63=0|64=20210909|60=20210907-11:42:41.618|55=LTC/USD|167=CASH|537=1|63=1|64=20210907|60=20210907-11:42:41.619|55=USD/JPY|167=FXSPOT|537=1|63=0|64=20210909|60=20210907-11:42:41.619|10=045|">>,
    {ok, DecodedQuoteRequest, _, <<>>} = fix:decode_printable(QuoteRequest),
    ?assertMatch(#quote_request{
                    quote_req_id = <<"QRS_11631014961606">>,
                    no_related_sym = 3,
                    private_quote = <<"N">>,
                    booking_type = regularbooking,
                    symbols = _,
                    fields = _
                   }, DecodedQuoteRequest).

quote_request_reject_encoding_test() ->
    Sender = <<"Crypto-LP-Q">>,
    Target = <<"Crypto-RFQ-Q">>,

    EncodedQuoteRequestReject = fix:pack(quote_request_reject,
           [{sending_time, "20210908-12:51:24.473"},
            {text, <<"Text...">>},
            {quote_req_id, <<"QRS_11631105484440">>},
            {quote_request_reject_reason, <<"99">>},
            {no_related_sym, 1},
            {symbol, <<"LTC/USD">>}
           ],
           2,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedQuoteRequestReject = "8=FIXT.1.1|9=125|35=AG|49=Crypto-LP-Q|56=Crypto-RFQ-Q|34=2|52=20210908-12:51:24.473|58=Text...|131=QRS_11631105484440|658=99|146=1|55=LTC/USD|10=090|",
    ok = compare(ExpectedQuoteRequestReject, fix:convert_pretty(EncodedQuoteRequestReject)),
    ?assertEqual(ExpectedQuoteRequestReject, fix:convert_pretty(EncodedQuoteRequestReject)).

quote_request_reject_decoding_test() ->
    QuoteRequestReject = <<"8=FIXT.1.1|9=125|35=AG|34=2|49=Crypto-LP-Q|52=20210908-12:51:24.473|56=Crypto-RFQ-Q|58=Text...|131=QRS_11631105484440|658=99|146=1|55=LTC/USD|10=090|">>,
    {ok, DecodedQuoteRequestReject, _, <<>>} = fix:decode_printable(QuoteRequestReject),
    ?assertMatch(#quote_request_reject{
                    quote_req_id = <<"QRS_11631105484440">>,
                    quote_request_reject_reason = other,
                    no_related_sym = 1,
                    symbols = _,
                    fields = _
                   }, DecodedQuoteRequestReject).

mass_quote_encoding_test() ->
    Sender = <<"Crypto-LP-Q">>,
    Target = <<"Crypto-RFQ-Q">>,

    EncodedMassQuote = fix:pack(mass_quote,
           [{sending_time, "20210907-11:42:41.671"},
            {quote_id, <<"q_id_mass_EUR/USD">>},
            {quote_req_id, <<"QRS_11631014961606">>},
            {quote_type, tradeable},
            {no_quote_sets, 1},
            {quote_set_id, <<"1">>},
            {underlying_symbol, <<"EUR/USD">>},
            {tot_no_quote_entries, 3},
            {no_quote_entries, 3},
            {quote_entry_id, <<"q_entry_id_EUR/USD_1">>},
            {bid_px, <<"-0.17776730575781">>},
            {offer_px, <<"2.57776730575781">>},
            {bid_size, <<"1000000">>},
            {offer_size, <<"1000000">>},
            {transact_time, <<"20210907-13:42:41.670">>},
            {currency, <<"EUR">>},
            {quote_entry_id, <<"q_entry_id_EUR/USD_2">>},
            {bid_px, <<"-1.17776730575781">>},
            {offer_px, <<"3.57776730575781">>},
            {bid_size, <<"5000000">>},
            {offer_size, <<"5000000">>},
            {transact_time, <<"20210907-13:42:41.671">>},
            {currency, <<"EUR">>},
            {quote_entry_id, <<"q_entry_id_EUR/USD_3">>},
            {bid_px, <<"-2.17776730575781">>},
            {offer_px, <<"4.57776730575781">>},
            {bid_size, <<"10000000">>},
            {offer_size, <<"10000000">>},
            {transact_time, <<"20210907-13:42:41.671">>},
            {currency, <<"EUR">>}
           ],
           4,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedMassQuote = "8=FIXT.1.1|9=527|35=i|49=Crypto-LP-Q|56=Crypto-RFQ-Q|34=4|52=20210907-11:42:41.671|117=q_id_mass_EUR/USD|131=QRS_11631014961606|537=1|296=1|302=1|311=EUR/USD|304=3|295=3|299=q_entry_id_EUR/USD_1|132=-0.17776730575781|133=2.57776730575781|134=1000000|135=1000000|60=20210907-13:42:41.670|15=EUR|299=q_entry_id_EUR/USD_2|132=-1.17776730575781|133=3.57776730575781|134=5000000|135=5000000|60=20210907-13:42:41.671|15=EUR|299=q_entry_id_EUR/USD_3|132=-2.17776730575781|133=4.57776730575781|134=10000000|135=10000000|60=20210907-13:42:41.671|15=EUR|10=056|",
    ok = compare(ExpectedMassQuote, fix:convert_pretty(EncodedMassQuote)),
    ?assertEqual(ExpectedMassQuote, fix:convert_pretty(EncodedMassQuote)).

mass_quote_decoding_test() ->
    MassQuote = <<"8=FIXT.1.1|9=527|35=i|34=4|49=Crypto-LP-Q|52=20210907-11:42:41.671|56=Crypto-RFQ-Q|117=q_id_mass_EUR/USD|131=QRS_11631014961606|537=1|296=1|302=1|311=EUR/USD|304=3|295=3|299=q_entry_id_EUR/USD_1|132=-0.17776730575781|133=2.57776730575781|134=1000000|135=1000000|60=20210907-13:42:41.670|15=EUR|299=q_entry_id_EUR/USD_2|132=-1.17776730575781|133=3.57776730575781|134=5000000|135=5000000|60=20210907-13:42:41.671|15=EUR|299=q_entry_id_EUR/USD_3|132=-2.17776730575781|133=4.57776730575781|134=10000000|135=10000000|60=20210907-13:42:41.671|15=EUR|10=056|">>,
    {ok, DecodedMassQuote, _, <<>>} = fix:decode_printable(MassQuote),
    ?assertMatch(#mass_quote{
                    quote_id = <<"q_id_mass_EUR/USD">>,
                    quote_req_id = <<"QRS_11631014961606">>,
                    quote_type = tradeable,
                    no_quote_sets = 1,
                    quote_sets = _, % TODO: should have values
                    fields = _  % TODO: should be []
                   }, DecodedMassQuote).

quote_cancel_encoding_test() ->
    Sender = <<"Crypto-LP-Q">>,
    Target = <<"Crypto-RFQ-Q">>,

    EncodedQuoteCancel = fix:pack(quote_cancel,
           [{sending_time, "20211119-18:09:16.770"},
            {quote_req_id, <<"foo_id">>},
            {no_quote_entries, 1},
            {quote_cancel_type, cxlsym},
            {symbol, <<"USD/UAH">>}
           ],
           1,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedQuoteCancel = "8=FIXT.1.1|9=100|35=Z|49=Crypto-LP-Q|56=Crypto-RFQ-Q|34=1|52=20211119-18:09:16.770|131=foo_id|295=1|298=1|55=USD/UAH|10=095|",
    ok = compare(ExpectedQuoteCancel, fix:convert_pretty(EncodedQuoteCancel)),
    ?assertEqual(ExpectedQuoteCancel, fix:convert_pretty(EncodedQuoteCancel)).

quote_cancel_decoding_test() ->
    QuoteCancel = <<"8=FIXT.1.1|9=100|35=Z|49=Crypto-LP-Q|56=Crypto-RFQ-Q|34=1|52=20211119-18:09:16.770|131=foo_id|298=1|295=1|55=USD/UAH|10=095|">>,
    {ok, DecodedQuoteCancel, _, <<>>} = fix:decode_printable(QuoteCancel),
    ?assertMatch(#quote_cancel{
                    quote_req_id = <<"foo_id">>,
                    quote_cancel_type = cxlsym,
                    no_quote_entries = 1,
                    symbols = _,
                    fields = _
                   }, DecodedQuoteCancel).

quote_status_report_encoding_test() ->
    Sender = <<"OTPUATQUOTE">>,
    Target = <<"OTPSMQUOTE">>,

    EncodedQuoteStatusReport = fix:pack(quote_status_report,
           [{sending_time, "20220125-16:52:33.679"},
            {quote_id, <<"q_id_USD/UAH">>},
            {quote_status, canceled},
            {quote_type, tradeable}
           ],
           5,
           Sender,
           Target,
           ?FIX_5_0_SP2),
    ExpectedQuoteStatusReport = "8=FIXT.1.1|9=95|35=AI|49=OTPUATQUOTE|56=OTPSMQUOTE|34=5|52=20220125-16:52:33.679|117=q_id_USD/UAH|297=17|537=1|10=060|",
    ok = compare(ExpectedQuoteStatusReport, fix:convert_pretty(EncodedQuoteStatusReport)),
    ?assertEqual(ExpectedQuoteStatusReport, fix:convert_pretty(EncodedQuoteStatusReport)).

quote_status_report_decoding_test() ->
    QuoteStatusReport = <<"8=FIXT.1.1|9=95|35=AI|34=5|49=OTPUATQUOTE|52=20220125-16:52:33.679|56=OTPSMQUOTE|117=q_id_USD/UAH|297=17|537=1|10=060|">>,
    {ok, DecodedQuoteStatusReport, _, <<>>} = fix:decode_printable(QuoteStatusReport),
    ?assertMatch(#quote_status_report{
                    quote_id = <<"q_id_USD/UAH">>,
                    quote_status = canceled,
                    quote_type = tradeable
                   }, DecodedQuoteStatusReport).

compare(Expected, Encoded) ->
    ExpectedList = string:split(Expected, "|", all),
    EncodedList = string:split(Encoded, "|", all),
    compare_lists(ExpectedList, EncodedList).

compare_lists([], []) ->
    ok;
compare_lists([X | ExpectedRest], [X | EncodedRest]) ->
    compare_lists(ExpectedRest, EncodedRest);
compare_lists([ExpectedElement | ExpectedRest], [EncodedElement | EncodedRest]) ->
    io:format("~p ~p:~p EncodedElement: '~p'~n", [calendar:local_time(), ?MODULE, ?LINE, EncodedElement]),
    io:format("~p ~p:~p ExpectedElement: '~p'~n", [calendar:local_time(), ?MODULE, ?LINE, ExpectedElement]),
    compare_lists(ExpectedRest, EncodedRest).
