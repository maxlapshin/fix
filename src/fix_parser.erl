-module(fix_parser).
-include("../include/admin.hrl").
-include("../include/business.hrl").

-export([decode_message/1, field_by_number/1, number_by_field/1, decode_typed_field/2, encode_typed_field/2, message_by_number/1, number_by_message/1]).

decode_message(<<"35=0",1,Message/binary>>) -> % Heartbeat
  decode_message_heartbeat(Message, #heartbeat{});

decode_message(<<"35=1",1,Message/binary>>) -> % TestRequest
  decode_message_test_request(Message, #test_request{});

decode_message(<<"35=2",1,Message/binary>>) -> % ResendRequest
  decode_message_resend_request(Message, #resend_request{});

decode_message(<<"35=3",1,Message/binary>>) -> % Reject
  decode_message_reject(Message, #reject{});

decode_message(<<"35=4",1,Message/binary>>) -> % SequenceReset
  decode_message_sequence_reset(Message, #sequence_reset{});

decode_message(<<"35=5",1,Message/binary>>) -> % Logout
  decode_message_logout(Message, #logout{});

decode_message(<<"35=6",1,Message/binary>>) -> % IOI
  decode_message_ioi(Message, #ioi{});

decode_message(<<"35=7",1,Message/binary>>) -> % Advertisement
  decode_message_advertisement(Message, #advertisement{});

decode_message(<<"35=8",1,Message/binary>>) -> % ExecutionReport
  decode_message_execution_report(Message, #execution_report{});

decode_message(<<"35=9",1,Message/binary>>) -> % OrderCancelReject
  decode_message_order_cancel_reject(Message, #order_cancel_reject{});

decode_message(<<"35=A",1,Message/binary>>) -> % Logon
  decode_message_logon(Message, #logon{});

decode_message(<<"35=B",1,Message/binary>>) -> % News
  decode_message_news(Message, #news{});

decode_message(<<"35=C",1,Message/binary>>) -> % Email
  decode_message_email(Message, #email{});

decode_message(<<"35=D",1,Message/binary>>) -> % NewOrderSingle
  decode_message_new_order_single(Message, #new_order_single{});

decode_message(<<"35=E",1,Message/binary>>) -> % NewOrderList
  decode_message_new_order_list(Message, #new_order_list{});

decode_message(<<"35=F",1,Message/binary>>) -> % OrderCancelRequest
  decode_message_order_cancel_request(Message, #order_cancel_request{});

decode_message(<<"35=G",1,Message/binary>>) -> % OrderCancelReplaceRequest
  decode_message_order_cancel_replace_request(Message, #order_cancel_replace_request{});

decode_message(<<"35=H",1,Message/binary>>) -> % OrderStatusRequest
  decode_message_order_status_request(Message, #order_status_request{});

decode_message(<<"35=J",1,Message/binary>>) -> % AllocationInstruction
  decode_message_allocation_instruction(Message, #allocation_instruction{});

decode_message(<<"35=K",1,Message/binary>>) -> % ListCancelRequest
  decode_message_list_cancel_request(Message, #list_cancel_request{});

decode_message(<<"35=L",1,Message/binary>>) -> % ListExecute
  decode_message_list_execute(Message, #list_execute{});

decode_message(<<"35=M",1,Message/binary>>) -> % ListStatusRequest
  decode_message_list_status_request(Message, #list_status_request{});

decode_message(<<"35=N",1,Message/binary>>) -> % ListStatus
  decode_message_list_status(Message, #list_status{});

decode_message(<<"35=P",1,Message/binary>>) -> % AllocationInstructionAck
  decode_message_allocation_instruction_ack(Message, #allocation_instruction_ack{});

decode_message(<<"35=Q",1,Message/binary>>) -> % DontKnowTrade
  decode_message_dont_know_trade(Message, #dont_know_trade{});

decode_message(<<"35=R",1,Message/binary>>) -> % QuoteRequest
  decode_message_quote_request(Message, #quote_request{});

decode_message(<<"35=S",1,Message/binary>>) -> % Quote
  decode_message_quote(Message, #quote{});

decode_message(<<"35=T",1,Message/binary>>) -> % SettlementInstructions
  decode_message_settlement_instructions(Message, #settlement_instructions{});

decode_message(<<"35=V",1,Message/binary>>) -> % MarketDataRequest
  decode_message_market_data_request(Message, #market_data_request{});

decode_message(<<"35=W",1,Message/binary>>) -> % MarketDataSnapshotFullRefresh
  decode_message_market_data_snapshot_full_refresh(Message, #market_data_snapshot_full_refresh{});

decode_message(<<"35=X",1,Message/binary>>) -> % MarketDataIncrementalRefresh
  decode_message_market_data_incremental_refresh(Message, #market_data_incremental_refresh{});

decode_message(<<"35=Y",1,Message/binary>>) -> % MarketDataRequestReject
  decode_message_market_data_request_reject(Message, #market_data_request_reject{});

decode_message(<<"35=Z",1,Message/binary>>) -> % QuoteCancel
  decode_message_quote_cancel(Message, #quote_cancel{});

decode_message(<<"35=a",1,Message/binary>>) -> % QuoteStatusRequest
  decode_message_quote_status_request(Message, #quote_status_request{});

decode_message(<<"35=b",1,Message/binary>>) -> % MassQuoteAcknowledgement
  decode_message_mass_quote_acknowledgement(Message, #mass_quote_acknowledgement{});

decode_message(<<"35=c",1,Message/binary>>) -> % SecurityDefinitionRequest
  decode_message_security_definition_request(Message, #security_definition_request{});

decode_message(<<"35=d",1,Message/binary>>) -> % SecurityDefinition
  decode_message_security_definition(Message, #security_definition{});

decode_message(<<"35=e",1,Message/binary>>) -> % SecurityStatusRequest
  decode_message_security_status_request(Message, #security_status_request{});

decode_message(<<"35=f",1,Message/binary>>) -> % SecurityStatus
  decode_message_security_status(Message, #security_status{});

decode_message(<<"35=g",1,Message/binary>>) -> % TradingSessionStatusRequest
  decode_message_trading_session_status_request(Message, #trading_session_status_request{});

decode_message(<<"35=h",1,Message/binary>>) -> % TradingSessionStatus
  decode_message_trading_session_status(Message, #trading_session_status{});

decode_message(<<"35=i",1,Message/binary>>) -> % MassQuote
  decode_message_mass_quote(Message, #mass_quote{});

decode_message(<<"35=j",1,Message/binary>>) -> % BusinessMessageReject
  decode_message_business_message_reject(Message, #business_message_reject{});

decode_message(<<"35=k",1,Message/binary>>) -> % BidRequest
  decode_message_bid_request(Message, #bid_request{});

decode_message(<<"35=l",1,Message/binary>>) -> % BidResponse
  decode_message_bid_response(Message, #bid_response{});

decode_message(<<"35=m",1,Message/binary>>) -> % ListStrikePrice
  decode_message_list_strike_price(Message, #list_strike_price{});

decode_message(<<"35=o",1,Message/binary>>) -> % RegistrationInstructions
  decode_message_registration_instructions(Message, #registration_instructions{});

decode_message(<<"35=p",1,Message/binary>>) -> % RegistrationInstructionsResponse
  decode_message_registration_instructions_response(Message, #registration_instructions_response{});

decode_message(<<"35=q",1,Message/binary>>) -> % OrderMassCancelRequest
  decode_message_order_mass_cancel_request(Message, #order_mass_cancel_request{});

decode_message(<<"35=r",1,Message/binary>>) -> % OrderMassCancelReport
  decode_message_order_mass_cancel_report(Message, #order_mass_cancel_report{});

decode_message(<<"35=s",1,Message/binary>>) -> % NewOrderCross
  decode_message_new_order_cross(Message, #new_order_cross{});

decode_message(<<"35=t",1,Message/binary>>) -> % CrossOrderCancelReplaceRequest
  decode_message_cross_order_cancel_replace_request(Message, #cross_order_cancel_replace_request{});

decode_message(<<"35=u",1,Message/binary>>) -> % CrossOrderCancelRequest
  decode_message_cross_order_cancel_request(Message, #cross_order_cancel_request{});

decode_message(<<"35=v",1,Message/binary>>) -> % SecurityTypeRequest
  decode_message_security_type_request(Message, #security_type_request{});

decode_message(<<"35=w",1,Message/binary>>) -> % SecurityTypes
  decode_message_security_types(Message, #security_types{});

decode_message(<<"35=x",1,Message/binary>>) -> % SecurityListRequest
  decode_message_security_list_request(Message, #security_list_request{});

decode_message(<<"35=y",1,Message/binary>>) -> % SecurityList
  decode_message_security_list(Message, #security_list{});

decode_message(<<"35=z",1,Message/binary>>) -> % DerivativeSecurityListRequest
  decode_message_derivative_security_list_request(Message, #derivative_security_list_request{});

decode_message(<<"35=AA",1,Message/binary>>) -> % DerivativeSecurityList
  decode_message_derivative_security_list(Message, #derivative_security_list{});

decode_message(<<"35=AB",1,Message/binary>>) -> % NewOrderMultileg
  decode_message_new_order_multileg(Message, #new_order_multileg{});

decode_message(<<"35=AC",1,Message/binary>>) -> % MultilegOrderCancelReplace
  decode_message_multileg_order_cancel_replace(Message, #multileg_order_cancel_replace{});

decode_message(<<"35=AD",1,Message/binary>>) -> % TradeCaptureReportRequest
  decode_message_trade_capture_report_request(Message, #trade_capture_report_request{});

decode_message(<<"35=AE",1,Message/binary>>) -> % TradeCaptureReport
  decode_message_trade_capture_report(Message, #trade_capture_report{});

decode_message(<<"35=AF",1,Message/binary>>) -> % OrderMassStatusRequest
  decode_message_order_mass_status_request(Message, #order_mass_status_request{});

decode_message(<<"35=AG",1,Message/binary>>) -> % QuoteRequestReject
  decode_message_quote_request_reject(Message, #quote_request_reject{});

decode_message(<<"35=AH",1,Message/binary>>) -> % RFQRequest
  decode_message_rfq_request(Message, #rfq_request{});

decode_message(<<"35=AI",1,Message/binary>>) -> % QuoteStatusReport
  decode_message_quote_status_report(Message, #quote_status_report{});

decode_message(<<"35=AJ",1,Message/binary>>) -> % QuoteResponse
  decode_message_quote_response(Message, #quote_response{});

decode_message(<<"35=AK",1,Message/binary>>) -> % Confirmation
  decode_message_confirmation(Message, #confirmation{});

decode_message(<<"35=AL",1,Message/binary>>) -> % PositionMaintenanceRequest
  decode_message_position_maintenance_request(Message, #position_maintenance_request{});

decode_message(<<"35=AM",1,Message/binary>>) -> % PositionMaintenanceReport
  decode_message_position_maintenance_report(Message, #position_maintenance_report{});

decode_message(<<"35=AN",1,Message/binary>>) -> % RequestForPositions
  decode_message_request_for_positions(Message, #request_for_positions{});

decode_message(<<"35=AO",1,Message/binary>>) -> % RequestForPositionsAck
  decode_message_request_for_positions_ack(Message, #request_for_positions_ack{});

decode_message(<<"35=AP",1,Message/binary>>) -> % PositionReport
  decode_message_position_report(Message, #position_report{});

decode_message(<<"35=AQ",1,Message/binary>>) -> % TradeCaptureReportRequestAck
  decode_message_trade_capture_report_request_ack(Message, #trade_capture_report_request_ack{});

decode_message(<<"35=AR",1,Message/binary>>) -> % TradeCaptureReportAck
  decode_message_trade_capture_report_ack(Message, #trade_capture_report_ack{});

decode_message(<<"35=AS",1,Message/binary>>) -> % AllocationReport
  decode_message_allocation_report(Message, #allocation_report{});

decode_message(<<"35=AT",1,Message/binary>>) -> % AllocationReportAck
  decode_message_allocation_report_ack(Message, #allocation_report_ack{});

decode_message(<<"35=AU",1,Message/binary>>) -> % ConfirmationAck
  decode_message_confirmation_ack(Message, #confirmation_ack{});

decode_message(<<"35=AV",1,Message/binary>>) -> % SettlementInstructionRequest
  decode_message_settlement_instruction_request(Message, #settlement_instruction_request{});

decode_message(<<"35=AW",1,Message/binary>>) -> % AssignmentReport
  decode_message_assignment_report(Message, #assignment_report{});

decode_message(<<"35=AX",1,Message/binary>>) -> % CollateralRequest
  decode_message_collateral_request(Message, #collateral_request{});

decode_message(<<"35=AY",1,Message/binary>>) -> % CollateralAssignment
  decode_message_collateral_assignment(Message, #collateral_assignment{});

decode_message(<<"35=AZ",1,Message/binary>>) -> % CollateralResponse
  decode_message_collateral_response(Message, #collateral_response{});

decode_message(<<"35=BA",1,Message/binary>>) -> % CollateralReport
  decode_message_collateral_report(Message, #collateral_report{});

decode_message(<<"35=BB",1,Message/binary>>) -> % CollateralInquiry
  decode_message_collateral_inquiry(Message, #collateral_inquiry{});

decode_message(<<"35=BC",1,Message/binary>>) -> % NetworkCounterpartySystemStatusRequest
  decode_message_network_counterparty_system_status_request(Message, #network_counterparty_system_status_request{});

decode_message(<<"35=BD",1,Message/binary>>) -> % NetworkCounterpartySystemStatusResponse
  decode_message_network_counterparty_system_status_response(Message, #network_counterparty_system_status_response{});

decode_message(<<"35=BE",1,Message/binary>>) -> % UserRequest
  decode_message_user_request(Message, #user_request{});

decode_message(<<"35=BF",1,Message/binary>>) -> % UserResponse
  decode_message_user_response(Message, #user_response{});

decode_message(<<"35=BG",1,Message/binary>>) -> % CollateralInquiryAck
  decode_message_collateral_inquiry_ack(Message, #collateral_inquiry_ack{});

decode_message(<<"35=BH",1,Message/binary>>) -> % ConfirmationRequest
  decode_message_confirmation_request(Message, #confirmation_request{}).

decode_message_heartbeat(Message, #heartbeat{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #heartbeat{fields = F} = lists:foldl(fun
    ({test_req_id,V}, Rec) -> Rec#heartbeat{test_req_id = V};
    ({K,V}, #heartbeat{fields = F} = Rec) -> Rec#heartbeat{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#heartbeat{fields = lists:reverse(F)}.

decode_message_test_request(Message, #test_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #test_request{fields = F} = lists:foldl(fun
    ({test_req_id,V}, Rec) -> Rec#test_request{test_req_id = V};
    ({K,V}, #test_request{fields = F} = Rec) -> Rec#test_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#test_request{fields = lists:reverse(F)}.

decode_message_resend_request(Message, #resend_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #resend_request{fields = F} = lists:foldl(fun
    ({begin_seq_no,V}, Rec) -> Rec#resend_request{begin_seq_no = list_to_integer(binary_to_list(V))};
    ({end_seq_no,V}, Rec) -> Rec#resend_request{end_seq_no = list_to_integer(binary_to_list(V))};
    ({K,V}, #resend_request{fields = F} = Rec) -> Rec#resend_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#resend_request{fields = lists:reverse(F)}.

decode_message_reject(Message, #reject{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #reject{fields = F} = lists:foldl(fun
    ({ref_seq_num,V}, Rec) -> Rec#reject{ref_seq_num = list_to_integer(binary_to_list(V))};
    ({ref_tag_id,V}, Rec) -> Rec#reject{ref_tag_id = list_to_integer(binary_to_list(V))};
    ({ref_msg_type,V}, Rec) -> Rec#reject{ref_msg_type = V};
    ({session_reject_reason,V}, Rec) -> Rec#reject{session_reject_reason = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#reject{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#reject{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#reject{encoded_text = V};
    ({K,V}, #reject{fields = F} = Rec) -> Rec#reject{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#reject{fields = lists:reverse(F)}.

decode_message_sequence_reset(Message, #sequence_reset{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #sequence_reset{fields = F} = lists:foldl(fun
    ({gap_fill_flag,V}, Rec) -> Rec#sequence_reset{gap_fill_flag = V == <<"Y">>};
    ({new_seq_no,V}, Rec) -> Rec#sequence_reset{new_seq_no = list_to_integer(binary_to_list(V))};
    ({K,V}, #sequence_reset{fields = F} = Rec) -> Rec#sequence_reset{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#sequence_reset{fields = lists:reverse(F)}.

decode_message_logout(Message, #logout{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #logout{fields = F} = lists:foldl(fun
    ({text,V}, Rec) -> Rec#logout{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#logout{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#logout{encoded_text = V};
    ({K,V}, #logout{fields = F} = Rec) -> Rec#logout{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#logout{fields = lists:reverse(F)}.

decode_message_ioi(Message, #ioi{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #ioi{fields = F} = lists:foldl(fun
    ({ioi_id,V}, Rec) -> Rec#ioi{ioi_id = V};
    ({ioi_trans_type,V}, Rec) -> Rec#ioi{ioi_trans_type = V};
    ({ioi_ref_id,V}, Rec) -> Rec#ioi{ioi_ref_id = V};
    ({side,V}, Rec) -> Rec#ioi{side = V};
    ({qty_type,V}, Rec) -> Rec#ioi{qty_type = list_to_integer(binary_to_list(V))};
    ({ioi_qty,V}, Rec) -> Rec#ioi{ioi_qty = V};
    ({currency,V}, Rec) -> Rec#ioi{currency = V};
    ({leg_ioi_qty,V}, Rec) -> Rec#ioi{leg_ioi_qty = V};
    ({price_type,V}, Rec) -> Rec#ioi{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#ioi{price = list_to_float(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#ioi{valid_until_time = V};
    ({ioi_qlty_ind,V}, Rec) -> Rec#ioi{ioi_qlty_ind = V};
    ({ioi_natural_flag,V}, Rec) -> Rec#ioi{ioi_natural_flag = V == <<"Y">>};
    ({ioi_qualifier,V}, Rec) -> Rec#ioi{ioi_qualifier = V};
    ({text,V}, Rec) -> Rec#ioi{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#ioi{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#ioi{encoded_text = V};
    ({transact_time,V}, Rec) -> Rec#ioi{transact_time = V};
    ({url_link,V}, Rec) -> Rec#ioi{url_link = V};
    ({routing_type,V}, Rec) -> Rec#ioi{routing_type = list_to_integer(binary_to_list(V))};
    ({routing_id,V}, Rec) -> Rec#ioi{routing_id = V};
    ({K,V}, #ioi{fields = F} = Rec) -> Rec#ioi{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#ioi{fields = lists:reverse(F)}.

decode_message_advertisement(Message, #advertisement{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #advertisement{fields = F} = lists:foldl(fun
    ({adv_id,V}, Rec) -> Rec#advertisement{adv_id = V};
    ({adv_trans_type,V}, Rec) -> Rec#advertisement{adv_trans_type = V};
    ({adv_ref_id,V}, Rec) -> Rec#advertisement{adv_ref_id = V};
    ({adv_side,V}, Rec) -> Rec#advertisement{adv_side = V};
    ({quantity,V}, Rec) -> Rec#advertisement{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#advertisement{qty_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#advertisement{price = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#advertisement{currency = V};
    ({trade_date,V}, Rec) -> Rec#advertisement{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#advertisement{transact_time = V};
    ({text,V}, Rec) -> Rec#advertisement{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#advertisement{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#advertisement{encoded_text = V};
    ({url_link,V}, Rec) -> Rec#advertisement{url_link = V};
    ({last_mkt,V}, Rec) -> Rec#advertisement{last_mkt = V};
    ({trading_session_id,V}, Rec) -> Rec#advertisement{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#advertisement{trading_session_sub_id = V};
    ({K,V}, #advertisement{fields = F} = Rec) -> Rec#advertisement{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#advertisement{fields = lists:reverse(F)}.

decode_message_execution_report(Message, #execution_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #execution_report{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#execution_report{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#execution_report{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#execution_report{secondary_cl_ord_id = V};
    ({secondary_exec_id,V}, Rec) -> Rec#execution_report{secondary_exec_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#execution_report{cl_ord_id = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#execution_report{orig_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#execution_report{cl_ord_link_id = V};
    ({quote_resp_id,V}, Rec) -> Rec#execution_report{quote_resp_id = V};
    ({ord_status_req_id,V}, Rec) -> Rec#execution_report{ord_status_req_id = V};
    ({mass_status_req_id,V}, Rec) -> Rec#execution_report{mass_status_req_id = V};
    ({tot_num_reports,V}, Rec) -> Rec#execution_report{tot_num_reports = list_to_integer(binary_to_list(V))};
    ({last_rpt_requested,V}, Rec) -> Rec#execution_report{last_rpt_requested = V == <<"Y">>};
    ({trade_origination_date,V}, Rec) -> Rec#execution_report{trade_origination_date = V};
    ({contra_broker,V}, Rec) -> Rec#execution_report{contra_broker = V};
    ({contra_trader,V}, Rec) -> Rec#execution_report{contra_trader = V};
    ({contra_trade_qty,V}, Rec) -> Rec#execution_report{contra_trade_qty = list_to_integer(binary_to_list(V))};
    ({contra_trade_time,V}, Rec) -> Rec#execution_report{contra_trade_time = V};
    ({contra_leg_ref_id,V}, Rec) -> Rec#execution_report{contra_leg_ref_id = V};
    ({list_id,V}, Rec) -> Rec#execution_report{list_id = V};
    ({cross_id,V}, Rec) -> Rec#execution_report{cross_id = V};
    ({orig_cross_id,V}, Rec) -> Rec#execution_report{orig_cross_id = V};
    ({cross_type,V}, Rec) -> Rec#execution_report{cross_type = list_to_integer(binary_to_list(V))};
    ({exec_id,V}, Rec) -> Rec#execution_report{exec_id = V};
    ({exec_ref_id,V}, Rec) -> Rec#execution_report{exec_ref_id = V};
    ({exec_type,V}, Rec) -> Rec#execution_report{exec_type = V};
    ({ord_status,V}, Rec) -> Rec#execution_report{ord_status = V};
    ({working_indicator,V}, Rec) -> Rec#execution_report{working_indicator = V == <<"Y">>};
    ({ord_rej_reason,V}, Rec) -> Rec#execution_report{ord_rej_reason = list_to_integer(binary_to_list(V))};
    ({exec_restatement_reason,V}, Rec) -> Rec#execution_report{exec_restatement_reason = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#execution_report{account = V};
    ({acct_id_source,V}, Rec) -> Rec#execution_report{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#execution_report{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#execution_report{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#execution_report{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#execution_report{prealloc_method = V};
    ({settl_type,V}, Rec) -> Rec#execution_report{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#execution_report{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#execution_report{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#execution_report{clearing_fee_indicator = V};
    ({side,V}, Rec) -> Rec#execution_report{side = V};
    ({qty_type,V}, Rec) -> Rec#execution_report{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#execution_report{ord_type = V};
    ({price_type,V}, Rec) -> Rec#execution_report{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#execution_report{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#execution_report{stop_px = list_to_float(binary_to_list(V))};
    ({pegged_price,V}, Rec) -> Rec#execution_report{pegged_price = list_to_float(binary_to_list(V))};
    ({discretion_price,V}, Rec) -> Rec#execution_report{discretion_price = list_to_float(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#execution_report{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#execution_report{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#execution_report{participation_rate = V};
    ({target_strategy_performance,V}, Rec) -> Rec#execution_report{target_strategy_performance = V};
    ({currency,V}, Rec) -> Rec#execution_report{currency = V};
    ({compliance_id,V}, Rec) -> Rec#execution_report{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#execution_report{solicited_flag = V == <<"Y">>};
    ({time_in_force,V}, Rec) -> Rec#execution_report{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#execution_report{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#execution_report{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#execution_report{expire_time = V};
    ({exec_inst,V}, Rec) -> Rec#execution_report{exec_inst = V};
    ({order_capacity,V}, Rec) -> Rec#execution_report{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#execution_report{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#execution_report{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({last_qty,V}, Rec) -> Rec#execution_report{last_qty = list_to_integer(binary_to_list(V))};
    ({underlying_last_qty,V}, Rec) -> Rec#execution_report{underlying_last_qty = list_to_integer(binary_to_list(V))};
    ({last_px,V}, Rec) -> Rec#execution_report{last_px = list_to_float(binary_to_list(V))};
    ({underlying_last_px,V}, Rec) -> Rec#execution_report{underlying_last_px = list_to_float(binary_to_list(V))};
    ({last_par_px,V}, Rec) -> Rec#execution_report{last_par_px = list_to_float(binary_to_list(V))};
    ({last_spot_rate,V}, Rec) -> Rec#execution_report{last_spot_rate = list_to_float(binary_to_list(V))};
    ({last_forward_points,V}, Rec) -> Rec#execution_report{last_forward_points = V};
    ({last_mkt,V}, Rec) -> Rec#execution_report{last_mkt = V};
    ({trading_session_id,V}, Rec) -> Rec#execution_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#execution_report{trading_session_sub_id = V};
    ({time_bracket,V}, Rec) -> Rec#execution_report{time_bracket = V};
    ({last_capacity,V}, Rec) -> Rec#execution_report{last_capacity = V};
    ({leaves_qty,V}, Rec) -> Rec#execution_report{leaves_qty = list_to_integer(binary_to_list(V))};
    ({cum_qty,V}, Rec) -> Rec#execution_report{cum_qty = list_to_integer(binary_to_list(V))};
    ({avg_px,V}, Rec) -> Rec#execution_report{avg_px = list_to_float(binary_to_list(V))};
    ({day_order_qty,V}, Rec) -> Rec#execution_report{day_order_qty = list_to_integer(binary_to_list(V))};
    ({day_cum_qty,V}, Rec) -> Rec#execution_report{day_cum_qty = list_to_integer(binary_to_list(V))};
    ({day_avg_px,V}, Rec) -> Rec#execution_report{day_avg_px = list_to_float(binary_to_list(V))};
    ({gt_booking_inst,V}, Rec) -> Rec#execution_report{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({trade_date,V}, Rec) -> Rec#execution_report{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#execution_report{transact_time = V};
    ({report_to_exch,V}, Rec) -> Rec#execution_report{report_to_exch = V == <<"Y">>};
    ({gross_trade_amt,V}, Rec) -> Rec#execution_report{gross_trade_amt = V};
    ({num_days_interest,V}, Rec) -> Rec#execution_report{num_days_interest = list_to_integer(binary_to_list(V))};
    ({ex_date,V}, Rec) -> Rec#execution_report{ex_date = V};
    ({accrued_interest_rate,V}, Rec) -> Rec#execution_report{accrued_interest_rate = V};
    ({accrued_interest_amt,V}, Rec) -> Rec#execution_report{accrued_interest_amt = V};
    ({interest_at_maturity,V}, Rec) -> Rec#execution_report{interest_at_maturity = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#execution_report{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#execution_report{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#execution_report{end_cash = V};
    ({traded_flat_switch,V}, Rec) -> Rec#execution_report{traded_flat_switch = V == <<"Y">>};
    ({basis_feature_date,V}, Rec) -> Rec#execution_report{basis_feature_date = V};
    ({basis_feature_price,V}, Rec) -> Rec#execution_report{basis_feature_price = list_to_float(binary_to_list(V))};
    ({concession,V}, Rec) -> Rec#execution_report{concession = V};
    ({total_takedown,V}, Rec) -> Rec#execution_report{total_takedown = V};
    ({net_money,V}, Rec) -> Rec#execution_report{net_money = V};
    ({settl_curr_amt,V}, Rec) -> Rec#execution_report{settl_curr_amt = V};
    ({settl_currency,V}, Rec) -> Rec#execution_report{settl_currency = V};
    ({settl_curr_fx_rate,V}, Rec) -> Rec#execution_report{settl_curr_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#execution_report{settl_curr_fx_rate_calc = V};
    ({handl_inst,V}, Rec) -> Rec#execution_report{handl_inst = V};
    ({min_qty,V}, Rec) -> Rec#execution_report{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#execution_report{max_floor = list_to_integer(binary_to_list(V))};
    ({position_effect,V}, Rec) -> Rec#execution_report{position_effect = V};
    ({max_show,V}, Rec) -> Rec#execution_report{max_show = list_to_integer(binary_to_list(V))};
    ({booking_type,V}, Rec) -> Rec#execution_report{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#execution_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#execution_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#execution_report{encoded_text = V};
    ({settl_date2,V}, Rec) -> Rec#execution_report{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#execution_report{order_qty2 = list_to_integer(binary_to_list(V))};
    ({last_forward_points2,V}, Rec) -> Rec#execution_report{last_forward_points2 = V};
    ({multi_leg_reporting_type,V}, Rec) -> Rec#execution_report{multi_leg_reporting_type = V};
    ({cancellation_rights,V}, Rec) -> Rec#execution_report{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#execution_report{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#execution_report{regist_id = V};
    ({designation,V}, Rec) -> Rec#execution_report{designation = V};
    ({trans_bkd_time,V}, Rec) -> Rec#execution_report{trans_bkd_time = V};
    ({exec_valuation_point,V}, Rec) -> Rec#execution_report{exec_valuation_point = V};
    ({exec_price_type,V}, Rec) -> Rec#execution_report{exec_price_type = V};
    ({exec_price_adjustment,V}, Rec) -> Rec#execution_report{exec_price_adjustment = V};
    ({priority_indicator,V}, Rec) -> Rec#execution_report{priority_indicator = list_to_integer(binary_to_list(V))};
    ({price_improvement,V}, Rec) -> Rec#execution_report{price_improvement = V};
    ({last_liquidity_ind,V}, Rec) -> Rec#execution_report{last_liquidity_ind = list_to_integer(binary_to_list(V))};
    ({cont_amt_type,V}, Rec) -> Rec#execution_report{cont_amt_type = list_to_integer(binary_to_list(V))};
    ({cont_amt_value,V}, Rec) -> Rec#execution_report{cont_amt_value = V};
    ({cont_amt_curr,V}, Rec) -> Rec#execution_report{cont_amt_curr = V};
    ({leg_qty,V}, Rec) -> Rec#execution_report{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#execution_report{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_position_effect,V}, Rec) -> Rec#execution_report{leg_position_effect = V};
    ({leg_covered_or_uncovered,V}, Rec) -> Rec#execution_report{leg_covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({leg_ref_id,V}, Rec) -> Rec#execution_report{leg_ref_id = V};
    ({leg_price,V}, Rec) -> Rec#execution_report{leg_price = list_to_float(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#execution_report{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#execution_report{leg_settl_date = V};
    ({leg_last_px,V}, Rec) -> Rec#execution_report{leg_last_px = list_to_float(binary_to_list(V))};
    ({copy_msg_indicator,V}, Rec) -> Rec#execution_report{copy_msg_indicator = V == <<"Y">>};
    ({misc_fee_amt,V}, Rec) -> Rec#execution_report{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#execution_report{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#execution_report{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#execution_report{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({K,V}, #execution_report{fields = F} = Rec) -> Rec#execution_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#execution_report{fields = lists:reverse(F)}.

decode_message_order_cancel_reject(Message, #order_cancel_reject{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_cancel_reject{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#order_cancel_reject{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#order_cancel_reject{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_cancel_reject{secondary_cl_ord_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#order_cancel_reject{cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#order_cancel_reject{cl_ord_link_id = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#order_cancel_reject{orig_cl_ord_id = V};
    ({ord_status,V}, Rec) -> Rec#order_cancel_reject{ord_status = V};
    ({working_indicator,V}, Rec) -> Rec#order_cancel_reject{working_indicator = V == <<"Y">>};
    ({orig_ord_mod_time,V}, Rec) -> Rec#order_cancel_reject{orig_ord_mod_time = V};
    ({list_id,V}, Rec) -> Rec#order_cancel_reject{list_id = V};
    ({account,V}, Rec) -> Rec#order_cancel_reject{account = V};
    ({acct_id_source,V}, Rec) -> Rec#order_cancel_reject{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#order_cancel_reject{account_type = list_to_integer(binary_to_list(V))};
    ({trade_origination_date,V}, Rec) -> Rec#order_cancel_reject{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#order_cancel_reject{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#order_cancel_reject{transact_time = V};
    ({cxl_rej_response_to,V}, Rec) -> Rec#order_cancel_reject{cxl_rej_response_to = V};
    ({cxl_rej_reason,V}, Rec) -> Rec#order_cancel_reject{cxl_rej_reason = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#order_cancel_reject{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#order_cancel_reject{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#order_cancel_reject{encoded_text = V};
    ({K,V}, #order_cancel_reject{fields = F} = Rec) -> Rec#order_cancel_reject{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_cancel_reject{fields = lists:reverse(F)}.

decode_message_logon(Message, #logon{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #logon{fields = F} = lists:foldl(fun
    ({encrypt_method,V}, Rec) -> Rec#logon{encrypt_method = list_to_integer(binary_to_list(V))};
    ({heart_bt_int,V}, Rec) -> Rec#logon{heart_bt_int = list_to_integer(binary_to_list(V))};
    ({raw_data_length,V}, Rec) -> Rec#logon{raw_data_length = list_to_integer(binary_to_list(V))};
    ({raw_data,V}, Rec) -> Rec#logon{raw_data = V};
    ({reset_seq_num_flag,V}, Rec) -> Rec#logon{reset_seq_num_flag = V == <<"Y">>};
    ({next_expected_msg_seq_num,V}, Rec) -> Rec#logon{next_expected_msg_seq_num = list_to_integer(binary_to_list(V))};
    ({max_message_size,V}, Rec) -> Rec#logon{max_message_size = list_to_integer(binary_to_list(V))};
    ({ref_msg_type,V}, Rec) -> Rec#logon{ref_msg_type = V};
    ({msg_direction,V}, Rec) -> Rec#logon{msg_direction = V};
    ({test_message_indicator,V}, Rec) -> Rec#logon{test_message_indicator = V == <<"Y">>};
    ({username,V}, Rec) -> Rec#logon{username = V};
    ({password,V}, Rec) -> Rec#logon{password = V};
    ({K,V}, #logon{fields = F} = Rec) -> Rec#logon{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#logon{fields = lists:reverse(F)}.

decode_message_news(Message, #news{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #news{fields = F} = lists:foldl(fun
    ({orig_time,V}, Rec) -> Rec#news{orig_time = V};
    ({urgency,V}, Rec) -> Rec#news{urgency = V};
    ({headline,V}, Rec) -> Rec#news{headline = V};
    ({encoded_headline_len,V}, Rec) -> Rec#news{encoded_headline_len = list_to_integer(binary_to_list(V))};
    ({encoded_headline,V}, Rec) -> Rec#news{encoded_headline = V};
    ({routing_type,V}, Rec) -> Rec#news{routing_type = list_to_integer(binary_to_list(V))};
    ({routing_id,V}, Rec) -> Rec#news{routing_id = V};
    ({text,V}, Rec) -> Rec#news{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#news{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#news{encoded_text = V};
    ({url_link,V}, Rec) -> Rec#news{url_link = V};
    ({raw_data_length,V}, Rec) -> Rec#news{raw_data_length = list_to_integer(binary_to_list(V))};
    ({raw_data,V}, Rec) -> Rec#news{raw_data = V};
    ({K,V}, #news{fields = F} = Rec) -> Rec#news{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#news{fields = lists:reverse(F)}.

decode_message_email(Message, #email{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #email{fields = F} = lists:foldl(fun
    ({email_thread_id,V}, Rec) -> Rec#email{email_thread_id = V};
    ({email_type,V}, Rec) -> Rec#email{email_type = V};
    ({orig_time,V}, Rec) -> Rec#email{orig_time = V};
    ({subject,V}, Rec) -> Rec#email{subject = V};
    ({encoded_subject_len,V}, Rec) -> Rec#email{encoded_subject_len = list_to_integer(binary_to_list(V))};
    ({encoded_subject,V}, Rec) -> Rec#email{encoded_subject = V};
    ({routing_type,V}, Rec) -> Rec#email{routing_type = list_to_integer(binary_to_list(V))};
    ({routing_id,V}, Rec) -> Rec#email{routing_id = V};
    ({order_id,V}, Rec) -> Rec#email{order_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#email{cl_ord_id = V};
    ({text,V}, Rec) -> Rec#email{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#email{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#email{encoded_text = V};
    ({raw_data_length,V}, Rec) -> Rec#email{raw_data_length = list_to_integer(binary_to_list(V))};
    ({raw_data,V}, Rec) -> Rec#email{raw_data = V};
    ({K,V}, #email{fields = F} = Rec) -> Rec#email{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#email{fields = lists:reverse(F)}.

decode_message_new_order_single(Message, #new_order_single{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #new_order_single{fields = F} = lists:foldl(fun
    ({cl_ord_id,V}, Rec) -> Rec#new_order_single{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#new_order_single{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#new_order_single{cl_ord_link_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#new_order_single{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#new_order_single{trade_date = V};
    ({account,V}, Rec) -> Rec#new_order_single{account = V};
    ({acct_id_source,V}, Rec) -> Rec#new_order_single{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#new_order_single{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#new_order_single{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#new_order_single{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#new_order_single{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#new_order_single{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#new_order_single{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#new_order_single{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#new_order_single{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#new_order_single{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#new_order_single{alloc_qty = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#new_order_single{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#new_order_single{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#new_order_single{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#new_order_single{clearing_fee_indicator = V};
    ({handl_inst,V}, Rec) -> Rec#new_order_single{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#new_order_single{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#new_order_single{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#new_order_single{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#new_order_single{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#new_order_single{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#new_order_single{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#new_order_single{process_code = V};
    ({prev_close_px,V}, Rec) -> Rec#new_order_single{prev_close_px = list_to_float(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#new_order_single{side = V};
    ({locate_reqd,V}, Rec) -> Rec#new_order_single{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#new_order_single{transact_time = V};
    ({qty_type,V}, Rec) -> Rec#new_order_single{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#new_order_single{ord_type = V};
    ({price_type,V}, Rec) -> Rec#new_order_single{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#new_order_single{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#new_order_single{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#new_order_single{currency = V};
    ({compliance_id,V}, Rec) -> Rec#new_order_single{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#new_order_single{solicited_flag = V == <<"Y">>};
    ({ioi_id,V}, Rec) -> Rec#new_order_single{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#new_order_single{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#new_order_single{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#new_order_single{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#new_order_single{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#new_order_single{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#new_order_single{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#new_order_single{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#new_order_single{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#new_order_single{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#new_order_single{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#new_order_single{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#new_order_single{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#new_order_single{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#new_order_single{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#new_order_single{encoded_text = V};
    ({settl_date2,V}, Rec) -> Rec#new_order_single{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#new_order_single{order_qty2 = list_to_integer(binary_to_list(V))};
    ({price2,V}, Rec) -> Rec#new_order_single{price2 = list_to_float(binary_to_list(V))};
    ({position_effect,V}, Rec) -> Rec#new_order_single{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#new_order_single{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#new_order_single{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#new_order_single{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#new_order_single{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#new_order_single{participation_rate = V};
    ({cancellation_rights,V}, Rec) -> Rec#new_order_single{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#new_order_single{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#new_order_single{regist_id = V};
    ({designation,V}, Rec) -> Rec#new_order_single{designation = V};
    ({K,V}, #new_order_single{fields = F} = Rec) -> Rec#new_order_single{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#new_order_single{fields = lists:reverse(F)}.

decode_message_new_order_list(Message, #new_order_list{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #new_order_list{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#new_order_list{list_id = V};
    ({bid_id,V}, Rec) -> Rec#new_order_list{bid_id = V};
    ({client_bid_id,V}, Rec) -> Rec#new_order_list{client_bid_id = V};
    ({prog_rpt_reqs,V}, Rec) -> Rec#new_order_list{prog_rpt_reqs = list_to_integer(binary_to_list(V))};
    ({bid_type,V}, Rec) -> Rec#new_order_list{bid_type = list_to_integer(binary_to_list(V))};
    ({prog_period_interval,V}, Rec) -> Rec#new_order_list{prog_period_interval = list_to_integer(binary_to_list(V))};
    ({cancellation_rights,V}, Rec) -> Rec#new_order_list{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#new_order_list{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#new_order_list{regist_id = V};
    ({list_exec_inst_type,V}, Rec) -> Rec#new_order_list{list_exec_inst_type = V};
    ({list_exec_inst,V}, Rec) -> Rec#new_order_list{list_exec_inst = V};
    ({encoded_list_exec_inst_len,V}, Rec) -> Rec#new_order_list{encoded_list_exec_inst_len = list_to_integer(binary_to_list(V))};
    ({encoded_list_exec_inst,V}, Rec) -> Rec#new_order_list{encoded_list_exec_inst = V};
    ({allowable_one_sidedness_pct,V}, Rec) -> Rec#new_order_list{allowable_one_sidedness_pct = V};
    ({allowable_one_sidedness_value,V}, Rec) -> Rec#new_order_list{allowable_one_sidedness_value = V};
    ({allowable_one_sidedness_curr,V}, Rec) -> Rec#new_order_list{allowable_one_sidedness_curr = V};
    ({tot_no_orders,V}, Rec) -> Rec#new_order_list{tot_no_orders = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#new_order_list{last_fragment = V == <<"Y">>};
    ({cl_ord_id,V}, Rec) -> Rec#new_order_list{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#new_order_list{secondary_cl_ord_id = V};
    ({list_seq_no,V}, Rec) -> Rec#new_order_list{list_seq_no = list_to_integer(binary_to_list(V))};
    ({cl_ord_link_id,V}, Rec) -> Rec#new_order_list{cl_ord_link_id = V};
    ({settl_inst_mode,V}, Rec) -> Rec#new_order_list{settl_inst_mode = V};
    ({trade_origination_date,V}, Rec) -> Rec#new_order_list{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#new_order_list{trade_date = V};
    ({account,V}, Rec) -> Rec#new_order_list{account = V};
    ({acct_id_source,V}, Rec) -> Rec#new_order_list{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#new_order_list{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#new_order_list{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#new_order_list{booking_unit = V};
    ({alloc_id,V}, Rec) -> Rec#new_order_list{alloc_id = V};
    ({prealloc_method,V}, Rec) -> Rec#new_order_list{prealloc_method = V};
    ({alloc_account,V}, Rec) -> Rec#new_order_list{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#new_order_list{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#new_order_list{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#new_order_list{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#new_order_list{alloc_qty = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#new_order_list{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#new_order_list{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#new_order_list{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#new_order_list{clearing_fee_indicator = V};
    ({handl_inst,V}, Rec) -> Rec#new_order_list{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#new_order_list{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#new_order_list{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#new_order_list{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#new_order_list{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#new_order_list{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#new_order_list{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#new_order_list{process_code = V};
    ({prev_close_px,V}, Rec) -> Rec#new_order_list{prev_close_px = list_to_float(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#new_order_list{side = V};
    ({side_value_ind,V}, Rec) -> Rec#new_order_list{side_value_ind = list_to_integer(binary_to_list(V))};
    ({locate_reqd,V}, Rec) -> Rec#new_order_list{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#new_order_list{transact_time = V};
    ({qty_type,V}, Rec) -> Rec#new_order_list{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#new_order_list{ord_type = V};
    ({price_type,V}, Rec) -> Rec#new_order_list{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#new_order_list{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#new_order_list{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#new_order_list{currency = V};
    ({compliance_id,V}, Rec) -> Rec#new_order_list{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#new_order_list{solicited_flag = V == <<"Y">>};
    ({ioi_id,V}, Rec) -> Rec#new_order_list{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#new_order_list{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#new_order_list{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#new_order_list{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#new_order_list{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#new_order_list{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#new_order_list{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#new_order_list{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#new_order_list{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#new_order_list{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#new_order_list{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#new_order_list{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#new_order_list{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#new_order_list{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#new_order_list{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#new_order_list{encoded_text = V};
    ({settl_date2,V}, Rec) -> Rec#new_order_list{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#new_order_list{order_qty2 = list_to_integer(binary_to_list(V))};
    ({price2,V}, Rec) -> Rec#new_order_list{price2 = list_to_float(binary_to_list(V))};
    ({position_effect,V}, Rec) -> Rec#new_order_list{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#new_order_list{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#new_order_list{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#new_order_list{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#new_order_list{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#new_order_list{participation_rate = V};
    ({designation,V}, Rec) -> Rec#new_order_list{designation = V};
    ({K,V}, #new_order_list{fields = F} = Rec) -> Rec#new_order_list{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#new_order_list{fields = lists:reverse(F)}.

decode_message_order_cancel_request(Message, #order_cancel_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_cancel_request{fields = F} = lists:foldl(fun
    ({orig_cl_ord_id,V}, Rec) -> Rec#order_cancel_request{orig_cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#order_cancel_request{order_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#order_cancel_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_cancel_request{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#order_cancel_request{cl_ord_link_id = V};
    ({list_id,V}, Rec) -> Rec#order_cancel_request{list_id = V};
    ({orig_ord_mod_time,V}, Rec) -> Rec#order_cancel_request{orig_ord_mod_time = V};
    ({account,V}, Rec) -> Rec#order_cancel_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#order_cancel_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#order_cancel_request{account_type = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#order_cancel_request{side = V};
    ({transact_time,V}, Rec) -> Rec#order_cancel_request{transact_time = V};
    ({compliance_id,V}, Rec) -> Rec#order_cancel_request{compliance_id = V};
    ({text,V}, Rec) -> Rec#order_cancel_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#order_cancel_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#order_cancel_request{encoded_text = V};
    ({K,V}, #order_cancel_request{fields = F} = Rec) -> Rec#order_cancel_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_cancel_request{fields = lists:reverse(F)}.

decode_message_order_cancel_replace_request(Message, #order_cancel_replace_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_cancel_replace_request{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#order_cancel_replace_request{order_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#order_cancel_replace_request{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#order_cancel_replace_request{trade_date = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#order_cancel_replace_request{orig_cl_ord_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#order_cancel_replace_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_cancel_replace_request{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#order_cancel_replace_request{cl_ord_link_id = V};
    ({list_id,V}, Rec) -> Rec#order_cancel_replace_request{list_id = V};
    ({orig_ord_mod_time,V}, Rec) -> Rec#order_cancel_replace_request{orig_ord_mod_time = V};
    ({account,V}, Rec) -> Rec#order_cancel_replace_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#order_cancel_replace_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#order_cancel_replace_request{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#order_cancel_replace_request{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#order_cancel_replace_request{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#order_cancel_replace_request{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#order_cancel_replace_request{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#order_cancel_replace_request{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#order_cancel_replace_request{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#order_cancel_replace_request{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#order_cancel_replace_request{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#order_cancel_replace_request{alloc_qty = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#order_cancel_replace_request{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#order_cancel_replace_request{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#order_cancel_replace_request{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#order_cancel_replace_request{clearing_fee_indicator = V};
    ({handl_inst,V}, Rec) -> Rec#order_cancel_replace_request{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#order_cancel_replace_request{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#order_cancel_replace_request{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#order_cancel_replace_request{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#order_cancel_replace_request{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#order_cancel_replace_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#order_cancel_replace_request{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#order_cancel_replace_request{side = V};
    ({transact_time,V}, Rec) -> Rec#order_cancel_replace_request{transact_time = V};
    ({qty_type,V}, Rec) -> Rec#order_cancel_replace_request{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#order_cancel_replace_request{ord_type = V};
    ({price_type,V}, Rec) -> Rec#order_cancel_replace_request{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#order_cancel_replace_request{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#order_cancel_replace_request{stop_px = list_to_float(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#order_cancel_replace_request{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#order_cancel_replace_request{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#order_cancel_replace_request{participation_rate = V};
    ({compliance_id,V}, Rec) -> Rec#order_cancel_replace_request{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#order_cancel_replace_request{solicited_flag = V == <<"Y">>};
    ({currency,V}, Rec) -> Rec#order_cancel_replace_request{currency = V};
    ({time_in_force,V}, Rec) -> Rec#order_cancel_replace_request{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#order_cancel_replace_request{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#order_cancel_replace_request{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#order_cancel_replace_request{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#order_cancel_replace_request{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#order_cancel_replace_request{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#order_cancel_replace_request{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#order_cancel_replace_request{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#order_cancel_replace_request{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#order_cancel_replace_request{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#order_cancel_replace_request{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#order_cancel_replace_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#order_cancel_replace_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#order_cancel_replace_request{encoded_text = V};
    ({settl_date2,V}, Rec) -> Rec#order_cancel_replace_request{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#order_cancel_replace_request{order_qty2 = list_to_integer(binary_to_list(V))};
    ({price2,V}, Rec) -> Rec#order_cancel_replace_request{price2 = list_to_float(binary_to_list(V))};
    ({position_effect,V}, Rec) -> Rec#order_cancel_replace_request{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#order_cancel_replace_request{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#order_cancel_replace_request{max_show = list_to_integer(binary_to_list(V))};
    ({locate_reqd,V}, Rec) -> Rec#order_cancel_replace_request{locate_reqd = V == <<"Y">>};
    ({cancellation_rights,V}, Rec) -> Rec#order_cancel_replace_request{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#order_cancel_replace_request{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#order_cancel_replace_request{regist_id = V};
    ({designation,V}, Rec) -> Rec#order_cancel_replace_request{designation = V};
    ({K,V}, #order_cancel_replace_request{fields = F} = Rec) -> Rec#order_cancel_replace_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_cancel_replace_request{fields = lists:reverse(F)}.

decode_message_order_status_request(Message, #order_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_status_request{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#order_status_request{order_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#order_status_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_status_request{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#order_status_request{cl_ord_link_id = V};
    ({ord_status_req_id,V}, Rec) -> Rec#order_status_request{ord_status_req_id = V};
    ({account,V}, Rec) -> Rec#order_status_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#order_status_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#order_status_request{side = V};
    ({K,V}, #order_status_request{fields = F} = Rec) -> Rec#order_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_status_request{fields = lists:reverse(F)}.

decode_message_allocation_instruction(Message, #allocation_instruction{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #allocation_instruction{fields = F} = lists:foldl(fun
    ({alloc_id,V}, Rec) -> Rec#allocation_instruction{alloc_id = V};
    ({alloc_trans_type,V}, Rec) -> Rec#allocation_instruction{alloc_trans_type = V};
    ({alloc_type,V}, Rec) -> Rec#allocation_instruction{alloc_type = list_to_integer(binary_to_list(V))};
    ({secondary_alloc_id,V}, Rec) -> Rec#allocation_instruction{secondary_alloc_id = V};
    ({ref_alloc_id,V}, Rec) -> Rec#allocation_instruction{ref_alloc_id = V};
    ({alloc_canc_replace_reason,V}, Rec) -> Rec#allocation_instruction{alloc_canc_replace_reason = list_to_integer(binary_to_list(V))};
    ({alloc_intermed_req_type,V}, Rec) -> Rec#allocation_instruction{alloc_intermed_req_type = list_to_integer(binary_to_list(V))};
    ({alloc_link_id,V}, Rec) -> Rec#allocation_instruction{alloc_link_id = V};
    ({alloc_link_type,V}, Rec) -> Rec#allocation_instruction{alloc_link_type = list_to_integer(binary_to_list(V))};
    ({booking_ref_id,V}, Rec) -> Rec#allocation_instruction{booking_ref_id = V};
    ({alloc_no_orders_type,V}, Rec) -> Rec#allocation_instruction{alloc_no_orders_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#allocation_instruction{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#allocation_instruction{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#allocation_instruction{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#allocation_instruction{secondary_cl_ord_id = V};
    ({list_id,V}, Rec) -> Rec#allocation_instruction{list_id = V};
    ({order_qty,V}, Rec) -> Rec#allocation_instruction{order_qty = list_to_integer(binary_to_list(V))};
    ({order_avg_px,V}, Rec) -> Rec#allocation_instruction{order_avg_px = list_to_float(binary_to_list(V))};
    ({order_booking_qty,V}, Rec) -> Rec#allocation_instruction{order_booking_qty = list_to_integer(binary_to_list(V))};
    ({last_qty,V}, Rec) -> Rec#allocation_instruction{last_qty = list_to_integer(binary_to_list(V))};
    ({exec_id,V}, Rec) -> Rec#allocation_instruction{exec_id = V};
    ({secondary_exec_id,V}, Rec) -> Rec#allocation_instruction{secondary_exec_id = V};
    ({last_px,V}, Rec) -> Rec#allocation_instruction{last_px = list_to_float(binary_to_list(V))};
    ({last_par_px,V}, Rec) -> Rec#allocation_instruction{last_par_px = list_to_float(binary_to_list(V))};
    ({last_capacity,V}, Rec) -> Rec#allocation_instruction{last_capacity = V};
    ({previously_reported,V}, Rec) -> Rec#allocation_instruction{previously_reported = V == <<"Y">>};
    ({reversal_indicator,V}, Rec) -> Rec#allocation_instruction{reversal_indicator = V == <<"Y">>};
    ({match_type,V}, Rec) -> Rec#allocation_instruction{match_type = V};
    ({side,V}, Rec) -> Rec#allocation_instruction{side = V};
    ({quantity,V}, Rec) -> Rec#allocation_instruction{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#allocation_instruction{qty_type = list_to_integer(binary_to_list(V))};
    ({last_mkt,V}, Rec) -> Rec#allocation_instruction{last_mkt = V};
    ({trade_origination_date,V}, Rec) -> Rec#allocation_instruction{trade_origination_date = V};
    ({trading_session_id,V}, Rec) -> Rec#allocation_instruction{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#allocation_instruction{trading_session_sub_id = V};
    ({price_type,V}, Rec) -> Rec#allocation_instruction{price_type = list_to_integer(binary_to_list(V))};
    ({avg_px,V}, Rec) -> Rec#allocation_instruction{avg_px = list_to_float(binary_to_list(V))};
    ({avg_par_px,V}, Rec) -> Rec#allocation_instruction{avg_par_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#allocation_instruction{currency = V};
    ({avg_px_precision,V}, Rec) -> Rec#allocation_instruction{avg_px_precision = list_to_integer(binary_to_list(V))};
    ({trade_date,V}, Rec) -> Rec#allocation_instruction{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#allocation_instruction{transact_time = V};
    ({settl_type,V}, Rec) -> Rec#allocation_instruction{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#allocation_instruction{settl_date = V};
    ({booking_type,V}, Rec) -> Rec#allocation_instruction{booking_type = list_to_integer(binary_to_list(V))};
    ({gross_trade_amt,V}, Rec) -> Rec#allocation_instruction{gross_trade_amt = V};
    ({concession,V}, Rec) -> Rec#allocation_instruction{concession = V};
    ({total_takedown,V}, Rec) -> Rec#allocation_instruction{total_takedown = V};
    ({net_money,V}, Rec) -> Rec#allocation_instruction{net_money = V};
    ({position_effect,V}, Rec) -> Rec#allocation_instruction{position_effect = V};
    ({auto_accept_indicator,V}, Rec) -> Rec#allocation_instruction{auto_accept_indicator = V == <<"Y">>};
    ({text,V}, Rec) -> Rec#allocation_instruction{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#allocation_instruction{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#allocation_instruction{encoded_text = V};
    ({num_days_interest,V}, Rec) -> Rec#allocation_instruction{num_days_interest = list_to_integer(binary_to_list(V))};
    ({accrued_interest_rate,V}, Rec) -> Rec#allocation_instruction{accrued_interest_rate = V};
    ({accrued_interest_amt,V}, Rec) -> Rec#allocation_instruction{accrued_interest_amt = V};
    ({total_accrued_interest_amt,V}, Rec) -> Rec#allocation_instruction{total_accrued_interest_amt = V};
    ({interest_at_maturity,V}, Rec) -> Rec#allocation_instruction{interest_at_maturity = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#allocation_instruction{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#allocation_instruction{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#allocation_instruction{end_cash = V};
    ({legal_confirm,V}, Rec) -> Rec#allocation_instruction{legal_confirm = V == <<"Y">>};
    ({tot_no_allocs,V}, Rec) -> Rec#allocation_instruction{tot_no_allocs = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#allocation_instruction{last_fragment = V == <<"Y">>};
    ({alloc_account,V}, Rec) -> Rec#allocation_instruction{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#allocation_instruction{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#allocation_instruction{match_status = V};
    ({alloc_price,V}, Rec) -> Rec#allocation_instruction{alloc_price = list_to_float(binary_to_list(V))};
    ({alloc_qty,V}, Rec) -> Rec#allocation_instruction{alloc_qty = list_to_integer(binary_to_list(V))};
    ({individual_alloc_id,V}, Rec) -> Rec#allocation_instruction{individual_alloc_id = V};
    ({process_code,V}, Rec) -> Rec#allocation_instruction{process_code = V};
    ({notify_broker_of_credit,V}, Rec) -> Rec#allocation_instruction{notify_broker_of_credit = V == <<"Y">>};
    ({alloc_handl_inst,V}, Rec) -> Rec#allocation_instruction{alloc_handl_inst = list_to_integer(binary_to_list(V))};
    ({alloc_text,V}, Rec) -> Rec#allocation_instruction{alloc_text = V};
    ({encoded_alloc_text_len,V}, Rec) -> Rec#allocation_instruction{encoded_alloc_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_alloc_text,V}, Rec) -> Rec#allocation_instruction{encoded_alloc_text = V};
    ({alloc_avg_px,V}, Rec) -> Rec#allocation_instruction{alloc_avg_px = list_to_float(binary_to_list(V))};
    ({alloc_net_money,V}, Rec) -> Rec#allocation_instruction{alloc_net_money = V};
    ({settl_curr_amt,V}, Rec) -> Rec#allocation_instruction{settl_curr_amt = V};
    ({alloc_settl_curr_amt,V}, Rec) -> Rec#allocation_instruction{alloc_settl_curr_amt = V};
    ({settl_currency,V}, Rec) -> Rec#allocation_instruction{settl_currency = V};
    ({alloc_settl_currency,V}, Rec) -> Rec#allocation_instruction{alloc_settl_currency = V};
    ({settl_curr_fx_rate,V}, Rec) -> Rec#allocation_instruction{settl_curr_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#allocation_instruction{settl_curr_fx_rate_calc = V};
    ({alloc_accrued_interest_amt,V}, Rec) -> Rec#allocation_instruction{alloc_accrued_interest_amt = V};
    ({alloc_interest_at_maturity,V}, Rec) -> Rec#allocation_instruction{alloc_interest_at_maturity = V};
    ({misc_fee_amt,V}, Rec) -> Rec#allocation_instruction{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#allocation_instruction{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#allocation_instruction{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#allocation_instruction{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({clearing_instruction,V}, Rec) -> Rec#allocation_instruction{clearing_instruction = list_to_integer(binary_to_list(V))};
    ({clearing_fee_indicator,V}, Rec) -> Rec#allocation_instruction{clearing_fee_indicator = V};
    ({alloc_settl_inst_type,V}, Rec) -> Rec#allocation_instruction{alloc_settl_inst_type = list_to_integer(binary_to_list(V))};
    ({K,V}, #allocation_instruction{fields = F} = Rec) -> Rec#allocation_instruction{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#allocation_instruction{fields = lists:reverse(F)}.

decode_message_list_cancel_request(Message, #list_cancel_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #list_cancel_request{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#list_cancel_request{list_id = V};
    ({transact_time,V}, Rec) -> Rec#list_cancel_request{transact_time = V};
    ({trade_origination_date,V}, Rec) -> Rec#list_cancel_request{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#list_cancel_request{trade_date = V};
    ({text,V}, Rec) -> Rec#list_cancel_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#list_cancel_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#list_cancel_request{encoded_text = V};
    ({K,V}, #list_cancel_request{fields = F} = Rec) -> Rec#list_cancel_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#list_cancel_request{fields = lists:reverse(F)}.

decode_message_list_execute(Message, #list_execute{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #list_execute{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#list_execute{list_id = V};
    ({client_bid_id,V}, Rec) -> Rec#list_execute{client_bid_id = V};
    ({bid_id,V}, Rec) -> Rec#list_execute{bid_id = V};
    ({transact_time,V}, Rec) -> Rec#list_execute{transact_time = V};
    ({text,V}, Rec) -> Rec#list_execute{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#list_execute{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#list_execute{encoded_text = V};
    ({K,V}, #list_execute{fields = F} = Rec) -> Rec#list_execute{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#list_execute{fields = lists:reverse(F)}.

decode_message_list_status_request(Message, #list_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #list_status_request{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#list_status_request{list_id = V};
    ({text,V}, Rec) -> Rec#list_status_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#list_status_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#list_status_request{encoded_text = V};
    ({K,V}, #list_status_request{fields = F} = Rec) -> Rec#list_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#list_status_request{fields = lists:reverse(F)}.

decode_message_list_status(Message, #list_status{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #list_status{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#list_status{list_id = V};
    ({list_status_type,V}, Rec) -> Rec#list_status{list_status_type = list_to_integer(binary_to_list(V))};
    ({no_rpts,V}, Rec) -> Rec#list_status{no_rpts = list_to_integer(binary_to_list(V))};
    ({list_order_status,V}, Rec) -> Rec#list_status{list_order_status = list_to_integer(binary_to_list(V))};
    ({rpt_seq,V}, Rec) -> Rec#list_status{rpt_seq = list_to_integer(binary_to_list(V))};
    ({list_status_text,V}, Rec) -> Rec#list_status{list_status_text = V};
    ({encoded_list_status_text_len,V}, Rec) -> Rec#list_status{encoded_list_status_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_list_status_text,V}, Rec) -> Rec#list_status{encoded_list_status_text = V};
    ({transact_time,V}, Rec) -> Rec#list_status{transact_time = V};
    ({tot_no_orders,V}, Rec) -> Rec#list_status{tot_no_orders = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#list_status{last_fragment = V == <<"Y">>};
    ({cl_ord_id,V}, Rec) -> Rec#list_status{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#list_status{secondary_cl_ord_id = V};
    ({cum_qty,V}, Rec) -> Rec#list_status{cum_qty = list_to_integer(binary_to_list(V))};
    ({ord_status,V}, Rec) -> Rec#list_status{ord_status = V};
    ({working_indicator,V}, Rec) -> Rec#list_status{working_indicator = V == <<"Y">>};
    ({leaves_qty,V}, Rec) -> Rec#list_status{leaves_qty = list_to_integer(binary_to_list(V))};
    ({cxl_qty,V}, Rec) -> Rec#list_status{cxl_qty = list_to_integer(binary_to_list(V))};
    ({avg_px,V}, Rec) -> Rec#list_status{avg_px = list_to_float(binary_to_list(V))};
    ({ord_rej_reason,V}, Rec) -> Rec#list_status{ord_rej_reason = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#list_status{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#list_status{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#list_status{encoded_text = V};
    ({K,V}, #list_status{fields = F} = Rec) -> Rec#list_status{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#list_status{fields = lists:reverse(F)}.

decode_message_allocation_instruction_ack(Message, #allocation_instruction_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #allocation_instruction_ack{fields = F} = lists:foldl(fun
    ({alloc_id,V}, Rec) -> Rec#allocation_instruction_ack{alloc_id = V};
    ({secondary_alloc_id,V}, Rec) -> Rec#allocation_instruction_ack{secondary_alloc_id = V};
    ({trade_date,V}, Rec) -> Rec#allocation_instruction_ack{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#allocation_instruction_ack{transact_time = V};
    ({alloc_status,V}, Rec) -> Rec#allocation_instruction_ack{alloc_status = list_to_integer(binary_to_list(V))};
    ({alloc_rej_code,V}, Rec) -> Rec#allocation_instruction_ack{alloc_rej_code = list_to_integer(binary_to_list(V))};
    ({alloc_type,V}, Rec) -> Rec#allocation_instruction_ack{alloc_type = list_to_integer(binary_to_list(V))};
    ({alloc_intermed_req_type,V}, Rec) -> Rec#allocation_instruction_ack{alloc_intermed_req_type = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#allocation_instruction_ack{match_status = V};
    ({product,V}, Rec) -> Rec#allocation_instruction_ack{product = list_to_integer(binary_to_list(V))};
    ({security_type,V}, Rec) -> Rec#allocation_instruction_ack{security_type = V};
    ({text,V}, Rec) -> Rec#allocation_instruction_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#allocation_instruction_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#allocation_instruction_ack{encoded_text = V};
    ({alloc_account,V}, Rec) -> Rec#allocation_instruction_ack{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#allocation_instruction_ack{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_price,V}, Rec) -> Rec#allocation_instruction_ack{alloc_price = list_to_float(binary_to_list(V))};
    ({individual_alloc_id,V}, Rec) -> Rec#allocation_instruction_ack{individual_alloc_id = V};
    ({individual_alloc_rej_code,V}, Rec) -> Rec#allocation_instruction_ack{individual_alloc_rej_code = list_to_integer(binary_to_list(V))};
    ({alloc_text,V}, Rec) -> Rec#allocation_instruction_ack{alloc_text = V};
    ({encoded_alloc_text_len,V}, Rec) -> Rec#allocation_instruction_ack{encoded_alloc_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_alloc_text,V}, Rec) -> Rec#allocation_instruction_ack{encoded_alloc_text = V};
    ({K,V}, #allocation_instruction_ack{fields = F} = Rec) -> Rec#allocation_instruction_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#allocation_instruction_ack{fields = lists:reverse(F)}.

decode_message_dont_know_trade(Message, #dont_know_trade{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #dont_know_trade{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#dont_know_trade{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#dont_know_trade{secondary_order_id = V};
    ({exec_id,V}, Rec) -> Rec#dont_know_trade{exec_id = V};
    ({dk_reason,V}, Rec) -> Rec#dont_know_trade{dk_reason = V};
    ({side,V}, Rec) -> Rec#dont_know_trade{side = V};
    ({last_qty,V}, Rec) -> Rec#dont_know_trade{last_qty = list_to_integer(binary_to_list(V))};
    ({last_px,V}, Rec) -> Rec#dont_know_trade{last_px = list_to_float(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#dont_know_trade{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#dont_know_trade{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#dont_know_trade{encoded_text = V};
    ({K,V}, #dont_know_trade{fields = F} = Rec) -> Rec#dont_know_trade{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#dont_know_trade{fields = lists:reverse(F)}.

decode_message_quote_request(Message, #quote_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_request{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#quote_request{quote_req_id = V};
    ({rfq_req_id,V}, Rec) -> Rec#quote_request{rfq_req_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#quote_request{cl_ord_id = V};
    ({order_capacity,V}, Rec) -> Rec#quote_request{order_capacity = V};
    ({prev_close_px,V}, Rec) -> Rec#quote_request{prev_close_px = list_to_float(binary_to_list(V))};
    ({quote_request_type,V}, Rec) -> Rec#quote_request{quote_request_type = list_to_integer(binary_to_list(V))};
    ({quote_type,V}, Rec) -> Rec#quote_request{quote_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_request{trading_session_sub_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#quote_request{trade_origination_date = V};
    ({side,V}, Rec) -> Rec#quote_request{side = V};
    ({qty_type,V}, Rec) -> Rec#quote_request{qty_type = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#quote_request{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#quote_request{settl_date = V};
    ({settl_date2,V}, Rec) -> Rec#quote_request{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#quote_request{order_qty2 = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#quote_request{currency = V};
    ({account,V}, Rec) -> Rec#quote_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_request{account_type = list_to_integer(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#quote_request{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#quote_request{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#quote_request{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#quote_request{leg_settl_date = V};
    ({quote_qualifier,V}, Rec) -> Rec#quote_request{quote_qualifier = V};
    ({quote_price_type,V}, Rec) -> Rec#quote_request{quote_price_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#quote_request{ord_type = V};
    ({valid_until_time,V}, Rec) -> Rec#quote_request{valid_until_time = V};
    ({expire_time,V}, Rec) -> Rec#quote_request{expire_time = V};
    ({transact_time,V}, Rec) -> Rec#quote_request{transact_time = V};
    ({price_type,V}, Rec) -> Rec#quote_request{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#quote_request{price = list_to_float(binary_to_list(V))};
    ({price2,V}, Rec) -> Rec#quote_request{price2 = list_to_float(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#quote_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#quote_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#quote_request{encoded_text = V};
    ({K,V}, #quote_request{fields = F} = Rec) -> Rec#quote_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_request{fields = lists:reverse(F)}.

decode_message_quote(Message, #quote{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#quote{quote_req_id = V};
    ({quote_id,V}, Rec) -> Rec#quote{quote_id = V};
    ({quote_resp_id,V}, Rec) -> Rec#quote{quote_resp_id = V};
    ({quote_type,V}, Rec) -> Rec#quote{quote_type = list_to_integer(binary_to_list(V))};
    ({quote_qualifier,V}, Rec) -> Rec#quote{quote_qualifier = V};
    ({quote_response_level,V}, Rec) -> Rec#quote{quote_response_level = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#quote{side = V};
    ({settl_type,V}, Rec) -> Rec#quote{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#quote{settl_date = V};
    ({settl_date2,V}, Rec) -> Rec#quote{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#quote{order_qty2 = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#quote{currency = V};
    ({account,V}, Rec) -> Rec#quote{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote{account_type = list_to_integer(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#quote{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#quote{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#quote{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#quote{leg_settl_date = V};
    ({leg_price_type,V}, Rec) -> Rec#quote{leg_price_type = list_to_integer(binary_to_list(V))};
    ({leg_bid_px,V}, Rec) -> Rec#quote{leg_bid_px = list_to_float(binary_to_list(V))};
    ({leg_offer_px,V}, Rec) -> Rec#quote{leg_offer_px = list_to_float(binary_to_list(V))};
    ({bid_px,V}, Rec) -> Rec#quote{bid_px = list_to_float(binary_to_list(V))};
    ({offer_px,V}, Rec) -> Rec#quote{offer_px = list_to_float(binary_to_list(V))};
    ({mkt_bid_px,V}, Rec) -> Rec#quote{mkt_bid_px = list_to_float(binary_to_list(V))};
    ({mkt_offer_px,V}, Rec) -> Rec#quote{mkt_offer_px = list_to_float(binary_to_list(V))};
    ({min_bid_size,V}, Rec) -> Rec#quote{min_bid_size = list_to_integer(binary_to_list(V))};
    ({bid_size,V}, Rec) -> Rec#quote{bid_size = list_to_integer(binary_to_list(V))};
    ({min_offer_size,V}, Rec) -> Rec#quote{min_offer_size = list_to_integer(binary_to_list(V))};
    ({offer_size,V}, Rec) -> Rec#quote{offer_size = list_to_integer(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#quote{valid_until_time = V};
    ({bid_spot_rate,V}, Rec) -> Rec#quote{bid_spot_rate = list_to_float(binary_to_list(V))};
    ({offer_spot_rate,V}, Rec) -> Rec#quote{offer_spot_rate = list_to_float(binary_to_list(V))};
    ({bid_forward_points,V}, Rec) -> Rec#quote{bid_forward_points = V};
    ({offer_forward_points,V}, Rec) -> Rec#quote{offer_forward_points = V};
    ({mid_px,V}, Rec) -> Rec#quote{mid_px = list_to_float(binary_to_list(V))};
    ({bid_yield,V}, Rec) -> Rec#quote{bid_yield = V};
    ({mid_yield,V}, Rec) -> Rec#quote{mid_yield = V};
    ({offer_yield,V}, Rec) -> Rec#quote{offer_yield = V};
    ({transact_time,V}, Rec) -> Rec#quote{transact_time = V};
    ({ord_type,V}, Rec) -> Rec#quote{ord_type = V};
    ({bid_forward_points2,V}, Rec) -> Rec#quote{bid_forward_points2 = V};
    ({offer_forward_points2,V}, Rec) -> Rec#quote{offer_forward_points2 = V};
    ({settl_curr_bid_fx_rate,V}, Rec) -> Rec#quote{settl_curr_bid_fx_rate = V};
    ({settl_curr_offer_fx_rate,V}, Rec) -> Rec#quote{settl_curr_offer_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#quote{settl_curr_fx_rate_calc = V};
    ({comm_type,V}, Rec) -> Rec#quote{comm_type = V};
    ({commission,V}, Rec) -> Rec#quote{commission = V};
    ({cust_order_capacity,V}, Rec) -> Rec#quote{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#quote{ex_destination = V};
    ({order_capacity,V}, Rec) -> Rec#quote{order_capacity = V};
    ({price_type,V}, Rec) -> Rec#quote{price_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#quote{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#quote{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#quote{encoded_text = V};
    ({K,V}, #quote{fields = F} = Rec) -> Rec#quote{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote{fields = lists:reverse(F)}.

decode_message_settlement_instructions(Message, #settlement_instructions{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #settlement_instructions{fields = F} = lists:foldl(fun
    ({settl_inst_msg_id,V}, Rec) -> Rec#settlement_instructions{settl_inst_msg_id = V};
    ({settl_inst_req_id,V}, Rec) -> Rec#settlement_instructions{settl_inst_req_id = V};
    ({settl_inst_mode,V}, Rec) -> Rec#settlement_instructions{settl_inst_mode = V};
    ({settl_inst_req_rej_code,V}, Rec) -> Rec#settlement_instructions{settl_inst_req_rej_code = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#settlement_instructions{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#settlement_instructions{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#settlement_instructions{encoded_text = V};
    ({cl_ord_id,V}, Rec) -> Rec#settlement_instructions{cl_ord_id = V};
    ({transact_time,V}, Rec) -> Rec#settlement_instructions{transact_time = V};
    ({settl_inst_id,V}, Rec) -> Rec#settlement_instructions{settl_inst_id = V};
    ({settl_inst_trans_type,V}, Rec) -> Rec#settlement_instructions{settl_inst_trans_type = V};
    ({settl_inst_ref_id,V}, Rec) -> Rec#settlement_instructions{settl_inst_ref_id = V};
    ({side,V}, Rec) -> Rec#settlement_instructions{side = V};
    ({product,V}, Rec) -> Rec#settlement_instructions{product = list_to_integer(binary_to_list(V))};
    ({security_type,V}, Rec) -> Rec#settlement_instructions{security_type = V};
    ({cfi_code,V}, Rec) -> Rec#settlement_instructions{cfi_code = V};
    ({effective_time,V}, Rec) -> Rec#settlement_instructions{effective_time = V};
    ({expire_time,V}, Rec) -> Rec#settlement_instructions{expire_time = V};
    ({last_update_time,V}, Rec) -> Rec#settlement_instructions{last_update_time = V};
    ({payment_method,V}, Rec) -> Rec#settlement_instructions{payment_method = list_to_integer(binary_to_list(V))};
    ({payment_ref,V}, Rec) -> Rec#settlement_instructions{payment_ref = V};
    ({card_holder_name,V}, Rec) -> Rec#settlement_instructions{card_holder_name = V};
    ({card_number,V}, Rec) -> Rec#settlement_instructions{card_number = V};
    ({card_start_date,V}, Rec) -> Rec#settlement_instructions{card_start_date = V};
    ({card_exp_date,V}, Rec) -> Rec#settlement_instructions{card_exp_date = V};
    ({card_iss_num,V}, Rec) -> Rec#settlement_instructions{card_iss_num = V};
    ({payment_date,V}, Rec) -> Rec#settlement_instructions{payment_date = V};
    ({payment_remitter_id,V}, Rec) -> Rec#settlement_instructions{payment_remitter_id = V};
    ({K,V}, #settlement_instructions{fields = F} = Rec) -> Rec#settlement_instructions{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#settlement_instructions{fields = lists:reverse(F)}.

decode_message_market_data_request(Message, #market_data_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #market_data_request{fields = F} = lists:foldl(fun
    ({md_req_id,V}, Rec) -> Rec#market_data_request{md_req_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#market_data_request{subscription_request_type = V};
    ({market_depth,V}, Rec) -> Rec#market_data_request{market_depth = list_to_integer(binary_to_list(V))};
    ({md_update_type,V}, Rec) -> Rec#market_data_request{md_update_type = list_to_integer(binary_to_list(V))};
    ({aggregated_book,V}, Rec) -> Rec#market_data_request{aggregated_book = V == <<"Y">>};
    ({open_close_settl_flag,V}, Rec) -> Rec#market_data_request{open_close_settl_flag = V};
    ({scope,V}, Rec) -> Rec#market_data_request{scope = V};
    ({md_implicit_delete,V}, Rec) -> Rec#market_data_request{md_implicit_delete = V == <<"Y">>};
    ({md_entry_type,V}, Rec) -> Rec#market_data_request{md_entry_type = V};
    ({trading_session_id,V}, Rec) -> Rec#market_data_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#market_data_request{trading_session_sub_id = V};
    ({appl_queue_action,V}, Rec) -> Rec#market_data_request{appl_queue_action = list_to_integer(binary_to_list(V))};
    ({appl_queue_max,V}, Rec) -> Rec#market_data_request{appl_queue_max = list_to_integer(binary_to_list(V))};
    ({K,V}, #market_data_request{fields = F} = Rec) -> Rec#market_data_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#market_data_request{fields = lists:reverse(F)}.

decode_message_market_data_snapshot_full_refresh(Message, #market_data_snapshot_full_refresh{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #market_data_snapshot_full_refresh{fields = F} = lists:foldl(fun
    ({md_req_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_req_id = V};
    ({financial_status,V}, Rec) -> Rec#market_data_snapshot_full_refresh{financial_status = V};
    ({corporate_action,V}, Rec) -> Rec#market_data_snapshot_full_refresh{corporate_action = V};
    ({net_chg_prev_day,V}, Rec) -> Rec#market_data_snapshot_full_refresh{net_chg_prev_day = V};
    ({md_entry_type,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_type = V};
    ({md_entry_px,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#market_data_snapshot_full_refresh{currency = V};
    ({md_entry_size,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_size = list_to_integer(binary_to_list(V))};
    ({md_entry_date,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_date = V};
    ({md_entry_time,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_time = V};
    ({tick_direction,V}, Rec) -> Rec#market_data_snapshot_full_refresh{tick_direction = V};
    ({md_mkt,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_mkt = V};
    ({trading_session_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{trading_session_sub_id = V};
    ({quote_condition,V}, Rec) -> Rec#market_data_snapshot_full_refresh{quote_condition = V};
    ({trade_condition,V}, Rec) -> Rec#market_data_snapshot_full_refresh{trade_condition = V};
    ({md_entry_originator,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_originator = V};
    ({location_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{location_id = V};
    ({desk_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{desk_id = V};
    ({open_close_settl_flag,V}, Rec) -> Rec#market_data_snapshot_full_refresh{open_close_settl_flag = V};
    ({time_in_force,V}, Rec) -> Rec#market_data_snapshot_full_refresh{time_in_force = V};
    ({expire_date,V}, Rec) -> Rec#market_data_snapshot_full_refresh{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#market_data_snapshot_full_refresh{expire_time = V};
    ({min_qty,V}, Rec) -> Rec#market_data_snapshot_full_refresh{min_qty = list_to_integer(binary_to_list(V))};
    ({exec_inst,V}, Rec) -> Rec#market_data_snapshot_full_refresh{exec_inst = V};
    ({seller_days,V}, Rec) -> Rec#market_data_snapshot_full_refresh{seller_days = list_to_integer(binary_to_list(V))};
    ({order_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{order_id = V};
    ({quote_entry_id,V}, Rec) -> Rec#market_data_snapshot_full_refresh{quote_entry_id = V};
    ({md_entry_buyer,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_buyer = V};
    ({md_entry_seller,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_seller = V};
    ({number_of_orders,V}, Rec) -> Rec#market_data_snapshot_full_refresh{number_of_orders = list_to_integer(binary_to_list(V))};
    ({md_entry_position_no,V}, Rec) -> Rec#market_data_snapshot_full_refresh{md_entry_position_no = list_to_integer(binary_to_list(V))};
    ({scope,V}, Rec) -> Rec#market_data_snapshot_full_refresh{scope = V};
    ({price_delta,V}, Rec) -> Rec#market_data_snapshot_full_refresh{price_delta = V};
    ({text,V}, Rec) -> Rec#market_data_snapshot_full_refresh{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#market_data_snapshot_full_refresh{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#market_data_snapshot_full_refresh{encoded_text = V};
    ({appl_queue_depth,V}, Rec) -> Rec#market_data_snapshot_full_refresh{appl_queue_depth = list_to_integer(binary_to_list(V))};
    ({appl_queue_resolution,V}, Rec) -> Rec#market_data_snapshot_full_refresh{appl_queue_resolution = list_to_integer(binary_to_list(V))};
    ({K,V}, #market_data_snapshot_full_refresh{fields = F} = Rec) -> Rec#market_data_snapshot_full_refresh{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#market_data_snapshot_full_refresh{fields = lists:reverse(F)}.

decode_message_market_data_incremental_refresh(Message, #market_data_incremental_refresh{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #market_data_incremental_refresh{fields = F} = lists:foldl(fun
    ({md_req_id,V}, Rec) -> Rec#market_data_incremental_refresh{md_req_id = V};
    ({md_update_action,V}, Rec) -> Rec#market_data_incremental_refresh{md_update_action = V};
    ({delete_reason,V}, Rec) -> Rec#market_data_incremental_refresh{delete_reason = V};
    ({md_entry_type,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_type = V};
    ({md_entry_id,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_id = V};
    ({md_entry_ref_id,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_ref_id = V};
    ({financial_status,V}, Rec) -> Rec#market_data_incremental_refresh{financial_status = V};
    ({corporate_action,V}, Rec) -> Rec#market_data_incremental_refresh{corporate_action = V};
    ({md_entry_px,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#market_data_incremental_refresh{currency = V};
    ({md_entry_size,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_size = list_to_integer(binary_to_list(V))};
    ({md_entry_date,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_date = V};
    ({md_entry_time,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_time = V};
    ({tick_direction,V}, Rec) -> Rec#market_data_incremental_refresh{tick_direction = V};
    ({md_mkt,V}, Rec) -> Rec#market_data_incremental_refresh{md_mkt = V};
    ({trading_session_id,V}, Rec) -> Rec#market_data_incremental_refresh{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#market_data_incremental_refresh{trading_session_sub_id = V};
    ({quote_condition,V}, Rec) -> Rec#market_data_incremental_refresh{quote_condition = V};
    ({trade_condition,V}, Rec) -> Rec#market_data_incremental_refresh{trade_condition = V};
    ({md_entry_originator,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_originator = V};
    ({location_id,V}, Rec) -> Rec#market_data_incremental_refresh{location_id = V};
    ({desk_id,V}, Rec) -> Rec#market_data_incremental_refresh{desk_id = V};
    ({open_close_settl_flag,V}, Rec) -> Rec#market_data_incremental_refresh{open_close_settl_flag = V};
    ({time_in_force,V}, Rec) -> Rec#market_data_incremental_refresh{time_in_force = V};
    ({expire_date,V}, Rec) -> Rec#market_data_incremental_refresh{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#market_data_incremental_refresh{expire_time = V};
    ({min_qty,V}, Rec) -> Rec#market_data_incremental_refresh{min_qty = list_to_integer(binary_to_list(V))};
    ({exec_inst,V}, Rec) -> Rec#market_data_incremental_refresh{exec_inst = V};
    ({seller_days,V}, Rec) -> Rec#market_data_incremental_refresh{seller_days = list_to_integer(binary_to_list(V))};
    ({order_id,V}, Rec) -> Rec#market_data_incremental_refresh{order_id = V};
    ({quote_entry_id,V}, Rec) -> Rec#market_data_incremental_refresh{quote_entry_id = V};
    ({md_entry_buyer,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_buyer = V};
    ({md_entry_seller,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_seller = V};
    ({number_of_orders,V}, Rec) -> Rec#market_data_incremental_refresh{number_of_orders = list_to_integer(binary_to_list(V))};
    ({md_entry_position_no,V}, Rec) -> Rec#market_data_incremental_refresh{md_entry_position_no = list_to_integer(binary_to_list(V))};
    ({scope,V}, Rec) -> Rec#market_data_incremental_refresh{scope = V};
    ({price_delta,V}, Rec) -> Rec#market_data_incremental_refresh{price_delta = V};
    ({net_chg_prev_day,V}, Rec) -> Rec#market_data_incremental_refresh{net_chg_prev_day = V};
    ({text,V}, Rec) -> Rec#market_data_incremental_refresh{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#market_data_incremental_refresh{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#market_data_incremental_refresh{encoded_text = V};
    ({appl_queue_depth,V}, Rec) -> Rec#market_data_incremental_refresh{appl_queue_depth = list_to_integer(binary_to_list(V))};
    ({appl_queue_resolution,V}, Rec) -> Rec#market_data_incremental_refresh{appl_queue_resolution = list_to_integer(binary_to_list(V))};
    ({K,V}, #market_data_incremental_refresh{fields = F} = Rec) -> Rec#market_data_incremental_refresh{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#market_data_incremental_refresh{fields = lists:reverse(F)}.

decode_message_market_data_request_reject(Message, #market_data_request_reject{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #market_data_request_reject{fields = F} = lists:foldl(fun
    ({md_req_id,V}, Rec) -> Rec#market_data_request_reject{md_req_id = V};
    ({md_req_rej_reason,V}, Rec) -> Rec#market_data_request_reject{md_req_rej_reason = V};
    ({alt_md_source_id,V}, Rec) -> Rec#market_data_request_reject{alt_md_source_id = V};
    ({text,V}, Rec) -> Rec#market_data_request_reject{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#market_data_request_reject{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#market_data_request_reject{encoded_text = V};
    ({K,V}, #market_data_request_reject{fields = F} = Rec) -> Rec#market_data_request_reject{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#market_data_request_reject{fields = lists:reverse(F)}.

decode_message_quote_cancel(Message, #quote_cancel{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_cancel{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#quote_cancel{quote_req_id = V};
    ({quote_id,V}, Rec) -> Rec#quote_cancel{quote_id = V};
    ({quote_cancel_type,V}, Rec) -> Rec#quote_cancel{quote_cancel_type = list_to_integer(binary_to_list(V))};
    ({quote_response_level,V}, Rec) -> Rec#quote_cancel{quote_response_level = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#quote_cancel{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_cancel{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_cancel{account_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote_cancel{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_cancel{trading_session_sub_id = V};
    ({K,V}, #quote_cancel{fields = F} = Rec) -> Rec#quote_cancel{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_cancel{fields = lists:reverse(F)}.

decode_message_quote_status_request(Message, #quote_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_status_request{fields = F} = lists:foldl(fun
    ({quote_status_req_id,V}, Rec) -> Rec#quote_status_request{quote_status_req_id = V};
    ({quote_id,V}, Rec) -> Rec#quote_status_request{quote_id = V};
    ({account,V}, Rec) -> Rec#quote_status_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_status_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_status_request{account_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote_status_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_status_request{trading_session_sub_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#quote_status_request{subscription_request_type = V};
    ({K,V}, #quote_status_request{fields = F} = Rec) -> Rec#quote_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_status_request{fields = lists:reverse(F)}.

decode_message_mass_quote_acknowledgement(Message, #mass_quote_acknowledgement{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #mass_quote_acknowledgement{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_req_id = V};
    ({quote_id,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_id = V};
    ({quote_status,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_status = list_to_integer(binary_to_list(V))};
    ({quote_reject_reason,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_reject_reason = list_to_integer(binary_to_list(V))};
    ({quote_response_level,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_response_level = list_to_integer(binary_to_list(V))};
    ({quote_type,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_type = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#mass_quote_acknowledgement{account = V};
    ({acct_id_source,V}, Rec) -> Rec#mass_quote_acknowledgement{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#mass_quote_acknowledgement{account_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#mass_quote_acknowledgement{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#mass_quote_acknowledgement{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#mass_quote_acknowledgement{encoded_text = V};
    ({quote_set_id,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_set_id = V};
    ({tot_no_quote_entries,V}, Rec) -> Rec#mass_quote_acknowledgement{tot_no_quote_entries = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#mass_quote_acknowledgement{last_fragment = V == <<"Y">>};
    ({quote_entry_id,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_entry_id = V};
    ({bid_px,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_px = list_to_float(binary_to_list(V))};
    ({offer_px,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_px = list_to_float(binary_to_list(V))};
    ({bid_size,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_size = list_to_integer(binary_to_list(V))};
    ({offer_size,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_size = list_to_integer(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#mass_quote_acknowledgement{valid_until_time = V};
    ({bid_spot_rate,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_spot_rate = list_to_float(binary_to_list(V))};
    ({offer_spot_rate,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_spot_rate = list_to_float(binary_to_list(V))};
    ({bid_forward_points,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_forward_points = V};
    ({offer_forward_points,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_forward_points = V};
    ({mid_px,V}, Rec) -> Rec#mass_quote_acknowledgement{mid_px = list_to_float(binary_to_list(V))};
    ({bid_yield,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_yield = V};
    ({mid_yield,V}, Rec) -> Rec#mass_quote_acknowledgement{mid_yield = V};
    ({offer_yield,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_yield = V};
    ({transact_time,V}, Rec) -> Rec#mass_quote_acknowledgement{transact_time = V};
    ({trading_session_id,V}, Rec) -> Rec#mass_quote_acknowledgement{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#mass_quote_acknowledgement{trading_session_sub_id = V};
    ({settl_date,V}, Rec) -> Rec#mass_quote_acknowledgement{settl_date = V};
    ({ord_type,V}, Rec) -> Rec#mass_quote_acknowledgement{ord_type = V};
    ({settl_date2,V}, Rec) -> Rec#mass_quote_acknowledgement{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#mass_quote_acknowledgement{order_qty2 = list_to_integer(binary_to_list(V))};
    ({bid_forward_points2,V}, Rec) -> Rec#mass_quote_acknowledgement{bid_forward_points2 = V};
    ({offer_forward_points2,V}, Rec) -> Rec#mass_quote_acknowledgement{offer_forward_points2 = V};
    ({currency,V}, Rec) -> Rec#mass_quote_acknowledgement{currency = V};
    ({quote_entry_reject_reason,V}, Rec) -> Rec#mass_quote_acknowledgement{quote_entry_reject_reason = list_to_integer(binary_to_list(V))};
    ({K,V}, #mass_quote_acknowledgement{fields = F} = Rec) -> Rec#mass_quote_acknowledgement{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#mass_quote_acknowledgement{fields = lists:reverse(F)}.

decode_message_security_definition_request(Message, #security_definition_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_definition_request{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_definition_request{security_req_id = V};
    ({security_request_type,V}, Rec) -> Rec#security_definition_request{security_request_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#security_definition_request{currency = V};
    ({text,V}, Rec) -> Rec#security_definition_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_definition_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_definition_request{encoded_text = V};
    ({trading_session_id,V}, Rec) -> Rec#security_definition_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_definition_request{trading_session_sub_id = V};
    ({expiration_cycle,V}, Rec) -> Rec#security_definition_request{expiration_cycle = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#security_definition_request{subscription_request_type = V};
    ({K,V}, #security_definition_request{fields = F} = Rec) -> Rec#security_definition_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_definition_request{fields = lists:reverse(F)}.

decode_message_security_definition(Message, #security_definition{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_definition{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_definition{security_req_id = V};
    ({security_response_id,V}, Rec) -> Rec#security_definition{security_response_id = V};
    ({security_response_type,V}, Rec) -> Rec#security_definition{security_response_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#security_definition{currency = V};
    ({trading_session_id,V}, Rec) -> Rec#security_definition{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_definition{trading_session_sub_id = V};
    ({text,V}, Rec) -> Rec#security_definition{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_definition{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_definition{encoded_text = V};
    ({expiration_cycle,V}, Rec) -> Rec#security_definition{expiration_cycle = list_to_integer(binary_to_list(V))};
    ({round_lot,V}, Rec) -> Rec#security_definition{round_lot = list_to_integer(binary_to_list(V))};
    ({min_trade_vol,V}, Rec) -> Rec#security_definition{min_trade_vol = list_to_integer(binary_to_list(V))};
    ({K,V}, #security_definition{fields = F} = Rec) -> Rec#security_definition{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_definition{fields = lists:reverse(F)}.

decode_message_security_status_request(Message, #security_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_status_request{fields = F} = lists:foldl(fun
    ({security_status_req_id,V}, Rec) -> Rec#security_status_request{security_status_req_id = V};
    ({currency,V}, Rec) -> Rec#security_status_request{currency = V};
    ({subscription_request_type,V}, Rec) -> Rec#security_status_request{subscription_request_type = V};
    ({trading_session_id,V}, Rec) -> Rec#security_status_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_status_request{trading_session_sub_id = V};
    ({K,V}, #security_status_request{fields = F} = Rec) -> Rec#security_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_status_request{fields = lists:reverse(F)}.

decode_message_security_status(Message, #security_status{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_status{fields = F} = lists:foldl(fun
    ({security_status_req_id,V}, Rec) -> Rec#security_status{security_status_req_id = V};
    ({currency,V}, Rec) -> Rec#security_status{currency = V};
    ({trading_session_id,V}, Rec) -> Rec#security_status{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_status{trading_session_sub_id = V};
    ({unsolicited_indicator,V}, Rec) -> Rec#security_status{unsolicited_indicator = V == <<"Y">>};
    ({security_trading_status,V}, Rec) -> Rec#security_status{security_trading_status = list_to_integer(binary_to_list(V))};
    ({financial_status,V}, Rec) -> Rec#security_status{financial_status = V};
    ({corporate_action,V}, Rec) -> Rec#security_status{corporate_action = V};
    ({halt_reason_char,V}, Rec) -> Rec#security_status{halt_reason_char = V};
    ({in_view_of_common,V}, Rec) -> Rec#security_status{in_view_of_common = V == <<"Y">>};
    ({due_to_related,V}, Rec) -> Rec#security_status{due_to_related = V == <<"Y">>};
    ({buy_volume,V}, Rec) -> Rec#security_status{buy_volume = list_to_integer(binary_to_list(V))};
    ({sell_volume,V}, Rec) -> Rec#security_status{sell_volume = list_to_integer(binary_to_list(V))};
    ({high_px,V}, Rec) -> Rec#security_status{high_px = list_to_float(binary_to_list(V))};
    ({low_px,V}, Rec) -> Rec#security_status{low_px = list_to_float(binary_to_list(V))};
    ({last_px,V}, Rec) -> Rec#security_status{last_px = list_to_float(binary_to_list(V))};
    ({transact_time,V}, Rec) -> Rec#security_status{transact_time = V};
    ({adjustment,V}, Rec) -> Rec#security_status{adjustment = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#security_status{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_status{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_status{encoded_text = V};
    ({K,V}, #security_status{fields = F} = Rec) -> Rec#security_status{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_status{fields = lists:reverse(F)}.

decode_message_trading_session_status_request(Message, #trading_session_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trading_session_status_request{fields = F} = lists:foldl(fun
    ({trad_ses_req_id,V}, Rec) -> Rec#trading_session_status_request{trad_ses_req_id = V};
    ({trading_session_id,V}, Rec) -> Rec#trading_session_status_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#trading_session_status_request{trading_session_sub_id = V};
    ({trad_ses_method,V}, Rec) -> Rec#trading_session_status_request{trad_ses_method = list_to_integer(binary_to_list(V))};
    ({trad_ses_mode,V}, Rec) -> Rec#trading_session_status_request{trad_ses_mode = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#trading_session_status_request{subscription_request_type = V};
    ({K,V}, #trading_session_status_request{fields = F} = Rec) -> Rec#trading_session_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trading_session_status_request{fields = lists:reverse(F)}.

decode_message_trading_session_status(Message, #trading_session_status{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trading_session_status{fields = F} = lists:foldl(fun
    ({trad_ses_req_id,V}, Rec) -> Rec#trading_session_status{trad_ses_req_id = V};
    ({trading_session_id,V}, Rec) -> Rec#trading_session_status{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#trading_session_status{trading_session_sub_id = V};
    ({trad_ses_method,V}, Rec) -> Rec#trading_session_status{trad_ses_method = list_to_integer(binary_to_list(V))};
    ({trad_ses_mode,V}, Rec) -> Rec#trading_session_status{trad_ses_mode = list_to_integer(binary_to_list(V))};
    ({unsolicited_indicator,V}, Rec) -> Rec#trading_session_status{unsolicited_indicator = V == <<"Y">>};
    ({trad_ses_status,V}, Rec) -> Rec#trading_session_status{trad_ses_status = list_to_integer(binary_to_list(V))};
    ({trad_ses_status_rej_reason,V}, Rec) -> Rec#trading_session_status{trad_ses_status_rej_reason = list_to_integer(binary_to_list(V))};
    ({trad_ses_start_time,V}, Rec) -> Rec#trading_session_status{trad_ses_start_time = V};
    ({trad_ses_open_time,V}, Rec) -> Rec#trading_session_status{trad_ses_open_time = V};
    ({trad_ses_pre_close_time,V}, Rec) -> Rec#trading_session_status{trad_ses_pre_close_time = V};
    ({trad_ses_close_time,V}, Rec) -> Rec#trading_session_status{trad_ses_close_time = V};
    ({trad_ses_end_time,V}, Rec) -> Rec#trading_session_status{trad_ses_end_time = V};
    ({total_volume_traded,V}, Rec) -> Rec#trading_session_status{total_volume_traded = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#trading_session_status{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#trading_session_status{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#trading_session_status{encoded_text = V};
    ({K,V}, #trading_session_status{fields = F} = Rec) -> Rec#trading_session_status{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trading_session_status{fields = lists:reverse(F)}.

decode_message_mass_quote(Message, #mass_quote{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #mass_quote{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#mass_quote{quote_req_id = V};
    ({quote_id,V}, Rec) -> Rec#mass_quote{quote_id = V};
    ({quote_type,V}, Rec) -> Rec#mass_quote{quote_type = list_to_integer(binary_to_list(V))};
    ({quote_response_level,V}, Rec) -> Rec#mass_quote{quote_response_level = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#mass_quote{account = V};
    ({acct_id_source,V}, Rec) -> Rec#mass_quote{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#mass_quote{account_type = list_to_integer(binary_to_list(V))};
    ({def_bid_size,V}, Rec) -> Rec#mass_quote{def_bid_size = list_to_integer(binary_to_list(V))};
    ({def_offer_size,V}, Rec) -> Rec#mass_quote{def_offer_size = list_to_integer(binary_to_list(V))};
    ({quote_set_id,V}, Rec) -> Rec#mass_quote{quote_set_id = V};
    ({quote_set_valid_until_time,V}, Rec) -> Rec#mass_quote{quote_set_valid_until_time = V};
    ({tot_no_quote_entries,V}, Rec) -> Rec#mass_quote{tot_no_quote_entries = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#mass_quote{last_fragment = V == <<"Y">>};
    ({quote_entry_id,V}, Rec) -> Rec#mass_quote{quote_entry_id = V};
    ({bid_px,V}, Rec) -> Rec#mass_quote{bid_px = list_to_float(binary_to_list(V))};
    ({offer_px,V}, Rec) -> Rec#mass_quote{offer_px = list_to_float(binary_to_list(V))};
    ({bid_size,V}, Rec) -> Rec#mass_quote{bid_size = list_to_integer(binary_to_list(V))};
    ({offer_size,V}, Rec) -> Rec#mass_quote{offer_size = list_to_integer(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#mass_quote{valid_until_time = V};
    ({bid_spot_rate,V}, Rec) -> Rec#mass_quote{bid_spot_rate = list_to_float(binary_to_list(V))};
    ({offer_spot_rate,V}, Rec) -> Rec#mass_quote{offer_spot_rate = list_to_float(binary_to_list(V))};
    ({bid_forward_points,V}, Rec) -> Rec#mass_quote{bid_forward_points = V};
    ({offer_forward_points,V}, Rec) -> Rec#mass_quote{offer_forward_points = V};
    ({mid_px,V}, Rec) -> Rec#mass_quote{mid_px = list_to_float(binary_to_list(V))};
    ({bid_yield,V}, Rec) -> Rec#mass_quote{bid_yield = V};
    ({mid_yield,V}, Rec) -> Rec#mass_quote{mid_yield = V};
    ({offer_yield,V}, Rec) -> Rec#mass_quote{offer_yield = V};
    ({transact_time,V}, Rec) -> Rec#mass_quote{transact_time = V};
    ({trading_session_id,V}, Rec) -> Rec#mass_quote{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#mass_quote{trading_session_sub_id = V};
    ({settl_date,V}, Rec) -> Rec#mass_quote{settl_date = V};
    ({ord_type,V}, Rec) -> Rec#mass_quote{ord_type = V};
    ({settl_date2,V}, Rec) -> Rec#mass_quote{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#mass_quote{order_qty2 = list_to_integer(binary_to_list(V))};
    ({bid_forward_points2,V}, Rec) -> Rec#mass_quote{bid_forward_points2 = V};
    ({offer_forward_points2,V}, Rec) -> Rec#mass_quote{offer_forward_points2 = V};
    ({currency,V}, Rec) -> Rec#mass_quote{currency = V};
    ({K,V}, #mass_quote{fields = F} = Rec) -> Rec#mass_quote{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#mass_quote{fields = lists:reverse(F)}.

decode_message_business_message_reject(Message, #business_message_reject{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #business_message_reject{fields = F} = lists:foldl(fun
    ({ref_seq_num,V}, Rec) -> Rec#business_message_reject{ref_seq_num = list_to_integer(binary_to_list(V))};
    ({ref_msg_type,V}, Rec) -> Rec#business_message_reject{ref_msg_type = V};
    ({business_reject_ref_id,V}, Rec) -> Rec#business_message_reject{business_reject_ref_id = V};
    ({business_reject_reason,V}, Rec) -> Rec#business_message_reject{business_reject_reason = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#business_message_reject{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#business_message_reject{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#business_message_reject{encoded_text = V};
    ({K,V}, #business_message_reject{fields = F} = Rec) -> Rec#business_message_reject{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#business_message_reject{fields = lists:reverse(F)}.

decode_message_bid_request(Message, #bid_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #bid_request{fields = F} = lists:foldl(fun
    ({bid_id,V}, Rec) -> Rec#bid_request{bid_id = V};
    ({client_bid_id,V}, Rec) -> Rec#bid_request{client_bid_id = V};
    ({bid_request_trans_type,V}, Rec) -> Rec#bid_request{bid_request_trans_type = V};
    ({list_name,V}, Rec) -> Rec#bid_request{list_name = V};
    ({tot_no_related_sym,V}, Rec) -> Rec#bid_request{tot_no_related_sym = list_to_integer(binary_to_list(V))};
    ({bid_type,V}, Rec) -> Rec#bid_request{bid_type = list_to_integer(binary_to_list(V))};
    ({num_tickets,V}, Rec) -> Rec#bid_request{num_tickets = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#bid_request{currency = V};
    ({side_value1,V}, Rec) -> Rec#bid_request{side_value1 = V};
    ({side_value2,V}, Rec) -> Rec#bid_request{side_value2 = V};
    ({bid_descriptor_type,V}, Rec) -> Rec#bid_request{bid_descriptor_type = list_to_integer(binary_to_list(V))};
    ({bid_descriptor,V}, Rec) -> Rec#bid_request{bid_descriptor = V};
    ({side_value_ind,V}, Rec) -> Rec#bid_request{side_value_ind = list_to_integer(binary_to_list(V))};
    ({liquidity_value,V}, Rec) -> Rec#bid_request{liquidity_value = V};
    ({liquidity_num_securities,V}, Rec) -> Rec#bid_request{liquidity_num_securities = list_to_integer(binary_to_list(V))};
    ({liquidity_pct_low,V}, Rec) -> Rec#bid_request{liquidity_pct_low = V};
    ({liquidity_pct_high,V}, Rec) -> Rec#bid_request{liquidity_pct_high = V};
    ({efp_tracking_error,V}, Rec) -> Rec#bid_request{efp_tracking_error = V};
    ({fair_value,V}, Rec) -> Rec#bid_request{fair_value = V};
    ({outside_index_pct,V}, Rec) -> Rec#bid_request{outside_index_pct = V};
    ({value_of_futures,V}, Rec) -> Rec#bid_request{value_of_futures = V};
    ({list_id,V}, Rec) -> Rec#bid_request{list_id = V};
    ({side,V}, Rec) -> Rec#bid_request{side = V};
    ({trading_session_id,V}, Rec) -> Rec#bid_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#bid_request{trading_session_sub_id = V};
    ({net_gross_ind,V}, Rec) -> Rec#bid_request{net_gross_ind = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#bid_request{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#bid_request{settl_date = V};
    ({account,V}, Rec) -> Rec#bid_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#bid_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({liquidity_ind_type,V}, Rec) -> Rec#bid_request{liquidity_ind_type = list_to_integer(binary_to_list(V))};
    ({wt_average_liquidity,V}, Rec) -> Rec#bid_request{wt_average_liquidity = V};
    ({exchange_for_physical,V}, Rec) -> Rec#bid_request{exchange_for_physical = V == <<"Y">>};
    ({out_main_cntry_u_index,V}, Rec) -> Rec#bid_request{out_main_cntry_u_index = V};
    ({cross_percent,V}, Rec) -> Rec#bid_request{cross_percent = V};
    ({prog_rpt_reqs,V}, Rec) -> Rec#bid_request{prog_rpt_reqs = list_to_integer(binary_to_list(V))};
    ({prog_period_interval,V}, Rec) -> Rec#bid_request{prog_period_interval = list_to_integer(binary_to_list(V))};
    ({inc_tax_ind,V}, Rec) -> Rec#bid_request{inc_tax_ind = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#bid_request{forex_req = V == <<"Y">>};
    ({num_bidders,V}, Rec) -> Rec#bid_request{num_bidders = list_to_integer(binary_to_list(V))};
    ({trade_date,V}, Rec) -> Rec#bid_request{trade_date = V};
    ({bid_trade_type,V}, Rec) -> Rec#bid_request{bid_trade_type = V};
    ({basis_px_type,V}, Rec) -> Rec#bid_request{basis_px_type = V};
    ({strike_time,V}, Rec) -> Rec#bid_request{strike_time = V};
    ({text,V}, Rec) -> Rec#bid_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#bid_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#bid_request{encoded_text = V};
    ({K,V}, #bid_request{fields = F} = Rec) -> Rec#bid_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#bid_request{fields = lists:reverse(F)}.

decode_message_bid_response(Message, #bid_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #bid_response{fields = F} = lists:foldl(fun
    ({bid_id,V}, Rec) -> Rec#bid_response{bid_id = V};
    ({client_bid_id,V}, Rec) -> Rec#bid_response{client_bid_id = V};
    ({list_id,V}, Rec) -> Rec#bid_response{list_id = V};
    ({country,V}, Rec) -> Rec#bid_response{country = V};
    ({side,V}, Rec) -> Rec#bid_response{side = V};
    ({price,V}, Rec) -> Rec#bid_response{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#bid_response{price_type = list_to_integer(binary_to_list(V))};
    ({fair_value,V}, Rec) -> Rec#bid_response{fair_value = V};
    ({net_gross_ind,V}, Rec) -> Rec#bid_response{net_gross_ind = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#bid_response{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#bid_response{settl_date = V};
    ({trading_session_id,V}, Rec) -> Rec#bid_response{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#bid_response{trading_session_sub_id = V};
    ({text,V}, Rec) -> Rec#bid_response{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#bid_response{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#bid_response{encoded_text = V};
    ({K,V}, #bid_response{fields = F} = Rec) -> Rec#bid_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#bid_response{fields = lists:reverse(F)}.

decode_message_list_strike_price(Message, #list_strike_price{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #list_strike_price{fields = F} = lists:foldl(fun
    ({list_id,V}, Rec) -> Rec#list_strike_price{list_id = V};
    ({tot_no_strikes,V}, Rec) -> Rec#list_strike_price{tot_no_strikes = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#list_strike_price{last_fragment = V == <<"Y">>};
    ({prev_close_px,V}, Rec) -> Rec#list_strike_price{prev_close_px = list_to_float(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#list_strike_price{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#list_strike_price{secondary_cl_ord_id = V};
    ({side,V}, Rec) -> Rec#list_strike_price{side = V};
    ({price,V}, Rec) -> Rec#list_strike_price{price = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#list_strike_price{currency = V};
    ({text,V}, Rec) -> Rec#list_strike_price{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#list_strike_price{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#list_strike_price{encoded_text = V};
    ({K,V}, #list_strike_price{fields = F} = Rec) -> Rec#list_strike_price{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#list_strike_price{fields = lists:reverse(F)}.

decode_message_registration_instructions(Message, #registration_instructions{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #registration_instructions{fields = F} = lists:foldl(fun
    ({regist_id,V}, Rec) -> Rec#registration_instructions{regist_id = V};
    ({regist_trans_type,V}, Rec) -> Rec#registration_instructions{regist_trans_type = V};
    ({regist_ref_id,V}, Rec) -> Rec#registration_instructions{regist_ref_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#registration_instructions{cl_ord_id = V};
    ({account,V}, Rec) -> Rec#registration_instructions{account = V};
    ({acct_id_source,V}, Rec) -> Rec#registration_instructions{acct_id_source = list_to_integer(binary_to_list(V))};
    ({regist_acct_type,V}, Rec) -> Rec#registration_instructions{regist_acct_type = V};
    ({tax_advantage_type,V}, Rec) -> Rec#registration_instructions{tax_advantage_type = list_to_integer(binary_to_list(V))};
    ({ownership_type,V}, Rec) -> Rec#registration_instructions{ownership_type = V};
    ({regist_dtls,V}, Rec) -> Rec#registration_instructions{regist_dtls = V};
    ({regist_email,V}, Rec) -> Rec#registration_instructions{regist_email = V};
    ({mailing_dtls,V}, Rec) -> Rec#registration_instructions{mailing_dtls = V};
    ({mailing_inst,V}, Rec) -> Rec#registration_instructions{mailing_inst = V};
    ({owner_type,V}, Rec) -> Rec#registration_instructions{owner_type = list_to_integer(binary_to_list(V))};
    ({date_of_birth,V}, Rec) -> Rec#registration_instructions{date_of_birth = V};
    ({investor_country_of_residence,V}, Rec) -> Rec#registration_instructions{investor_country_of_residence = V};
    ({distrib_payment_method,V}, Rec) -> Rec#registration_instructions{distrib_payment_method = list_to_integer(binary_to_list(V))};
    ({distrib_percentage,V}, Rec) -> Rec#registration_instructions{distrib_percentage = V};
    ({cash_distrib_curr,V}, Rec) -> Rec#registration_instructions{cash_distrib_curr = V};
    ({cash_distrib_agent_name,V}, Rec) -> Rec#registration_instructions{cash_distrib_agent_name = V};
    ({cash_distrib_agent_code,V}, Rec) -> Rec#registration_instructions{cash_distrib_agent_code = V};
    ({cash_distrib_agent_acct_number,V}, Rec) -> Rec#registration_instructions{cash_distrib_agent_acct_number = V};
    ({cash_distrib_pay_ref,V}, Rec) -> Rec#registration_instructions{cash_distrib_pay_ref = V};
    ({cash_distrib_agent_acct_name,V}, Rec) -> Rec#registration_instructions{cash_distrib_agent_acct_name = V};
    ({K,V}, #registration_instructions{fields = F} = Rec) -> Rec#registration_instructions{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#registration_instructions{fields = lists:reverse(F)}.

decode_message_registration_instructions_response(Message, #registration_instructions_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #registration_instructions_response{fields = F} = lists:foldl(fun
    ({regist_id,V}, Rec) -> Rec#registration_instructions_response{regist_id = V};
    ({regist_trans_type,V}, Rec) -> Rec#registration_instructions_response{regist_trans_type = V};
    ({regist_ref_id,V}, Rec) -> Rec#registration_instructions_response{regist_ref_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#registration_instructions_response{cl_ord_id = V};
    ({account,V}, Rec) -> Rec#registration_instructions_response{account = V};
    ({acct_id_source,V}, Rec) -> Rec#registration_instructions_response{acct_id_source = list_to_integer(binary_to_list(V))};
    ({regist_status,V}, Rec) -> Rec#registration_instructions_response{regist_status = V};
    ({regist_rej_reason_code,V}, Rec) -> Rec#registration_instructions_response{regist_rej_reason_code = list_to_integer(binary_to_list(V))};
    ({regist_rej_reason_text,V}, Rec) -> Rec#registration_instructions_response{regist_rej_reason_text = V};
    ({K,V}, #registration_instructions_response{fields = F} = Rec) -> Rec#registration_instructions_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#registration_instructions_response{fields = lists:reverse(F)}.

decode_message_order_mass_cancel_request(Message, #order_mass_cancel_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_mass_cancel_request{fields = F} = lists:foldl(fun
    ({cl_ord_id,V}, Rec) -> Rec#order_mass_cancel_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_mass_cancel_request{secondary_cl_ord_id = V};
    ({mass_cancel_request_type,V}, Rec) -> Rec#order_mass_cancel_request{mass_cancel_request_type = V};
    ({trading_session_id,V}, Rec) -> Rec#order_mass_cancel_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#order_mass_cancel_request{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#order_mass_cancel_request{side = V};
    ({transact_time,V}, Rec) -> Rec#order_mass_cancel_request{transact_time = V};
    ({text,V}, Rec) -> Rec#order_mass_cancel_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#order_mass_cancel_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#order_mass_cancel_request{encoded_text = V};
    ({K,V}, #order_mass_cancel_request{fields = F} = Rec) -> Rec#order_mass_cancel_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_mass_cancel_request{fields = lists:reverse(F)}.

decode_message_order_mass_cancel_report(Message, #order_mass_cancel_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_mass_cancel_report{fields = F} = lists:foldl(fun
    ({cl_ord_id,V}, Rec) -> Rec#order_mass_cancel_report{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#order_mass_cancel_report{secondary_cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#order_mass_cancel_report{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#order_mass_cancel_report{secondary_order_id = V};
    ({mass_cancel_request_type,V}, Rec) -> Rec#order_mass_cancel_report{mass_cancel_request_type = V};
    ({mass_cancel_response,V}, Rec) -> Rec#order_mass_cancel_report{mass_cancel_response = V};
    ({mass_cancel_reject_reason,V}, Rec) -> Rec#order_mass_cancel_report{mass_cancel_reject_reason = V};
    ({total_affected_orders,V}, Rec) -> Rec#order_mass_cancel_report{total_affected_orders = list_to_integer(binary_to_list(V))};
    ({orig_cl_ord_id,V}, Rec) -> Rec#order_mass_cancel_report{orig_cl_ord_id = V};
    ({affected_order_id,V}, Rec) -> Rec#order_mass_cancel_report{affected_order_id = V};
    ({affected_secondary_order_id,V}, Rec) -> Rec#order_mass_cancel_report{affected_secondary_order_id = V};
    ({trading_session_id,V}, Rec) -> Rec#order_mass_cancel_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#order_mass_cancel_report{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#order_mass_cancel_report{side = V};
    ({transact_time,V}, Rec) -> Rec#order_mass_cancel_report{transact_time = V};
    ({text,V}, Rec) -> Rec#order_mass_cancel_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#order_mass_cancel_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#order_mass_cancel_report{encoded_text = V};
    ({K,V}, #order_mass_cancel_report{fields = F} = Rec) -> Rec#order_mass_cancel_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_mass_cancel_report{fields = lists:reverse(F)}.

decode_message_new_order_cross(Message, #new_order_cross{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #new_order_cross{fields = F} = lists:foldl(fun
    ({cross_id,V}, Rec) -> Rec#new_order_cross{cross_id = V};
    ({cross_type,V}, Rec) -> Rec#new_order_cross{cross_type = list_to_integer(binary_to_list(V))};
    ({cross_prioritization,V}, Rec) -> Rec#new_order_cross{cross_prioritization = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#new_order_cross{side = V};
    ({cl_ord_id,V}, Rec) -> Rec#new_order_cross{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#new_order_cross{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#new_order_cross{cl_ord_link_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#new_order_cross{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#new_order_cross{trade_date = V};
    ({account,V}, Rec) -> Rec#new_order_cross{account = V};
    ({acct_id_source,V}, Rec) -> Rec#new_order_cross{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#new_order_cross{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#new_order_cross{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#new_order_cross{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#new_order_cross{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#new_order_cross{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#new_order_cross{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#new_order_cross{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#new_order_cross{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#new_order_cross{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#new_order_cross{alloc_qty = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#new_order_cross{qty_type = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#new_order_cross{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#new_order_cross{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#new_order_cross{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#new_order_cross{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#new_order_cross{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#new_order_cross{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#new_order_cross{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#new_order_cross{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#new_order_cross{encoded_text = V};
    ({position_effect,V}, Rec) -> Rec#new_order_cross{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#new_order_cross{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({cash_margin,V}, Rec) -> Rec#new_order_cross{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#new_order_cross{clearing_fee_indicator = V};
    ({solicited_flag,V}, Rec) -> Rec#new_order_cross{solicited_flag = V == <<"Y">>};
    ({side_compliance_id,V}, Rec) -> Rec#new_order_cross{side_compliance_id = V};
    ({settl_type,V}, Rec) -> Rec#new_order_cross{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#new_order_cross{settl_date = V};
    ({handl_inst,V}, Rec) -> Rec#new_order_cross{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#new_order_cross{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#new_order_cross{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#new_order_cross{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#new_order_cross{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#new_order_cross{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#new_order_cross{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#new_order_cross{process_code = V};
    ({prev_close_px,V}, Rec) -> Rec#new_order_cross{prev_close_px = list_to_float(binary_to_list(V))};
    ({locate_reqd,V}, Rec) -> Rec#new_order_cross{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#new_order_cross{transact_time = V};
    ({ord_type,V}, Rec) -> Rec#new_order_cross{ord_type = V};
    ({price_type,V}, Rec) -> Rec#new_order_cross{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#new_order_cross{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#new_order_cross{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#new_order_cross{currency = V};
    ({compliance_id,V}, Rec) -> Rec#new_order_cross{compliance_id = V};
    ({ioi_id,V}, Rec) -> Rec#new_order_cross{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#new_order_cross{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#new_order_cross{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#new_order_cross{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#new_order_cross{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#new_order_cross{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#new_order_cross{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#new_order_cross{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#new_order_cross{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#new_order_cross{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#new_order_cross{participation_rate = V};
    ({cancellation_rights,V}, Rec) -> Rec#new_order_cross{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#new_order_cross{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#new_order_cross{regist_id = V};
    ({designation,V}, Rec) -> Rec#new_order_cross{designation = V};
    ({K,V}, #new_order_cross{fields = F} = Rec) -> Rec#new_order_cross{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#new_order_cross{fields = lists:reverse(F)}.

decode_message_cross_order_cancel_replace_request(Message, #cross_order_cancel_replace_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #cross_order_cancel_replace_request{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{order_id = V};
    ({cross_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{cross_id = V};
    ({orig_cross_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{orig_cross_id = V};
    ({cross_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{cross_type = list_to_integer(binary_to_list(V))};
    ({cross_prioritization,V}, Rec) -> Rec#cross_order_cancel_replace_request{cross_prioritization = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#cross_order_cancel_replace_request{side = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{orig_cl_ord_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{cl_ord_link_id = V};
    ({orig_ord_mod_time,V}, Rec) -> Rec#cross_order_cancel_replace_request{orig_ord_mod_time = V};
    ({trade_origination_date,V}, Rec) -> Rec#cross_order_cancel_replace_request{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#cross_order_cancel_replace_request{trade_date = V};
    ({account,V}, Rec) -> Rec#cross_order_cancel_replace_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#cross_order_cancel_replace_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#cross_order_cancel_replace_request{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#cross_order_cancel_replace_request{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#cross_order_cancel_replace_request{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#cross_order_cancel_replace_request{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#cross_order_cancel_replace_request{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#cross_order_cancel_replace_request{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#cross_order_cancel_replace_request{alloc_qty = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{qty_type = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#cross_order_cancel_replace_request{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#cross_order_cancel_replace_request{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#cross_order_cancel_replace_request{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#cross_order_cancel_replace_request{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#cross_order_cancel_replace_request{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#cross_order_cancel_replace_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#cross_order_cancel_replace_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#cross_order_cancel_replace_request{encoded_text = V};
    ({position_effect,V}, Rec) -> Rec#cross_order_cancel_replace_request{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#cross_order_cancel_replace_request{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({cash_margin,V}, Rec) -> Rec#cross_order_cancel_replace_request{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#cross_order_cancel_replace_request{clearing_fee_indicator = V};
    ({solicited_flag,V}, Rec) -> Rec#cross_order_cancel_replace_request{solicited_flag = V == <<"Y">>};
    ({side_compliance_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{side_compliance_id = V};
    ({settl_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#cross_order_cancel_replace_request{settl_date = V};
    ({handl_inst,V}, Rec) -> Rec#cross_order_cancel_replace_request{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#cross_order_cancel_replace_request{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#cross_order_cancel_replace_request{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#cross_order_cancel_replace_request{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#cross_order_cancel_replace_request{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#cross_order_cancel_replace_request{process_code = V};
    ({prev_close_px,V}, Rec) -> Rec#cross_order_cancel_replace_request{prev_close_px = list_to_float(binary_to_list(V))};
    ({locate_reqd,V}, Rec) -> Rec#cross_order_cancel_replace_request{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#cross_order_cancel_replace_request{transact_time = V};
    ({ord_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{ord_type = V};
    ({price_type,V}, Rec) -> Rec#cross_order_cancel_replace_request{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#cross_order_cancel_replace_request{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#cross_order_cancel_replace_request{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#cross_order_cancel_replace_request{currency = V};
    ({compliance_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{compliance_id = V};
    ({ioi_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#cross_order_cancel_replace_request{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#cross_order_cancel_replace_request{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#cross_order_cancel_replace_request{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#cross_order_cancel_replace_request{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#cross_order_cancel_replace_request{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#cross_order_cancel_replace_request{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#cross_order_cancel_replace_request{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#cross_order_cancel_replace_request{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#cross_order_cancel_replace_request{participation_rate = V};
    ({cancellation_rights,V}, Rec) -> Rec#cross_order_cancel_replace_request{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#cross_order_cancel_replace_request{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#cross_order_cancel_replace_request{regist_id = V};
    ({designation,V}, Rec) -> Rec#cross_order_cancel_replace_request{designation = V};
    ({K,V}, #cross_order_cancel_replace_request{fields = F} = Rec) -> Rec#cross_order_cancel_replace_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#cross_order_cancel_replace_request{fields = lists:reverse(F)}.

decode_message_cross_order_cancel_request(Message, #cross_order_cancel_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #cross_order_cancel_request{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#cross_order_cancel_request{order_id = V};
    ({cross_id,V}, Rec) -> Rec#cross_order_cancel_request{cross_id = V};
    ({orig_cross_id,V}, Rec) -> Rec#cross_order_cancel_request{orig_cross_id = V};
    ({cross_type,V}, Rec) -> Rec#cross_order_cancel_request{cross_type = list_to_integer(binary_to_list(V))};
    ({cross_prioritization,V}, Rec) -> Rec#cross_order_cancel_request{cross_prioritization = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#cross_order_cancel_request{side = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_request{orig_cl_ord_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_request{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#cross_order_cancel_request{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#cross_order_cancel_request{cl_ord_link_id = V};
    ({orig_ord_mod_time,V}, Rec) -> Rec#cross_order_cancel_request{orig_ord_mod_time = V};
    ({trade_origination_date,V}, Rec) -> Rec#cross_order_cancel_request{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#cross_order_cancel_request{trade_date = V};
    ({compliance_id,V}, Rec) -> Rec#cross_order_cancel_request{compliance_id = V};
    ({text,V}, Rec) -> Rec#cross_order_cancel_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#cross_order_cancel_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#cross_order_cancel_request{encoded_text = V};
    ({transact_time,V}, Rec) -> Rec#cross_order_cancel_request{transact_time = V};
    ({K,V}, #cross_order_cancel_request{fields = F} = Rec) -> Rec#cross_order_cancel_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#cross_order_cancel_request{fields = lists:reverse(F)}.

decode_message_security_type_request(Message, #security_type_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_type_request{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_type_request{security_req_id = V};
    ({text,V}, Rec) -> Rec#security_type_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_type_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_type_request{encoded_text = V};
    ({trading_session_id,V}, Rec) -> Rec#security_type_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_type_request{trading_session_sub_id = V};
    ({product,V}, Rec) -> Rec#security_type_request{product = list_to_integer(binary_to_list(V))};
    ({security_type,V}, Rec) -> Rec#security_type_request{security_type = V};
    ({security_sub_type,V}, Rec) -> Rec#security_type_request{security_sub_type = V};
    ({K,V}, #security_type_request{fields = F} = Rec) -> Rec#security_type_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_type_request{fields = lists:reverse(F)}.

decode_message_security_types(Message, #security_types{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_types{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_types{security_req_id = V};
    ({security_response_id,V}, Rec) -> Rec#security_types{security_response_id = V};
    ({security_response_type,V}, Rec) -> Rec#security_types{security_response_type = list_to_integer(binary_to_list(V))};
    ({tot_no_security_types,V}, Rec) -> Rec#security_types{tot_no_security_types = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#security_types{last_fragment = V == <<"Y">>};
    ({security_type,V}, Rec) -> Rec#security_types{security_type = V};
    ({security_sub_type,V}, Rec) -> Rec#security_types{security_sub_type = V};
    ({product,V}, Rec) -> Rec#security_types{product = list_to_integer(binary_to_list(V))};
    ({cfi_code,V}, Rec) -> Rec#security_types{cfi_code = V};
    ({text,V}, Rec) -> Rec#security_types{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_types{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_types{encoded_text = V};
    ({trading_session_id,V}, Rec) -> Rec#security_types{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_types{trading_session_sub_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#security_types{subscription_request_type = V};
    ({K,V}, #security_types{fields = F} = Rec) -> Rec#security_types{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_types{fields = lists:reverse(F)}.

decode_message_security_list_request(Message, #security_list_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_list_request{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_list_request{security_req_id = V};
    ({security_list_request_type,V}, Rec) -> Rec#security_list_request{security_list_request_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#security_list_request{currency = V};
    ({text,V}, Rec) -> Rec#security_list_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_list_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_list_request{encoded_text = V};
    ({trading_session_id,V}, Rec) -> Rec#security_list_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_list_request{trading_session_sub_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#security_list_request{subscription_request_type = V};
    ({K,V}, #security_list_request{fields = F} = Rec) -> Rec#security_list_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_list_request{fields = lists:reverse(F)}.

decode_message_security_list(Message, #security_list{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #security_list{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#security_list{security_req_id = V};
    ({security_response_id,V}, Rec) -> Rec#security_list{security_response_id = V};
    ({security_request_result,V}, Rec) -> Rec#security_list{security_request_result = list_to_integer(binary_to_list(V))};
    ({tot_no_related_sym,V}, Rec) -> Rec#security_list{tot_no_related_sym = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#security_list{last_fragment = V == <<"Y">>};
    ({currency,V}, Rec) -> Rec#security_list{currency = V};
    ({leg_swap_type,V}, Rec) -> Rec#security_list{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#security_list{leg_settl_type = V};
    ({round_lot,V}, Rec) -> Rec#security_list{round_lot = list_to_integer(binary_to_list(V))};
    ({min_trade_vol,V}, Rec) -> Rec#security_list{min_trade_vol = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#security_list{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#security_list{trading_session_sub_id = V};
    ({expiration_cycle,V}, Rec) -> Rec#security_list{expiration_cycle = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#security_list{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#security_list{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#security_list{encoded_text = V};
    ({K,V}, #security_list{fields = F} = Rec) -> Rec#security_list{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#security_list{fields = lists:reverse(F)}.

decode_message_derivative_security_list_request(Message, #derivative_security_list_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #derivative_security_list_request{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#derivative_security_list_request{security_req_id = V};
    ({security_list_request_type,V}, Rec) -> Rec#derivative_security_list_request{security_list_request_type = list_to_integer(binary_to_list(V))};
    ({security_sub_type,V}, Rec) -> Rec#derivative_security_list_request{security_sub_type = V};
    ({currency,V}, Rec) -> Rec#derivative_security_list_request{currency = V};
    ({text,V}, Rec) -> Rec#derivative_security_list_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#derivative_security_list_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#derivative_security_list_request{encoded_text = V};
    ({trading_session_id,V}, Rec) -> Rec#derivative_security_list_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#derivative_security_list_request{trading_session_sub_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#derivative_security_list_request{subscription_request_type = V};
    ({K,V}, #derivative_security_list_request{fields = F} = Rec) -> Rec#derivative_security_list_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#derivative_security_list_request{fields = lists:reverse(F)}.

decode_message_derivative_security_list(Message, #derivative_security_list{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #derivative_security_list{fields = F} = lists:foldl(fun
    ({security_req_id,V}, Rec) -> Rec#derivative_security_list{security_req_id = V};
    ({security_response_id,V}, Rec) -> Rec#derivative_security_list{security_response_id = V};
    ({security_request_result,V}, Rec) -> Rec#derivative_security_list{security_request_result = list_to_integer(binary_to_list(V))};
    ({tot_no_related_sym,V}, Rec) -> Rec#derivative_security_list{tot_no_related_sym = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#derivative_security_list{last_fragment = V == <<"Y">>};
    ({currency,V}, Rec) -> Rec#derivative_security_list{currency = V};
    ({expiration_cycle,V}, Rec) -> Rec#derivative_security_list{expiration_cycle = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#derivative_security_list{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#derivative_security_list{trading_session_sub_id = V};
    ({text,V}, Rec) -> Rec#derivative_security_list{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#derivative_security_list{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#derivative_security_list{encoded_text = V};
    ({K,V}, #derivative_security_list{fields = F} = Rec) -> Rec#derivative_security_list{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#derivative_security_list{fields = lists:reverse(F)}.

decode_message_new_order_multileg(Message, #new_order_multileg{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #new_order_multileg{fields = F} = lists:foldl(fun
    ({cl_ord_id,V}, Rec) -> Rec#new_order_multileg{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#new_order_multileg{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#new_order_multileg{cl_ord_link_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#new_order_multileg{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#new_order_multileg{trade_date = V};
    ({account,V}, Rec) -> Rec#new_order_multileg{account = V};
    ({acct_id_source,V}, Rec) -> Rec#new_order_multileg{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#new_order_multileg{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#new_order_multileg{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#new_order_multileg{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#new_order_multileg{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#new_order_multileg{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#new_order_multileg{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#new_order_multileg{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#new_order_multileg{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#new_order_multileg{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#new_order_multileg{alloc_qty = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#new_order_multileg{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#new_order_multileg{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#new_order_multileg{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#new_order_multileg{clearing_fee_indicator = V};
    ({handl_inst,V}, Rec) -> Rec#new_order_multileg{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#new_order_multileg{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#new_order_multileg{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#new_order_multileg{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#new_order_multileg{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#new_order_multileg{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#new_order_multileg{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#new_order_multileg{process_code = V};
    ({side,V}, Rec) -> Rec#new_order_multileg{side = V};
    ({prev_close_px,V}, Rec) -> Rec#new_order_multileg{prev_close_px = list_to_float(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#new_order_multileg{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#new_order_multileg{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_alloc_account,V}, Rec) -> Rec#new_order_multileg{leg_alloc_account = V};
    ({leg_individual_alloc_id,V}, Rec) -> Rec#new_order_multileg{leg_individual_alloc_id = V};
    ({leg_alloc_qty,V}, Rec) -> Rec#new_order_multileg{leg_alloc_qty = list_to_integer(binary_to_list(V))};
    ({leg_alloc_acct_id_source,V}, Rec) -> Rec#new_order_multileg{leg_alloc_acct_id_source = V};
    ({leg_settl_currency,V}, Rec) -> Rec#new_order_multileg{leg_settl_currency = V};
    ({leg_position_effect,V}, Rec) -> Rec#new_order_multileg{leg_position_effect = V};
    ({leg_covered_or_uncovered,V}, Rec) -> Rec#new_order_multileg{leg_covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({leg_ref_id,V}, Rec) -> Rec#new_order_multileg{leg_ref_id = V};
    ({leg_price,V}, Rec) -> Rec#new_order_multileg{leg_price = list_to_float(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#new_order_multileg{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#new_order_multileg{leg_settl_date = V};
    ({locate_reqd,V}, Rec) -> Rec#new_order_multileg{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#new_order_multileg{transact_time = V};
    ({qty_type,V}, Rec) -> Rec#new_order_multileg{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#new_order_multileg{ord_type = V};
    ({price_type,V}, Rec) -> Rec#new_order_multileg{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#new_order_multileg{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#new_order_multileg{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#new_order_multileg{currency = V};
    ({compliance_id,V}, Rec) -> Rec#new_order_multileg{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#new_order_multileg{solicited_flag = V == <<"Y">>};
    ({ioi_id,V}, Rec) -> Rec#new_order_multileg{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#new_order_multileg{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#new_order_multileg{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#new_order_multileg{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#new_order_multileg{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#new_order_multileg{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#new_order_multileg{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#new_order_multileg{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#new_order_multileg{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#new_order_multileg{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#new_order_multileg{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#new_order_multileg{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#new_order_multileg{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#new_order_multileg{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#new_order_multileg{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#new_order_multileg{encoded_text = V};
    ({position_effect,V}, Rec) -> Rec#new_order_multileg{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#new_order_multileg{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#new_order_multileg{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#new_order_multileg{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#new_order_multileg{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#new_order_multileg{participation_rate = V};
    ({cancellation_rights,V}, Rec) -> Rec#new_order_multileg{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#new_order_multileg{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#new_order_multileg{regist_id = V};
    ({designation,V}, Rec) -> Rec#new_order_multileg{designation = V};
    ({multi_leg_rpt_type_req,V}, Rec) -> Rec#new_order_multileg{multi_leg_rpt_type_req = list_to_integer(binary_to_list(V))};
    ({K,V}, #new_order_multileg{fields = F} = Rec) -> Rec#new_order_multileg{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#new_order_multileg{fields = lists:reverse(F)}.

decode_message_multileg_order_cancel_replace(Message, #multileg_order_cancel_replace{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #multileg_order_cancel_replace{fields = F} = lists:foldl(fun
    ({order_id,V}, Rec) -> Rec#multileg_order_cancel_replace{order_id = V};
    ({orig_cl_ord_id,V}, Rec) -> Rec#multileg_order_cancel_replace{orig_cl_ord_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#multileg_order_cancel_replace{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#multileg_order_cancel_replace{secondary_cl_ord_id = V};
    ({cl_ord_link_id,V}, Rec) -> Rec#multileg_order_cancel_replace{cl_ord_link_id = V};
    ({orig_ord_mod_time,V}, Rec) -> Rec#multileg_order_cancel_replace{orig_ord_mod_time = V};
    ({trade_origination_date,V}, Rec) -> Rec#multileg_order_cancel_replace{trade_origination_date = V};
    ({trade_date,V}, Rec) -> Rec#multileg_order_cancel_replace{trade_date = V};
    ({account,V}, Rec) -> Rec#multileg_order_cancel_replace{account = V};
    ({acct_id_source,V}, Rec) -> Rec#multileg_order_cancel_replace{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#multileg_order_cancel_replace{account_type = list_to_integer(binary_to_list(V))};
    ({day_booking_inst,V}, Rec) -> Rec#multileg_order_cancel_replace{day_booking_inst = V};
    ({booking_unit,V}, Rec) -> Rec#multileg_order_cancel_replace{booking_unit = V};
    ({prealloc_method,V}, Rec) -> Rec#multileg_order_cancel_replace{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#multileg_order_cancel_replace{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#multileg_order_cancel_replace{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#multileg_order_cancel_replace{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#multileg_order_cancel_replace{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#multileg_order_cancel_replace{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#multileg_order_cancel_replace{alloc_qty = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#multileg_order_cancel_replace{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#multileg_order_cancel_replace{settl_date = V};
    ({cash_margin,V}, Rec) -> Rec#multileg_order_cancel_replace{cash_margin = V};
    ({clearing_fee_indicator,V}, Rec) -> Rec#multileg_order_cancel_replace{clearing_fee_indicator = V};
    ({handl_inst,V}, Rec) -> Rec#multileg_order_cancel_replace{handl_inst = V};
    ({exec_inst,V}, Rec) -> Rec#multileg_order_cancel_replace{exec_inst = V};
    ({min_qty,V}, Rec) -> Rec#multileg_order_cancel_replace{min_qty = list_to_integer(binary_to_list(V))};
    ({max_floor,V}, Rec) -> Rec#multileg_order_cancel_replace{max_floor = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#multileg_order_cancel_replace{ex_destination = V};
    ({trading_session_id,V}, Rec) -> Rec#multileg_order_cancel_replace{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#multileg_order_cancel_replace{trading_session_sub_id = V};
    ({process_code,V}, Rec) -> Rec#multileg_order_cancel_replace{process_code = V};
    ({side,V}, Rec) -> Rec#multileg_order_cancel_replace{side = V};
    ({prev_close_px,V}, Rec) -> Rec#multileg_order_cancel_replace{prev_close_px = list_to_float(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_alloc_account,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_alloc_account = V};
    ({leg_individual_alloc_id,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_individual_alloc_id = V};
    ({leg_alloc_qty,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_alloc_qty = list_to_integer(binary_to_list(V))};
    ({leg_alloc_acct_id_source,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_alloc_acct_id_source = V};
    ({leg_settl_currency,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_settl_currency = V};
    ({leg_position_effect,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_position_effect = V};
    ({leg_covered_or_uncovered,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({leg_ref_id,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_ref_id = V};
    ({leg_price,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_price = list_to_float(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#multileg_order_cancel_replace{leg_settl_date = V};
    ({locate_reqd,V}, Rec) -> Rec#multileg_order_cancel_replace{locate_reqd = V == <<"Y">>};
    ({transact_time,V}, Rec) -> Rec#multileg_order_cancel_replace{transact_time = V};
    ({qty_type,V}, Rec) -> Rec#multileg_order_cancel_replace{qty_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#multileg_order_cancel_replace{ord_type = V};
    ({price_type,V}, Rec) -> Rec#multileg_order_cancel_replace{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#multileg_order_cancel_replace{price = list_to_float(binary_to_list(V))};
    ({stop_px,V}, Rec) -> Rec#multileg_order_cancel_replace{stop_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#multileg_order_cancel_replace{currency = V};
    ({compliance_id,V}, Rec) -> Rec#multileg_order_cancel_replace{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#multileg_order_cancel_replace{solicited_flag = V == <<"Y">>};
    ({ioi_id,V}, Rec) -> Rec#multileg_order_cancel_replace{ioi_id = V};
    ({quote_id,V}, Rec) -> Rec#multileg_order_cancel_replace{quote_id = V};
    ({time_in_force,V}, Rec) -> Rec#multileg_order_cancel_replace{time_in_force = V};
    ({effective_time,V}, Rec) -> Rec#multileg_order_cancel_replace{effective_time = V};
    ({expire_date,V}, Rec) -> Rec#multileg_order_cancel_replace{expire_date = V};
    ({expire_time,V}, Rec) -> Rec#multileg_order_cancel_replace{expire_time = V};
    ({gt_booking_inst,V}, Rec) -> Rec#multileg_order_cancel_replace{gt_booking_inst = list_to_integer(binary_to_list(V))};
    ({order_capacity,V}, Rec) -> Rec#multileg_order_cancel_replace{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#multileg_order_cancel_replace{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#multileg_order_cancel_replace{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({forex_req,V}, Rec) -> Rec#multileg_order_cancel_replace{forex_req = V == <<"Y">>};
    ({settl_currency,V}, Rec) -> Rec#multileg_order_cancel_replace{settl_currency = V};
    ({booking_type,V}, Rec) -> Rec#multileg_order_cancel_replace{booking_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#multileg_order_cancel_replace{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#multileg_order_cancel_replace{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#multileg_order_cancel_replace{encoded_text = V};
    ({position_effect,V}, Rec) -> Rec#multileg_order_cancel_replace{position_effect = V};
    ({covered_or_uncovered,V}, Rec) -> Rec#multileg_order_cancel_replace{covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({max_show,V}, Rec) -> Rec#multileg_order_cancel_replace{max_show = list_to_integer(binary_to_list(V))};
    ({target_strategy,V}, Rec) -> Rec#multileg_order_cancel_replace{target_strategy = list_to_integer(binary_to_list(V))};
    ({target_strategy_parameters,V}, Rec) -> Rec#multileg_order_cancel_replace{target_strategy_parameters = V};
    ({participation_rate,V}, Rec) -> Rec#multileg_order_cancel_replace{participation_rate = V};
    ({cancellation_rights,V}, Rec) -> Rec#multileg_order_cancel_replace{cancellation_rights = V};
    ({money_laundering_status,V}, Rec) -> Rec#multileg_order_cancel_replace{money_laundering_status = V};
    ({regist_id,V}, Rec) -> Rec#multileg_order_cancel_replace{regist_id = V};
    ({designation,V}, Rec) -> Rec#multileg_order_cancel_replace{designation = V};
    ({multi_leg_rpt_type_req,V}, Rec) -> Rec#multileg_order_cancel_replace{multi_leg_rpt_type_req = list_to_integer(binary_to_list(V))};
    ({K,V}, #multileg_order_cancel_replace{fields = F} = Rec) -> Rec#multileg_order_cancel_replace{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#multileg_order_cancel_replace{fields = lists:reverse(F)}.

decode_message_trade_capture_report_request(Message, #trade_capture_report_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trade_capture_report_request{fields = F} = lists:foldl(fun
    ({trade_request_id,V}, Rec) -> Rec#trade_capture_report_request{trade_request_id = V};
    ({trade_request_type,V}, Rec) -> Rec#trade_capture_report_request{trade_request_type = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#trade_capture_report_request{subscription_request_type = V};
    ({trade_report_id,V}, Rec) -> Rec#trade_capture_report_request{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#trade_capture_report_request{secondary_trade_report_id = V};
    ({exec_id,V}, Rec) -> Rec#trade_capture_report_request{exec_id = V};
    ({exec_type,V}, Rec) -> Rec#trade_capture_report_request{exec_type = V};
    ({order_id,V}, Rec) -> Rec#trade_capture_report_request{order_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#trade_capture_report_request{cl_ord_id = V};
    ({match_status,V}, Rec) -> Rec#trade_capture_report_request{match_status = V};
    ({trd_type,V}, Rec) -> Rec#trade_capture_report_request{trd_type = list_to_integer(binary_to_list(V))};
    ({trd_sub_type,V}, Rec) -> Rec#trade_capture_report_request{trd_sub_type = list_to_integer(binary_to_list(V))};
    ({transfer_reason,V}, Rec) -> Rec#trade_capture_report_request{transfer_reason = V};
    ({secondary_trd_type,V}, Rec) -> Rec#trade_capture_report_request{secondary_trd_type = list_to_integer(binary_to_list(V))};
    ({trade_link_id,V}, Rec) -> Rec#trade_capture_report_request{trade_link_id = V};
    ({trd_match_id,V}, Rec) -> Rec#trade_capture_report_request{trd_match_id = V};
    ({trade_date,V}, Rec) -> Rec#trade_capture_report_request{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#trade_capture_report_request{transact_time = V};
    ({clearing_business_date,V}, Rec) -> Rec#trade_capture_report_request{clearing_business_date = V};
    ({trading_session_id,V}, Rec) -> Rec#trade_capture_report_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#trade_capture_report_request{trading_session_sub_id = V};
    ({time_bracket,V}, Rec) -> Rec#trade_capture_report_request{time_bracket = V};
    ({side,V}, Rec) -> Rec#trade_capture_report_request{side = V};
    ({multi_leg_reporting_type,V}, Rec) -> Rec#trade_capture_report_request{multi_leg_reporting_type = V};
    ({trade_input_source,V}, Rec) -> Rec#trade_capture_report_request{trade_input_source = V};
    ({trade_input_device,V}, Rec) -> Rec#trade_capture_report_request{trade_input_device = V};
    ({response_transport_type,V}, Rec) -> Rec#trade_capture_report_request{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#trade_capture_report_request{response_destination = V};
    ({text,V}, Rec) -> Rec#trade_capture_report_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#trade_capture_report_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#trade_capture_report_request{encoded_text = V};
    ({K,V}, #trade_capture_report_request{fields = F} = Rec) -> Rec#trade_capture_report_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trade_capture_report_request{fields = lists:reverse(F)}.

decode_message_trade_capture_report(Message, #trade_capture_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trade_capture_report{fields = F} = lists:foldl(fun
    ({trade_report_id,V}, Rec) -> Rec#trade_capture_report{trade_report_id = V};
    ({trade_report_trans_type,V}, Rec) -> Rec#trade_capture_report{trade_report_trans_type = list_to_integer(binary_to_list(V))};
    ({trade_report_type,V}, Rec) -> Rec#trade_capture_report{trade_report_type = list_to_integer(binary_to_list(V))};
    ({trade_request_id,V}, Rec) -> Rec#trade_capture_report{trade_request_id = V};
    ({trd_type,V}, Rec) -> Rec#trade_capture_report{trd_type = list_to_integer(binary_to_list(V))};
    ({trd_sub_type,V}, Rec) -> Rec#trade_capture_report{trd_sub_type = list_to_integer(binary_to_list(V))};
    ({secondary_trd_type,V}, Rec) -> Rec#trade_capture_report{secondary_trd_type = list_to_integer(binary_to_list(V))};
    ({transfer_reason,V}, Rec) -> Rec#trade_capture_report{transfer_reason = V};
    ({exec_type,V}, Rec) -> Rec#trade_capture_report{exec_type = V};
    ({tot_num_trade_reports,V}, Rec) -> Rec#trade_capture_report{tot_num_trade_reports = list_to_integer(binary_to_list(V))};
    ({last_rpt_requested,V}, Rec) -> Rec#trade_capture_report{last_rpt_requested = V == <<"Y">>};
    ({unsolicited_indicator,V}, Rec) -> Rec#trade_capture_report{unsolicited_indicator = V == <<"Y">>};
    ({subscription_request_type,V}, Rec) -> Rec#trade_capture_report{subscription_request_type = V};
    ({trade_report_ref_id,V}, Rec) -> Rec#trade_capture_report{trade_report_ref_id = V};
    ({secondary_trade_report_ref_id,V}, Rec) -> Rec#trade_capture_report{secondary_trade_report_ref_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#trade_capture_report{secondary_trade_report_id = V};
    ({trade_link_id,V}, Rec) -> Rec#trade_capture_report{trade_link_id = V};
    ({trd_match_id,V}, Rec) -> Rec#trade_capture_report{trd_match_id = V};
    ({exec_id,V}, Rec) -> Rec#trade_capture_report{exec_id = V};
    ({ord_status,V}, Rec) -> Rec#trade_capture_report{ord_status = V};
    ({secondary_exec_id,V}, Rec) -> Rec#trade_capture_report{secondary_exec_id = V};
    ({exec_restatement_reason,V}, Rec) -> Rec#trade_capture_report{exec_restatement_reason = list_to_integer(binary_to_list(V))};
    ({previously_reported,V}, Rec) -> Rec#trade_capture_report{previously_reported = V == <<"Y">>};
    ({price_type,V}, Rec) -> Rec#trade_capture_report{price_type = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#trade_capture_report{qty_type = list_to_integer(binary_to_list(V))};
    ({underlying_trading_session_id,V}, Rec) -> Rec#trade_capture_report{underlying_trading_session_id = V};
    ({underlying_trading_session_sub_id,V}, Rec) -> Rec#trade_capture_report{underlying_trading_session_sub_id = V};
    ({last_qty,V}, Rec) -> Rec#trade_capture_report{last_qty = list_to_integer(binary_to_list(V))};
    ({last_px,V}, Rec) -> Rec#trade_capture_report{last_px = list_to_float(binary_to_list(V))};
    ({last_par_px,V}, Rec) -> Rec#trade_capture_report{last_par_px = list_to_float(binary_to_list(V))};
    ({last_spot_rate,V}, Rec) -> Rec#trade_capture_report{last_spot_rate = list_to_float(binary_to_list(V))};
    ({last_forward_points,V}, Rec) -> Rec#trade_capture_report{last_forward_points = V};
    ({last_mkt,V}, Rec) -> Rec#trade_capture_report{last_mkt = V};
    ({trade_date,V}, Rec) -> Rec#trade_capture_report{trade_date = V};
    ({clearing_business_date,V}, Rec) -> Rec#trade_capture_report{clearing_business_date = V};
    ({avg_px,V}, Rec) -> Rec#trade_capture_report{avg_px = list_to_float(binary_to_list(V))};
    ({avg_px_indicator,V}, Rec) -> Rec#trade_capture_report{avg_px_indicator = list_to_integer(binary_to_list(V))};
    ({multi_leg_reporting_type,V}, Rec) -> Rec#trade_capture_report{multi_leg_reporting_type = V};
    ({trade_leg_ref_id,V}, Rec) -> Rec#trade_capture_report{trade_leg_ref_id = V};
    ({leg_qty,V}, Rec) -> Rec#trade_capture_report{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#trade_capture_report{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_position_effect,V}, Rec) -> Rec#trade_capture_report{leg_position_effect = V};
    ({leg_covered_or_uncovered,V}, Rec) -> Rec#trade_capture_report{leg_covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({leg_ref_id,V}, Rec) -> Rec#trade_capture_report{leg_ref_id = V};
    ({leg_price,V}, Rec) -> Rec#trade_capture_report{leg_price = list_to_float(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#trade_capture_report{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#trade_capture_report{leg_settl_date = V};
    ({leg_last_px,V}, Rec) -> Rec#trade_capture_report{leg_last_px = list_to_float(binary_to_list(V))};
    ({transact_time,V}, Rec) -> Rec#trade_capture_report{transact_time = V};
    ({settl_type,V}, Rec) -> Rec#trade_capture_report{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#trade_capture_report{settl_date = V};
    ({match_status,V}, Rec) -> Rec#trade_capture_report{match_status = V};
    ({match_type,V}, Rec) -> Rec#trade_capture_report{match_type = V};
    ({side,V}, Rec) -> Rec#trade_capture_report{side = V};
    ({order_id,V}, Rec) -> Rec#trade_capture_report{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#trade_capture_report{secondary_order_id = V};
    ({cl_ord_id,V}, Rec) -> Rec#trade_capture_report{cl_ord_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#trade_capture_report{secondary_cl_ord_id = V};
    ({list_id,V}, Rec) -> Rec#trade_capture_report{list_id = V};
    ({account,V}, Rec) -> Rec#trade_capture_report{account = V};
    ({acct_id_source,V}, Rec) -> Rec#trade_capture_report{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#trade_capture_report{account_type = list_to_integer(binary_to_list(V))};
    ({process_code,V}, Rec) -> Rec#trade_capture_report{process_code = V};
    ({odd_lot,V}, Rec) -> Rec#trade_capture_report{odd_lot = V == <<"Y">>};
    ({clearing_instruction,V}, Rec) -> Rec#trade_capture_report{clearing_instruction = list_to_integer(binary_to_list(V))};
    ({clearing_fee_indicator,V}, Rec) -> Rec#trade_capture_report{clearing_fee_indicator = V};
    ({trade_input_source,V}, Rec) -> Rec#trade_capture_report{trade_input_source = V};
    ({trade_input_device,V}, Rec) -> Rec#trade_capture_report{trade_input_device = V};
    ({order_input_device,V}, Rec) -> Rec#trade_capture_report{order_input_device = V};
    ({currency,V}, Rec) -> Rec#trade_capture_report{currency = V};
    ({compliance_id,V}, Rec) -> Rec#trade_capture_report{compliance_id = V};
    ({solicited_flag,V}, Rec) -> Rec#trade_capture_report{solicited_flag = V == <<"Y">>};
    ({order_capacity,V}, Rec) -> Rec#trade_capture_report{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#trade_capture_report{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#trade_capture_report{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#trade_capture_report{ord_type = V};
    ({exec_inst,V}, Rec) -> Rec#trade_capture_report{exec_inst = V};
    ({trans_bkd_time,V}, Rec) -> Rec#trade_capture_report{trans_bkd_time = V};
    ({trading_session_id,V}, Rec) -> Rec#trade_capture_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#trade_capture_report{trading_session_sub_id = V};
    ({time_bracket,V}, Rec) -> Rec#trade_capture_report{time_bracket = V};
    ({gross_trade_amt,V}, Rec) -> Rec#trade_capture_report{gross_trade_amt = V};
    ({num_days_interest,V}, Rec) -> Rec#trade_capture_report{num_days_interest = list_to_integer(binary_to_list(V))};
    ({ex_date,V}, Rec) -> Rec#trade_capture_report{ex_date = V};
    ({accrued_interest_rate,V}, Rec) -> Rec#trade_capture_report{accrued_interest_rate = V};
    ({accrued_interest_amt,V}, Rec) -> Rec#trade_capture_report{accrued_interest_amt = V};
    ({interest_at_maturity,V}, Rec) -> Rec#trade_capture_report{interest_at_maturity = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#trade_capture_report{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#trade_capture_report{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#trade_capture_report{end_cash = V};
    ({concession,V}, Rec) -> Rec#trade_capture_report{concession = V};
    ({total_takedown,V}, Rec) -> Rec#trade_capture_report{total_takedown = V};
    ({net_money,V}, Rec) -> Rec#trade_capture_report{net_money = V};
    ({settl_curr_amt,V}, Rec) -> Rec#trade_capture_report{settl_curr_amt = V};
    ({settl_currency,V}, Rec) -> Rec#trade_capture_report{settl_currency = V};
    ({settl_curr_fx_rate,V}, Rec) -> Rec#trade_capture_report{settl_curr_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#trade_capture_report{settl_curr_fx_rate_calc = V};
    ({position_effect,V}, Rec) -> Rec#trade_capture_report{position_effect = V};
    ({text,V}, Rec) -> Rec#trade_capture_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#trade_capture_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#trade_capture_report{encoded_text = V};
    ({side_multi_leg_reporting_type,V}, Rec) -> Rec#trade_capture_report{side_multi_leg_reporting_type = list_to_integer(binary_to_list(V))};
    ({cont_amt_type,V}, Rec) -> Rec#trade_capture_report{cont_amt_type = list_to_integer(binary_to_list(V))};
    ({cont_amt_value,V}, Rec) -> Rec#trade_capture_report{cont_amt_value = V};
    ({cont_amt_curr,V}, Rec) -> Rec#trade_capture_report{cont_amt_curr = V};
    ({misc_fee_amt,V}, Rec) -> Rec#trade_capture_report{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#trade_capture_report{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#trade_capture_report{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#trade_capture_report{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({exchange_rule,V}, Rec) -> Rec#trade_capture_report{exchange_rule = V};
    ({trade_alloc_indicator,V}, Rec) -> Rec#trade_capture_report{trade_alloc_indicator = list_to_integer(binary_to_list(V))};
    ({prealloc_method,V}, Rec) -> Rec#trade_capture_report{prealloc_method = V};
    ({alloc_id,V}, Rec) -> Rec#trade_capture_report{alloc_id = V};
    ({alloc_account,V}, Rec) -> Rec#trade_capture_report{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#trade_capture_report{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#trade_capture_report{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#trade_capture_report{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#trade_capture_report{alloc_qty = list_to_integer(binary_to_list(V))};
    ({copy_msg_indicator,V}, Rec) -> Rec#trade_capture_report{copy_msg_indicator = V == <<"Y">>};
    ({publish_trd_indicator,V}, Rec) -> Rec#trade_capture_report{publish_trd_indicator = V == <<"Y">>};
    ({short_sale_reason,V}, Rec) -> Rec#trade_capture_report{short_sale_reason = list_to_integer(binary_to_list(V))};
    ({K,V}, #trade_capture_report{fields = F} = Rec) -> Rec#trade_capture_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trade_capture_report{fields = lists:reverse(F)}.

decode_message_order_mass_status_request(Message, #order_mass_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #order_mass_status_request{fields = F} = lists:foldl(fun
    ({mass_status_req_id,V}, Rec) -> Rec#order_mass_status_request{mass_status_req_id = V};
    ({mass_status_req_type,V}, Rec) -> Rec#order_mass_status_request{mass_status_req_type = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#order_mass_status_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#order_mass_status_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#order_mass_status_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#order_mass_status_request{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#order_mass_status_request{side = V};
    ({K,V}, #order_mass_status_request{fields = F} = Rec) -> Rec#order_mass_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#order_mass_status_request{fields = lists:reverse(F)}.

decode_message_quote_request_reject(Message, #quote_request_reject{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_request_reject{fields = F} = lists:foldl(fun
    ({quote_req_id,V}, Rec) -> Rec#quote_request_reject{quote_req_id = V};
    ({rfq_req_id,V}, Rec) -> Rec#quote_request_reject{rfq_req_id = V};
    ({quote_request_reject_reason,V}, Rec) -> Rec#quote_request_reject{quote_request_reject_reason = list_to_integer(binary_to_list(V))};
    ({prev_close_px,V}, Rec) -> Rec#quote_request_reject{prev_close_px = list_to_float(binary_to_list(V))};
    ({quote_request_type,V}, Rec) -> Rec#quote_request_reject{quote_request_type = list_to_integer(binary_to_list(V))};
    ({quote_type,V}, Rec) -> Rec#quote_request_reject{quote_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote_request_reject{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_request_reject{trading_session_sub_id = V};
    ({trade_origination_date,V}, Rec) -> Rec#quote_request_reject{trade_origination_date = V};
    ({side,V}, Rec) -> Rec#quote_request_reject{side = V};
    ({qty_type,V}, Rec) -> Rec#quote_request_reject{qty_type = list_to_integer(binary_to_list(V))};
    ({settl_type,V}, Rec) -> Rec#quote_request_reject{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#quote_request_reject{settl_date = V};
    ({settl_date2,V}, Rec) -> Rec#quote_request_reject{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#quote_request_reject{order_qty2 = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#quote_request_reject{currency = V};
    ({account,V}, Rec) -> Rec#quote_request_reject{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_request_reject{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_request_reject{account_type = list_to_integer(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#quote_request_reject{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#quote_request_reject{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#quote_request_reject{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#quote_request_reject{leg_settl_date = V};
    ({quote_qualifier,V}, Rec) -> Rec#quote_request_reject{quote_qualifier = V};
    ({quote_price_type,V}, Rec) -> Rec#quote_request_reject{quote_price_type = list_to_integer(binary_to_list(V))};
    ({ord_type,V}, Rec) -> Rec#quote_request_reject{ord_type = V};
    ({expire_time,V}, Rec) -> Rec#quote_request_reject{expire_time = V};
    ({transact_time,V}, Rec) -> Rec#quote_request_reject{transact_time = V};
    ({price_type,V}, Rec) -> Rec#quote_request_reject{price_type = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#quote_request_reject{price = list_to_float(binary_to_list(V))};
    ({price2,V}, Rec) -> Rec#quote_request_reject{price2 = list_to_float(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#quote_request_reject{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#quote_request_reject{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#quote_request_reject{encoded_text = V};
    ({K,V}, #quote_request_reject{fields = F} = Rec) -> Rec#quote_request_reject{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_request_reject{fields = lists:reverse(F)}.

decode_message_rfq_request(Message, #rfq_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #rfq_request{fields = F} = lists:foldl(fun
    ({rfq_req_id,V}, Rec) -> Rec#rfq_request{rfq_req_id = V};
    ({prev_close_px,V}, Rec) -> Rec#rfq_request{prev_close_px = list_to_float(binary_to_list(V))};
    ({quote_request_type,V}, Rec) -> Rec#rfq_request{quote_request_type = list_to_integer(binary_to_list(V))};
    ({quote_type,V}, Rec) -> Rec#rfq_request{quote_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#rfq_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#rfq_request{trading_session_sub_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#rfq_request{subscription_request_type = V};
    ({K,V}, #rfq_request{fields = F} = Rec) -> Rec#rfq_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#rfq_request{fields = lists:reverse(F)}.

decode_message_quote_status_report(Message, #quote_status_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_status_report{fields = F} = lists:foldl(fun
    ({quote_status_req_id,V}, Rec) -> Rec#quote_status_report{quote_status_req_id = V};
    ({quote_req_id,V}, Rec) -> Rec#quote_status_report{quote_req_id = V};
    ({quote_id,V}, Rec) -> Rec#quote_status_report{quote_id = V};
    ({quote_resp_id,V}, Rec) -> Rec#quote_status_report{quote_resp_id = V};
    ({quote_type,V}, Rec) -> Rec#quote_status_report{quote_type = list_to_integer(binary_to_list(V))};
    ({trading_session_id,V}, Rec) -> Rec#quote_status_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_status_report{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#quote_status_report{side = V};
    ({settl_type,V}, Rec) -> Rec#quote_status_report{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#quote_status_report{settl_date = V};
    ({settl_date2,V}, Rec) -> Rec#quote_status_report{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#quote_status_report{order_qty2 = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#quote_status_report{currency = V};
    ({account,V}, Rec) -> Rec#quote_status_report{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_status_report{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_status_report{account_type = list_to_integer(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#quote_status_report{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#quote_status_report{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#quote_status_report{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#quote_status_report{leg_settl_date = V};
    ({quote_qualifier,V}, Rec) -> Rec#quote_status_report{quote_qualifier = V};
    ({expire_time,V}, Rec) -> Rec#quote_status_report{expire_time = V};
    ({price,V}, Rec) -> Rec#quote_status_report{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#quote_status_report{price_type = list_to_integer(binary_to_list(V))};
    ({bid_px,V}, Rec) -> Rec#quote_status_report{bid_px = list_to_float(binary_to_list(V))};
    ({offer_px,V}, Rec) -> Rec#quote_status_report{offer_px = list_to_float(binary_to_list(V))};
    ({mkt_bid_px,V}, Rec) -> Rec#quote_status_report{mkt_bid_px = list_to_float(binary_to_list(V))};
    ({mkt_offer_px,V}, Rec) -> Rec#quote_status_report{mkt_offer_px = list_to_float(binary_to_list(V))};
    ({min_bid_size,V}, Rec) -> Rec#quote_status_report{min_bid_size = list_to_integer(binary_to_list(V))};
    ({bid_size,V}, Rec) -> Rec#quote_status_report{bid_size = list_to_integer(binary_to_list(V))};
    ({min_offer_size,V}, Rec) -> Rec#quote_status_report{min_offer_size = list_to_integer(binary_to_list(V))};
    ({offer_size,V}, Rec) -> Rec#quote_status_report{offer_size = list_to_integer(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#quote_status_report{valid_until_time = V};
    ({bid_spot_rate,V}, Rec) -> Rec#quote_status_report{bid_spot_rate = list_to_float(binary_to_list(V))};
    ({offer_spot_rate,V}, Rec) -> Rec#quote_status_report{offer_spot_rate = list_to_float(binary_to_list(V))};
    ({bid_forward_points,V}, Rec) -> Rec#quote_status_report{bid_forward_points = V};
    ({offer_forward_points,V}, Rec) -> Rec#quote_status_report{offer_forward_points = V};
    ({mid_px,V}, Rec) -> Rec#quote_status_report{mid_px = list_to_float(binary_to_list(V))};
    ({bid_yield,V}, Rec) -> Rec#quote_status_report{bid_yield = V};
    ({mid_yield,V}, Rec) -> Rec#quote_status_report{mid_yield = V};
    ({offer_yield,V}, Rec) -> Rec#quote_status_report{offer_yield = V};
    ({transact_time,V}, Rec) -> Rec#quote_status_report{transact_time = V};
    ({ord_type,V}, Rec) -> Rec#quote_status_report{ord_type = V};
    ({bid_forward_points2,V}, Rec) -> Rec#quote_status_report{bid_forward_points2 = V};
    ({offer_forward_points2,V}, Rec) -> Rec#quote_status_report{offer_forward_points2 = V};
    ({settl_curr_bid_fx_rate,V}, Rec) -> Rec#quote_status_report{settl_curr_bid_fx_rate = V};
    ({settl_curr_offer_fx_rate,V}, Rec) -> Rec#quote_status_report{settl_curr_offer_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#quote_status_report{settl_curr_fx_rate_calc = V};
    ({comm_type,V}, Rec) -> Rec#quote_status_report{comm_type = V};
    ({commission,V}, Rec) -> Rec#quote_status_report{commission = V};
    ({cust_order_capacity,V}, Rec) -> Rec#quote_status_report{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#quote_status_report{ex_destination = V};
    ({quote_status,V}, Rec) -> Rec#quote_status_report{quote_status = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#quote_status_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#quote_status_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#quote_status_report{encoded_text = V};
    ({K,V}, #quote_status_report{fields = F} = Rec) -> Rec#quote_status_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_status_report{fields = lists:reverse(F)}.

decode_message_quote_response(Message, #quote_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #quote_response{fields = F} = lists:foldl(fun
    ({quote_resp_id,V}, Rec) -> Rec#quote_response{quote_resp_id = V};
    ({quote_id,V}, Rec) -> Rec#quote_response{quote_id = V};
    ({quote_resp_type,V}, Rec) -> Rec#quote_response{quote_resp_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#quote_response{cl_ord_id = V};
    ({order_capacity,V}, Rec) -> Rec#quote_response{order_capacity = V};
    ({ioi_id,V}, Rec) -> Rec#quote_response{ioi_id = V};
    ({quote_type,V}, Rec) -> Rec#quote_response{quote_type = list_to_integer(binary_to_list(V))};
    ({quote_qualifier,V}, Rec) -> Rec#quote_response{quote_qualifier = V};
    ({trading_session_id,V}, Rec) -> Rec#quote_response{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#quote_response{trading_session_sub_id = V};
    ({side,V}, Rec) -> Rec#quote_response{side = V};
    ({settl_type,V}, Rec) -> Rec#quote_response{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#quote_response{settl_date = V};
    ({settl_date2,V}, Rec) -> Rec#quote_response{settl_date2 = V};
    ({order_qty2,V}, Rec) -> Rec#quote_response{order_qty2 = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#quote_response{currency = V};
    ({account,V}, Rec) -> Rec#quote_response{account = V};
    ({acct_id_source,V}, Rec) -> Rec#quote_response{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#quote_response{account_type = list_to_integer(binary_to_list(V))};
    ({leg_qty,V}, Rec) -> Rec#quote_response{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#quote_response{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#quote_response{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#quote_response{leg_settl_date = V};
    ({leg_price_type,V}, Rec) -> Rec#quote_response{leg_price_type = list_to_integer(binary_to_list(V))};
    ({leg_bid_px,V}, Rec) -> Rec#quote_response{leg_bid_px = list_to_float(binary_to_list(V))};
    ({leg_offer_px,V}, Rec) -> Rec#quote_response{leg_offer_px = list_to_float(binary_to_list(V))};
    ({bid_px,V}, Rec) -> Rec#quote_response{bid_px = list_to_float(binary_to_list(V))};
    ({offer_px,V}, Rec) -> Rec#quote_response{offer_px = list_to_float(binary_to_list(V))};
    ({mkt_bid_px,V}, Rec) -> Rec#quote_response{mkt_bid_px = list_to_float(binary_to_list(V))};
    ({mkt_offer_px,V}, Rec) -> Rec#quote_response{mkt_offer_px = list_to_float(binary_to_list(V))};
    ({min_bid_size,V}, Rec) -> Rec#quote_response{min_bid_size = list_to_integer(binary_to_list(V))};
    ({bid_size,V}, Rec) -> Rec#quote_response{bid_size = list_to_integer(binary_to_list(V))};
    ({min_offer_size,V}, Rec) -> Rec#quote_response{min_offer_size = list_to_integer(binary_to_list(V))};
    ({offer_size,V}, Rec) -> Rec#quote_response{offer_size = list_to_integer(binary_to_list(V))};
    ({valid_until_time,V}, Rec) -> Rec#quote_response{valid_until_time = V};
    ({bid_spot_rate,V}, Rec) -> Rec#quote_response{bid_spot_rate = list_to_float(binary_to_list(V))};
    ({offer_spot_rate,V}, Rec) -> Rec#quote_response{offer_spot_rate = list_to_float(binary_to_list(V))};
    ({bid_forward_points,V}, Rec) -> Rec#quote_response{bid_forward_points = V};
    ({offer_forward_points,V}, Rec) -> Rec#quote_response{offer_forward_points = V};
    ({mid_px,V}, Rec) -> Rec#quote_response{mid_px = list_to_float(binary_to_list(V))};
    ({bid_yield,V}, Rec) -> Rec#quote_response{bid_yield = V};
    ({mid_yield,V}, Rec) -> Rec#quote_response{mid_yield = V};
    ({offer_yield,V}, Rec) -> Rec#quote_response{offer_yield = V};
    ({transact_time,V}, Rec) -> Rec#quote_response{transact_time = V};
    ({ord_type,V}, Rec) -> Rec#quote_response{ord_type = V};
    ({bid_forward_points2,V}, Rec) -> Rec#quote_response{bid_forward_points2 = V};
    ({offer_forward_points2,V}, Rec) -> Rec#quote_response{offer_forward_points2 = V};
    ({settl_curr_bid_fx_rate,V}, Rec) -> Rec#quote_response{settl_curr_bid_fx_rate = V};
    ({settl_curr_offer_fx_rate,V}, Rec) -> Rec#quote_response{settl_curr_offer_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#quote_response{settl_curr_fx_rate_calc = V};
    ({commission,V}, Rec) -> Rec#quote_response{commission = V};
    ({comm_type,V}, Rec) -> Rec#quote_response{comm_type = V};
    ({cust_order_capacity,V}, Rec) -> Rec#quote_response{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({ex_destination,V}, Rec) -> Rec#quote_response{ex_destination = V};
    ({text,V}, Rec) -> Rec#quote_response{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#quote_response{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#quote_response{encoded_text = V};
    ({price,V}, Rec) -> Rec#quote_response{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#quote_response{price_type = list_to_integer(binary_to_list(V))};
    ({K,V}, #quote_response{fields = F} = Rec) -> Rec#quote_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#quote_response{fields = lists:reverse(F)}.

decode_message_confirmation(Message, #confirmation{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #confirmation{fields = F} = lists:foldl(fun
    ({confirm_id,V}, Rec) -> Rec#confirmation{confirm_id = V};
    ({confirm_ref_id,V}, Rec) -> Rec#confirmation{confirm_ref_id = V};
    ({confirm_req_id,V}, Rec) -> Rec#confirmation{confirm_req_id = V};
    ({confirm_trans_type,V}, Rec) -> Rec#confirmation{confirm_trans_type = list_to_integer(binary_to_list(V))};
    ({confirm_type,V}, Rec) -> Rec#confirmation{confirm_type = list_to_integer(binary_to_list(V))};
    ({copy_msg_indicator,V}, Rec) -> Rec#confirmation{copy_msg_indicator = V == <<"Y">>};
    ({legal_confirm,V}, Rec) -> Rec#confirmation{legal_confirm = V == <<"Y">>};
    ({confirm_status,V}, Rec) -> Rec#confirmation{confirm_status = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#confirmation{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#confirmation{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#confirmation{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#confirmation{secondary_cl_ord_id = V};
    ({list_id,V}, Rec) -> Rec#confirmation{list_id = V};
    ({order_qty,V}, Rec) -> Rec#confirmation{order_qty = list_to_integer(binary_to_list(V))};
    ({order_avg_px,V}, Rec) -> Rec#confirmation{order_avg_px = list_to_float(binary_to_list(V))};
    ({order_booking_qty,V}, Rec) -> Rec#confirmation{order_booking_qty = list_to_integer(binary_to_list(V))};
    ({alloc_id,V}, Rec) -> Rec#confirmation{alloc_id = V};
    ({secondary_alloc_id,V}, Rec) -> Rec#confirmation{secondary_alloc_id = V};
    ({individual_alloc_id,V}, Rec) -> Rec#confirmation{individual_alloc_id = V};
    ({transact_time,V}, Rec) -> Rec#confirmation{transact_time = V};
    ({trade_date,V}, Rec) -> Rec#confirmation{trade_date = V};
    ({alloc_qty,V}, Rec) -> Rec#confirmation{alloc_qty = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#confirmation{qty_type = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#confirmation{side = V};
    ({currency,V}, Rec) -> Rec#confirmation{currency = V};
    ({last_mkt,V}, Rec) -> Rec#confirmation{last_mkt = V};
    ({order_capacity,V}, Rec) -> Rec#confirmation{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#confirmation{order_restrictions = V};
    ({order_capacity_qty,V}, Rec) -> Rec#confirmation{order_capacity_qty = list_to_integer(binary_to_list(V))};
    ({alloc_account,V}, Rec) -> Rec#confirmation{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#confirmation{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_account_type,V}, Rec) -> Rec#confirmation{alloc_account_type = list_to_integer(binary_to_list(V))};
    ({avg_px,V}, Rec) -> Rec#confirmation{avg_px = list_to_float(binary_to_list(V))};
    ({avg_px_precision,V}, Rec) -> Rec#confirmation{avg_px_precision = list_to_integer(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#confirmation{price_type = list_to_integer(binary_to_list(V))};
    ({avg_par_px,V}, Rec) -> Rec#confirmation{avg_par_px = list_to_float(binary_to_list(V))};
    ({reported_px,V}, Rec) -> Rec#confirmation{reported_px = list_to_float(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#confirmation{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#confirmation{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#confirmation{encoded_text = V};
    ({process_code,V}, Rec) -> Rec#confirmation{process_code = V};
    ({gross_trade_amt,V}, Rec) -> Rec#confirmation{gross_trade_amt = V};
    ({num_days_interest,V}, Rec) -> Rec#confirmation{num_days_interest = list_to_integer(binary_to_list(V))};
    ({ex_date,V}, Rec) -> Rec#confirmation{ex_date = V};
    ({accrued_interest_rate,V}, Rec) -> Rec#confirmation{accrued_interest_rate = V};
    ({accrued_interest_amt,V}, Rec) -> Rec#confirmation{accrued_interest_amt = V};
    ({interest_at_maturity,V}, Rec) -> Rec#confirmation{interest_at_maturity = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#confirmation{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#confirmation{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#confirmation{end_cash = V};
    ({concession,V}, Rec) -> Rec#confirmation{concession = V};
    ({total_takedown,V}, Rec) -> Rec#confirmation{total_takedown = V};
    ({net_money,V}, Rec) -> Rec#confirmation{net_money = V};
    ({maturity_net_money,V}, Rec) -> Rec#confirmation{maturity_net_money = V};
    ({settl_curr_amt,V}, Rec) -> Rec#confirmation{settl_curr_amt = V};
    ({settl_currency,V}, Rec) -> Rec#confirmation{settl_currency = V};
    ({settl_curr_fx_rate,V}, Rec) -> Rec#confirmation{settl_curr_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#confirmation{settl_curr_fx_rate_calc = V};
    ({settl_type,V}, Rec) -> Rec#confirmation{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#confirmation{settl_date = V};
    ({shared_commission,V}, Rec) -> Rec#confirmation{shared_commission = V};
    ({misc_fee_amt,V}, Rec) -> Rec#confirmation{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#confirmation{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#confirmation{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#confirmation{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({K,V}, #confirmation{fields = F} = Rec) -> Rec#confirmation{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#confirmation{fields = lists:reverse(F)}.

decode_message_position_maintenance_request(Message, #position_maintenance_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #position_maintenance_request{fields = F} = lists:foldl(fun
    ({pos_req_id,V}, Rec) -> Rec#position_maintenance_request{pos_req_id = V};
    ({pos_trans_type,V}, Rec) -> Rec#position_maintenance_request{pos_trans_type = list_to_integer(binary_to_list(V))};
    ({pos_maint_action,V}, Rec) -> Rec#position_maintenance_request{pos_maint_action = list_to_integer(binary_to_list(V))};
    ({orig_pos_req_ref_id,V}, Rec) -> Rec#position_maintenance_request{orig_pos_req_ref_id = V};
    ({pos_maint_rpt_ref_id,V}, Rec) -> Rec#position_maintenance_request{pos_maint_rpt_ref_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#position_maintenance_request{clearing_business_date = V};
    ({settl_sess_id,V}, Rec) -> Rec#position_maintenance_request{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#position_maintenance_request{settl_sess_sub_id = V};
    ({account,V}, Rec) -> Rec#position_maintenance_request{account = V};
    ({acct_id_source,V}, Rec) -> Rec#position_maintenance_request{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#position_maintenance_request{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#position_maintenance_request{currency = V};
    ({trading_session_id,V}, Rec) -> Rec#position_maintenance_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#position_maintenance_request{trading_session_sub_id = V};
    ({transact_time,V}, Rec) -> Rec#position_maintenance_request{transact_time = V};
    ({adjustment_type,V}, Rec) -> Rec#position_maintenance_request{adjustment_type = list_to_integer(binary_to_list(V))};
    ({contrary_instruction_indicator,V}, Rec) -> Rec#position_maintenance_request{contrary_instruction_indicator = V == <<"Y">>};
    ({prior_spread_indicator,V}, Rec) -> Rec#position_maintenance_request{prior_spread_indicator = V == <<"Y">>};
    ({threshold_amount,V}, Rec) -> Rec#position_maintenance_request{threshold_amount = V};
    ({text,V}, Rec) -> Rec#position_maintenance_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#position_maintenance_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#position_maintenance_request{encoded_text = V};
    ({K,V}, #position_maintenance_request{fields = F} = Rec) -> Rec#position_maintenance_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#position_maintenance_request{fields = lists:reverse(F)}.

decode_message_position_maintenance_report(Message, #position_maintenance_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #position_maintenance_report{fields = F} = lists:foldl(fun
    ({pos_maint_rpt_id,V}, Rec) -> Rec#position_maintenance_report{pos_maint_rpt_id = V};
    ({pos_trans_type,V}, Rec) -> Rec#position_maintenance_report{pos_trans_type = list_to_integer(binary_to_list(V))};
    ({pos_req_id,V}, Rec) -> Rec#position_maintenance_report{pos_req_id = V};
    ({pos_maint_action,V}, Rec) -> Rec#position_maintenance_report{pos_maint_action = list_to_integer(binary_to_list(V))};
    ({orig_pos_req_ref_id,V}, Rec) -> Rec#position_maintenance_report{orig_pos_req_ref_id = V};
    ({pos_maint_status,V}, Rec) -> Rec#position_maintenance_report{pos_maint_status = list_to_integer(binary_to_list(V))};
    ({pos_maint_result,V}, Rec) -> Rec#position_maintenance_report{pos_maint_result = list_to_integer(binary_to_list(V))};
    ({clearing_business_date,V}, Rec) -> Rec#position_maintenance_report{clearing_business_date = V};
    ({settl_sess_id,V}, Rec) -> Rec#position_maintenance_report{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#position_maintenance_report{settl_sess_sub_id = V};
    ({account,V}, Rec) -> Rec#position_maintenance_report{account = V};
    ({acct_id_source,V}, Rec) -> Rec#position_maintenance_report{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#position_maintenance_report{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#position_maintenance_report{currency = V};
    ({trading_session_id,V}, Rec) -> Rec#position_maintenance_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#position_maintenance_report{trading_session_sub_id = V};
    ({transact_time,V}, Rec) -> Rec#position_maintenance_report{transact_time = V};
    ({adjustment_type,V}, Rec) -> Rec#position_maintenance_report{adjustment_type = list_to_integer(binary_to_list(V))};
    ({threshold_amount,V}, Rec) -> Rec#position_maintenance_report{threshold_amount = V};
    ({text,V}, Rec) -> Rec#position_maintenance_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#position_maintenance_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#position_maintenance_report{encoded_text = V};
    ({K,V}, #position_maintenance_report{fields = F} = Rec) -> Rec#position_maintenance_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#position_maintenance_report{fields = lists:reverse(F)}.

decode_message_request_for_positions(Message, #request_for_positions{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #request_for_positions{fields = F} = lists:foldl(fun
    ({pos_req_id,V}, Rec) -> Rec#request_for_positions{pos_req_id = V};
    ({pos_req_type,V}, Rec) -> Rec#request_for_positions{pos_req_type = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#request_for_positions{match_status = V};
    ({subscription_request_type,V}, Rec) -> Rec#request_for_positions{subscription_request_type = V};
    ({account,V}, Rec) -> Rec#request_for_positions{account = V};
    ({acct_id_source,V}, Rec) -> Rec#request_for_positions{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#request_for_positions{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#request_for_positions{currency = V};
    ({clearing_business_date,V}, Rec) -> Rec#request_for_positions{clearing_business_date = V};
    ({settl_sess_id,V}, Rec) -> Rec#request_for_positions{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#request_for_positions{settl_sess_sub_id = V};
    ({trading_session_id,V}, Rec) -> Rec#request_for_positions{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#request_for_positions{trading_session_sub_id = V};
    ({transact_time,V}, Rec) -> Rec#request_for_positions{transact_time = V};
    ({response_transport_type,V}, Rec) -> Rec#request_for_positions{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#request_for_positions{response_destination = V};
    ({text,V}, Rec) -> Rec#request_for_positions{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#request_for_positions{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#request_for_positions{encoded_text = V};
    ({K,V}, #request_for_positions{fields = F} = Rec) -> Rec#request_for_positions{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#request_for_positions{fields = lists:reverse(F)}.

decode_message_request_for_positions_ack(Message, #request_for_positions_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #request_for_positions_ack{fields = F} = lists:foldl(fun
    ({pos_maint_rpt_id,V}, Rec) -> Rec#request_for_positions_ack{pos_maint_rpt_id = V};
    ({pos_req_id,V}, Rec) -> Rec#request_for_positions_ack{pos_req_id = V};
    ({total_num_pos_reports,V}, Rec) -> Rec#request_for_positions_ack{total_num_pos_reports = list_to_integer(binary_to_list(V))};
    ({unsolicited_indicator,V}, Rec) -> Rec#request_for_positions_ack{unsolicited_indicator = V == <<"Y">>};
    ({pos_req_result,V}, Rec) -> Rec#request_for_positions_ack{pos_req_result = list_to_integer(binary_to_list(V))};
    ({pos_req_status,V}, Rec) -> Rec#request_for_positions_ack{pos_req_status = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#request_for_positions_ack{account = V};
    ({acct_id_source,V}, Rec) -> Rec#request_for_positions_ack{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#request_for_positions_ack{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#request_for_positions_ack{currency = V};
    ({response_transport_type,V}, Rec) -> Rec#request_for_positions_ack{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#request_for_positions_ack{response_destination = V};
    ({text,V}, Rec) -> Rec#request_for_positions_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#request_for_positions_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#request_for_positions_ack{encoded_text = V};
    ({K,V}, #request_for_positions_ack{fields = F} = Rec) -> Rec#request_for_positions_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#request_for_positions_ack{fields = lists:reverse(F)}.

decode_message_position_report(Message, #position_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #position_report{fields = F} = lists:foldl(fun
    ({pos_maint_rpt_id,V}, Rec) -> Rec#position_report{pos_maint_rpt_id = V};
    ({pos_req_id,V}, Rec) -> Rec#position_report{pos_req_id = V};
    ({pos_req_type,V}, Rec) -> Rec#position_report{pos_req_type = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#position_report{subscription_request_type = V};
    ({total_num_pos_reports,V}, Rec) -> Rec#position_report{total_num_pos_reports = list_to_integer(binary_to_list(V))};
    ({unsolicited_indicator,V}, Rec) -> Rec#position_report{unsolicited_indicator = V == <<"Y">>};
    ({pos_req_result,V}, Rec) -> Rec#position_report{pos_req_result = list_to_integer(binary_to_list(V))};
    ({clearing_business_date,V}, Rec) -> Rec#position_report{clearing_business_date = V};
    ({settl_sess_id,V}, Rec) -> Rec#position_report{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#position_report{settl_sess_sub_id = V};
    ({account,V}, Rec) -> Rec#position_report{account = V};
    ({acct_id_source,V}, Rec) -> Rec#position_report{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#position_report{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#position_report{currency = V};
    ({settl_price,V}, Rec) -> Rec#position_report{settl_price = list_to_float(binary_to_list(V))};
    ({settl_price_type,V}, Rec) -> Rec#position_report{settl_price_type = list_to_integer(binary_to_list(V))};
    ({prior_settl_price,V}, Rec) -> Rec#position_report{prior_settl_price = list_to_float(binary_to_list(V))};
    ({underlying_settl_price,V}, Rec) -> Rec#position_report{underlying_settl_price = list_to_float(binary_to_list(V))};
    ({underlying_settl_price_type,V}, Rec) -> Rec#position_report{underlying_settl_price_type = list_to_integer(binary_to_list(V))};
    ({regist_status,V}, Rec) -> Rec#position_report{regist_status = V};
    ({delivery_date,V}, Rec) -> Rec#position_report{delivery_date = V};
    ({text,V}, Rec) -> Rec#position_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#position_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#position_report{encoded_text = V};
    ({K,V}, #position_report{fields = F} = Rec) -> Rec#position_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#position_report{fields = lists:reverse(F)}.

decode_message_trade_capture_report_request_ack(Message, #trade_capture_report_request_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trade_capture_report_request_ack{fields = F} = lists:foldl(fun
    ({trade_request_id,V}, Rec) -> Rec#trade_capture_report_request_ack{trade_request_id = V};
    ({trade_request_type,V}, Rec) -> Rec#trade_capture_report_request_ack{trade_request_type = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#trade_capture_report_request_ack{subscription_request_type = V};
    ({tot_num_trade_reports,V}, Rec) -> Rec#trade_capture_report_request_ack{tot_num_trade_reports = list_to_integer(binary_to_list(V))};
    ({trade_request_result,V}, Rec) -> Rec#trade_capture_report_request_ack{trade_request_result = list_to_integer(binary_to_list(V))};
    ({trade_request_status,V}, Rec) -> Rec#trade_capture_report_request_ack{trade_request_status = list_to_integer(binary_to_list(V))};
    ({multi_leg_reporting_type,V}, Rec) -> Rec#trade_capture_report_request_ack{multi_leg_reporting_type = V};
    ({response_transport_type,V}, Rec) -> Rec#trade_capture_report_request_ack{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#trade_capture_report_request_ack{response_destination = V};
    ({text,V}, Rec) -> Rec#trade_capture_report_request_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#trade_capture_report_request_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#trade_capture_report_request_ack{encoded_text = V};
    ({K,V}, #trade_capture_report_request_ack{fields = F} = Rec) -> Rec#trade_capture_report_request_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trade_capture_report_request_ack{fields = lists:reverse(F)}.

decode_message_trade_capture_report_ack(Message, #trade_capture_report_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #trade_capture_report_ack{fields = F} = lists:foldl(fun
    ({trade_report_id,V}, Rec) -> Rec#trade_capture_report_ack{trade_report_id = V};
    ({trade_report_trans_type,V}, Rec) -> Rec#trade_capture_report_ack{trade_report_trans_type = list_to_integer(binary_to_list(V))};
    ({trade_report_type,V}, Rec) -> Rec#trade_capture_report_ack{trade_report_type = list_to_integer(binary_to_list(V))};
    ({trd_type,V}, Rec) -> Rec#trade_capture_report_ack{trd_type = list_to_integer(binary_to_list(V))};
    ({trd_sub_type,V}, Rec) -> Rec#trade_capture_report_ack{trd_sub_type = list_to_integer(binary_to_list(V))};
    ({secondary_trd_type,V}, Rec) -> Rec#trade_capture_report_ack{secondary_trd_type = list_to_integer(binary_to_list(V))};
    ({transfer_reason,V}, Rec) -> Rec#trade_capture_report_ack{transfer_reason = V};
    ({exec_type,V}, Rec) -> Rec#trade_capture_report_ack{exec_type = V};
    ({trade_report_ref_id,V}, Rec) -> Rec#trade_capture_report_ack{trade_report_ref_id = V};
    ({secondary_trade_report_ref_id,V}, Rec) -> Rec#trade_capture_report_ack{secondary_trade_report_ref_id = V};
    ({trd_rpt_status,V}, Rec) -> Rec#trade_capture_report_ack{trd_rpt_status = list_to_integer(binary_to_list(V))};
    ({trade_report_reject_reason,V}, Rec) -> Rec#trade_capture_report_ack{trade_report_reject_reason = list_to_integer(binary_to_list(V))};
    ({secondary_trade_report_id,V}, Rec) -> Rec#trade_capture_report_ack{secondary_trade_report_id = V};
    ({subscription_request_type,V}, Rec) -> Rec#trade_capture_report_ack{subscription_request_type = V};
    ({trade_link_id,V}, Rec) -> Rec#trade_capture_report_ack{trade_link_id = V};
    ({trd_match_id,V}, Rec) -> Rec#trade_capture_report_ack{trd_match_id = V};
    ({exec_id,V}, Rec) -> Rec#trade_capture_report_ack{exec_id = V};
    ({secondary_exec_id,V}, Rec) -> Rec#trade_capture_report_ack{secondary_exec_id = V};
    ({transact_time,V}, Rec) -> Rec#trade_capture_report_ack{transact_time = V};
    ({response_transport_type,V}, Rec) -> Rec#trade_capture_report_ack{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#trade_capture_report_ack{response_destination = V};
    ({text,V}, Rec) -> Rec#trade_capture_report_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#trade_capture_report_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#trade_capture_report_ack{encoded_text = V};
    ({leg_qty,V}, Rec) -> Rec#trade_capture_report_ack{leg_qty = list_to_integer(binary_to_list(V))};
    ({leg_swap_type,V}, Rec) -> Rec#trade_capture_report_ack{leg_swap_type = list_to_integer(binary_to_list(V))};
    ({leg_position_effect,V}, Rec) -> Rec#trade_capture_report_ack{leg_position_effect = V};
    ({leg_covered_or_uncovered,V}, Rec) -> Rec#trade_capture_report_ack{leg_covered_or_uncovered = list_to_integer(binary_to_list(V))};
    ({leg_ref_id,V}, Rec) -> Rec#trade_capture_report_ack{leg_ref_id = V};
    ({leg_price,V}, Rec) -> Rec#trade_capture_report_ack{leg_price = list_to_float(binary_to_list(V))};
    ({leg_settl_type,V}, Rec) -> Rec#trade_capture_report_ack{leg_settl_type = V};
    ({leg_settl_date,V}, Rec) -> Rec#trade_capture_report_ack{leg_settl_date = V};
    ({leg_last_px,V}, Rec) -> Rec#trade_capture_report_ack{leg_last_px = list_to_float(binary_to_list(V))};
    ({clearing_fee_indicator,V}, Rec) -> Rec#trade_capture_report_ack{clearing_fee_indicator = V};
    ({order_capacity,V}, Rec) -> Rec#trade_capture_report_ack{order_capacity = V};
    ({order_restrictions,V}, Rec) -> Rec#trade_capture_report_ack{order_restrictions = V};
    ({cust_order_capacity,V}, Rec) -> Rec#trade_capture_report_ack{cust_order_capacity = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#trade_capture_report_ack{account = V};
    ({acct_id_source,V}, Rec) -> Rec#trade_capture_report_ack{acct_id_source = list_to_integer(binary_to_list(V))};
    ({account_type,V}, Rec) -> Rec#trade_capture_report_ack{account_type = list_to_integer(binary_to_list(V))};
    ({position_effect,V}, Rec) -> Rec#trade_capture_report_ack{position_effect = V};
    ({prealloc_method,V}, Rec) -> Rec#trade_capture_report_ack{prealloc_method = V};
    ({alloc_account,V}, Rec) -> Rec#trade_capture_report_ack{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#trade_capture_report_ack{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_settl_currency,V}, Rec) -> Rec#trade_capture_report_ack{alloc_settl_currency = V};
    ({individual_alloc_id,V}, Rec) -> Rec#trade_capture_report_ack{individual_alloc_id = V};
    ({alloc_qty,V}, Rec) -> Rec#trade_capture_report_ack{alloc_qty = list_to_integer(binary_to_list(V))};
    ({K,V}, #trade_capture_report_ack{fields = F} = Rec) -> Rec#trade_capture_report_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#trade_capture_report_ack{fields = lists:reverse(F)}.

decode_message_allocation_report(Message, #allocation_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #allocation_report{fields = F} = lists:foldl(fun
    ({alloc_report_id,V}, Rec) -> Rec#allocation_report{alloc_report_id = V};
    ({alloc_id,V}, Rec) -> Rec#allocation_report{alloc_id = V};
    ({alloc_trans_type,V}, Rec) -> Rec#allocation_report{alloc_trans_type = V};
    ({alloc_report_ref_id,V}, Rec) -> Rec#allocation_report{alloc_report_ref_id = V};
    ({alloc_canc_replace_reason,V}, Rec) -> Rec#allocation_report{alloc_canc_replace_reason = list_to_integer(binary_to_list(V))};
    ({secondary_alloc_id,V}, Rec) -> Rec#allocation_report{secondary_alloc_id = V};
    ({alloc_report_type,V}, Rec) -> Rec#allocation_report{alloc_report_type = list_to_integer(binary_to_list(V))};
    ({alloc_status,V}, Rec) -> Rec#allocation_report{alloc_status = list_to_integer(binary_to_list(V))};
    ({alloc_rej_code,V}, Rec) -> Rec#allocation_report{alloc_rej_code = list_to_integer(binary_to_list(V))};
    ({ref_alloc_id,V}, Rec) -> Rec#allocation_report{ref_alloc_id = V};
    ({alloc_intermed_req_type,V}, Rec) -> Rec#allocation_report{alloc_intermed_req_type = list_to_integer(binary_to_list(V))};
    ({alloc_link_id,V}, Rec) -> Rec#allocation_report{alloc_link_id = V};
    ({alloc_link_type,V}, Rec) -> Rec#allocation_report{alloc_link_type = list_to_integer(binary_to_list(V))};
    ({booking_ref_id,V}, Rec) -> Rec#allocation_report{booking_ref_id = V};
    ({alloc_no_orders_type,V}, Rec) -> Rec#allocation_report{alloc_no_orders_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#allocation_report{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#allocation_report{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#allocation_report{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#allocation_report{secondary_cl_ord_id = V};
    ({list_id,V}, Rec) -> Rec#allocation_report{list_id = V};
    ({order_qty,V}, Rec) -> Rec#allocation_report{order_qty = list_to_integer(binary_to_list(V))};
    ({order_avg_px,V}, Rec) -> Rec#allocation_report{order_avg_px = list_to_float(binary_to_list(V))};
    ({order_booking_qty,V}, Rec) -> Rec#allocation_report{order_booking_qty = list_to_integer(binary_to_list(V))};
    ({last_qty,V}, Rec) -> Rec#allocation_report{last_qty = list_to_integer(binary_to_list(V))};
    ({exec_id,V}, Rec) -> Rec#allocation_report{exec_id = V};
    ({secondary_exec_id,V}, Rec) -> Rec#allocation_report{secondary_exec_id = V};
    ({last_px,V}, Rec) -> Rec#allocation_report{last_px = list_to_float(binary_to_list(V))};
    ({last_par_px,V}, Rec) -> Rec#allocation_report{last_par_px = list_to_float(binary_to_list(V))};
    ({last_capacity,V}, Rec) -> Rec#allocation_report{last_capacity = V};
    ({previously_reported,V}, Rec) -> Rec#allocation_report{previously_reported = V == <<"Y">>};
    ({reversal_indicator,V}, Rec) -> Rec#allocation_report{reversal_indicator = V == <<"Y">>};
    ({match_type,V}, Rec) -> Rec#allocation_report{match_type = V};
    ({side,V}, Rec) -> Rec#allocation_report{side = V};
    ({quantity,V}, Rec) -> Rec#allocation_report{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#allocation_report{qty_type = list_to_integer(binary_to_list(V))};
    ({last_mkt,V}, Rec) -> Rec#allocation_report{last_mkt = V};
    ({trade_origination_date,V}, Rec) -> Rec#allocation_report{trade_origination_date = V};
    ({trading_session_id,V}, Rec) -> Rec#allocation_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#allocation_report{trading_session_sub_id = V};
    ({price_type,V}, Rec) -> Rec#allocation_report{price_type = list_to_integer(binary_to_list(V))};
    ({avg_px,V}, Rec) -> Rec#allocation_report{avg_px = list_to_float(binary_to_list(V))};
    ({avg_par_px,V}, Rec) -> Rec#allocation_report{avg_par_px = list_to_float(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#allocation_report{currency = V};
    ({avg_px_precision,V}, Rec) -> Rec#allocation_report{avg_px_precision = list_to_integer(binary_to_list(V))};
    ({trade_date,V}, Rec) -> Rec#allocation_report{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#allocation_report{transact_time = V};
    ({settl_type,V}, Rec) -> Rec#allocation_report{settl_type = V};
    ({settl_date,V}, Rec) -> Rec#allocation_report{settl_date = V};
    ({booking_type,V}, Rec) -> Rec#allocation_report{booking_type = list_to_integer(binary_to_list(V))};
    ({gross_trade_amt,V}, Rec) -> Rec#allocation_report{gross_trade_amt = V};
    ({concession,V}, Rec) -> Rec#allocation_report{concession = V};
    ({total_takedown,V}, Rec) -> Rec#allocation_report{total_takedown = V};
    ({net_money,V}, Rec) -> Rec#allocation_report{net_money = V};
    ({position_effect,V}, Rec) -> Rec#allocation_report{position_effect = V};
    ({auto_accept_indicator,V}, Rec) -> Rec#allocation_report{auto_accept_indicator = V == <<"Y">>};
    ({text,V}, Rec) -> Rec#allocation_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#allocation_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#allocation_report{encoded_text = V};
    ({num_days_interest,V}, Rec) -> Rec#allocation_report{num_days_interest = list_to_integer(binary_to_list(V))};
    ({accrued_interest_rate,V}, Rec) -> Rec#allocation_report{accrued_interest_rate = V};
    ({accrued_interest_amt,V}, Rec) -> Rec#allocation_report{accrued_interest_amt = V};
    ({total_accrued_interest_amt,V}, Rec) -> Rec#allocation_report{total_accrued_interest_amt = V};
    ({interest_at_maturity,V}, Rec) -> Rec#allocation_report{interest_at_maturity = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#allocation_report{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#allocation_report{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#allocation_report{end_cash = V};
    ({legal_confirm,V}, Rec) -> Rec#allocation_report{legal_confirm = V == <<"Y">>};
    ({tot_no_allocs,V}, Rec) -> Rec#allocation_report{tot_no_allocs = list_to_integer(binary_to_list(V))};
    ({last_fragment,V}, Rec) -> Rec#allocation_report{last_fragment = V == <<"Y">>};
    ({alloc_account,V}, Rec) -> Rec#allocation_report{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#allocation_report{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#allocation_report{match_status = V};
    ({alloc_price,V}, Rec) -> Rec#allocation_report{alloc_price = list_to_float(binary_to_list(V))};
    ({alloc_qty,V}, Rec) -> Rec#allocation_report{alloc_qty = list_to_integer(binary_to_list(V))};
    ({individual_alloc_id,V}, Rec) -> Rec#allocation_report{individual_alloc_id = V};
    ({process_code,V}, Rec) -> Rec#allocation_report{process_code = V};
    ({notify_broker_of_credit,V}, Rec) -> Rec#allocation_report{notify_broker_of_credit = V == <<"Y">>};
    ({alloc_handl_inst,V}, Rec) -> Rec#allocation_report{alloc_handl_inst = list_to_integer(binary_to_list(V))};
    ({alloc_text,V}, Rec) -> Rec#allocation_report{alloc_text = V};
    ({encoded_alloc_text_len,V}, Rec) -> Rec#allocation_report{encoded_alloc_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_alloc_text,V}, Rec) -> Rec#allocation_report{encoded_alloc_text = V};
    ({alloc_avg_px,V}, Rec) -> Rec#allocation_report{alloc_avg_px = list_to_float(binary_to_list(V))};
    ({alloc_net_money,V}, Rec) -> Rec#allocation_report{alloc_net_money = V};
    ({settl_curr_amt,V}, Rec) -> Rec#allocation_report{settl_curr_amt = V};
    ({alloc_settl_curr_amt,V}, Rec) -> Rec#allocation_report{alloc_settl_curr_amt = V};
    ({settl_currency,V}, Rec) -> Rec#allocation_report{settl_currency = V};
    ({alloc_settl_currency,V}, Rec) -> Rec#allocation_report{alloc_settl_currency = V};
    ({settl_curr_fx_rate,V}, Rec) -> Rec#allocation_report{settl_curr_fx_rate = V};
    ({settl_curr_fx_rate_calc,V}, Rec) -> Rec#allocation_report{settl_curr_fx_rate_calc = V};
    ({alloc_accrued_interest_amt,V}, Rec) -> Rec#allocation_report{alloc_accrued_interest_amt = V};
    ({alloc_interest_at_maturity,V}, Rec) -> Rec#allocation_report{alloc_interest_at_maturity = V};
    ({misc_fee_amt,V}, Rec) -> Rec#allocation_report{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#allocation_report{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#allocation_report{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#allocation_report{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({clearing_instruction,V}, Rec) -> Rec#allocation_report{clearing_instruction = list_to_integer(binary_to_list(V))};
    ({clearing_fee_indicator,V}, Rec) -> Rec#allocation_report{clearing_fee_indicator = V};
    ({alloc_settl_inst_type,V}, Rec) -> Rec#allocation_report{alloc_settl_inst_type = list_to_integer(binary_to_list(V))};
    ({K,V}, #allocation_report{fields = F} = Rec) -> Rec#allocation_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#allocation_report{fields = lists:reverse(F)}.

decode_message_allocation_report_ack(Message, #allocation_report_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #allocation_report_ack{fields = F} = lists:foldl(fun
    ({alloc_report_id,V}, Rec) -> Rec#allocation_report_ack{alloc_report_id = V};
    ({alloc_id,V}, Rec) -> Rec#allocation_report_ack{alloc_id = V};
    ({secondary_alloc_id,V}, Rec) -> Rec#allocation_report_ack{secondary_alloc_id = V};
    ({trade_date,V}, Rec) -> Rec#allocation_report_ack{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#allocation_report_ack{transact_time = V};
    ({alloc_status,V}, Rec) -> Rec#allocation_report_ack{alloc_status = list_to_integer(binary_to_list(V))};
    ({alloc_rej_code,V}, Rec) -> Rec#allocation_report_ack{alloc_rej_code = list_to_integer(binary_to_list(V))};
    ({alloc_report_type,V}, Rec) -> Rec#allocation_report_ack{alloc_report_type = list_to_integer(binary_to_list(V))};
    ({alloc_intermed_req_type,V}, Rec) -> Rec#allocation_report_ack{alloc_intermed_req_type = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#allocation_report_ack{match_status = V};
    ({product,V}, Rec) -> Rec#allocation_report_ack{product = list_to_integer(binary_to_list(V))};
    ({security_type,V}, Rec) -> Rec#allocation_report_ack{security_type = V};
    ({text,V}, Rec) -> Rec#allocation_report_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#allocation_report_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#allocation_report_ack{encoded_text = V};
    ({alloc_account,V}, Rec) -> Rec#allocation_report_ack{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#allocation_report_ack{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_price,V}, Rec) -> Rec#allocation_report_ack{alloc_price = list_to_float(binary_to_list(V))};
    ({individual_alloc_id,V}, Rec) -> Rec#allocation_report_ack{individual_alloc_id = V};
    ({individual_alloc_rej_code,V}, Rec) -> Rec#allocation_report_ack{individual_alloc_rej_code = list_to_integer(binary_to_list(V))};
    ({alloc_text,V}, Rec) -> Rec#allocation_report_ack{alloc_text = V};
    ({encoded_alloc_text_len,V}, Rec) -> Rec#allocation_report_ack{encoded_alloc_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_alloc_text,V}, Rec) -> Rec#allocation_report_ack{encoded_alloc_text = V};
    ({K,V}, #allocation_report_ack{fields = F} = Rec) -> Rec#allocation_report_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#allocation_report_ack{fields = lists:reverse(F)}.

decode_message_confirmation_ack(Message, #confirmation_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #confirmation_ack{fields = F} = lists:foldl(fun
    ({confirm_id,V}, Rec) -> Rec#confirmation_ack{confirm_id = V};
    ({trade_date,V}, Rec) -> Rec#confirmation_ack{trade_date = V};
    ({transact_time,V}, Rec) -> Rec#confirmation_ack{transact_time = V};
    ({affirm_status,V}, Rec) -> Rec#confirmation_ack{affirm_status = list_to_integer(binary_to_list(V))};
    ({confirm_rej_reason,V}, Rec) -> Rec#confirmation_ack{confirm_rej_reason = list_to_integer(binary_to_list(V))};
    ({match_status,V}, Rec) -> Rec#confirmation_ack{match_status = V};
    ({text,V}, Rec) -> Rec#confirmation_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#confirmation_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#confirmation_ack{encoded_text = V};
    ({K,V}, #confirmation_ack{fields = F} = Rec) -> Rec#confirmation_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#confirmation_ack{fields = lists:reverse(F)}.

decode_message_settlement_instruction_request(Message, #settlement_instruction_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #settlement_instruction_request{fields = F} = lists:foldl(fun
    ({settl_inst_req_id,V}, Rec) -> Rec#settlement_instruction_request{settl_inst_req_id = V};
    ({transact_time,V}, Rec) -> Rec#settlement_instruction_request{transact_time = V};
    ({alloc_account,V}, Rec) -> Rec#settlement_instruction_request{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#settlement_instruction_request{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({side,V}, Rec) -> Rec#settlement_instruction_request{side = V};
    ({product,V}, Rec) -> Rec#settlement_instruction_request{product = list_to_integer(binary_to_list(V))};
    ({security_type,V}, Rec) -> Rec#settlement_instruction_request{security_type = V};
    ({cfi_code,V}, Rec) -> Rec#settlement_instruction_request{cfi_code = V};
    ({effective_time,V}, Rec) -> Rec#settlement_instruction_request{effective_time = V};
    ({expire_time,V}, Rec) -> Rec#settlement_instruction_request{expire_time = V};
    ({last_update_time,V}, Rec) -> Rec#settlement_instruction_request{last_update_time = V};
    ({stand_inst_db_type,V}, Rec) -> Rec#settlement_instruction_request{stand_inst_db_type = list_to_integer(binary_to_list(V))};
    ({stand_inst_db_name,V}, Rec) -> Rec#settlement_instruction_request{stand_inst_db_name = V};
    ({stand_inst_db_id,V}, Rec) -> Rec#settlement_instruction_request{stand_inst_db_id = V};
    ({K,V}, #settlement_instruction_request{fields = F} = Rec) -> Rec#settlement_instruction_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#settlement_instruction_request{fields = lists:reverse(F)}.

decode_message_assignment_report(Message, #assignment_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #assignment_report{fields = F} = lists:foldl(fun
    ({asgn_rpt_id,V}, Rec) -> Rec#assignment_report{asgn_rpt_id = V};
    ({tot_num_assignment_reports,V}, Rec) -> Rec#assignment_report{tot_num_assignment_reports = list_to_integer(binary_to_list(V))};
    ({last_rpt_requested,V}, Rec) -> Rec#assignment_report{last_rpt_requested = V == <<"Y">>};
    ({account,V}, Rec) -> Rec#assignment_report{account = V};
    ({account_type,V}, Rec) -> Rec#assignment_report{account_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#assignment_report{currency = V};
    ({threshold_amount,V}, Rec) -> Rec#assignment_report{threshold_amount = V};
    ({settl_price,V}, Rec) -> Rec#assignment_report{settl_price = list_to_float(binary_to_list(V))};
    ({settl_price_type,V}, Rec) -> Rec#assignment_report{settl_price_type = list_to_integer(binary_to_list(V))};
    ({underlying_settl_price,V}, Rec) -> Rec#assignment_report{underlying_settl_price = list_to_float(binary_to_list(V))};
    ({expire_date,V}, Rec) -> Rec#assignment_report{expire_date = V};
    ({assignment_method,V}, Rec) -> Rec#assignment_report{assignment_method = V};
    ({assignment_unit,V}, Rec) -> Rec#assignment_report{assignment_unit = list_to_integer(binary_to_list(V))};
    ({open_interest,V}, Rec) -> Rec#assignment_report{open_interest = V};
    ({exercise_method,V}, Rec) -> Rec#assignment_report{exercise_method = V};
    ({settl_sess_id,V}, Rec) -> Rec#assignment_report{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#assignment_report{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#assignment_report{clearing_business_date = V};
    ({text,V}, Rec) -> Rec#assignment_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#assignment_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#assignment_report{encoded_text = V};
    ({K,V}, #assignment_report{fields = F} = Rec) -> Rec#assignment_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#assignment_report{fields = lists:reverse(F)}.

decode_message_collateral_request(Message, #collateral_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_request{fields = F} = lists:foldl(fun
    ({coll_req_id,V}, Rec) -> Rec#collateral_request{coll_req_id = V};
    ({coll_asgn_reason,V}, Rec) -> Rec#collateral_request{coll_asgn_reason = list_to_integer(binary_to_list(V))};
    ({transact_time,V}, Rec) -> Rec#collateral_request{transact_time = V};
    ({expire_time,V}, Rec) -> Rec#collateral_request{expire_time = V};
    ({account,V}, Rec) -> Rec#collateral_request{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_request{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_request{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_request{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_request{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_request{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_request{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_request{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_request{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_request{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_request{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_request{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_request{currency = V};
    ({coll_action,V}, Rec) -> Rec#collateral_request{coll_action = list_to_integer(binary_to_list(V))};
    ({margin_excess,V}, Rec) -> Rec#collateral_request{margin_excess = V};
    ({total_net_value,V}, Rec) -> Rec#collateral_request{total_net_value = V};
    ({cash_outstanding,V}, Rec) -> Rec#collateral_request{cash_outstanding = V};
    ({side,V}, Rec) -> Rec#collateral_request{side = V};
    ({misc_fee_amt,V}, Rec) -> Rec#collateral_request{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#collateral_request{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#collateral_request{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#collateral_request{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#collateral_request{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#collateral_request{price_type = list_to_integer(binary_to_list(V))};
    ({accrued_interest_amt,V}, Rec) -> Rec#collateral_request{accrued_interest_amt = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#collateral_request{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#collateral_request{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#collateral_request{end_cash = V};
    ({trading_session_id,V}, Rec) -> Rec#collateral_request{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#collateral_request{trading_session_sub_id = V};
    ({settl_sess_id,V}, Rec) -> Rec#collateral_request{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#collateral_request{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#collateral_request{clearing_business_date = V};
    ({text,V}, Rec) -> Rec#collateral_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_request{encoded_text = V};
    ({K,V}, #collateral_request{fields = F} = Rec) -> Rec#collateral_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_request{fields = lists:reverse(F)}.

decode_message_collateral_assignment(Message, #collateral_assignment{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_assignment{fields = F} = lists:foldl(fun
    ({coll_asgn_id,V}, Rec) -> Rec#collateral_assignment{coll_asgn_id = V};
    ({coll_req_id,V}, Rec) -> Rec#collateral_assignment{coll_req_id = V};
    ({coll_asgn_reason,V}, Rec) -> Rec#collateral_assignment{coll_asgn_reason = list_to_integer(binary_to_list(V))};
    ({coll_asgn_trans_type,V}, Rec) -> Rec#collateral_assignment{coll_asgn_trans_type = list_to_integer(binary_to_list(V))};
    ({coll_asgn_ref_id,V}, Rec) -> Rec#collateral_assignment{coll_asgn_ref_id = V};
    ({transact_time,V}, Rec) -> Rec#collateral_assignment{transact_time = V};
    ({expire_time,V}, Rec) -> Rec#collateral_assignment{expire_time = V};
    ({account,V}, Rec) -> Rec#collateral_assignment{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_assignment{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_assignment{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_assignment{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_assignment{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_assignment{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_assignment{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_assignment{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_assignment{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_assignment{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_assignment{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_assignment{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_assignment{currency = V};
    ({coll_action,V}, Rec) -> Rec#collateral_assignment{coll_action = list_to_integer(binary_to_list(V))};
    ({margin_excess,V}, Rec) -> Rec#collateral_assignment{margin_excess = V};
    ({total_net_value,V}, Rec) -> Rec#collateral_assignment{total_net_value = V};
    ({cash_outstanding,V}, Rec) -> Rec#collateral_assignment{cash_outstanding = V};
    ({side,V}, Rec) -> Rec#collateral_assignment{side = V};
    ({misc_fee_amt,V}, Rec) -> Rec#collateral_assignment{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#collateral_assignment{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#collateral_assignment{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#collateral_assignment{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#collateral_assignment{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#collateral_assignment{price_type = list_to_integer(binary_to_list(V))};
    ({accrued_interest_amt,V}, Rec) -> Rec#collateral_assignment{accrued_interest_amt = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#collateral_assignment{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#collateral_assignment{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#collateral_assignment{end_cash = V};
    ({trading_session_id,V}, Rec) -> Rec#collateral_assignment{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#collateral_assignment{trading_session_sub_id = V};
    ({settl_sess_id,V}, Rec) -> Rec#collateral_assignment{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#collateral_assignment{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#collateral_assignment{clearing_business_date = V};
    ({text,V}, Rec) -> Rec#collateral_assignment{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_assignment{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_assignment{encoded_text = V};
    ({K,V}, #collateral_assignment{fields = F} = Rec) -> Rec#collateral_assignment{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_assignment{fields = lists:reverse(F)}.

decode_message_collateral_response(Message, #collateral_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_response{fields = F} = lists:foldl(fun
    ({coll_resp_id,V}, Rec) -> Rec#collateral_response{coll_resp_id = V};
    ({coll_asgn_id,V}, Rec) -> Rec#collateral_response{coll_asgn_id = V};
    ({coll_req_id,V}, Rec) -> Rec#collateral_response{coll_req_id = V};
    ({coll_asgn_reason,V}, Rec) -> Rec#collateral_response{coll_asgn_reason = list_to_integer(binary_to_list(V))};
    ({coll_asgn_trans_type,V}, Rec) -> Rec#collateral_response{coll_asgn_trans_type = list_to_integer(binary_to_list(V))};
    ({coll_asgn_resp_type,V}, Rec) -> Rec#collateral_response{coll_asgn_resp_type = list_to_integer(binary_to_list(V))};
    ({coll_asgn_reject_reason,V}, Rec) -> Rec#collateral_response{coll_asgn_reject_reason = list_to_integer(binary_to_list(V))};
    ({transact_time,V}, Rec) -> Rec#collateral_response{transact_time = V};
    ({account,V}, Rec) -> Rec#collateral_response{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_response{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_response{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_response{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_response{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_response{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_response{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_response{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_response{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_response{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_response{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_response{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_response{currency = V};
    ({coll_action,V}, Rec) -> Rec#collateral_response{coll_action = list_to_integer(binary_to_list(V))};
    ({margin_excess,V}, Rec) -> Rec#collateral_response{margin_excess = V};
    ({total_net_value,V}, Rec) -> Rec#collateral_response{total_net_value = V};
    ({cash_outstanding,V}, Rec) -> Rec#collateral_response{cash_outstanding = V};
    ({side,V}, Rec) -> Rec#collateral_response{side = V};
    ({misc_fee_amt,V}, Rec) -> Rec#collateral_response{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#collateral_response{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#collateral_response{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#collateral_response{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#collateral_response{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#collateral_response{price_type = list_to_integer(binary_to_list(V))};
    ({accrued_interest_amt,V}, Rec) -> Rec#collateral_response{accrued_interest_amt = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#collateral_response{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#collateral_response{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#collateral_response{end_cash = V};
    ({text,V}, Rec) -> Rec#collateral_response{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_response{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_response{encoded_text = V};
    ({K,V}, #collateral_response{fields = F} = Rec) -> Rec#collateral_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_response{fields = lists:reverse(F)}.

decode_message_collateral_report(Message, #collateral_report{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_report{fields = F} = lists:foldl(fun
    ({coll_rpt_id,V}, Rec) -> Rec#collateral_report{coll_rpt_id = V};
    ({coll_inquiry_id,V}, Rec) -> Rec#collateral_report{coll_inquiry_id = V};
    ({coll_status,V}, Rec) -> Rec#collateral_report{coll_status = list_to_integer(binary_to_list(V))};
    ({tot_num_reports,V}, Rec) -> Rec#collateral_report{tot_num_reports = list_to_integer(binary_to_list(V))};
    ({last_rpt_requested,V}, Rec) -> Rec#collateral_report{last_rpt_requested = V == <<"Y">>};
    ({account,V}, Rec) -> Rec#collateral_report{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_report{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_report{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_report{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_report{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_report{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_report{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_report{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_report{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_report{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_report{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_report{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_report{currency = V};
    ({margin_excess,V}, Rec) -> Rec#collateral_report{margin_excess = V};
    ({total_net_value,V}, Rec) -> Rec#collateral_report{total_net_value = V};
    ({cash_outstanding,V}, Rec) -> Rec#collateral_report{cash_outstanding = V};
    ({side,V}, Rec) -> Rec#collateral_report{side = V};
    ({misc_fee_amt,V}, Rec) -> Rec#collateral_report{misc_fee_amt = V};
    ({misc_fee_curr,V}, Rec) -> Rec#collateral_report{misc_fee_curr = V};
    ({misc_fee_type,V}, Rec) -> Rec#collateral_report{misc_fee_type = V};
    ({misc_fee_basis,V}, Rec) -> Rec#collateral_report{misc_fee_basis = list_to_integer(binary_to_list(V))};
    ({price,V}, Rec) -> Rec#collateral_report{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#collateral_report{price_type = list_to_integer(binary_to_list(V))};
    ({accrued_interest_amt,V}, Rec) -> Rec#collateral_report{accrued_interest_amt = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#collateral_report{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#collateral_report{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#collateral_report{end_cash = V};
    ({trading_session_id,V}, Rec) -> Rec#collateral_report{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#collateral_report{trading_session_sub_id = V};
    ({settl_sess_id,V}, Rec) -> Rec#collateral_report{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#collateral_report{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#collateral_report{clearing_business_date = V};
    ({text,V}, Rec) -> Rec#collateral_report{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_report{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_report{encoded_text = V};
    ({K,V}, #collateral_report{fields = F} = Rec) -> Rec#collateral_report{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_report{fields = lists:reverse(F)}.

decode_message_collateral_inquiry(Message, #collateral_inquiry{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_inquiry{fields = F} = lists:foldl(fun
    ({coll_inquiry_id,V}, Rec) -> Rec#collateral_inquiry{coll_inquiry_id = V};
    ({coll_inquiry_qualifier,V}, Rec) -> Rec#collateral_inquiry{coll_inquiry_qualifier = list_to_integer(binary_to_list(V))};
    ({subscription_request_type,V}, Rec) -> Rec#collateral_inquiry{subscription_request_type = V};
    ({response_transport_type,V}, Rec) -> Rec#collateral_inquiry{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#collateral_inquiry{response_destination = V};
    ({account,V}, Rec) -> Rec#collateral_inquiry{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_inquiry{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_inquiry{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_inquiry{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_inquiry{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_inquiry{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_inquiry{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_inquiry{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_inquiry{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_inquiry{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_inquiry{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_inquiry{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_inquiry{currency = V};
    ({margin_excess,V}, Rec) -> Rec#collateral_inquiry{margin_excess = V};
    ({total_net_value,V}, Rec) -> Rec#collateral_inquiry{total_net_value = V};
    ({cash_outstanding,V}, Rec) -> Rec#collateral_inquiry{cash_outstanding = V};
    ({side,V}, Rec) -> Rec#collateral_inquiry{side = V};
    ({price,V}, Rec) -> Rec#collateral_inquiry{price = list_to_float(binary_to_list(V))};
    ({price_type,V}, Rec) -> Rec#collateral_inquiry{price_type = list_to_integer(binary_to_list(V))};
    ({accrued_interest_amt,V}, Rec) -> Rec#collateral_inquiry{accrued_interest_amt = V};
    ({end_accrued_interest_amt,V}, Rec) -> Rec#collateral_inquiry{end_accrued_interest_amt = V};
    ({start_cash,V}, Rec) -> Rec#collateral_inquiry{start_cash = V};
    ({end_cash,V}, Rec) -> Rec#collateral_inquiry{end_cash = V};
    ({trading_session_id,V}, Rec) -> Rec#collateral_inquiry{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#collateral_inquiry{trading_session_sub_id = V};
    ({settl_sess_id,V}, Rec) -> Rec#collateral_inquiry{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#collateral_inquiry{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#collateral_inquiry{clearing_business_date = V};
    ({text,V}, Rec) -> Rec#collateral_inquiry{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_inquiry{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_inquiry{encoded_text = V};
    ({K,V}, #collateral_inquiry{fields = F} = Rec) -> Rec#collateral_inquiry{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_inquiry{fields = lists:reverse(F)}.

decode_message_network_counterparty_system_status_request(Message, #network_counterparty_system_status_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #network_counterparty_system_status_request{fields = F} = lists:foldl(fun
    ({network_request_type,V}, Rec) -> Rec#network_counterparty_system_status_request{network_request_type = list_to_integer(binary_to_list(V))};
    ({network_request_id,V}, Rec) -> Rec#network_counterparty_system_status_request{network_request_id = V};
    ({ref_comp_id,V}, Rec) -> Rec#network_counterparty_system_status_request{ref_comp_id = V};
    ({ref_sub_id,V}, Rec) -> Rec#network_counterparty_system_status_request{ref_sub_id = V};
    ({location_id,V}, Rec) -> Rec#network_counterparty_system_status_request{location_id = V};
    ({desk_id,V}, Rec) -> Rec#network_counterparty_system_status_request{desk_id = V};
    ({K,V}, #network_counterparty_system_status_request{fields = F} = Rec) -> Rec#network_counterparty_system_status_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#network_counterparty_system_status_request{fields = lists:reverse(F)}.

decode_message_network_counterparty_system_status_response(Message, #network_counterparty_system_status_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #network_counterparty_system_status_response{fields = F} = lists:foldl(fun
    ({network_status_response_type,V}, Rec) -> Rec#network_counterparty_system_status_response{network_status_response_type = list_to_integer(binary_to_list(V))};
    ({network_request_id,V}, Rec) -> Rec#network_counterparty_system_status_response{network_request_id = V};
    ({network_response_id,V}, Rec) -> Rec#network_counterparty_system_status_response{network_response_id = V};
    ({last_network_response_id,V}, Rec) -> Rec#network_counterparty_system_status_response{last_network_response_id = V};
    ({ref_comp_id,V}, Rec) -> Rec#network_counterparty_system_status_response{ref_comp_id = V};
    ({ref_sub_id,V}, Rec) -> Rec#network_counterparty_system_status_response{ref_sub_id = V};
    ({location_id,V}, Rec) -> Rec#network_counterparty_system_status_response{location_id = V};
    ({desk_id,V}, Rec) -> Rec#network_counterparty_system_status_response{desk_id = V};
    ({status_value,V}, Rec) -> Rec#network_counterparty_system_status_response{status_value = list_to_integer(binary_to_list(V))};
    ({status_text,V}, Rec) -> Rec#network_counterparty_system_status_response{status_text = V};
    ({K,V}, #network_counterparty_system_status_response{fields = F} = Rec) -> Rec#network_counterparty_system_status_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#network_counterparty_system_status_response{fields = lists:reverse(F)}.

decode_message_user_request(Message, #user_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #user_request{fields = F} = lists:foldl(fun
    ({user_request_id,V}, Rec) -> Rec#user_request{user_request_id = V};
    ({user_request_type,V}, Rec) -> Rec#user_request{user_request_type = list_to_integer(binary_to_list(V))};
    ({username,V}, Rec) -> Rec#user_request{username = V};
    ({password,V}, Rec) -> Rec#user_request{password = V};
    ({new_password,V}, Rec) -> Rec#user_request{new_password = V};
    ({raw_data_length,V}, Rec) -> Rec#user_request{raw_data_length = list_to_integer(binary_to_list(V))};
    ({raw_data,V}, Rec) -> Rec#user_request{raw_data = V};
    ({K,V}, #user_request{fields = F} = Rec) -> Rec#user_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#user_request{fields = lists:reverse(F)}.

decode_message_user_response(Message, #user_response{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #user_response{fields = F} = lists:foldl(fun
    ({user_request_id,V}, Rec) -> Rec#user_response{user_request_id = V};
    ({username,V}, Rec) -> Rec#user_response{username = V};
    ({user_status,V}, Rec) -> Rec#user_response{user_status = list_to_integer(binary_to_list(V))};
    ({user_status_text,V}, Rec) -> Rec#user_response{user_status_text = V};
    ({K,V}, #user_response{fields = F} = Rec) -> Rec#user_response{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#user_response{fields = lists:reverse(F)}.

decode_message_collateral_inquiry_ack(Message, #collateral_inquiry_ack{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #collateral_inquiry_ack{fields = F} = lists:foldl(fun
    ({coll_inquiry_id,V}, Rec) -> Rec#collateral_inquiry_ack{coll_inquiry_id = V};
    ({coll_inquiry_status,V}, Rec) -> Rec#collateral_inquiry_ack{coll_inquiry_status = list_to_integer(binary_to_list(V))};
    ({coll_inquiry_result,V}, Rec) -> Rec#collateral_inquiry_ack{coll_inquiry_result = list_to_integer(binary_to_list(V))};
    ({coll_inquiry_qualifier,V}, Rec) -> Rec#collateral_inquiry_ack{coll_inquiry_qualifier = list_to_integer(binary_to_list(V))};
    ({tot_num_reports,V}, Rec) -> Rec#collateral_inquiry_ack{tot_num_reports = list_to_integer(binary_to_list(V))};
    ({account,V}, Rec) -> Rec#collateral_inquiry_ack{account = V};
    ({account_type,V}, Rec) -> Rec#collateral_inquiry_ack{account_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#collateral_inquiry_ack{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#collateral_inquiry_ack{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#collateral_inquiry_ack{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#collateral_inquiry_ack{secondary_cl_ord_id = V};
    ({exec_id,V}, Rec) -> Rec#collateral_inquiry_ack{exec_id = V};
    ({trade_report_id,V}, Rec) -> Rec#collateral_inquiry_ack{trade_report_id = V};
    ({secondary_trade_report_id,V}, Rec) -> Rec#collateral_inquiry_ack{secondary_trade_report_id = V};
    ({settl_date,V}, Rec) -> Rec#collateral_inquiry_ack{settl_date = V};
    ({quantity,V}, Rec) -> Rec#collateral_inquiry_ack{quantity = list_to_integer(binary_to_list(V))};
    ({qty_type,V}, Rec) -> Rec#collateral_inquiry_ack{qty_type = list_to_integer(binary_to_list(V))};
    ({currency,V}, Rec) -> Rec#collateral_inquiry_ack{currency = V};
    ({trading_session_id,V}, Rec) -> Rec#collateral_inquiry_ack{trading_session_id = V};
    ({trading_session_sub_id,V}, Rec) -> Rec#collateral_inquiry_ack{trading_session_sub_id = V};
    ({settl_sess_id,V}, Rec) -> Rec#collateral_inquiry_ack{settl_sess_id = V};
    ({settl_sess_sub_id,V}, Rec) -> Rec#collateral_inquiry_ack{settl_sess_sub_id = V};
    ({clearing_business_date,V}, Rec) -> Rec#collateral_inquiry_ack{clearing_business_date = V};
    ({response_transport_type,V}, Rec) -> Rec#collateral_inquiry_ack{response_transport_type = list_to_integer(binary_to_list(V))};
    ({response_destination,V}, Rec) -> Rec#collateral_inquiry_ack{response_destination = V};
    ({text,V}, Rec) -> Rec#collateral_inquiry_ack{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#collateral_inquiry_ack{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#collateral_inquiry_ack{encoded_text = V};
    ({K,V}, #collateral_inquiry_ack{fields = F} = Rec) -> Rec#collateral_inquiry_ack{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#collateral_inquiry_ack{fields = lists:reverse(F)}.

decode_message_confirmation_request(Message, #confirmation_request{} = Record) ->
  Fields = [begin
    [K,V] = binary:split(Field, <<"=">>),
    {field_by_number(K),V}
  end || Field <- binary:split(Message, <<1>>, [global]), size(Field) > 0],
  Record1 = #confirmation_request{fields = F} = lists:foldl(fun
    ({confirm_req_id,V}, Rec) -> Rec#confirmation_request{confirm_req_id = V};
    ({confirm_type,V}, Rec) -> Rec#confirmation_request{confirm_type = list_to_integer(binary_to_list(V))};
    ({cl_ord_id,V}, Rec) -> Rec#confirmation_request{cl_ord_id = V};
    ({order_id,V}, Rec) -> Rec#confirmation_request{order_id = V};
    ({secondary_order_id,V}, Rec) -> Rec#confirmation_request{secondary_order_id = V};
    ({secondary_cl_ord_id,V}, Rec) -> Rec#confirmation_request{secondary_cl_ord_id = V};
    ({list_id,V}, Rec) -> Rec#confirmation_request{list_id = V};
    ({order_qty,V}, Rec) -> Rec#confirmation_request{order_qty = list_to_integer(binary_to_list(V))};
    ({order_avg_px,V}, Rec) -> Rec#confirmation_request{order_avg_px = list_to_float(binary_to_list(V))};
    ({order_booking_qty,V}, Rec) -> Rec#confirmation_request{order_booking_qty = list_to_integer(binary_to_list(V))};
    ({alloc_id,V}, Rec) -> Rec#confirmation_request{alloc_id = V};
    ({secondary_alloc_id,V}, Rec) -> Rec#confirmation_request{secondary_alloc_id = V};
    ({individual_alloc_id,V}, Rec) -> Rec#confirmation_request{individual_alloc_id = V};
    ({transact_time,V}, Rec) -> Rec#confirmation_request{transact_time = V};
    ({alloc_account,V}, Rec) -> Rec#confirmation_request{alloc_account = V};
    ({alloc_acct_id_source,V}, Rec) -> Rec#confirmation_request{alloc_acct_id_source = list_to_integer(binary_to_list(V))};
    ({alloc_account_type,V}, Rec) -> Rec#confirmation_request{alloc_account_type = list_to_integer(binary_to_list(V))};
    ({text,V}, Rec) -> Rec#confirmation_request{text = V};
    ({encoded_text_len,V}, Rec) -> Rec#confirmation_request{encoded_text_len = list_to_integer(binary_to_list(V))};
    ({encoded_text,V}, Rec) -> Rec#confirmation_request{encoded_text = V};
    ({K,V}, #confirmation_request{fields = F} = Rec) -> Rec#confirmation_request{fields = [{K,decode_typed_field(K,V)}|F]}
  end, Record, Fields),
  Record1#confirmation_request{fields = lists:reverse(F)}.

field_by_number(<<"1">>) -> account;
field_by_number(<<"2">>) -> adv_id;
field_by_number(<<"3">>) -> adv_ref_id;
field_by_number(<<"4">>) -> adv_side;
field_by_number(<<"5">>) -> adv_trans_type;
field_by_number(<<"6">>) -> avg_px;
field_by_number(<<"7">>) -> begin_seq_no;
field_by_number(<<"8">>) -> begin_string;
field_by_number(<<"9">>) -> body_length;
field_by_number(<<"10">>) -> check_sum;
field_by_number(<<"11">>) -> cl_ord_id;
field_by_number(<<"12">>) -> commission;
field_by_number(<<"13">>) -> comm_type;
field_by_number(<<"14">>) -> cum_qty;
field_by_number(<<"15">>) -> currency;
field_by_number(<<"16">>) -> end_seq_no;
field_by_number(<<"17">>) -> exec_id;
field_by_number(<<"18">>) -> exec_inst;
field_by_number(<<"19">>) -> exec_ref_id;
field_by_number(<<"20">>) -> exec_trans_type;
field_by_number(<<"21">>) -> handl_inst;
field_by_number(<<"22">>) -> security_id_source;
field_by_number(<<"23">>) -> ioi_id;
field_by_number(<<"24">>) -> ioi_oth_svc;
field_by_number(<<"25">>) -> ioi_qlty_ind;
field_by_number(<<"26">>) -> ioi_ref_id;
field_by_number(<<"27">>) -> ioi_qty;
field_by_number(<<"28">>) -> ioi_trans_type;
field_by_number(<<"29">>) -> last_capacity;
field_by_number(<<"30">>) -> last_mkt;
field_by_number(<<"31">>) -> last_px;
field_by_number(<<"32">>) -> last_qty;
field_by_number(<<"33">>) -> no_lines_of_text;
field_by_number(<<"34">>) -> msg_seq_num;
field_by_number(<<"35">>) -> msg_type;
field_by_number(<<"36">>) -> new_seq_no;
field_by_number(<<"37">>) -> order_id;
field_by_number(<<"38">>) -> order_qty;
field_by_number(<<"39">>) -> ord_status;
field_by_number(<<"40">>) -> ord_type;
field_by_number(<<"41">>) -> orig_cl_ord_id;
field_by_number(<<"42">>) -> orig_time;
field_by_number(<<"43">>) -> poss_dup_flag;
field_by_number(<<"44">>) -> price;
field_by_number(<<"45">>) -> ref_seq_num;
field_by_number(<<"46">>) -> relatd_sym;
field_by_number(<<"47">>) -> rule80a;
field_by_number(<<"48">>) -> security_id;
field_by_number(<<"49">>) -> sender_comp_id;
field_by_number(<<"50">>) -> sender_sub_id;
field_by_number(<<"51">>) -> sending_date;
field_by_number(<<"52">>) -> sending_time;
field_by_number(<<"53">>) -> quantity;
field_by_number(<<"54">>) -> side;
field_by_number(<<"55">>) -> symbol;
field_by_number(<<"56">>) -> target_comp_id;
field_by_number(<<"57">>) -> target_sub_id;
field_by_number(<<"58">>) -> text;
field_by_number(<<"59">>) -> time_in_force;
field_by_number(<<"60">>) -> transact_time;
field_by_number(<<"61">>) -> urgency;
field_by_number(<<"62">>) -> valid_until_time;
field_by_number(<<"63">>) -> settl_type;
field_by_number(<<"64">>) -> settl_date;
field_by_number(<<"65">>) -> symbol_sfx;
field_by_number(<<"66">>) -> list_id;
field_by_number(<<"67">>) -> list_seq_no;
field_by_number(<<"68">>) -> tot_no_orders;
field_by_number(<<"69">>) -> list_exec_inst;
field_by_number(<<"70">>) -> alloc_id;
field_by_number(<<"71">>) -> alloc_trans_type;
field_by_number(<<"72">>) -> ref_alloc_id;
field_by_number(<<"73">>) -> no_orders;
field_by_number(<<"74">>) -> avg_px_precision;
field_by_number(<<"75">>) -> trade_date;
field_by_number(<<"76">>) -> exec_broker;
field_by_number(<<"77">>) -> position_effect;
field_by_number(<<"78">>) -> no_allocs;
field_by_number(<<"79">>) -> alloc_account;
field_by_number(<<"80">>) -> alloc_qty;
field_by_number(<<"81">>) -> process_code;
field_by_number(<<"82">>) -> no_rpts;
field_by_number(<<"83">>) -> rpt_seq;
field_by_number(<<"84">>) -> cxl_qty;
field_by_number(<<"85">>) -> no_dlvy_inst;
field_by_number(<<"86">>) -> dlvy_inst;
field_by_number(<<"87">>) -> alloc_status;
field_by_number(<<"88">>) -> alloc_rej_code;
field_by_number(<<"89">>) -> signature;
field_by_number(<<"90">>) -> secure_data_len;
field_by_number(<<"91">>) -> secure_data;
field_by_number(<<"92">>) -> broker_of_credit;
field_by_number(<<"93">>) -> signature_length;
field_by_number(<<"94">>) -> email_type;
field_by_number(<<"95">>) -> raw_data_length;
field_by_number(<<"96">>) -> raw_data;
field_by_number(<<"97">>) -> poss_resend;
field_by_number(<<"98">>) -> encrypt_method;
field_by_number(<<"99">>) -> stop_px;
field_by_number(<<"100">>) -> ex_destination;
field_by_number(<<"102">>) -> cxl_rej_reason;
field_by_number(<<"103">>) -> ord_rej_reason;
field_by_number(<<"104">>) -> ioi_qualifier;
field_by_number(<<"105">>) -> wave_no;
field_by_number(<<"106">>) -> issuer;
field_by_number(<<"107">>) -> security_desc;
field_by_number(<<"108">>) -> heart_bt_int;
field_by_number(<<"109">>) -> client_id;
field_by_number(<<"110">>) -> min_qty;
field_by_number(<<"111">>) -> max_floor;
field_by_number(<<"112">>) -> test_req_id;
field_by_number(<<"113">>) -> report_to_exch;
field_by_number(<<"114">>) -> locate_reqd;
field_by_number(<<"115">>) -> on_behalf_of_comp_id;
field_by_number(<<"116">>) -> on_behalf_of_sub_id;
field_by_number(<<"117">>) -> quote_id;
field_by_number(<<"118">>) -> net_money;
field_by_number(<<"119">>) -> settl_curr_amt;
field_by_number(<<"120">>) -> settl_currency;
field_by_number(<<"121">>) -> forex_req;
field_by_number(<<"122">>) -> orig_sending_time;
field_by_number(<<"123">>) -> gap_fill_flag;
field_by_number(<<"124">>) -> no_execs;
field_by_number(<<"125">>) -> cxl_type;
field_by_number(<<"126">>) -> expire_time;
field_by_number(<<"127">>) -> dk_reason;
field_by_number(<<"128">>) -> deliver_to_comp_id;
field_by_number(<<"129">>) -> deliver_to_sub_id;
field_by_number(<<"130">>) -> ioi_natural_flag;
field_by_number(<<"131">>) -> quote_req_id;
field_by_number(<<"132">>) -> bid_px;
field_by_number(<<"133">>) -> offer_px;
field_by_number(<<"134">>) -> bid_size;
field_by_number(<<"135">>) -> offer_size;
field_by_number(<<"136">>) -> no_misc_fees;
field_by_number(<<"137">>) -> misc_fee_amt;
field_by_number(<<"138">>) -> misc_fee_curr;
field_by_number(<<"139">>) -> misc_fee_type;
field_by_number(<<"140">>) -> prev_close_px;
field_by_number(<<"141">>) -> reset_seq_num_flag;
field_by_number(<<"142">>) -> sender_location_id;
field_by_number(<<"143">>) -> target_location_id;
field_by_number(<<"144">>) -> on_behalf_of_location_id;
field_by_number(<<"145">>) -> deliver_to_location_id;
field_by_number(<<"146">>) -> no_related_sym;
field_by_number(<<"147">>) -> subject;
field_by_number(<<"148">>) -> headline;
field_by_number(<<"149">>) -> url_link;
field_by_number(<<"150">>) -> exec_type;
field_by_number(<<"151">>) -> leaves_qty;
field_by_number(<<"152">>) -> cash_order_qty;
field_by_number(<<"153">>) -> alloc_avg_px;
field_by_number(<<"154">>) -> alloc_net_money;
field_by_number(<<"155">>) -> settl_curr_fx_rate;
field_by_number(<<"156">>) -> settl_curr_fx_rate_calc;
field_by_number(<<"157">>) -> num_days_interest;
field_by_number(<<"158">>) -> accrued_interest_rate;
field_by_number(<<"159">>) -> accrued_interest_amt;
field_by_number(<<"160">>) -> settl_inst_mode;
field_by_number(<<"161">>) -> alloc_text;
field_by_number(<<"162">>) -> settl_inst_id;
field_by_number(<<"163">>) -> settl_inst_trans_type;
field_by_number(<<"164">>) -> email_thread_id;
field_by_number(<<"165">>) -> settl_inst_source;
field_by_number(<<"166">>) -> settl_location;
field_by_number(<<"167">>) -> security_type;
field_by_number(<<"168">>) -> effective_time;
field_by_number(<<"169">>) -> stand_inst_db_type;
field_by_number(<<"170">>) -> stand_inst_db_name;
field_by_number(<<"171">>) -> stand_inst_db_id;
field_by_number(<<"172">>) -> settl_delivery_type;
field_by_number(<<"173">>) -> settl_depository_code;
field_by_number(<<"174">>) -> settl_brkr_code;
field_by_number(<<"175">>) -> settl_inst_code;
field_by_number(<<"176">>) -> security_settl_agent_name;
field_by_number(<<"177">>) -> security_settl_agent_code;
field_by_number(<<"178">>) -> security_settl_agent_acct_num;
field_by_number(<<"179">>) -> security_settl_agent_acct_name;
field_by_number(<<"180">>) -> security_settl_agent_contact_name;
field_by_number(<<"181">>) -> security_settl_agent_contact_phone;
field_by_number(<<"182">>) -> cash_settl_agent_name;
field_by_number(<<"183">>) -> cash_settl_agent_code;
field_by_number(<<"184">>) -> cash_settl_agent_acct_num;
field_by_number(<<"185">>) -> cash_settl_agent_acct_name;
field_by_number(<<"186">>) -> cash_settl_agent_contact_name;
field_by_number(<<"187">>) -> cash_settl_agent_contact_phone;
field_by_number(<<"188">>) -> bid_spot_rate;
field_by_number(<<"189">>) -> bid_forward_points;
field_by_number(<<"190">>) -> offer_spot_rate;
field_by_number(<<"191">>) -> offer_forward_points;
field_by_number(<<"192">>) -> order_qty2;
field_by_number(<<"193">>) -> settl_date2;
field_by_number(<<"194">>) -> last_spot_rate;
field_by_number(<<"195">>) -> last_forward_points;
field_by_number(<<"196">>) -> alloc_link_id;
field_by_number(<<"197">>) -> alloc_link_type;
field_by_number(<<"198">>) -> secondary_order_id;
field_by_number(<<"199">>) -> no_ioi_qualifiers;
field_by_number(<<"200">>) -> maturity_month_year;
field_by_number(<<"201">>) -> put_or_call;
field_by_number(<<"202">>) -> strike_price;
field_by_number(<<"203">>) -> covered_or_uncovered;
field_by_number(<<"204">>) -> customer_or_firm;
field_by_number(<<"205">>) -> maturity_day;
field_by_number(<<"206">>) -> opt_attribute;
field_by_number(<<"207">>) -> security_exchange;
field_by_number(<<"208">>) -> notify_broker_of_credit;
field_by_number(<<"209">>) -> alloc_handl_inst;
field_by_number(<<"210">>) -> max_show;
field_by_number(<<"211">>) -> peg_offset_value;
field_by_number(<<"212">>) -> xml_data_len;
field_by_number(<<"213">>) -> xml_data;
field_by_number(<<"214">>) -> settl_inst_ref_id;
field_by_number(<<"215">>) -> no_routing_ids;
field_by_number(<<"216">>) -> routing_type;
field_by_number(<<"217">>) -> routing_id;
field_by_number(<<"218">>) -> spread;
field_by_number(<<"219">>) -> benchmark;
field_by_number(<<"220">>) -> benchmark_curve_currency;
field_by_number(<<"221">>) -> benchmark_curve_name;
field_by_number(<<"222">>) -> benchmark_curve_point;
field_by_number(<<"223">>) -> coupon_rate;
field_by_number(<<"224">>) -> coupon_payment_date;
field_by_number(<<"225">>) -> issue_date;
field_by_number(<<"226">>) -> repurchase_term;
field_by_number(<<"227">>) -> repurchase_rate;
field_by_number(<<"228">>) -> factor;
field_by_number(<<"229">>) -> trade_origination_date;
field_by_number(<<"230">>) -> ex_date;
field_by_number(<<"231">>) -> contract_multiplier;
field_by_number(<<"232">>) -> no_stipulations;
field_by_number(<<"233">>) -> stipulation_type;
field_by_number(<<"234">>) -> stipulation_value;
field_by_number(<<"235">>) -> yield_type;
field_by_number(<<"236">>) -> yield;
field_by_number(<<"237">>) -> total_takedown;
field_by_number(<<"238">>) -> concession;
field_by_number(<<"239">>) -> repo_collateral_security_type;
field_by_number(<<"240">>) -> redemption_date;
field_by_number(<<"241">>) -> underlying_coupon_payment_date;
field_by_number(<<"242">>) -> underlying_issue_date;
field_by_number(<<"243">>) -> underlying_repo_collateral_security_type;
field_by_number(<<"244">>) -> underlying_repurchase_term;
field_by_number(<<"245">>) -> underlying_repurchase_rate;
field_by_number(<<"246">>) -> underlying_factor;
field_by_number(<<"247">>) -> underlying_redemption_date;
field_by_number(<<"248">>) -> leg_coupon_payment_date;
field_by_number(<<"249">>) -> leg_issue_date;
field_by_number(<<"250">>) -> leg_repo_collateral_security_type;
field_by_number(<<"251">>) -> leg_repurchase_term;
field_by_number(<<"252">>) -> leg_repurchase_rate;
field_by_number(<<"253">>) -> leg_factor;
field_by_number(<<"254">>) -> leg_redemption_date;
field_by_number(<<"255">>) -> credit_rating;
field_by_number(<<"256">>) -> underlying_credit_rating;
field_by_number(<<"257">>) -> leg_credit_rating;
field_by_number(<<"258">>) -> traded_flat_switch;
field_by_number(<<"259">>) -> basis_feature_date;
field_by_number(<<"260">>) -> basis_feature_price;
field_by_number(<<"262">>) -> md_req_id;
field_by_number(<<"263">>) -> subscription_request_type;
field_by_number(<<"264">>) -> market_depth;
field_by_number(<<"265">>) -> md_update_type;
field_by_number(<<"266">>) -> aggregated_book;
field_by_number(<<"267">>) -> no_md_entry_types;
field_by_number(<<"268">>) -> no_md_entries;
field_by_number(<<"269">>) -> md_entry_type;
field_by_number(<<"270">>) -> md_entry_px;
field_by_number(<<"271">>) -> md_entry_size;
field_by_number(<<"272">>) -> md_entry_date;
field_by_number(<<"273">>) -> md_entry_time;
field_by_number(<<"274">>) -> tick_direction;
field_by_number(<<"275">>) -> md_mkt;
field_by_number(<<"276">>) -> quote_condition;
field_by_number(<<"277">>) -> trade_condition;
field_by_number(<<"278">>) -> md_entry_id;
field_by_number(<<"279">>) -> md_update_action;
field_by_number(<<"280">>) -> md_entry_ref_id;
field_by_number(<<"281">>) -> md_req_rej_reason;
field_by_number(<<"282">>) -> md_entry_originator;
field_by_number(<<"283">>) -> location_id;
field_by_number(<<"284">>) -> desk_id;
field_by_number(<<"285">>) -> delete_reason;
field_by_number(<<"286">>) -> open_close_settl_flag;
field_by_number(<<"287">>) -> seller_days;
field_by_number(<<"288">>) -> md_entry_buyer;
field_by_number(<<"289">>) -> md_entry_seller;
field_by_number(<<"290">>) -> md_entry_position_no;
field_by_number(<<"291">>) -> financial_status;
field_by_number(<<"292">>) -> corporate_action;
field_by_number(<<"293">>) -> def_bid_size;
field_by_number(<<"294">>) -> def_offer_size;
field_by_number(<<"295">>) -> no_quote_entries;
field_by_number(<<"296">>) -> no_quote_sets;
field_by_number(<<"297">>) -> quote_status;
field_by_number(<<"298">>) -> quote_cancel_type;
field_by_number(<<"299">>) -> quote_entry_id;
field_by_number(<<"300">>) -> quote_reject_reason;
field_by_number(<<"301">>) -> quote_response_level;
field_by_number(<<"302">>) -> quote_set_id;
field_by_number(<<"303">>) -> quote_request_type;
field_by_number(<<"304">>) -> tot_no_quote_entries;
field_by_number(<<"305">>) -> underlying_security_id_source;
field_by_number(<<"306">>) -> underlying_issuer;
field_by_number(<<"307">>) -> underlying_security_desc;
field_by_number(<<"308">>) -> underlying_security_exchange;
field_by_number(<<"309">>) -> underlying_security_id;
field_by_number(<<"310">>) -> underlying_security_type;
field_by_number(<<"311">>) -> underlying_symbol;
field_by_number(<<"312">>) -> underlying_symbol_sfx;
field_by_number(<<"313">>) -> underlying_maturity_month_year;
field_by_number(<<"314">>) -> underlying_maturity_day;
field_by_number(<<"315">>) -> underlying_put_or_call;
field_by_number(<<"316">>) -> underlying_strike_price;
field_by_number(<<"317">>) -> underlying_opt_attribute;
field_by_number(<<"318">>) -> underlying_currency;
field_by_number(<<"319">>) -> ratio_qty;
field_by_number(<<"320">>) -> security_req_id;
field_by_number(<<"321">>) -> security_request_type;
field_by_number(<<"322">>) -> security_response_id;
field_by_number(<<"323">>) -> security_response_type;
field_by_number(<<"324">>) -> security_status_req_id;
field_by_number(<<"325">>) -> unsolicited_indicator;
field_by_number(<<"326">>) -> security_trading_status;
field_by_number(<<"327">>) -> halt_reason_char;
field_by_number(<<"328">>) -> in_view_of_common;
field_by_number(<<"329">>) -> due_to_related;
field_by_number(<<"330">>) -> buy_volume;
field_by_number(<<"331">>) -> sell_volume;
field_by_number(<<"332">>) -> high_px;
field_by_number(<<"333">>) -> low_px;
field_by_number(<<"334">>) -> adjustment;
field_by_number(<<"335">>) -> trad_ses_req_id;
field_by_number(<<"336">>) -> trading_session_id;
field_by_number(<<"337">>) -> contra_trader;
field_by_number(<<"338">>) -> trad_ses_method;
field_by_number(<<"339">>) -> trad_ses_mode;
field_by_number(<<"340">>) -> trad_ses_status;
field_by_number(<<"341">>) -> trad_ses_start_time;
field_by_number(<<"342">>) -> trad_ses_open_time;
field_by_number(<<"343">>) -> trad_ses_pre_close_time;
field_by_number(<<"344">>) -> trad_ses_close_time;
field_by_number(<<"345">>) -> trad_ses_end_time;
field_by_number(<<"346">>) -> number_of_orders;
field_by_number(<<"347">>) -> message_encoding;
field_by_number(<<"348">>) -> encoded_issuer_len;
field_by_number(<<"349">>) -> encoded_issuer;
field_by_number(<<"350">>) -> encoded_security_desc_len;
field_by_number(<<"351">>) -> encoded_security_desc;
field_by_number(<<"352">>) -> encoded_list_exec_inst_len;
field_by_number(<<"353">>) -> encoded_list_exec_inst;
field_by_number(<<"354">>) -> encoded_text_len;
field_by_number(<<"355">>) -> encoded_text;
field_by_number(<<"356">>) -> encoded_subject_len;
field_by_number(<<"357">>) -> encoded_subject;
field_by_number(<<"358">>) -> encoded_headline_len;
field_by_number(<<"359">>) -> encoded_headline;
field_by_number(<<"360">>) -> encoded_alloc_text_len;
field_by_number(<<"361">>) -> encoded_alloc_text;
field_by_number(<<"362">>) -> encoded_underlying_issuer_len;
field_by_number(<<"363">>) -> encoded_underlying_issuer;
field_by_number(<<"364">>) -> encoded_underlying_security_desc_len;
field_by_number(<<"365">>) -> encoded_underlying_security_desc;
field_by_number(<<"366">>) -> alloc_price;
field_by_number(<<"367">>) -> quote_set_valid_until_time;
field_by_number(<<"368">>) -> quote_entry_reject_reason;
field_by_number(<<"369">>) -> last_msg_seq_num_processed;
field_by_number(<<"370">>) -> on_behalf_of_sending_time;
field_by_number(<<"371">>) -> ref_tag_id;
field_by_number(<<"372">>) -> ref_msg_type;
field_by_number(<<"373">>) -> session_reject_reason;
field_by_number(<<"374">>) -> bid_request_trans_type;
field_by_number(<<"375">>) -> contra_broker;
field_by_number(<<"376">>) -> compliance_id;
field_by_number(<<"377">>) -> solicited_flag;
field_by_number(<<"378">>) -> exec_restatement_reason;
field_by_number(<<"379">>) -> business_reject_ref_id;
field_by_number(<<"380">>) -> business_reject_reason;
field_by_number(<<"381">>) -> gross_trade_amt;
field_by_number(<<"382">>) -> no_contra_brokers;
field_by_number(<<"383">>) -> max_message_size;
field_by_number(<<"384">>) -> no_msg_types;
field_by_number(<<"385">>) -> msg_direction;
field_by_number(<<"386">>) -> no_trading_sessions;
field_by_number(<<"387">>) -> total_volume_traded;
field_by_number(<<"388">>) -> discretion_inst;
field_by_number(<<"389">>) -> discretion_offset_value;
field_by_number(<<"390">>) -> bid_id;
field_by_number(<<"391">>) -> client_bid_id;
field_by_number(<<"392">>) -> list_name;
field_by_number(<<"393">>) -> tot_no_related_sym;
field_by_number(<<"394">>) -> bid_type;
field_by_number(<<"395">>) -> num_tickets;
field_by_number(<<"396">>) -> side_value1;
field_by_number(<<"397">>) -> side_value2;
field_by_number(<<"398">>) -> no_bid_descriptors;
field_by_number(<<"399">>) -> bid_descriptor_type;
field_by_number(<<"400">>) -> bid_descriptor;
field_by_number(<<"401">>) -> side_value_ind;
field_by_number(<<"402">>) -> liquidity_pct_low;
field_by_number(<<"403">>) -> liquidity_pct_high;
field_by_number(<<"404">>) -> liquidity_value;
field_by_number(<<"405">>) -> efp_tracking_error;
field_by_number(<<"406">>) -> fair_value;
field_by_number(<<"407">>) -> outside_index_pct;
field_by_number(<<"408">>) -> value_of_futures;
field_by_number(<<"409">>) -> liquidity_ind_type;
field_by_number(<<"410">>) -> wt_average_liquidity;
field_by_number(<<"411">>) -> exchange_for_physical;
field_by_number(<<"412">>) -> out_main_cntry_u_index;
field_by_number(<<"413">>) -> cross_percent;
field_by_number(<<"414">>) -> prog_rpt_reqs;
field_by_number(<<"415">>) -> prog_period_interval;
field_by_number(<<"416">>) -> inc_tax_ind;
field_by_number(<<"417">>) -> num_bidders;
field_by_number(<<"418">>) -> bid_trade_type;
field_by_number(<<"419">>) -> basis_px_type;
field_by_number(<<"420">>) -> no_bid_components;
field_by_number(<<"421">>) -> country;
field_by_number(<<"422">>) -> tot_no_strikes;
field_by_number(<<"423">>) -> price_type;
field_by_number(<<"424">>) -> day_order_qty;
field_by_number(<<"425">>) -> day_cum_qty;
field_by_number(<<"426">>) -> day_avg_px;
field_by_number(<<"427">>) -> gt_booking_inst;
field_by_number(<<"428">>) -> no_strikes;
field_by_number(<<"429">>) -> list_status_type;
field_by_number(<<"430">>) -> net_gross_ind;
field_by_number(<<"431">>) -> list_order_status;
field_by_number(<<"432">>) -> expire_date;
field_by_number(<<"433">>) -> list_exec_inst_type;
field_by_number(<<"434">>) -> cxl_rej_response_to;
field_by_number(<<"435">>) -> underlying_coupon_rate;
field_by_number(<<"436">>) -> underlying_contract_multiplier;
field_by_number(<<"437">>) -> contra_trade_qty;
field_by_number(<<"438">>) -> contra_trade_time;
field_by_number(<<"439">>) -> clearing_firm;
field_by_number(<<"440">>) -> clearing_account;
field_by_number(<<"441">>) -> liquidity_num_securities;
field_by_number(<<"442">>) -> multi_leg_reporting_type;
field_by_number(<<"443">>) -> strike_time;
field_by_number(<<"444">>) -> list_status_text;
field_by_number(<<"445">>) -> encoded_list_status_text_len;
field_by_number(<<"446">>) -> encoded_list_status_text;
field_by_number(<<"447">>) -> party_id_source;
field_by_number(<<"448">>) -> party_id;
field_by_number(<<"449">>) -> total_volume_traded_date;
field_by_number(<<"450">>) -> total_volume_traded_time;
field_by_number(<<"451">>) -> net_chg_prev_day;
field_by_number(<<"452">>) -> party_role;
field_by_number(<<"453">>) -> no_party_ids;
field_by_number(<<"454">>) -> no_security_alt_id;
field_by_number(<<"455">>) -> security_alt_id;
field_by_number(<<"456">>) -> security_alt_id_source;
field_by_number(<<"457">>) -> no_underlying_security_alt_id;
field_by_number(<<"458">>) -> underlying_security_alt_id;
field_by_number(<<"459">>) -> underlying_security_alt_id_source;
field_by_number(<<"460">>) -> product;
field_by_number(<<"461">>) -> cfi_code;
field_by_number(<<"462">>) -> underlying_product;
field_by_number(<<"463">>) -> underlying_cfi_code;
field_by_number(<<"464">>) -> test_message_indicator;
field_by_number(<<"465">>) -> quantity_type;
field_by_number(<<"466">>) -> booking_ref_id;
field_by_number(<<"467">>) -> individual_alloc_id;
field_by_number(<<"468">>) -> rounding_direction;
field_by_number(<<"469">>) -> rounding_modulus;
field_by_number(<<"470">>) -> country_of_issue;
field_by_number(<<"471">>) -> state_or_province_of_issue;
field_by_number(<<"472">>) -> locale_of_issue;
field_by_number(<<"473">>) -> no_regist_dtls;
field_by_number(<<"474">>) -> mailing_dtls;
field_by_number(<<"475">>) -> investor_country_of_residence;
field_by_number(<<"476">>) -> payment_ref;
field_by_number(<<"477">>) -> distrib_payment_method;
field_by_number(<<"478">>) -> cash_distrib_curr;
field_by_number(<<"479">>) -> comm_currency;
field_by_number(<<"480">>) -> cancellation_rights;
field_by_number(<<"481">>) -> money_laundering_status;
field_by_number(<<"482">>) -> mailing_inst;
field_by_number(<<"483">>) -> trans_bkd_time;
field_by_number(<<"484">>) -> exec_price_type;
field_by_number(<<"485">>) -> exec_price_adjustment;
field_by_number(<<"486">>) -> date_of_birth;
field_by_number(<<"487">>) -> trade_report_trans_type;
field_by_number(<<"488">>) -> card_holder_name;
field_by_number(<<"489">>) -> card_number;
field_by_number(<<"490">>) -> card_exp_date;
field_by_number(<<"491">>) -> card_iss_num;
field_by_number(<<"492">>) -> payment_method;
field_by_number(<<"493">>) -> regist_acct_type;
field_by_number(<<"494">>) -> designation;
field_by_number(<<"495">>) -> tax_advantage_type;
field_by_number(<<"496">>) -> regist_rej_reason_text;
field_by_number(<<"497">>) -> fund_renew_waiv;
field_by_number(<<"498">>) -> cash_distrib_agent_name;
field_by_number(<<"499">>) -> cash_distrib_agent_code;
field_by_number(<<"500">>) -> cash_distrib_agent_acct_number;
field_by_number(<<"501">>) -> cash_distrib_pay_ref;
field_by_number(<<"502">>) -> cash_distrib_agent_acct_name;
field_by_number(<<"503">>) -> card_start_date;
field_by_number(<<"504">>) -> payment_date;
field_by_number(<<"505">>) -> payment_remitter_id;
field_by_number(<<"506">>) -> regist_status;
field_by_number(<<"507">>) -> regist_rej_reason_code;
field_by_number(<<"508">>) -> regist_ref_id;
field_by_number(<<"509">>) -> regist_dtls;
field_by_number(<<"510">>) -> no_distrib_insts;
field_by_number(<<"511">>) -> regist_email;
field_by_number(<<"512">>) -> distrib_percentage;
field_by_number(<<"513">>) -> regist_id;
field_by_number(<<"514">>) -> regist_trans_type;
field_by_number(<<"515">>) -> exec_valuation_point;
field_by_number(<<"516">>) -> order_percent;
field_by_number(<<"517">>) -> ownership_type;
field_by_number(<<"518">>) -> no_cont_amts;
field_by_number(<<"519">>) -> cont_amt_type;
field_by_number(<<"520">>) -> cont_amt_value;
field_by_number(<<"521">>) -> cont_amt_curr;
field_by_number(<<"522">>) -> owner_type;
field_by_number(<<"523">>) -> party_sub_id;
field_by_number(<<"524">>) -> nested_party_id;
field_by_number(<<"525">>) -> nested_party_id_source;
field_by_number(<<"526">>) -> secondary_cl_ord_id;
field_by_number(<<"527">>) -> secondary_exec_id;
field_by_number(<<"528">>) -> order_capacity;
field_by_number(<<"529">>) -> order_restrictions;
field_by_number(<<"530">>) -> mass_cancel_request_type;
field_by_number(<<"531">>) -> mass_cancel_response;
field_by_number(<<"532">>) -> mass_cancel_reject_reason;
field_by_number(<<"533">>) -> total_affected_orders;
field_by_number(<<"534">>) -> no_affected_orders;
field_by_number(<<"535">>) -> affected_order_id;
field_by_number(<<"536">>) -> affected_secondary_order_id;
field_by_number(<<"537">>) -> quote_type;
field_by_number(<<"538">>) -> nested_party_role;
field_by_number(<<"539">>) -> no_nested_party_ids;
field_by_number(<<"540">>) -> total_accrued_interest_amt;
field_by_number(<<"541">>) -> maturity_date;
field_by_number(<<"542">>) -> underlying_maturity_date;
field_by_number(<<"543">>) -> instr_registry;
field_by_number(<<"544">>) -> cash_margin;
field_by_number(<<"545">>) -> nested_party_sub_id;
field_by_number(<<"546">>) -> scope;
field_by_number(<<"547">>) -> md_implicit_delete;
field_by_number(<<"548">>) -> cross_id;
field_by_number(<<"549">>) -> cross_type;
field_by_number(<<"550">>) -> cross_prioritization;
field_by_number(<<"551">>) -> orig_cross_id;
field_by_number(<<"552">>) -> no_sides;
field_by_number(<<"553">>) -> username;
field_by_number(<<"554">>) -> password;
field_by_number(<<"555">>) -> no_legs;
field_by_number(<<"556">>) -> leg_currency;
field_by_number(<<"557">>) -> tot_no_security_types;
field_by_number(<<"558">>) -> no_security_types;
field_by_number(<<"559">>) -> security_list_request_type;
field_by_number(<<"560">>) -> security_request_result;
field_by_number(<<"561">>) -> round_lot;
field_by_number(<<"562">>) -> min_trade_vol;
field_by_number(<<"563">>) -> multi_leg_rpt_type_req;
field_by_number(<<"564">>) -> leg_position_effect;
field_by_number(<<"565">>) -> leg_covered_or_uncovered;
field_by_number(<<"566">>) -> leg_price;
field_by_number(<<"567">>) -> trad_ses_status_rej_reason;
field_by_number(<<"568">>) -> trade_request_id;
field_by_number(<<"569">>) -> trade_request_type;
field_by_number(<<"570">>) -> previously_reported;
field_by_number(<<"571">>) -> trade_report_id;
field_by_number(<<"572">>) -> trade_report_ref_id;
field_by_number(<<"573">>) -> match_status;
field_by_number(<<"574">>) -> match_type;
field_by_number(<<"575">>) -> odd_lot;
field_by_number(<<"576">>) -> no_clearing_instructions;
field_by_number(<<"577">>) -> clearing_instruction;
field_by_number(<<"578">>) -> trade_input_source;
field_by_number(<<"579">>) -> trade_input_device;
field_by_number(<<"580">>) -> no_dates;
field_by_number(<<"581">>) -> account_type;
field_by_number(<<"582">>) -> cust_order_capacity;
field_by_number(<<"583">>) -> cl_ord_link_id;
field_by_number(<<"584">>) -> mass_status_req_id;
field_by_number(<<"585">>) -> mass_status_req_type;
field_by_number(<<"586">>) -> orig_ord_mod_time;
field_by_number(<<"587">>) -> leg_settl_type;
field_by_number(<<"588">>) -> leg_settl_date;
field_by_number(<<"589">>) -> day_booking_inst;
field_by_number(<<"590">>) -> booking_unit;
field_by_number(<<"591">>) -> prealloc_method;
field_by_number(<<"592">>) -> underlying_country_of_issue;
field_by_number(<<"593">>) -> underlying_state_or_province_of_issue;
field_by_number(<<"594">>) -> underlying_locale_of_issue;
field_by_number(<<"595">>) -> underlying_instr_registry;
field_by_number(<<"596">>) -> leg_country_of_issue;
field_by_number(<<"597">>) -> leg_state_or_province_of_issue;
field_by_number(<<"598">>) -> leg_locale_of_issue;
field_by_number(<<"599">>) -> leg_instr_registry;
field_by_number(<<"600">>) -> leg_symbol;
field_by_number(<<"601">>) -> leg_symbol_sfx;
field_by_number(<<"602">>) -> leg_security_id;
field_by_number(<<"603">>) -> leg_security_id_source;
field_by_number(<<"604">>) -> no_leg_security_alt_id;
field_by_number(<<"605">>) -> leg_security_alt_id;
field_by_number(<<"606">>) -> leg_security_alt_id_source;
field_by_number(<<"607">>) -> leg_product;
field_by_number(<<"608">>) -> leg_cfi_code;
field_by_number(<<"609">>) -> leg_security_type;
field_by_number(<<"610">>) -> leg_maturity_month_year;
field_by_number(<<"611">>) -> leg_maturity_date;
field_by_number(<<"612">>) -> leg_strike_price;
field_by_number(<<"613">>) -> leg_opt_attribute;
field_by_number(<<"614">>) -> leg_contract_multiplier;
field_by_number(<<"615">>) -> leg_coupon_rate;
field_by_number(<<"616">>) -> leg_security_exchange;
field_by_number(<<"617">>) -> leg_issuer;
field_by_number(<<"618">>) -> encoded_leg_issuer_len;
field_by_number(<<"619">>) -> encoded_leg_issuer;
field_by_number(<<"620">>) -> leg_security_desc;
field_by_number(<<"621">>) -> encoded_leg_security_desc_len;
field_by_number(<<"622">>) -> encoded_leg_security_desc;
field_by_number(<<"623">>) -> leg_ratio_qty;
field_by_number(<<"624">>) -> leg_side;
field_by_number(<<"625">>) -> trading_session_sub_id;
field_by_number(<<"626">>) -> alloc_type;
field_by_number(<<"627">>) -> no_hops;
field_by_number(<<"628">>) -> hop_comp_id;
field_by_number(<<"629">>) -> hop_sending_time;
field_by_number(<<"630">>) -> hop_ref_id;
field_by_number(<<"631">>) -> mid_px;
field_by_number(<<"632">>) -> bid_yield;
field_by_number(<<"633">>) -> mid_yield;
field_by_number(<<"634">>) -> offer_yield;
field_by_number(<<"635">>) -> clearing_fee_indicator;
field_by_number(<<"636">>) -> working_indicator;
field_by_number(<<"637">>) -> leg_last_px;
field_by_number(<<"638">>) -> priority_indicator;
field_by_number(<<"639">>) -> price_improvement;
field_by_number(<<"640">>) -> price2;
field_by_number(<<"641">>) -> last_forward_points2;
field_by_number(<<"642">>) -> bid_forward_points2;
field_by_number(<<"643">>) -> offer_forward_points2;
field_by_number(<<"644">>) -> rfq_req_id;
field_by_number(<<"645">>) -> mkt_bid_px;
field_by_number(<<"646">>) -> mkt_offer_px;
field_by_number(<<"647">>) -> min_bid_size;
field_by_number(<<"648">>) -> min_offer_size;
field_by_number(<<"649">>) -> quote_status_req_id;
field_by_number(<<"650">>) -> legal_confirm;
field_by_number(<<"651">>) -> underlying_last_px;
field_by_number(<<"652">>) -> underlying_last_qty;
field_by_number(<<"653">>) -> sec_def_status;
field_by_number(<<"654">>) -> leg_ref_id;
field_by_number(<<"655">>) -> contra_leg_ref_id;
field_by_number(<<"656">>) -> settl_curr_bid_fx_rate;
field_by_number(<<"657">>) -> settl_curr_offer_fx_rate;
field_by_number(<<"658">>) -> quote_request_reject_reason;
field_by_number(<<"659">>) -> side_compliance_id;
field_by_number(<<"660">>) -> acct_id_source;
field_by_number(<<"661">>) -> alloc_acct_id_source;
field_by_number(<<"662">>) -> benchmark_price;
field_by_number(<<"663">>) -> benchmark_price_type;
field_by_number(<<"664">>) -> confirm_id;
field_by_number(<<"665">>) -> confirm_status;
field_by_number(<<"666">>) -> confirm_trans_type;
field_by_number(<<"667">>) -> contract_settl_month;
field_by_number(<<"668">>) -> delivery_form;
field_by_number(<<"669">>) -> last_par_px;
field_by_number(<<"670">>) -> no_leg_allocs;
field_by_number(<<"671">>) -> leg_alloc_account;
field_by_number(<<"672">>) -> leg_individual_alloc_id;
field_by_number(<<"673">>) -> leg_alloc_qty;
field_by_number(<<"674">>) -> leg_alloc_acct_id_source;
field_by_number(<<"675">>) -> leg_settl_currency;
field_by_number(<<"676">>) -> leg_benchmark_curve_currency;
field_by_number(<<"677">>) -> leg_benchmark_curve_name;
field_by_number(<<"678">>) -> leg_benchmark_curve_point;
field_by_number(<<"679">>) -> leg_benchmark_price;
field_by_number(<<"680">>) -> leg_benchmark_price_type;
field_by_number(<<"681">>) -> leg_bid_px;
field_by_number(<<"682">>) -> leg_ioi_qty;
field_by_number(<<"683">>) -> no_leg_stipulations;
field_by_number(<<"684">>) -> leg_offer_px;
field_by_number(<<"685">>) -> leg_order_qty;
field_by_number(<<"686">>) -> leg_price_type;
field_by_number(<<"687">>) -> leg_qty;
field_by_number(<<"688">>) -> leg_stipulation_type;
field_by_number(<<"689">>) -> leg_stipulation_value;
field_by_number(<<"690">>) -> leg_swap_type;
field_by_number(<<"691">>) -> pool;
field_by_number(<<"692">>) -> quote_price_type;
field_by_number(<<"693">>) -> quote_resp_id;
field_by_number(<<"694">>) -> quote_resp_type;
field_by_number(<<"695">>) -> quote_qualifier;
field_by_number(<<"696">>) -> yield_redemption_date;
field_by_number(<<"697">>) -> yield_redemption_price;
field_by_number(<<"698">>) -> yield_redemption_price_type;
field_by_number(<<"699">>) -> benchmark_security_id;
field_by_number(<<"700">>) -> reversal_indicator;
field_by_number(<<"701">>) -> yield_calc_date;
field_by_number(<<"702">>) -> no_positions;
field_by_number(<<"703">>) -> pos_type;
field_by_number(<<"704">>) -> long_qty;
field_by_number(<<"705">>) -> short_qty;
field_by_number(<<"706">>) -> pos_qty_status;
field_by_number(<<"707">>) -> pos_amt_type;
field_by_number(<<"708">>) -> pos_amt;
field_by_number(<<"709">>) -> pos_trans_type;
field_by_number(<<"710">>) -> pos_req_id;
field_by_number(<<"711">>) -> no_underlyings;
field_by_number(<<"712">>) -> pos_maint_action;
field_by_number(<<"713">>) -> orig_pos_req_ref_id;
field_by_number(<<"714">>) -> pos_maint_rpt_ref_id;
field_by_number(<<"715">>) -> clearing_business_date;
field_by_number(<<"716">>) -> settl_sess_id;
field_by_number(<<"717">>) -> settl_sess_sub_id;
field_by_number(<<"718">>) -> adjustment_type;
field_by_number(<<"719">>) -> contrary_instruction_indicator;
field_by_number(<<"720">>) -> prior_spread_indicator;
field_by_number(<<"721">>) -> pos_maint_rpt_id;
field_by_number(<<"722">>) -> pos_maint_status;
field_by_number(<<"723">>) -> pos_maint_result;
field_by_number(<<"724">>) -> pos_req_type;
field_by_number(<<"725">>) -> response_transport_type;
field_by_number(<<"726">>) -> response_destination;
field_by_number(<<"727">>) -> total_num_pos_reports;
field_by_number(<<"728">>) -> pos_req_result;
field_by_number(<<"729">>) -> pos_req_status;
field_by_number(<<"730">>) -> settl_price;
field_by_number(<<"731">>) -> settl_price_type;
field_by_number(<<"732">>) -> underlying_settl_price;
field_by_number(<<"733">>) -> underlying_settl_price_type;
field_by_number(<<"734">>) -> prior_settl_price;
field_by_number(<<"735">>) -> no_quote_qualifiers;
field_by_number(<<"736">>) -> alloc_settl_currency;
field_by_number(<<"737">>) -> alloc_settl_curr_amt;
field_by_number(<<"738">>) -> interest_at_maturity;
field_by_number(<<"739">>) -> leg_dated_date;
field_by_number(<<"740">>) -> leg_pool;
field_by_number(<<"741">>) -> alloc_interest_at_maturity;
field_by_number(<<"742">>) -> alloc_accrued_interest_amt;
field_by_number(<<"743">>) -> delivery_date;
field_by_number(<<"744">>) -> assignment_method;
field_by_number(<<"745">>) -> assignment_unit;
field_by_number(<<"746">>) -> open_interest;
field_by_number(<<"747">>) -> exercise_method;
field_by_number(<<"748">>) -> tot_num_trade_reports;
field_by_number(<<"749">>) -> trade_request_result;
field_by_number(<<"750">>) -> trade_request_status;
field_by_number(<<"751">>) -> trade_report_reject_reason;
field_by_number(<<"752">>) -> side_multi_leg_reporting_type;
field_by_number(<<"753">>) -> no_pos_amt;
field_by_number(<<"754">>) -> auto_accept_indicator;
field_by_number(<<"755">>) -> alloc_report_id;
field_by_number(<<"756">>) -> no_nested2_party_ids;
field_by_number(<<"757">>) -> nested2_party_id;
field_by_number(<<"758">>) -> nested2_party_id_source;
field_by_number(<<"759">>) -> nested2_party_role;
field_by_number(<<"760">>) -> nested2_party_sub_id;
field_by_number(<<"761">>) -> benchmark_security_id_source;
field_by_number(<<"762">>) -> security_sub_type;
field_by_number(<<"763">>) -> underlying_security_sub_type;
field_by_number(<<"764">>) -> leg_security_sub_type;
field_by_number(<<"765">>) -> allowable_one_sidedness_pct;
field_by_number(<<"766">>) -> allowable_one_sidedness_value;
field_by_number(<<"767">>) -> allowable_one_sidedness_curr;
field_by_number(<<"768">>) -> no_trd_reg_timestamps;
field_by_number(<<"769">>) -> trd_reg_timestamp;
field_by_number(<<"770">>) -> trd_reg_timestamp_type;
field_by_number(<<"771">>) -> trd_reg_timestamp_origin;
field_by_number(<<"772">>) -> confirm_ref_id;
field_by_number(<<"773">>) -> confirm_type;
field_by_number(<<"774">>) -> confirm_rej_reason;
field_by_number(<<"775">>) -> booking_type;
field_by_number(<<"776">>) -> individual_alloc_rej_code;
field_by_number(<<"777">>) -> settl_inst_msg_id;
field_by_number(<<"778">>) -> no_settl_inst;
field_by_number(<<"779">>) -> last_update_time;
field_by_number(<<"780">>) -> alloc_settl_inst_type;
field_by_number(<<"781">>) -> no_settl_party_ids;
field_by_number(<<"782">>) -> settl_party_id;
field_by_number(<<"783">>) -> settl_party_id_source;
field_by_number(<<"784">>) -> settl_party_role;
field_by_number(<<"785">>) -> settl_party_sub_id;
field_by_number(<<"786">>) -> settl_party_sub_id_type;
field_by_number(<<"787">>) -> dlvy_inst_type;
field_by_number(<<"788">>) -> termination_type;
field_by_number(<<"789">>) -> next_expected_msg_seq_num;
field_by_number(<<"790">>) -> ord_status_req_id;
field_by_number(<<"791">>) -> settl_inst_req_id;
field_by_number(<<"792">>) -> settl_inst_req_rej_code;
field_by_number(<<"793">>) -> secondary_alloc_id;
field_by_number(<<"794">>) -> alloc_report_type;
field_by_number(<<"795">>) -> alloc_report_ref_id;
field_by_number(<<"796">>) -> alloc_canc_replace_reason;
field_by_number(<<"797">>) -> copy_msg_indicator;
field_by_number(<<"798">>) -> alloc_account_type;
field_by_number(<<"799">>) -> order_avg_px;
field_by_number(<<"800">>) -> order_booking_qty;
field_by_number(<<"801">>) -> no_settl_party_sub_ids;
field_by_number(<<"802">>) -> no_party_sub_ids;
field_by_number(<<"803">>) -> party_sub_id_type;
field_by_number(<<"804">>) -> no_nested_party_sub_ids;
field_by_number(<<"805">>) -> nested_party_sub_id_type;
field_by_number(<<"806">>) -> no_nested2_party_sub_ids;
field_by_number(<<"807">>) -> nested2_party_sub_id_type;
field_by_number(<<"808">>) -> alloc_intermed_req_type;
field_by_number(<<"810">>) -> underlying_px;
field_by_number(<<"811">>) -> price_delta;
field_by_number(<<"812">>) -> appl_queue_max;
field_by_number(<<"813">>) -> appl_queue_depth;
field_by_number(<<"814">>) -> appl_queue_resolution;
field_by_number(<<"815">>) -> appl_queue_action;
field_by_number(<<"816">>) -> no_alt_md_source;
field_by_number(<<"817">>) -> alt_md_source_id;
field_by_number(<<"818">>) -> secondary_trade_report_id;
field_by_number(<<"819">>) -> avg_px_indicator;
field_by_number(<<"820">>) -> trade_link_id;
field_by_number(<<"821">>) -> order_input_device;
field_by_number(<<"822">>) -> underlying_trading_session_id;
field_by_number(<<"823">>) -> underlying_trading_session_sub_id;
field_by_number(<<"824">>) -> trade_leg_ref_id;
field_by_number(<<"825">>) -> exchange_rule;
field_by_number(<<"826">>) -> trade_alloc_indicator;
field_by_number(<<"827">>) -> expiration_cycle;
field_by_number(<<"828">>) -> trd_type;
field_by_number(<<"829">>) -> trd_sub_type;
field_by_number(<<"830">>) -> transfer_reason;
field_by_number(<<"831">>) -> asgn_req_id;
field_by_number(<<"832">>) -> tot_num_assignment_reports;
field_by_number(<<"833">>) -> asgn_rpt_id;
field_by_number(<<"834">>) -> threshold_amount;
field_by_number(<<"835">>) -> peg_move_type;
field_by_number(<<"836">>) -> peg_offset_type;
field_by_number(<<"837">>) -> peg_limit_type;
field_by_number(<<"838">>) -> peg_round_direction;
field_by_number(<<"839">>) -> pegged_price;
field_by_number(<<"840">>) -> peg_scope;
field_by_number(<<"841">>) -> discretion_move_type;
field_by_number(<<"842">>) -> discretion_offset_type;
field_by_number(<<"843">>) -> discretion_limit_type;
field_by_number(<<"844">>) -> discretion_round_direction;
field_by_number(<<"845">>) -> discretion_price;
field_by_number(<<"846">>) -> discretion_scope;
field_by_number(<<"847">>) -> target_strategy;
field_by_number(<<"848">>) -> target_strategy_parameters;
field_by_number(<<"849">>) -> participation_rate;
field_by_number(<<"850">>) -> target_strategy_performance;
field_by_number(<<"851">>) -> last_liquidity_ind;
field_by_number(<<"852">>) -> publish_trd_indicator;
field_by_number(<<"853">>) -> short_sale_reason;
field_by_number(<<"854">>) -> qty_type;
field_by_number(<<"855">>) -> secondary_trd_type;
field_by_number(<<"856">>) -> trade_report_type;
field_by_number(<<"857">>) -> alloc_no_orders_type;
field_by_number(<<"858">>) -> shared_commission;
field_by_number(<<"859">>) -> confirm_req_id;
field_by_number(<<"860">>) -> avg_par_px;
field_by_number(<<"861">>) -> reported_px;
field_by_number(<<"862">>) -> no_capacities;
field_by_number(<<"863">>) -> order_capacity_qty;
field_by_number(<<"864">>) -> no_events;
field_by_number(<<"865">>) -> event_type;
field_by_number(<<"866">>) -> event_date;
field_by_number(<<"867">>) -> event_px;
field_by_number(<<"868">>) -> event_text;
field_by_number(<<"869">>) -> pct_at_risk;
field_by_number(<<"870">>) -> no_instr_attrib;
field_by_number(<<"871">>) -> instr_attrib_type;
field_by_number(<<"872">>) -> instr_attrib_value;
field_by_number(<<"873">>) -> dated_date;
field_by_number(<<"874">>) -> interest_accrual_date;
field_by_number(<<"875">>) -> cp_program;
field_by_number(<<"876">>) -> cp_reg_type;
field_by_number(<<"877">>) -> underlying_cp_program;
field_by_number(<<"878">>) -> underlying_cp_reg_type;
field_by_number(<<"879">>) -> underlying_qty;
field_by_number(<<"880">>) -> trd_match_id;
field_by_number(<<"881">>) -> secondary_trade_report_ref_id;
field_by_number(<<"882">>) -> underlying_dirty_price;
field_by_number(<<"883">>) -> underlying_end_price;
field_by_number(<<"884">>) -> underlying_start_value;
field_by_number(<<"885">>) -> underlying_current_value;
field_by_number(<<"886">>) -> underlying_end_value;
field_by_number(<<"887">>) -> no_underlying_stips;
field_by_number(<<"888">>) -> underlying_stip_type;
field_by_number(<<"889">>) -> underlying_stip_value;
field_by_number(<<"890">>) -> maturity_net_money;
field_by_number(<<"891">>) -> misc_fee_basis;
field_by_number(<<"892">>) -> tot_no_allocs;
field_by_number(<<"893">>) -> last_fragment;
field_by_number(<<"894">>) -> coll_req_id;
field_by_number(<<"895">>) -> coll_asgn_reason;
field_by_number(<<"896">>) -> coll_inquiry_qualifier;
field_by_number(<<"897">>) -> no_trades;
field_by_number(<<"898">>) -> margin_ratio;
field_by_number(<<"899">>) -> margin_excess;
field_by_number(<<"900">>) -> total_net_value;
field_by_number(<<"901">>) -> cash_outstanding;
field_by_number(<<"902">>) -> coll_asgn_id;
field_by_number(<<"903">>) -> coll_asgn_trans_type;
field_by_number(<<"904">>) -> coll_resp_id;
field_by_number(<<"905">>) -> coll_asgn_resp_type;
field_by_number(<<"906">>) -> coll_asgn_reject_reason;
field_by_number(<<"907">>) -> coll_asgn_ref_id;
field_by_number(<<"908">>) -> coll_rpt_id;
field_by_number(<<"909">>) -> coll_inquiry_id;
field_by_number(<<"910">>) -> coll_status;
field_by_number(<<"911">>) -> tot_num_reports;
field_by_number(<<"912">>) -> last_rpt_requested;
field_by_number(<<"913">>) -> agreement_desc;
field_by_number(<<"914">>) -> agreement_id;
field_by_number(<<"915">>) -> agreement_date;
field_by_number(<<"916">>) -> start_date;
field_by_number(<<"917">>) -> end_date;
field_by_number(<<"918">>) -> agreement_currency;
field_by_number(<<"919">>) -> delivery_type;
field_by_number(<<"920">>) -> end_accrued_interest_amt;
field_by_number(<<"921">>) -> start_cash;
field_by_number(<<"922">>) -> end_cash;
field_by_number(<<"923">>) -> user_request_id;
field_by_number(<<"924">>) -> user_request_type;
field_by_number(<<"925">>) -> new_password;
field_by_number(<<"926">>) -> user_status;
field_by_number(<<"927">>) -> user_status_text;
field_by_number(<<"928">>) -> status_value;
field_by_number(<<"929">>) -> status_text;
field_by_number(<<"930">>) -> ref_comp_id;
field_by_number(<<"931">>) -> ref_sub_id;
field_by_number(<<"932">>) -> network_response_id;
field_by_number(<<"933">>) -> network_request_id;
field_by_number(<<"934">>) -> last_network_response_id;
field_by_number(<<"935">>) -> network_request_type;
field_by_number(<<"936">>) -> no_comp_ids;
field_by_number(<<"937">>) -> network_status_response_type;
field_by_number(<<"938">>) -> no_coll_inquiry_qualifier;
field_by_number(<<"939">>) -> trd_rpt_status;
field_by_number(<<"940">>) -> affirm_status;
field_by_number(<<"941">>) -> underlying_strike_currency;
field_by_number(<<"942">>) -> leg_strike_currency;
field_by_number(<<"943">>) -> time_bracket;
field_by_number(<<"944">>) -> coll_action;
field_by_number(<<"945">>) -> coll_inquiry_status;
field_by_number(<<"946">>) -> coll_inquiry_result;
field_by_number(<<"947">>) -> strike_currency;
field_by_number(<<"948">>) -> no_nested3_party_ids;
field_by_number(<<"949">>) -> nested3_party_id;
field_by_number(<<"950">>) -> nested3_party_id_source;
field_by_number(<<"951">>) -> nested3_party_role;
field_by_number(<<"952">>) -> no_nested3_party_sub_ids;
field_by_number(<<"953">>) -> nested3_party_sub_id;
field_by_number(<<"954">>) -> nested3_party_sub_id_type;
field_by_number(<<"955">>) -> leg_contract_settl_month;
field_by_number(<<"956">>) -> leg_interest_accrual_date;
field_by_number(Key) -> Key.

decode_typed_field(account, V) -> V;
decode_typed_field(adv_id, V) -> V;
decode_typed_field(adv_ref_id, V) -> V;
decode_typed_field(adv_side, V) -> V;
decode_typed_field(adv_trans_type, V) -> V;
decode_typed_field(avg_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(begin_seq_no, V) -> fix:parse_num(V);
decode_typed_field(begin_string, V) -> V;
decode_typed_field(body_length, V) -> fix:parse_num(V);
decode_typed_field(check_sum, V) -> V;
decode_typed_field(cl_ord_id, V) -> V;
decode_typed_field(commission, V) -> V;
decode_typed_field(comm_type, V) -> V;
decode_typed_field(cum_qty, V) -> fix:parse_num(V);
decode_typed_field(currency, V) -> V;
decode_typed_field(end_seq_no, V) -> fix:parse_num(V);
decode_typed_field(exec_id, V) -> V;
decode_typed_field(exec_inst, V) -> V;
decode_typed_field(exec_ref_id, V) -> V;
decode_typed_field(exec_trans_type, V) -> V;
decode_typed_field(handl_inst, V) -> V;
decode_typed_field(security_id_source, V) -> V;
decode_typed_field(ioi_id, V) -> V;
decode_typed_field(ioi_oth_svc, V) -> V;
decode_typed_field(ioi_qlty_ind, V) -> V;
decode_typed_field(ioi_ref_id, V) -> V;
decode_typed_field(ioi_qty, V) -> V;
decode_typed_field(ioi_trans_type, V) -> V;
decode_typed_field(last_capacity, V) -> V;
decode_typed_field(last_mkt, V) -> V;
decode_typed_field(last_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(last_qty, V) -> fix:parse_num(V);
decode_typed_field(no_lines_of_text, V) -> fix:parse_num(V);
decode_typed_field(msg_seq_num, V) -> fix:parse_num(V);
decode_typed_field(msg_type, V) -> message_by_number(V);
decode_typed_field(new_seq_no, V) -> fix:parse_num(V);
decode_typed_field(order_id, V) -> V;
decode_typed_field(order_qty, V) -> fix:parse_num(V);
decode_typed_field(ord_status, V) -> V;
decode_typed_field(ord_type, V) -> V;
decode_typed_field(orig_cl_ord_id, V) -> V;
decode_typed_field(orig_time, V) -> V;
decode_typed_field(poss_dup_flag, V) -> V == <<"Y">>;
decode_typed_field(price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(ref_seq_num, V) -> fix:parse_num(V);
decode_typed_field(relatd_sym, V) -> V;
decode_typed_field(rule80a, V) -> V;
decode_typed_field(security_id, V) -> V;
decode_typed_field(sender_comp_id, V) -> V;
decode_typed_field(sender_sub_id, V) -> V;
decode_typed_field(sending_date, V) -> V;
decode_typed_field(sending_time, V) -> V;
decode_typed_field(quantity, V) -> fix:parse_num(V);
decode_typed_field(side, V) -> V;
decode_typed_field(symbol, V) -> V;
decode_typed_field(target_comp_id, V) -> V;
decode_typed_field(target_sub_id, V) -> V;
decode_typed_field(text, V) -> V;
decode_typed_field(time_in_force, V) -> V;
decode_typed_field(transact_time, V) -> V;
decode_typed_field(urgency, V) -> V;
decode_typed_field(valid_until_time, V) -> V;
decode_typed_field(settl_type, V) -> V;
decode_typed_field(settl_date, V) -> V;
decode_typed_field(symbol_sfx, V) -> V;
decode_typed_field(list_id, V) -> V;
decode_typed_field(list_seq_no, V) -> fix:parse_num(V);
decode_typed_field(tot_no_orders, V) -> fix:parse_num(V);
decode_typed_field(list_exec_inst, V) -> V;
decode_typed_field(alloc_id, V) -> V;
decode_typed_field(alloc_trans_type, V) -> V;
decode_typed_field(ref_alloc_id, V) -> V;
decode_typed_field(no_orders, V) -> fix:parse_num(V);
decode_typed_field(avg_px_precision, V) -> fix:parse_num(V);
decode_typed_field(trade_date, V) -> V;
decode_typed_field(exec_broker, V) -> V;
decode_typed_field(position_effect, V) -> V;
decode_typed_field(no_allocs, V) -> fix:parse_num(V);
decode_typed_field(alloc_account, V) -> V;
decode_typed_field(alloc_qty, V) -> fix:parse_num(V);
decode_typed_field(process_code, V) -> V;
decode_typed_field(no_rpts, V) -> fix:parse_num(V);
decode_typed_field(rpt_seq, V) -> fix:parse_num(V);
decode_typed_field(cxl_qty, V) -> fix:parse_num(V);
decode_typed_field(no_dlvy_inst, V) -> fix:parse_num(V);
decode_typed_field(dlvy_inst, V) -> V;
decode_typed_field(alloc_status, V) -> fix:parse_num(V);
decode_typed_field(alloc_rej_code, V) -> fix:parse_num(V);
decode_typed_field(signature, V) -> V;
decode_typed_field(secure_data_len, V) -> fix:parse_num(V);
decode_typed_field(secure_data, V) -> V;
decode_typed_field(broker_of_credit, V) -> V;
decode_typed_field(signature_length, V) -> fix:parse_num(V);
decode_typed_field(email_type, V) -> V;
decode_typed_field(raw_data_length, V) -> fix:parse_num(V);
decode_typed_field(raw_data, V) -> V;
decode_typed_field(poss_resend, V) -> V == <<"Y">>;
decode_typed_field(encrypt_method, V) -> fix:parse_num(V);
decode_typed_field(stop_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(ex_destination, V) -> V;
decode_typed_field(cxl_rej_reason, V) -> fix:parse_num(V);
decode_typed_field(ord_rej_reason, V) -> fix:parse_num(V);
decode_typed_field(ioi_qualifier, V) -> V;
decode_typed_field(wave_no, V) -> V;
decode_typed_field(issuer, V) -> V;
decode_typed_field(security_desc, V) -> V;
decode_typed_field(heart_bt_int, V) -> fix:parse_num(V);
decode_typed_field(client_id, V) -> V;
decode_typed_field(min_qty, V) -> fix:parse_num(V);
decode_typed_field(max_floor, V) -> fix:parse_num(V);
decode_typed_field(test_req_id, V) -> V;
decode_typed_field(report_to_exch, V) -> V == <<"Y">>;
decode_typed_field(locate_reqd, V) -> V == <<"Y">>;
decode_typed_field(on_behalf_of_comp_id, V) -> V;
decode_typed_field(on_behalf_of_sub_id, V) -> V;
decode_typed_field(quote_id, V) -> V;
decode_typed_field(net_money, V) -> V;
decode_typed_field(settl_curr_amt, V) -> V;
decode_typed_field(settl_currency, V) -> V;
decode_typed_field(forex_req, V) -> V == <<"Y">>;
decode_typed_field(orig_sending_time, V) -> V;
decode_typed_field(gap_fill_flag, V) -> V == <<"Y">>;
decode_typed_field(no_execs, V) -> fix:parse_num(V);
decode_typed_field(cxl_type, V) -> V;
decode_typed_field(expire_time, V) -> V;
decode_typed_field(dk_reason, V) -> V;
decode_typed_field(deliver_to_comp_id, V) -> V;
decode_typed_field(deliver_to_sub_id, V) -> V;
decode_typed_field(ioi_natural_flag, V) -> V == <<"Y">>;
decode_typed_field(quote_req_id, V) -> V;
decode_typed_field(bid_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(offer_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(bid_size, V) -> fix:parse_num(V);
decode_typed_field(offer_size, V) -> fix:parse_num(V);
decode_typed_field(no_misc_fees, V) -> fix:parse_num(V);
decode_typed_field(misc_fee_amt, V) -> V;
decode_typed_field(misc_fee_curr, V) -> V;
decode_typed_field(misc_fee_type, V) -> V;
decode_typed_field(prev_close_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(reset_seq_num_flag, V) -> V == <<"Y">>;
decode_typed_field(sender_location_id, V) -> V;
decode_typed_field(target_location_id, V) -> V;
decode_typed_field(on_behalf_of_location_id, V) -> V;
decode_typed_field(deliver_to_location_id, V) -> V;
decode_typed_field(no_related_sym, V) -> fix:parse_num(V);
decode_typed_field(subject, V) -> V;
decode_typed_field(headline, V) -> V;
decode_typed_field(url_link, V) -> V;
decode_typed_field(exec_type, V) -> V;
decode_typed_field(leaves_qty, V) -> fix:parse_num(V);
decode_typed_field(cash_order_qty, V) -> fix:parse_num(V);
decode_typed_field(alloc_avg_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(alloc_net_money, V) -> V;
decode_typed_field(settl_curr_fx_rate, V) -> V;
decode_typed_field(settl_curr_fx_rate_calc, V) -> V;
decode_typed_field(num_days_interest, V) -> fix:parse_num(V);
decode_typed_field(accrued_interest_rate, V) -> V;
decode_typed_field(accrued_interest_amt, V) -> V;
decode_typed_field(settl_inst_mode, V) -> V;
decode_typed_field(alloc_text, V) -> V;
decode_typed_field(settl_inst_id, V) -> V;
decode_typed_field(settl_inst_trans_type, V) -> V;
decode_typed_field(email_thread_id, V) -> V;
decode_typed_field(settl_inst_source, V) -> V;
decode_typed_field(settl_location, V) -> V;
decode_typed_field(security_type, V) -> V;
decode_typed_field(effective_time, V) -> V;
decode_typed_field(stand_inst_db_type, V) -> fix:parse_num(V);
decode_typed_field(stand_inst_db_name, V) -> V;
decode_typed_field(stand_inst_db_id, V) -> V;
decode_typed_field(settl_delivery_type, V) -> fix:parse_num(V);
decode_typed_field(settl_depository_code, V) -> V;
decode_typed_field(settl_brkr_code, V) -> V;
decode_typed_field(settl_inst_code, V) -> V;
decode_typed_field(security_settl_agent_name, V) -> V;
decode_typed_field(security_settl_agent_code, V) -> V;
decode_typed_field(security_settl_agent_acct_num, V) -> V;
decode_typed_field(security_settl_agent_acct_name, V) -> V;
decode_typed_field(security_settl_agent_contact_name, V) -> V;
decode_typed_field(security_settl_agent_contact_phone, V) -> V;
decode_typed_field(cash_settl_agent_name, V) -> V;
decode_typed_field(cash_settl_agent_code, V) -> V;
decode_typed_field(cash_settl_agent_acct_num, V) -> V;
decode_typed_field(cash_settl_agent_acct_name, V) -> V;
decode_typed_field(cash_settl_agent_contact_name, V) -> V;
decode_typed_field(cash_settl_agent_contact_phone, V) -> V;
decode_typed_field(bid_spot_rate, V) -> fix:parse_num(V)*1.0;
decode_typed_field(bid_forward_points, V) -> V;
decode_typed_field(offer_spot_rate, V) -> fix:parse_num(V)*1.0;
decode_typed_field(offer_forward_points, V) -> V;
decode_typed_field(order_qty2, V) -> fix:parse_num(V);
decode_typed_field(settl_date2, V) -> V;
decode_typed_field(last_spot_rate, V) -> fix:parse_num(V)*1.0;
decode_typed_field(last_forward_points, V) -> V;
decode_typed_field(alloc_link_id, V) -> V;
decode_typed_field(alloc_link_type, V) -> fix:parse_num(V);
decode_typed_field(secondary_order_id, V) -> V;
decode_typed_field(no_ioi_qualifiers, V) -> fix:parse_num(V);
decode_typed_field(maturity_month_year, V) -> V;
decode_typed_field(put_or_call, V) -> fix:parse_num(V);
decode_typed_field(strike_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(covered_or_uncovered, V) -> fix:parse_num(V);
decode_typed_field(customer_or_firm, V) -> fix:parse_num(V);
decode_typed_field(maturity_day, V) -> V;
decode_typed_field(opt_attribute, V) -> V;
decode_typed_field(security_exchange, V) -> V;
decode_typed_field(notify_broker_of_credit, V) -> V == <<"Y">>;
decode_typed_field(alloc_handl_inst, V) -> fix:parse_num(V);
decode_typed_field(max_show, V) -> fix:parse_num(V);
decode_typed_field(peg_offset_value, V) -> V;
decode_typed_field(xml_data_len, V) -> fix:parse_num(V);
decode_typed_field(xml_data, V) -> V;
decode_typed_field(settl_inst_ref_id, V) -> V;
decode_typed_field(no_routing_ids, V) -> fix:parse_num(V);
decode_typed_field(routing_type, V) -> fix:parse_num(V);
decode_typed_field(routing_id, V) -> V;
decode_typed_field(spread, V) -> V;
decode_typed_field(benchmark, V) -> V;
decode_typed_field(benchmark_curve_currency, V) -> V;
decode_typed_field(benchmark_curve_name, V) -> V;
decode_typed_field(benchmark_curve_point, V) -> V;
decode_typed_field(coupon_rate, V) -> V;
decode_typed_field(coupon_payment_date, V) -> V;
decode_typed_field(issue_date, V) -> V;
decode_typed_field(repurchase_term, V) -> fix:parse_num(V);
decode_typed_field(repurchase_rate, V) -> V;
decode_typed_field(factor, V) -> V;
decode_typed_field(trade_origination_date, V) -> V;
decode_typed_field(ex_date, V) -> V;
decode_typed_field(contract_multiplier, V) -> V;
decode_typed_field(no_stipulations, V) -> fix:parse_num(V);
decode_typed_field(stipulation_type, V) -> V;
decode_typed_field(stipulation_value, V) -> V;
decode_typed_field(yield_type, V) -> V;
decode_typed_field(yield, V) -> V;
decode_typed_field(total_takedown, V) -> V;
decode_typed_field(concession, V) -> V;
decode_typed_field(repo_collateral_security_type, V) -> fix:parse_num(V);
decode_typed_field(redemption_date, V) -> V;
decode_typed_field(underlying_coupon_payment_date, V) -> V;
decode_typed_field(underlying_issue_date, V) -> V;
decode_typed_field(underlying_repo_collateral_security_type, V) -> fix:parse_num(V);
decode_typed_field(underlying_repurchase_term, V) -> fix:parse_num(V);
decode_typed_field(underlying_repurchase_rate, V) -> V;
decode_typed_field(underlying_factor, V) -> V;
decode_typed_field(underlying_redemption_date, V) -> V;
decode_typed_field(leg_coupon_payment_date, V) -> V;
decode_typed_field(leg_issue_date, V) -> V;
decode_typed_field(leg_repo_collateral_security_type, V) -> fix:parse_num(V);
decode_typed_field(leg_repurchase_term, V) -> fix:parse_num(V);
decode_typed_field(leg_repurchase_rate, V) -> V;
decode_typed_field(leg_factor, V) -> V;
decode_typed_field(leg_redemption_date, V) -> V;
decode_typed_field(credit_rating, V) -> V;
decode_typed_field(underlying_credit_rating, V) -> V;
decode_typed_field(leg_credit_rating, V) -> V;
decode_typed_field(traded_flat_switch, V) -> V == <<"Y">>;
decode_typed_field(basis_feature_date, V) -> V;
decode_typed_field(basis_feature_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(md_req_id, V) -> V;
decode_typed_field(subscription_request_type, V) -> V;
decode_typed_field(market_depth, V) -> fix:parse_num(V);
decode_typed_field(md_update_type, V) -> fix:parse_num(V);
decode_typed_field(aggregated_book, V) -> V == <<"Y">>;
decode_typed_field(no_md_entry_types, V) -> fix:parse_num(V);
decode_typed_field(no_md_entries, V) -> fix:parse_num(V);
decode_typed_field(md_entry_type, V) -> V;
decode_typed_field(md_entry_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(md_entry_size, V) -> fix:parse_num(V);
decode_typed_field(md_entry_date, V) -> V;
decode_typed_field(md_entry_time, V) -> V;
decode_typed_field(tick_direction, V) -> V;
decode_typed_field(md_mkt, V) -> V;
decode_typed_field(quote_condition, V) -> V;
decode_typed_field(trade_condition, V) -> V;
decode_typed_field(md_entry_id, V) -> V;
decode_typed_field(md_update_action, V) -> V;
decode_typed_field(md_entry_ref_id, V) -> V;
decode_typed_field(md_req_rej_reason, V) -> V;
decode_typed_field(md_entry_originator, V) -> V;
decode_typed_field(location_id, V) -> V;
decode_typed_field(desk_id, V) -> V;
decode_typed_field(delete_reason, V) -> V;
decode_typed_field(open_close_settl_flag, V) -> V;
decode_typed_field(seller_days, V) -> fix:parse_num(V);
decode_typed_field(md_entry_buyer, V) -> V;
decode_typed_field(md_entry_seller, V) -> V;
decode_typed_field(md_entry_position_no, V) -> fix:parse_num(V);
decode_typed_field(financial_status, V) -> V;
decode_typed_field(corporate_action, V) -> V;
decode_typed_field(def_bid_size, V) -> fix:parse_num(V);
decode_typed_field(def_offer_size, V) -> fix:parse_num(V);
decode_typed_field(no_quote_entries, V) -> fix:parse_num(V);
decode_typed_field(no_quote_sets, V) -> fix:parse_num(V);
decode_typed_field(quote_status, V) -> fix:parse_num(V);
decode_typed_field(quote_cancel_type, V) -> fix:parse_num(V);
decode_typed_field(quote_entry_id, V) -> V;
decode_typed_field(quote_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(quote_response_level, V) -> fix:parse_num(V);
decode_typed_field(quote_set_id, V) -> V;
decode_typed_field(quote_request_type, V) -> fix:parse_num(V);
decode_typed_field(tot_no_quote_entries, V) -> fix:parse_num(V);
decode_typed_field(underlying_security_id_source, V) -> V;
decode_typed_field(underlying_issuer, V) -> V;
decode_typed_field(underlying_security_desc, V) -> V;
decode_typed_field(underlying_security_exchange, V) -> V;
decode_typed_field(underlying_security_id, V) -> V;
decode_typed_field(underlying_security_type, V) -> V;
decode_typed_field(underlying_symbol, V) -> V;
decode_typed_field(underlying_symbol_sfx, V) -> V;
decode_typed_field(underlying_maturity_month_year, V) -> V;
decode_typed_field(underlying_maturity_day, V) -> V;
decode_typed_field(underlying_put_or_call, V) -> fix:parse_num(V);
decode_typed_field(underlying_strike_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(underlying_opt_attribute, V) -> V;
decode_typed_field(underlying_currency, V) -> V;
decode_typed_field(ratio_qty, V) -> fix:parse_num(V);
decode_typed_field(security_req_id, V) -> V;
decode_typed_field(security_request_type, V) -> fix:parse_num(V);
decode_typed_field(security_response_id, V) -> V;
decode_typed_field(security_response_type, V) -> fix:parse_num(V);
decode_typed_field(security_status_req_id, V) -> V;
decode_typed_field(unsolicited_indicator, V) -> V == <<"Y">>;
decode_typed_field(security_trading_status, V) -> fix:parse_num(V);
decode_typed_field(halt_reason_char, V) -> V;
decode_typed_field(in_view_of_common, V) -> V == <<"Y">>;
decode_typed_field(due_to_related, V) -> V == <<"Y">>;
decode_typed_field(buy_volume, V) -> fix:parse_num(V);
decode_typed_field(sell_volume, V) -> fix:parse_num(V);
decode_typed_field(high_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(low_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(adjustment, V) -> fix:parse_num(V);
decode_typed_field(trad_ses_req_id, V) -> V;
decode_typed_field(trading_session_id, V) -> V;
decode_typed_field(contra_trader, V) -> V;
decode_typed_field(trad_ses_method, V) -> fix:parse_num(V);
decode_typed_field(trad_ses_mode, V) -> fix:parse_num(V);
decode_typed_field(trad_ses_status, V) -> fix:parse_num(V);
decode_typed_field(trad_ses_start_time, V) -> V;
decode_typed_field(trad_ses_open_time, V) -> V;
decode_typed_field(trad_ses_pre_close_time, V) -> V;
decode_typed_field(trad_ses_close_time, V) -> V;
decode_typed_field(trad_ses_end_time, V) -> V;
decode_typed_field(number_of_orders, V) -> fix:parse_num(V);
decode_typed_field(message_encoding, V) -> V;
decode_typed_field(encoded_issuer_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_issuer, V) -> V;
decode_typed_field(encoded_security_desc_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_security_desc, V) -> V;
decode_typed_field(encoded_list_exec_inst_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_list_exec_inst, V) -> V;
decode_typed_field(encoded_text_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_text, V) -> V;
decode_typed_field(encoded_subject_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_subject, V) -> V;
decode_typed_field(encoded_headline_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_headline, V) -> V;
decode_typed_field(encoded_alloc_text_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_alloc_text, V) -> V;
decode_typed_field(encoded_underlying_issuer_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_underlying_issuer, V) -> V;
decode_typed_field(encoded_underlying_security_desc_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_underlying_security_desc, V) -> V;
decode_typed_field(alloc_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(quote_set_valid_until_time, V) -> V;
decode_typed_field(quote_entry_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(last_msg_seq_num_processed, V) -> fix:parse_num(V);
decode_typed_field(on_behalf_of_sending_time, V) -> V;
decode_typed_field(ref_tag_id, V) -> fix:parse_num(V);
decode_typed_field(ref_msg_type, V) -> V;
decode_typed_field(session_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(bid_request_trans_type, V) -> V;
decode_typed_field(contra_broker, V) -> V;
decode_typed_field(compliance_id, V) -> V;
decode_typed_field(solicited_flag, V) -> V == <<"Y">>;
decode_typed_field(exec_restatement_reason, V) -> fix:parse_num(V);
decode_typed_field(business_reject_ref_id, V) -> V;
decode_typed_field(business_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(gross_trade_amt, V) -> V;
decode_typed_field(no_contra_brokers, V) -> fix:parse_num(V);
decode_typed_field(max_message_size, V) -> fix:parse_num(V);
decode_typed_field(no_msg_types, V) -> fix:parse_num(V);
decode_typed_field(msg_direction, V) -> V;
decode_typed_field(no_trading_sessions, V) -> fix:parse_num(V);
decode_typed_field(total_volume_traded, V) -> fix:parse_num(V);
decode_typed_field(discretion_inst, V) -> V;
decode_typed_field(discretion_offset_value, V) -> V;
decode_typed_field(bid_id, V) -> V;
decode_typed_field(client_bid_id, V) -> V;
decode_typed_field(list_name, V) -> V;
decode_typed_field(tot_no_related_sym, V) -> fix:parse_num(V);
decode_typed_field(bid_type, V) -> fix:parse_num(V);
decode_typed_field(num_tickets, V) -> fix:parse_num(V);
decode_typed_field(side_value1, V) -> V;
decode_typed_field(side_value2, V) -> V;
decode_typed_field(no_bid_descriptors, V) -> fix:parse_num(V);
decode_typed_field(bid_descriptor_type, V) -> fix:parse_num(V);
decode_typed_field(bid_descriptor, V) -> V;
decode_typed_field(side_value_ind, V) -> fix:parse_num(V);
decode_typed_field(liquidity_pct_low, V) -> V;
decode_typed_field(liquidity_pct_high, V) -> V;
decode_typed_field(liquidity_value, V) -> V;
decode_typed_field(efp_tracking_error, V) -> V;
decode_typed_field(fair_value, V) -> V;
decode_typed_field(outside_index_pct, V) -> V;
decode_typed_field(value_of_futures, V) -> V;
decode_typed_field(liquidity_ind_type, V) -> fix:parse_num(V);
decode_typed_field(wt_average_liquidity, V) -> V;
decode_typed_field(exchange_for_physical, V) -> V == <<"Y">>;
decode_typed_field(out_main_cntry_u_index, V) -> V;
decode_typed_field(cross_percent, V) -> V;
decode_typed_field(prog_rpt_reqs, V) -> fix:parse_num(V);
decode_typed_field(prog_period_interval, V) -> fix:parse_num(V);
decode_typed_field(inc_tax_ind, V) -> fix:parse_num(V);
decode_typed_field(num_bidders, V) -> fix:parse_num(V);
decode_typed_field(bid_trade_type, V) -> V;
decode_typed_field(basis_px_type, V) -> V;
decode_typed_field(no_bid_components, V) -> fix:parse_num(V);
decode_typed_field(country, V) -> V;
decode_typed_field(tot_no_strikes, V) -> fix:parse_num(V);
decode_typed_field(price_type, V) -> fix:parse_num(V);
decode_typed_field(day_order_qty, V) -> fix:parse_num(V);
decode_typed_field(day_cum_qty, V) -> fix:parse_num(V);
decode_typed_field(day_avg_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(gt_booking_inst, V) -> fix:parse_num(V);
decode_typed_field(no_strikes, V) -> fix:parse_num(V);
decode_typed_field(list_status_type, V) -> fix:parse_num(V);
decode_typed_field(net_gross_ind, V) -> fix:parse_num(V);
decode_typed_field(list_order_status, V) -> fix:parse_num(V);
decode_typed_field(expire_date, V) -> V;
decode_typed_field(list_exec_inst_type, V) -> V;
decode_typed_field(cxl_rej_response_to, V) -> V;
decode_typed_field(underlying_coupon_rate, V) -> V;
decode_typed_field(underlying_contract_multiplier, V) -> V;
decode_typed_field(contra_trade_qty, V) -> fix:parse_num(V);
decode_typed_field(contra_trade_time, V) -> V;
decode_typed_field(clearing_firm, V) -> V;
decode_typed_field(clearing_account, V) -> V;
decode_typed_field(liquidity_num_securities, V) -> fix:parse_num(V);
decode_typed_field(multi_leg_reporting_type, V) -> V;
decode_typed_field(strike_time, V) -> V;
decode_typed_field(list_status_text, V) -> V;
decode_typed_field(encoded_list_status_text_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_list_status_text, V) -> V;
decode_typed_field(party_id_source, V) -> V;
decode_typed_field(party_id, V) -> V;
decode_typed_field(total_volume_traded_date, V) -> V;
decode_typed_field(total_volume_traded_time, V) -> V;
decode_typed_field(net_chg_prev_day, V) -> V;
decode_typed_field(party_role, V) -> fix:parse_num(V);
decode_typed_field(no_party_ids, V) -> fix:parse_num(V);
decode_typed_field(no_security_alt_id, V) -> fix:parse_num(V);
decode_typed_field(security_alt_id, V) -> V;
decode_typed_field(security_alt_id_source, V) -> V;
decode_typed_field(no_underlying_security_alt_id, V) -> fix:parse_num(V);
decode_typed_field(underlying_security_alt_id, V) -> V;
decode_typed_field(underlying_security_alt_id_source, V) -> V;
decode_typed_field(product, V) -> fix:parse_num(V);
decode_typed_field(cfi_code, V) -> V;
decode_typed_field(underlying_product, V) -> fix:parse_num(V);
decode_typed_field(underlying_cfi_code, V) -> V;
decode_typed_field(test_message_indicator, V) -> V == <<"Y">>;
decode_typed_field(quantity_type, V) -> fix:parse_num(V);
decode_typed_field(booking_ref_id, V) -> V;
decode_typed_field(individual_alloc_id, V) -> V;
decode_typed_field(rounding_direction, V) -> V;
decode_typed_field(rounding_modulus, V) -> V;
decode_typed_field(country_of_issue, V) -> V;
decode_typed_field(state_or_province_of_issue, V) -> V;
decode_typed_field(locale_of_issue, V) -> V;
decode_typed_field(no_regist_dtls, V) -> fix:parse_num(V);
decode_typed_field(mailing_dtls, V) -> V;
decode_typed_field(investor_country_of_residence, V) -> V;
decode_typed_field(payment_ref, V) -> V;
decode_typed_field(distrib_payment_method, V) -> fix:parse_num(V);
decode_typed_field(cash_distrib_curr, V) -> V;
decode_typed_field(comm_currency, V) -> V;
decode_typed_field(cancellation_rights, V) -> V;
decode_typed_field(money_laundering_status, V) -> V;
decode_typed_field(mailing_inst, V) -> V;
decode_typed_field(trans_bkd_time, V) -> V;
decode_typed_field(exec_price_type, V) -> V;
decode_typed_field(exec_price_adjustment, V) -> V;
decode_typed_field(date_of_birth, V) -> V;
decode_typed_field(trade_report_trans_type, V) -> fix:parse_num(V);
decode_typed_field(card_holder_name, V) -> V;
decode_typed_field(card_number, V) -> V;
decode_typed_field(card_exp_date, V) -> V;
decode_typed_field(card_iss_num, V) -> V;
decode_typed_field(payment_method, V) -> fix:parse_num(V);
decode_typed_field(regist_acct_type, V) -> V;
decode_typed_field(designation, V) -> V;
decode_typed_field(tax_advantage_type, V) -> fix:parse_num(V);
decode_typed_field(regist_rej_reason_text, V) -> V;
decode_typed_field(fund_renew_waiv, V) -> V;
decode_typed_field(cash_distrib_agent_name, V) -> V;
decode_typed_field(cash_distrib_agent_code, V) -> V;
decode_typed_field(cash_distrib_agent_acct_number, V) -> V;
decode_typed_field(cash_distrib_pay_ref, V) -> V;
decode_typed_field(cash_distrib_agent_acct_name, V) -> V;
decode_typed_field(card_start_date, V) -> V;
decode_typed_field(payment_date, V) -> V;
decode_typed_field(payment_remitter_id, V) -> V;
decode_typed_field(regist_status, V) -> V;
decode_typed_field(regist_rej_reason_code, V) -> fix:parse_num(V);
decode_typed_field(regist_ref_id, V) -> V;
decode_typed_field(regist_dtls, V) -> V;
decode_typed_field(no_distrib_insts, V) -> fix:parse_num(V);
decode_typed_field(regist_email, V) -> V;
decode_typed_field(distrib_percentage, V) -> V;
decode_typed_field(regist_id, V) -> V;
decode_typed_field(regist_trans_type, V) -> V;
decode_typed_field(exec_valuation_point, V) -> V;
decode_typed_field(order_percent, V) -> V;
decode_typed_field(ownership_type, V) -> V;
decode_typed_field(no_cont_amts, V) -> fix:parse_num(V);
decode_typed_field(cont_amt_type, V) -> fix:parse_num(V);
decode_typed_field(cont_amt_value, V) -> V;
decode_typed_field(cont_amt_curr, V) -> V;
decode_typed_field(owner_type, V) -> fix:parse_num(V);
decode_typed_field(party_sub_id, V) -> V;
decode_typed_field(nested_party_id, V) -> V;
decode_typed_field(nested_party_id_source, V) -> V;
decode_typed_field(secondary_cl_ord_id, V) -> V;
decode_typed_field(secondary_exec_id, V) -> V;
decode_typed_field(order_capacity, V) -> V;
decode_typed_field(order_restrictions, V) -> V;
decode_typed_field(mass_cancel_request_type, V) -> V;
decode_typed_field(mass_cancel_response, V) -> V;
decode_typed_field(mass_cancel_reject_reason, V) -> V;
decode_typed_field(total_affected_orders, V) -> fix:parse_num(V);
decode_typed_field(no_affected_orders, V) -> fix:parse_num(V);
decode_typed_field(affected_order_id, V) -> V;
decode_typed_field(affected_secondary_order_id, V) -> V;
decode_typed_field(quote_type, V) -> fix:parse_num(V);
decode_typed_field(nested_party_role, V) -> fix:parse_num(V);
decode_typed_field(no_nested_party_ids, V) -> fix:parse_num(V);
decode_typed_field(total_accrued_interest_amt, V) -> V;
decode_typed_field(maturity_date, V) -> V;
decode_typed_field(underlying_maturity_date, V) -> V;
decode_typed_field(instr_registry, V) -> V;
decode_typed_field(cash_margin, V) -> V;
decode_typed_field(nested_party_sub_id, V) -> V;
decode_typed_field(scope, V) -> V;
decode_typed_field(md_implicit_delete, V) -> V == <<"Y">>;
decode_typed_field(cross_id, V) -> V;
decode_typed_field(cross_type, V) -> fix:parse_num(V);
decode_typed_field(cross_prioritization, V) -> fix:parse_num(V);
decode_typed_field(orig_cross_id, V) -> V;
decode_typed_field(no_sides, V) -> fix:parse_num(V);
decode_typed_field(username, V) -> V;
decode_typed_field(password, V) -> V;
decode_typed_field(no_legs, V) -> fix:parse_num(V);
decode_typed_field(leg_currency, V) -> V;
decode_typed_field(tot_no_security_types, V) -> fix:parse_num(V);
decode_typed_field(no_security_types, V) -> fix:parse_num(V);
decode_typed_field(security_list_request_type, V) -> fix:parse_num(V);
decode_typed_field(security_request_result, V) -> fix:parse_num(V);
decode_typed_field(round_lot, V) -> fix:parse_num(V);
decode_typed_field(min_trade_vol, V) -> fix:parse_num(V);
decode_typed_field(multi_leg_rpt_type_req, V) -> fix:parse_num(V);
decode_typed_field(leg_position_effect, V) -> V;
decode_typed_field(leg_covered_or_uncovered, V) -> fix:parse_num(V);
decode_typed_field(leg_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(trad_ses_status_rej_reason, V) -> fix:parse_num(V);
decode_typed_field(trade_request_id, V) -> V;
decode_typed_field(trade_request_type, V) -> fix:parse_num(V);
decode_typed_field(previously_reported, V) -> V == <<"Y">>;
decode_typed_field(trade_report_id, V) -> V;
decode_typed_field(trade_report_ref_id, V) -> V;
decode_typed_field(match_status, V) -> V;
decode_typed_field(match_type, V) -> V;
decode_typed_field(odd_lot, V) -> V == <<"Y">>;
decode_typed_field(no_clearing_instructions, V) -> fix:parse_num(V);
decode_typed_field(clearing_instruction, V) -> fix:parse_num(V);
decode_typed_field(trade_input_source, V) -> V;
decode_typed_field(trade_input_device, V) -> V;
decode_typed_field(no_dates, V) -> fix:parse_num(V);
decode_typed_field(account_type, V) -> fix:parse_num(V);
decode_typed_field(cust_order_capacity, V) -> fix:parse_num(V);
decode_typed_field(cl_ord_link_id, V) -> V;
decode_typed_field(mass_status_req_id, V) -> V;
decode_typed_field(mass_status_req_type, V) -> fix:parse_num(V);
decode_typed_field(orig_ord_mod_time, V) -> V;
decode_typed_field(leg_settl_type, V) -> V;
decode_typed_field(leg_settl_date, V) -> V;
decode_typed_field(day_booking_inst, V) -> V;
decode_typed_field(booking_unit, V) -> V;
decode_typed_field(prealloc_method, V) -> V;
decode_typed_field(underlying_country_of_issue, V) -> V;
decode_typed_field(underlying_state_or_province_of_issue, V) -> V;
decode_typed_field(underlying_locale_of_issue, V) -> V;
decode_typed_field(underlying_instr_registry, V) -> V;
decode_typed_field(leg_country_of_issue, V) -> V;
decode_typed_field(leg_state_or_province_of_issue, V) -> V;
decode_typed_field(leg_locale_of_issue, V) -> V;
decode_typed_field(leg_instr_registry, V) -> V;
decode_typed_field(leg_symbol, V) -> V;
decode_typed_field(leg_symbol_sfx, V) -> V;
decode_typed_field(leg_security_id, V) -> V;
decode_typed_field(leg_security_id_source, V) -> V;
decode_typed_field(no_leg_security_alt_id, V) -> V;
decode_typed_field(leg_security_alt_id, V) -> V;
decode_typed_field(leg_security_alt_id_source, V) -> V;
decode_typed_field(leg_product, V) -> fix:parse_num(V);
decode_typed_field(leg_cfi_code, V) -> V;
decode_typed_field(leg_security_type, V) -> V;
decode_typed_field(leg_maturity_month_year, V) -> V;
decode_typed_field(leg_maturity_date, V) -> V;
decode_typed_field(leg_strike_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(leg_opt_attribute, V) -> V;
decode_typed_field(leg_contract_multiplier, V) -> V;
decode_typed_field(leg_coupon_rate, V) -> V;
decode_typed_field(leg_security_exchange, V) -> V;
decode_typed_field(leg_issuer, V) -> V;
decode_typed_field(encoded_leg_issuer_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_leg_issuer, V) -> V;
decode_typed_field(leg_security_desc, V) -> V;
decode_typed_field(encoded_leg_security_desc_len, V) -> fix:parse_num(V);
decode_typed_field(encoded_leg_security_desc, V) -> V;
decode_typed_field(leg_ratio_qty, V) -> V;
decode_typed_field(leg_side, V) -> V;
decode_typed_field(trading_session_sub_id, V) -> V;
decode_typed_field(alloc_type, V) -> fix:parse_num(V);
decode_typed_field(no_hops, V) -> fix:parse_num(V);
decode_typed_field(hop_comp_id, V) -> V;
decode_typed_field(hop_sending_time, V) -> V;
decode_typed_field(hop_ref_id, V) -> fix:parse_num(V);
decode_typed_field(mid_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(bid_yield, V) -> V;
decode_typed_field(mid_yield, V) -> V;
decode_typed_field(offer_yield, V) -> V;
decode_typed_field(clearing_fee_indicator, V) -> V;
decode_typed_field(working_indicator, V) -> V == <<"Y">>;
decode_typed_field(leg_last_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(priority_indicator, V) -> fix:parse_num(V);
decode_typed_field(price_improvement, V) -> V;
decode_typed_field(price2, V) -> fix:parse_num(V)*1.0;
decode_typed_field(last_forward_points2, V) -> V;
decode_typed_field(bid_forward_points2, V) -> V;
decode_typed_field(offer_forward_points2, V) -> V;
decode_typed_field(rfq_req_id, V) -> V;
decode_typed_field(mkt_bid_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(mkt_offer_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(min_bid_size, V) -> fix:parse_num(V);
decode_typed_field(min_offer_size, V) -> fix:parse_num(V);
decode_typed_field(quote_status_req_id, V) -> V;
decode_typed_field(legal_confirm, V) -> V == <<"Y">>;
decode_typed_field(underlying_last_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(underlying_last_qty, V) -> fix:parse_num(V);
decode_typed_field(sec_def_status, V) -> fix:parse_num(V);
decode_typed_field(leg_ref_id, V) -> V;
decode_typed_field(contra_leg_ref_id, V) -> V;
decode_typed_field(settl_curr_bid_fx_rate, V) -> V;
decode_typed_field(settl_curr_offer_fx_rate, V) -> V;
decode_typed_field(quote_request_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(side_compliance_id, V) -> V;
decode_typed_field(acct_id_source, V) -> fix:parse_num(V);
decode_typed_field(alloc_acct_id_source, V) -> fix:parse_num(V);
decode_typed_field(benchmark_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(benchmark_price_type, V) -> fix:parse_num(V);
decode_typed_field(confirm_id, V) -> V;
decode_typed_field(confirm_status, V) -> fix:parse_num(V);
decode_typed_field(confirm_trans_type, V) -> fix:parse_num(V);
decode_typed_field(contract_settl_month, V) -> V;
decode_typed_field(delivery_form, V) -> fix:parse_num(V);
decode_typed_field(last_par_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(no_leg_allocs, V) -> fix:parse_num(V);
decode_typed_field(leg_alloc_account, V) -> V;
decode_typed_field(leg_individual_alloc_id, V) -> V;
decode_typed_field(leg_alloc_qty, V) -> fix:parse_num(V);
decode_typed_field(leg_alloc_acct_id_source, V) -> V;
decode_typed_field(leg_settl_currency, V) -> V;
decode_typed_field(leg_benchmark_curve_currency, V) -> V;
decode_typed_field(leg_benchmark_curve_name, V) -> V;
decode_typed_field(leg_benchmark_curve_point, V) -> V;
decode_typed_field(leg_benchmark_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(leg_benchmark_price_type, V) -> fix:parse_num(V);
decode_typed_field(leg_bid_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(leg_ioi_qty, V) -> V;
decode_typed_field(no_leg_stipulations, V) -> fix:parse_num(V);
decode_typed_field(leg_offer_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(leg_order_qty, V) -> fix:parse_num(V);
decode_typed_field(leg_price_type, V) -> fix:parse_num(V);
decode_typed_field(leg_qty, V) -> fix:parse_num(V);
decode_typed_field(leg_stipulation_type, V) -> V;
decode_typed_field(leg_stipulation_value, V) -> V;
decode_typed_field(leg_swap_type, V) -> fix:parse_num(V);
decode_typed_field(pool, V) -> V;
decode_typed_field(quote_price_type, V) -> fix:parse_num(V);
decode_typed_field(quote_resp_id, V) -> V;
decode_typed_field(quote_resp_type, V) -> fix:parse_num(V);
decode_typed_field(quote_qualifier, V) -> V;
decode_typed_field(yield_redemption_date, V) -> V;
decode_typed_field(yield_redemption_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(yield_redemption_price_type, V) -> fix:parse_num(V);
decode_typed_field(benchmark_security_id, V) -> V;
decode_typed_field(reversal_indicator, V) -> V == <<"Y">>;
decode_typed_field(yield_calc_date, V) -> V;
decode_typed_field(no_positions, V) -> fix:parse_num(V);
decode_typed_field(pos_type, V) -> V;
decode_typed_field(long_qty, V) -> fix:parse_num(V);
decode_typed_field(short_qty, V) -> fix:parse_num(V);
decode_typed_field(pos_qty_status, V) -> fix:parse_num(V);
decode_typed_field(pos_amt_type, V) -> V;
decode_typed_field(pos_amt, V) -> V;
decode_typed_field(pos_trans_type, V) -> fix:parse_num(V);
decode_typed_field(pos_req_id, V) -> V;
decode_typed_field(no_underlyings, V) -> fix:parse_num(V);
decode_typed_field(pos_maint_action, V) -> fix:parse_num(V);
decode_typed_field(orig_pos_req_ref_id, V) -> V;
decode_typed_field(pos_maint_rpt_ref_id, V) -> V;
decode_typed_field(clearing_business_date, V) -> V;
decode_typed_field(settl_sess_id, V) -> V;
decode_typed_field(settl_sess_sub_id, V) -> V;
decode_typed_field(adjustment_type, V) -> fix:parse_num(V);
decode_typed_field(contrary_instruction_indicator, V) -> V == <<"Y">>;
decode_typed_field(prior_spread_indicator, V) -> V == <<"Y">>;
decode_typed_field(pos_maint_rpt_id, V) -> V;
decode_typed_field(pos_maint_status, V) -> fix:parse_num(V);
decode_typed_field(pos_maint_result, V) -> fix:parse_num(V);
decode_typed_field(pos_req_type, V) -> fix:parse_num(V);
decode_typed_field(response_transport_type, V) -> fix:parse_num(V);
decode_typed_field(response_destination, V) -> V;
decode_typed_field(total_num_pos_reports, V) -> fix:parse_num(V);
decode_typed_field(pos_req_result, V) -> fix:parse_num(V);
decode_typed_field(pos_req_status, V) -> fix:parse_num(V);
decode_typed_field(settl_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(settl_price_type, V) -> fix:parse_num(V);
decode_typed_field(underlying_settl_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(underlying_settl_price_type, V) -> fix:parse_num(V);
decode_typed_field(prior_settl_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(no_quote_qualifiers, V) -> fix:parse_num(V);
decode_typed_field(alloc_settl_currency, V) -> V;
decode_typed_field(alloc_settl_curr_amt, V) -> V;
decode_typed_field(interest_at_maturity, V) -> V;
decode_typed_field(leg_dated_date, V) -> V;
decode_typed_field(leg_pool, V) -> V;
decode_typed_field(alloc_interest_at_maturity, V) -> V;
decode_typed_field(alloc_accrued_interest_amt, V) -> V;
decode_typed_field(delivery_date, V) -> V;
decode_typed_field(assignment_method, V) -> V;
decode_typed_field(assignment_unit, V) -> fix:parse_num(V);
decode_typed_field(open_interest, V) -> V;
decode_typed_field(exercise_method, V) -> V;
decode_typed_field(tot_num_trade_reports, V) -> fix:parse_num(V);
decode_typed_field(trade_request_result, V) -> fix:parse_num(V);
decode_typed_field(trade_request_status, V) -> fix:parse_num(V);
decode_typed_field(trade_report_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(side_multi_leg_reporting_type, V) -> fix:parse_num(V);
decode_typed_field(no_pos_amt, V) -> fix:parse_num(V);
decode_typed_field(auto_accept_indicator, V) -> V == <<"Y">>;
decode_typed_field(alloc_report_id, V) -> V;
decode_typed_field(no_nested2_party_ids, V) -> fix:parse_num(V);
decode_typed_field(nested2_party_id, V) -> V;
decode_typed_field(nested2_party_id_source, V) -> V;
decode_typed_field(nested2_party_role, V) -> fix:parse_num(V);
decode_typed_field(nested2_party_sub_id, V) -> V;
decode_typed_field(benchmark_security_id_source, V) -> V;
decode_typed_field(security_sub_type, V) -> V;
decode_typed_field(underlying_security_sub_type, V) -> V;
decode_typed_field(leg_security_sub_type, V) -> V;
decode_typed_field(allowable_one_sidedness_pct, V) -> V;
decode_typed_field(allowable_one_sidedness_value, V) -> V;
decode_typed_field(allowable_one_sidedness_curr, V) -> V;
decode_typed_field(no_trd_reg_timestamps, V) -> fix:parse_num(V);
decode_typed_field(trd_reg_timestamp, V) -> V;
decode_typed_field(trd_reg_timestamp_type, V) -> fix:parse_num(V);
decode_typed_field(trd_reg_timestamp_origin, V) -> V;
decode_typed_field(confirm_ref_id, V) -> V;
decode_typed_field(confirm_type, V) -> fix:parse_num(V);
decode_typed_field(confirm_rej_reason, V) -> fix:parse_num(V);
decode_typed_field(booking_type, V) -> fix:parse_num(V);
decode_typed_field(individual_alloc_rej_code, V) -> fix:parse_num(V);
decode_typed_field(settl_inst_msg_id, V) -> V;
decode_typed_field(no_settl_inst, V) -> fix:parse_num(V);
decode_typed_field(last_update_time, V) -> V;
decode_typed_field(alloc_settl_inst_type, V) -> fix:parse_num(V);
decode_typed_field(no_settl_party_ids, V) -> fix:parse_num(V);
decode_typed_field(settl_party_id, V) -> V;
decode_typed_field(settl_party_id_source, V) -> V;
decode_typed_field(settl_party_role, V) -> fix:parse_num(V);
decode_typed_field(settl_party_sub_id, V) -> V;
decode_typed_field(settl_party_sub_id_type, V) -> fix:parse_num(V);
decode_typed_field(dlvy_inst_type, V) -> V;
decode_typed_field(termination_type, V) -> fix:parse_num(V);
decode_typed_field(next_expected_msg_seq_num, V) -> fix:parse_num(V);
decode_typed_field(ord_status_req_id, V) -> V;
decode_typed_field(settl_inst_req_id, V) -> V;
decode_typed_field(settl_inst_req_rej_code, V) -> fix:parse_num(V);
decode_typed_field(secondary_alloc_id, V) -> V;
decode_typed_field(alloc_report_type, V) -> fix:parse_num(V);
decode_typed_field(alloc_report_ref_id, V) -> V;
decode_typed_field(alloc_canc_replace_reason, V) -> fix:parse_num(V);
decode_typed_field(copy_msg_indicator, V) -> V == <<"Y">>;
decode_typed_field(alloc_account_type, V) -> fix:parse_num(V);
decode_typed_field(order_avg_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(order_booking_qty, V) -> fix:parse_num(V);
decode_typed_field(no_settl_party_sub_ids, V) -> fix:parse_num(V);
decode_typed_field(no_party_sub_ids, V) -> fix:parse_num(V);
decode_typed_field(party_sub_id_type, V) -> fix:parse_num(V);
decode_typed_field(no_nested_party_sub_ids, V) -> fix:parse_num(V);
decode_typed_field(nested_party_sub_id_type, V) -> fix:parse_num(V);
decode_typed_field(no_nested2_party_sub_ids, V) -> fix:parse_num(V);
decode_typed_field(nested2_party_sub_id_type, V) -> fix:parse_num(V);
decode_typed_field(alloc_intermed_req_type, V) -> fix:parse_num(V);
decode_typed_field(underlying_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(price_delta, V) -> V;
decode_typed_field(appl_queue_max, V) -> fix:parse_num(V);
decode_typed_field(appl_queue_depth, V) -> fix:parse_num(V);
decode_typed_field(appl_queue_resolution, V) -> fix:parse_num(V);
decode_typed_field(appl_queue_action, V) -> fix:parse_num(V);
decode_typed_field(no_alt_md_source, V) -> fix:parse_num(V);
decode_typed_field(alt_md_source_id, V) -> V;
decode_typed_field(secondary_trade_report_id, V) -> V;
decode_typed_field(avg_px_indicator, V) -> fix:parse_num(V);
decode_typed_field(trade_link_id, V) -> V;
decode_typed_field(order_input_device, V) -> V;
decode_typed_field(underlying_trading_session_id, V) -> V;
decode_typed_field(underlying_trading_session_sub_id, V) -> V;
decode_typed_field(trade_leg_ref_id, V) -> V;
decode_typed_field(exchange_rule, V) -> V;
decode_typed_field(trade_alloc_indicator, V) -> fix:parse_num(V);
decode_typed_field(expiration_cycle, V) -> fix:parse_num(V);
decode_typed_field(trd_type, V) -> fix:parse_num(V);
decode_typed_field(trd_sub_type, V) -> fix:parse_num(V);
decode_typed_field(transfer_reason, V) -> V;
decode_typed_field(asgn_req_id, V) -> V;
decode_typed_field(tot_num_assignment_reports, V) -> fix:parse_num(V);
decode_typed_field(asgn_rpt_id, V) -> V;
decode_typed_field(threshold_amount, V) -> V;
decode_typed_field(peg_move_type, V) -> fix:parse_num(V);
decode_typed_field(peg_offset_type, V) -> fix:parse_num(V);
decode_typed_field(peg_limit_type, V) -> fix:parse_num(V);
decode_typed_field(peg_round_direction, V) -> fix:parse_num(V);
decode_typed_field(pegged_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(peg_scope, V) -> fix:parse_num(V);
decode_typed_field(discretion_move_type, V) -> fix:parse_num(V);
decode_typed_field(discretion_offset_type, V) -> fix:parse_num(V);
decode_typed_field(discretion_limit_type, V) -> fix:parse_num(V);
decode_typed_field(discretion_round_direction, V) -> fix:parse_num(V);
decode_typed_field(discretion_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(discretion_scope, V) -> fix:parse_num(V);
decode_typed_field(target_strategy, V) -> fix:parse_num(V);
decode_typed_field(target_strategy_parameters, V) -> V;
decode_typed_field(participation_rate, V) -> V;
decode_typed_field(target_strategy_performance, V) -> V;
decode_typed_field(last_liquidity_ind, V) -> fix:parse_num(V);
decode_typed_field(publish_trd_indicator, V) -> V == <<"Y">>;
decode_typed_field(short_sale_reason, V) -> fix:parse_num(V);
decode_typed_field(qty_type, V) -> fix:parse_num(V);
decode_typed_field(secondary_trd_type, V) -> fix:parse_num(V);
decode_typed_field(trade_report_type, V) -> fix:parse_num(V);
decode_typed_field(alloc_no_orders_type, V) -> fix:parse_num(V);
decode_typed_field(shared_commission, V) -> V;
decode_typed_field(confirm_req_id, V) -> V;
decode_typed_field(avg_par_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(reported_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(no_capacities, V) -> fix:parse_num(V);
decode_typed_field(order_capacity_qty, V) -> fix:parse_num(V);
decode_typed_field(no_events, V) -> fix:parse_num(V);
decode_typed_field(event_type, V) -> fix:parse_num(V);
decode_typed_field(event_date, V) -> V;
decode_typed_field(event_px, V) -> fix:parse_num(V)*1.0;
decode_typed_field(event_text, V) -> V;
decode_typed_field(pct_at_risk, V) -> V;
decode_typed_field(no_instr_attrib, V) -> fix:parse_num(V);
decode_typed_field(instr_attrib_type, V) -> fix:parse_num(V);
decode_typed_field(instr_attrib_value, V) -> V;
decode_typed_field(dated_date, V) -> V;
decode_typed_field(interest_accrual_date, V) -> V;
decode_typed_field(cp_program, V) -> fix:parse_num(V);
decode_typed_field(cp_reg_type, V) -> V;
decode_typed_field(underlying_cp_program, V) -> V;
decode_typed_field(underlying_cp_reg_type, V) -> V;
decode_typed_field(underlying_qty, V) -> fix:parse_num(V);
decode_typed_field(trd_match_id, V) -> V;
decode_typed_field(secondary_trade_report_ref_id, V) -> V;
decode_typed_field(underlying_dirty_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(underlying_end_price, V) -> fix:parse_num(V)*1.0;
decode_typed_field(underlying_start_value, V) -> V;
decode_typed_field(underlying_current_value, V) -> V;
decode_typed_field(underlying_end_value, V) -> V;
decode_typed_field(no_underlying_stips, V) -> fix:parse_num(V);
decode_typed_field(underlying_stip_type, V) -> V;
decode_typed_field(underlying_stip_value, V) -> V;
decode_typed_field(maturity_net_money, V) -> V;
decode_typed_field(misc_fee_basis, V) -> fix:parse_num(V);
decode_typed_field(tot_no_allocs, V) -> fix:parse_num(V);
decode_typed_field(last_fragment, V) -> V == <<"Y">>;
decode_typed_field(coll_req_id, V) -> V;
decode_typed_field(coll_asgn_reason, V) -> fix:parse_num(V);
decode_typed_field(coll_inquiry_qualifier, V) -> fix:parse_num(V);
decode_typed_field(no_trades, V) -> fix:parse_num(V);
decode_typed_field(margin_ratio, V) -> V;
decode_typed_field(margin_excess, V) -> V;
decode_typed_field(total_net_value, V) -> V;
decode_typed_field(cash_outstanding, V) -> V;
decode_typed_field(coll_asgn_id, V) -> V;
decode_typed_field(coll_asgn_trans_type, V) -> fix:parse_num(V);
decode_typed_field(coll_resp_id, V) -> V;
decode_typed_field(coll_asgn_resp_type, V) -> fix:parse_num(V);
decode_typed_field(coll_asgn_reject_reason, V) -> fix:parse_num(V);
decode_typed_field(coll_asgn_ref_id, V) -> V;
decode_typed_field(coll_rpt_id, V) -> V;
decode_typed_field(coll_inquiry_id, V) -> V;
decode_typed_field(coll_status, V) -> fix:parse_num(V);
decode_typed_field(tot_num_reports, V) -> fix:parse_num(V);
decode_typed_field(last_rpt_requested, V) -> V == <<"Y">>;
decode_typed_field(agreement_desc, V) -> V;
decode_typed_field(agreement_id, V) -> V;
decode_typed_field(agreement_date, V) -> V;
decode_typed_field(start_date, V) -> V;
decode_typed_field(end_date, V) -> V;
decode_typed_field(agreement_currency, V) -> V;
decode_typed_field(delivery_type, V) -> fix:parse_num(V);
decode_typed_field(end_accrued_interest_amt, V) -> V;
decode_typed_field(start_cash, V) -> V;
decode_typed_field(end_cash, V) -> V;
decode_typed_field(user_request_id, V) -> V;
decode_typed_field(user_request_type, V) -> fix:parse_num(V);
decode_typed_field(new_password, V) -> V;
decode_typed_field(user_status, V) -> fix:parse_num(V);
decode_typed_field(user_status_text, V) -> V;
decode_typed_field(status_value, V) -> fix:parse_num(V);
decode_typed_field(status_text, V) -> V;
decode_typed_field(ref_comp_id, V) -> V;
decode_typed_field(ref_sub_id, V) -> V;
decode_typed_field(network_response_id, V) -> V;
decode_typed_field(network_request_id, V) -> V;
decode_typed_field(last_network_response_id, V) -> V;
decode_typed_field(network_request_type, V) -> fix:parse_num(V);
decode_typed_field(no_comp_ids, V) -> fix:parse_num(V);
decode_typed_field(network_status_response_type, V) -> fix:parse_num(V);
decode_typed_field(no_coll_inquiry_qualifier, V) -> fix:parse_num(V);
decode_typed_field(trd_rpt_status, V) -> fix:parse_num(V);
decode_typed_field(affirm_status, V) -> fix:parse_num(V);
decode_typed_field(underlying_strike_currency, V) -> V;
decode_typed_field(leg_strike_currency, V) -> V;
decode_typed_field(time_bracket, V) -> V;
decode_typed_field(coll_action, V) -> fix:parse_num(V);
decode_typed_field(coll_inquiry_status, V) -> fix:parse_num(V);
decode_typed_field(coll_inquiry_result, V) -> fix:parse_num(V);
decode_typed_field(strike_currency, V) -> V;
decode_typed_field(no_nested3_party_ids, V) -> fix:parse_num(V);
decode_typed_field(nested3_party_id, V) -> V;
decode_typed_field(nested3_party_id_source, V) -> V;
decode_typed_field(nested3_party_role, V) -> fix:parse_num(V);
decode_typed_field(no_nested3_party_sub_ids, V) -> fix:parse_num(V);
decode_typed_field(nested3_party_sub_id, V) -> V;
decode_typed_field(nested3_party_sub_id_type, V) -> fix:parse_num(V);
decode_typed_field(leg_contract_settl_month, V) -> V;
decode_typed_field(leg_interest_accrual_date, V) -> V;
decode_typed_field(_Key, V) -> V.

encode_typed_field(avg_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(begin_seq_no, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(body_length, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cum_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(end_seq_no, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(last_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_lines_of_text, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(msg_seq_num, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(msg_type, V) -> number_by_message(V);
encode_typed_field(new_seq_no, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(order_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(poss_dup_flag, true) -> <<"Y">>;
encode_typed_field(poss_dup_flag,false) -> <<"N">>;
encode_typed_field(price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(ref_seq_num, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quantity, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(list_seq_no, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_orders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_orders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(avg_px_precision, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_allocs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_rpts, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(rpt_seq, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cxl_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_dlvy_inst, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_rej_code, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(secure_data_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(signature_length, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(raw_data_length, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(poss_resend, true) -> <<"Y">>;
encode_typed_field(poss_resend,false) -> <<"N">>;
encode_typed_field(encrypt_method, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(stop_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(cxl_rej_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(ord_rej_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(heart_bt_int, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(min_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(max_floor, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(report_to_exch, true) -> <<"Y">>;
encode_typed_field(report_to_exch,false) -> <<"N">>;
encode_typed_field(locate_reqd, true) -> <<"Y">>;
encode_typed_field(locate_reqd,false) -> <<"N">>;
encode_typed_field(forex_req, true) -> <<"Y">>;
encode_typed_field(forex_req,false) -> <<"N">>;
encode_typed_field(gap_fill_flag, true) -> <<"Y">>;
encode_typed_field(gap_fill_flag,false) -> <<"N">>;
encode_typed_field(no_execs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(ioi_natural_flag, true) -> <<"Y">>;
encode_typed_field(ioi_natural_flag,false) -> <<"N">>;
encode_typed_field(bid_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(offer_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(bid_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(offer_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_misc_fees, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(prev_close_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(reset_seq_num_flag, true) -> <<"Y">>;
encode_typed_field(reset_seq_num_flag,false) -> <<"N">>;
encode_typed_field(no_related_sym, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leaves_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cash_order_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_avg_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(num_days_interest, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(stand_inst_db_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(settl_delivery_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(bid_spot_rate, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(offer_spot_rate, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(order_qty2, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_spot_rate, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(alloc_link_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_ioi_qualifiers, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(put_or_call, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(strike_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(covered_or_uncovered, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(customer_or_firm, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(notify_broker_of_credit, true) -> <<"Y">>;
encode_typed_field(notify_broker_of_credit,false) -> <<"N">>;
encode_typed_field(alloc_handl_inst, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(max_show, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(xml_data_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_routing_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(routing_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(repurchase_term, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_stipulations, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(repo_collateral_security_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_repo_collateral_security_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_repurchase_term, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_repo_collateral_security_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_repurchase_term, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(traded_flat_switch, true) -> <<"Y">>;
encode_typed_field(traded_flat_switch,false) -> <<"N">>;
encode_typed_field(basis_feature_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(market_depth, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(md_update_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(aggregated_book, true) -> <<"Y">>;
encode_typed_field(aggregated_book,false) -> <<"N">>;
encode_typed_field(no_md_entry_types, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_md_entries, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(md_entry_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(md_entry_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(seller_days, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(md_entry_position_no, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(def_bid_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(def_offer_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_quote_entries, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_quote_sets, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_cancel_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_response_level, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_quote_entries, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_put_or_call, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_strike_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(ratio_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(security_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(security_response_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(unsolicited_indicator, true) -> <<"Y">>;
encode_typed_field(unsolicited_indicator,false) -> <<"N">>;
encode_typed_field(security_trading_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(in_view_of_common, true) -> <<"Y">>;
encode_typed_field(in_view_of_common,false) -> <<"N">>;
encode_typed_field(due_to_related, true) -> <<"Y">>;
encode_typed_field(due_to_related,false) -> <<"N">>;
encode_typed_field(buy_volume, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(sell_volume, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(high_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(low_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(adjustment, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trad_ses_method, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trad_ses_mode, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trad_ses_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(number_of_orders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_issuer_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_security_desc_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_list_exec_inst_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_text_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_subject_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_headline_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_alloc_text_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_underlying_issuer_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_underlying_security_desc_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(quote_entry_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_msg_seq_num_processed, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(ref_tag_id, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(session_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(solicited_flag, true) -> <<"Y">>;
encode_typed_field(solicited_flag,false) -> <<"N">>;
encode_typed_field(exec_restatement_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(business_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_contra_brokers, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(max_message_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_msg_types, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_trading_sessions, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(total_volume_traded, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_related_sym, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(bid_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(num_tickets, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_bid_descriptors, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(bid_descriptor_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(side_value_ind, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(liquidity_ind_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(exchange_for_physical, true) -> <<"Y">>;
encode_typed_field(exchange_for_physical,false) -> <<"N">>;
encode_typed_field(prog_rpt_reqs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(prog_period_interval, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(inc_tax_ind, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(num_bidders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_bid_components, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_strikes, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(day_order_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(day_cum_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(day_avg_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(gt_booking_inst, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_strikes, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(list_status_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(net_gross_ind, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(list_order_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(contra_trade_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(liquidity_num_securities, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_list_status_text_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(party_role, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_party_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_security_alt_id, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_underlying_security_alt_id, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(product, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_product, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(test_message_indicator, true) -> <<"Y">>;
encode_typed_field(test_message_indicator,false) -> <<"N">>;
encode_typed_field(quantity_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_regist_dtls, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(distrib_payment_method, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_report_trans_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(payment_method, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tax_advantage_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(regist_rej_reason_code, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_distrib_insts, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_cont_amts, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cont_amt_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(owner_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(total_affected_orders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_affected_orders, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested_party_role, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_nested_party_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(md_implicit_delete, true) -> <<"Y">>;
encode_typed_field(md_implicit_delete,false) -> <<"N">>;
encode_typed_field(cross_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cross_prioritization, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_sides, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_legs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_security_types, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_security_types, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(security_list_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(security_request_result, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(round_lot, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(min_trade_vol, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(multi_leg_rpt_type_req, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_covered_or_uncovered, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(trad_ses_status_rej_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(previously_reported, true) -> <<"Y">>;
encode_typed_field(previously_reported,false) -> <<"N">>;
encode_typed_field(odd_lot, true) -> <<"Y">>;
encode_typed_field(odd_lot,false) -> <<"N">>;
encode_typed_field(no_clearing_instructions, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(clearing_instruction, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_dates, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(account_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cust_order_capacity, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(mass_status_req_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_product, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_strike_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(encoded_leg_issuer_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(encoded_leg_security_desc_len, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_hops, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(hop_ref_id, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(mid_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(working_indicator, true) -> <<"Y">>;
encode_typed_field(working_indicator,false) -> <<"N">>;
encode_typed_field(leg_last_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(priority_indicator, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(price2, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(mkt_bid_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(mkt_offer_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(min_bid_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(min_offer_size, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(legal_confirm, true) -> <<"Y">>;
encode_typed_field(legal_confirm,false) -> <<"N">>;
encode_typed_field(underlying_last_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(underlying_last_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(sec_def_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_request_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(acct_id_source, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_acct_id_source, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(benchmark_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(benchmark_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(confirm_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(confirm_trans_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(delivery_form, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_par_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_leg_allocs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_alloc_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_benchmark_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(leg_benchmark_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_bid_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_leg_stipulations, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_offer_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(leg_order_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(leg_swap_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(quote_resp_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(yield_redemption_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(yield_redemption_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(reversal_indicator, true) -> <<"Y">>;
encode_typed_field(reversal_indicator,false) -> <<"N">>;
encode_typed_field(no_positions, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(long_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(short_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_qty_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_trans_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_underlyings, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_maint_action, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(adjustment_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(contrary_instruction_indicator, true) -> <<"Y">>;
encode_typed_field(contrary_instruction_indicator,false) -> <<"N">>;
encode_typed_field(prior_spread_indicator, true) -> <<"Y">>;
encode_typed_field(prior_spread_indicator,false) -> <<"N">>;
encode_typed_field(pos_maint_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_maint_result, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_req_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(response_transport_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(total_num_pos_reports, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_req_result, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pos_req_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(settl_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(settl_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_settl_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(underlying_settl_price_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(prior_settl_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_quote_qualifiers, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(assignment_unit, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_num_trade_reports, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_request_result, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_request_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_report_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(side_multi_leg_reporting_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_pos_amt, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(auto_accept_indicator, true) -> <<"Y">>;
encode_typed_field(auto_accept_indicator,false) -> <<"N">>;
encode_typed_field(no_nested2_party_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested2_party_role, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_trd_reg_timestamps, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trd_reg_timestamp_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(confirm_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(confirm_rej_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(booking_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(individual_alloc_rej_code, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_settl_inst, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_settl_inst_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_settl_party_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(settl_party_role, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(settl_party_sub_id_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(termination_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(next_expected_msg_seq_num, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(settl_inst_req_rej_code, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_report_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_canc_replace_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(copy_msg_indicator, true) -> <<"Y">>;
encode_typed_field(copy_msg_indicator,false) -> <<"N">>;
encode_typed_field(alloc_account_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(order_avg_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(order_booking_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_settl_party_sub_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_party_sub_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(party_sub_id_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_nested_party_sub_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested_party_sub_id_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_nested2_party_sub_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested2_party_sub_id_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_intermed_req_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(appl_queue_max, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(appl_queue_depth, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(appl_queue_resolution, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(appl_queue_action, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_alt_md_source, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(avg_px_indicator, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_alloc_indicator, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(expiration_cycle, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trd_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trd_sub_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_num_assignment_reports, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(peg_move_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(peg_offset_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(peg_limit_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(peg_round_direction, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(pegged_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(peg_scope, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(discretion_move_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(discretion_offset_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(discretion_limit_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(discretion_round_direction, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(discretion_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(discretion_scope, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(target_strategy, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_liquidity_ind, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(publish_trd_indicator, true) -> <<"Y">>;
encode_typed_field(publish_trd_indicator,false) -> <<"N">>;
encode_typed_field(short_sale_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(qty_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(secondary_trd_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trade_report_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(alloc_no_orders_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(avg_par_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(reported_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_capacities, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(order_capacity_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_events, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(event_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(event_px, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_instr_attrib, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(instr_attrib_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(cp_program, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_qty, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(underlying_dirty_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(underlying_end_price, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0]));
encode_typed_field(no_underlying_stips, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(misc_fee_basis, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_no_allocs, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_fragment, true) -> <<"Y">>;
encode_typed_field(last_fragment,false) -> <<"N">>;
encode_typed_field(coll_asgn_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_inquiry_qualifier, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_trades, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_asgn_trans_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_asgn_resp_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_asgn_reject_reason, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(tot_num_reports, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(last_rpt_requested, true) -> <<"Y">>;
encode_typed_field(last_rpt_requested,false) -> <<"N">>;
encode_typed_field(delivery_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(user_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(user_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(status_value, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(network_request_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_comp_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(network_status_response_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_coll_inquiry_qualifier, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(trd_rpt_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(affirm_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_action, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_inquiry_status, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(coll_inquiry_result, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_nested3_party_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested3_party_role, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(no_nested3_party_sub_ids, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(nested3_party_sub_id_type, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(_Key, V) when is_binary(V) -> V;
encode_typed_field(_Key, V) when is_list(V) -> V;
encode_typed_field(_Key, V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_typed_field(_Key, V) when is_float(V) -> iolist_to_binary(io_lib:format("~.3f", [V*1.0])).

number_by_field(account) -> <<"1">>;
number_by_field(adv_id) -> <<"2">>;
number_by_field(adv_ref_id) -> <<"3">>;
number_by_field(adv_side) -> <<"4">>;
number_by_field(adv_trans_type) -> <<"5">>;
number_by_field(avg_px) -> <<"6">>;
number_by_field(begin_seq_no) -> <<"7">>;
number_by_field(begin_string) -> <<"8">>;
number_by_field(body_length) -> <<"9">>;
number_by_field(check_sum) -> <<"10">>;
number_by_field(cl_ord_id) -> <<"11">>;
number_by_field(commission) -> <<"12">>;
number_by_field(comm_type) -> <<"13">>;
number_by_field(cum_qty) -> <<"14">>;
number_by_field(currency) -> <<"15">>;
number_by_field(end_seq_no) -> <<"16">>;
number_by_field(exec_id) -> <<"17">>;
number_by_field(exec_inst) -> <<"18">>;
number_by_field(exec_ref_id) -> <<"19">>;
number_by_field(exec_trans_type) -> <<"20">>;
number_by_field(handl_inst) -> <<"21">>;
number_by_field(security_id_source) -> <<"22">>;
number_by_field(ioi_id) -> <<"23">>;
number_by_field(ioi_oth_svc) -> <<"24">>;
number_by_field(ioi_qlty_ind) -> <<"25">>;
number_by_field(ioi_ref_id) -> <<"26">>;
number_by_field(ioi_qty) -> <<"27">>;
number_by_field(ioi_trans_type) -> <<"28">>;
number_by_field(last_capacity) -> <<"29">>;
number_by_field(last_mkt) -> <<"30">>;
number_by_field(last_px) -> <<"31">>;
number_by_field(last_qty) -> <<"32">>;
number_by_field(no_lines_of_text) -> <<"33">>;
number_by_field(msg_seq_num) -> <<"34">>;
number_by_field(msg_type) -> <<"35">>;
number_by_field(new_seq_no) -> <<"36">>;
number_by_field(order_id) -> <<"37">>;
number_by_field(order_qty) -> <<"38">>;
number_by_field(ord_status) -> <<"39">>;
number_by_field(ord_type) -> <<"40">>;
number_by_field(orig_cl_ord_id) -> <<"41">>;
number_by_field(orig_time) -> <<"42">>;
number_by_field(poss_dup_flag) -> <<"43">>;
number_by_field(price) -> <<"44">>;
number_by_field(ref_seq_num) -> <<"45">>;
number_by_field(relatd_sym) -> <<"46">>;
number_by_field(rule80a) -> <<"47">>;
number_by_field(security_id) -> <<"48">>;
number_by_field(sender_comp_id) -> <<"49">>;
number_by_field(sender_sub_id) -> <<"50">>;
number_by_field(sending_date) -> <<"51">>;
number_by_field(sending_time) -> <<"52">>;
number_by_field(quantity) -> <<"53">>;
number_by_field(side) -> <<"54">>;
number_by_field(symbol) -> <<"55">>;
number_by_field(target_comp_id) -> <<"56">>;
number_by_field(target_sub_id) -> <<"57">>;
number_by_field(text) -> <<"58">>;
number_by_field(time_in_force) -> <<"59">>;
number_by_field(transact_time) -> <<"60">>;
number_by_field(urgency) -> <<"61">>;
number_by_field(valid_until_time) -> <<"62">>;
number_by_field(settl_type) -> <<"63">>;
number_by_field(settl_date) -> <<"64">>;
number_by_field(symbol_sfx) -> <<"65">>;
number_by_field(list_id) -> <<"66">>;
number_by_field(list_seq_no) -> <<"67">>;
number_by_field(tot_no_orders) -> <<"68">>;
number_by_field(list_exec_inst) -> <<"69">>;
number_by_field(alloc_id) -> <<"70">>;
number_by_field(alloc_trans_type) -> <<"71">>;
number_by_field(ref_alloc_id) -> <<"72">>;
number_by_field(no_orders) -> <<"73">>;
number_by_field(avg_px_precision) -> <<"74">>;
number_by_field(trade_date) -> <<"75">>;
number_by_field(exec_broker) -> <<"76">>;
number_by_field(position_effect) -> <<"77">>;
number_by_field(no_allocs) -> <<"78">>;
number_by_field(alloc_account) -> <<"79">>;
number_by_field(alloc_qty) -> <<"80">>;
number_by_field(process_code) -> <<"81">>;
number_by_field(no_rpts) -> <<"82">>;
number_by_field(rpt_seq) -> <<"83">>;
number_by_field(cxl_qty) -> <<"84">>;
number_by_field(no_dlvy_inst) -> <<"85">>;
number_by_field(dlvy_inst) -> <<"86">>;
number_by_field(alloc_status) -> <<"87">>;
number_by_field(alloc_rej_code) -> <<"88">>;
number_by_field(signature) -> <<"89">>;
number_by_field(secure_data_len) -> <<"90">>;
number_by_field(secure_data) -> <<"91">>;
number_by_field(broker_of_credit) -> <<"92">>;
number_by_field(signature_length) -> <<"93">>;
number_by_field(email_type) -> <<"94">>;
number_by_field(raw_data_length) -> <<"95">>;
number_by_field(raw_data) -> <<"96">>;
number_by_field(poss_resend) -> <<"97">>;
number_by_field(encrypt_method) -> <<"98">>;
number_by_field(stop_px) -> <<"99">>;
number_by_field(ex_destination) -> <<"100">>;
number_by_field(cxl_rej_reason) -> <<"102">>;
number_by_field(ord_rej_reason) -> <<"103">>;
number_by_field(ioi_qualifier) -> <<"104">>;
number_by_field(wave_no) -> <<"105">>;
number_by_field(issuer) -> <<"106">>;
number_by_field(security_desc) -> <<"107">>;
number_by_field(heart_bt_int) -> <<"108">>;
number_by_field(client_id) -> <<"109">>;
number_by_field(min_qty) -> <<"110">>;
number_by_field(max_floor) -> <<"111">>;
number_by_field(test_req_id) -> <<"112">>;
number_by_field(report_to_exch) -> <<"113">>;
number_by_field(locate_reqd) -> <<"114">>;
number_by_field(on_behalf_of_comp_id) -> <<"115">>;
number_by_field(on_behalf_of_sub_id) -> <<"116">>;
number_by_field(quote_id) -> <<"117">>;
number_by_field(net_money) -> <<"118">>;
number_by_field(settl_curr_amt) -> <<"119">>;
number_by_field(settl_currency) -> <<"120">>;
number_by_field(forex_req) -> <<"121">>;
number_by_field(orig_sending_time) -> <<"122">>;
number_by_field(gap_fill_flag) -> <<"123">>;
number_by_field(no_execs) -> <<"124">>;
number_by_field(cxl_type) -> <<"125">>;
number_by_field(expire_time) -> <<"126">>;
number_by_field(dk_reason) -> <<"127">>;
number_by_field(deliver_to_comp_id) -> <<"128">>;
number_by_field(deliver_to_sub_id) -> <<"129">>;
number_by_field(ioi_natural_flag) -> <<"130">>;
number_by_field(quote_req_id) -> <<"131">>;
number_by_field(bid_px) -> <<"132">>;
number_by_field(offer_px) -> <<"133">>;
number_by_field(bid_size) -> <<"134">>;
number_by_field(offer_size) -> <<"135">>;
number_by_field(no_misc_fees) -> <<"136">>;
number_by_field(misc_fee_amt) -> <<"137">>;
number_by_field(misc_fee_curr) -> <<"138">>;
number_by_field(misc_fee_type) -> <<"139">>;
number_by_field(prev_close_px) -> <<"140">>;
number_by_field(reset_seq_num_flag) -> <<"141">>;
number_by_field(sender_location_id) -> <<"142">>;
number_by_field(target_location_id) -> <<"143">>;
number_by_field(on_behalf_of_location_id) -> <<"144">>;
number_by_field(deliver_to_location_id) -> <<"145">>;
number_by_field(no_related_sym) -> <<"146">>;
number_by_field(subject) -> <<"147">>;
number_by_field(headline) -> <<"148">>;
number_by_field(url_link) -> <<"149">>;
number_by_field(exec_type) -> <<"150">>;
number_by_field(leaves_qty) -> <<"151">>;
number_by_field(cash_order_qty) -> <<"152">>;
number_by_field(alloc_avg_px) -> <<"153">>;
number_by_field(alloc_net_money) -> <<"154">>;
number_by_field(settl_curr_fx_rate) -> <<"155">>;
number_by_field(settl_curr_fx_rate_calc) -> <<"156">>;
number_by_field(num_days_interest) -> <<"157">>;
number_by_field(accrued_interest_rate) -> <<"158">>;
number_by_field(accrued_interest_amt) -> <<"159">>;
number_by_field(settl_inst_mode) -> <<"160">>;
number_by_field(alloc_text) -> <<"161">>;
number_by_field(settl_inst_id) -> <<"162">>;
number_by_field(settl_inst_trans_type) -> <<"163">>;
number_by_field(email_thread_id) -> <<"164">>;
number_by_field(settl_inst_source) -> <<"165">>;
number_by_field(settl_location) -> <<"166">>;
number_by_field(security_type) -> <<"167">>;
number_by_field(effective_time) -> <<"168">>;
number_by_field(stand_inst_db_type) -> <<"169">>;
number_by_field(stand_inst_db_name) -> <<"170">>;
number_by_field(stand_inst_db_id) -> <<"171">>;
number_by_field(settl_delivery_type) -> <<"172">>;
number_by_field(settl_depository_code) -> <<"173">>;
number_by_field(settl_brkr_code) -> <<"174">>;
number_by_field(settl_inst_code) -> <<"175">>;
number_by_field(security_settl_agent_name) -> <<"176">>;
number_by_field(security_settl_agent_code) -> <<"177">>;
number_by_field(security_settl_agent_acct_num) -> <<"178">>;
number_by_field(security_settl_agent_acct_name) -> <<"179">>;
number_by_field(security_settl_agent_contact_name) -> <<"180">>;
number_by_field(security_settl_agent_contact_phone) -> <<"181">>;
number_by_field(cash_settl_agent_name) -> <<"182">>;
number_by_field(cash_settl_agent_code) -> <<"183">>;
number_by_field(cash_settl_agent_acct_num) -> <<"184">>;
number_by_field(cash_settl_agent_acct_name) -> <<"185">>;
number_by_field(cash_settl_agent_contact_name) -> <<"186">>;
number_by_field(cash_settl_agent_contact_phone) -> <<"187">>;
number_by_field(bid_spot_rate) -> <<"188">>;
number_by_field(bid_forward_points) -> <<"189">>;
number_by_field(offer_spot_rate) -> <<"190">>;
number_by_field(offer_forward_points) -> <<"191">>;
number_by_field(order_qty2) -> <<"192">>;
number_by_field(settl_date2) -> <<"193">>;
number_by_field(last_spot_rate) -> <<"194">>;
number_by_field(last_forward_points) -> <<"195">>;
number_by_field(alloc_link_id) -> <<"196">>;
number_by_field(alloc_link_type) -> <<"197">>;
number_by_field(secondary_order_id) -> <<"198">>;
number_by_field(no_ioi_qualifiers) -> <<"199">>;
number_by_field(maturity_month_year) -> <<"200">>;
number_by_field(put_or_call) -> <<"201">>;
number_by_field(strike_price) -> <<"202">>;
number_by_field(covered_or_uncovered) -> <<"203">>;
number_by_field(customer_or_firm) -> <<"204">>;
number_by_field(maturity_day) -> <<"205">>;
number_by_field(opt_attribute) -> <<"206">>;
number_by_field(security_exchange) -> <<"207">>;
number_by_field(notify_broker_of_credit) -> <<"208">>;
number_by_field(alloc_handl_inst) -> <<"209">>;
number_by_field(max_show) -> <<"210">>;
number_by_field(peg_offset_value) -> <<"211">>;
number_by_field(xml_data_len) -> <<"212">>;
number_by_field(xml_data) -> <<"213">>;
number_by_field(settl_inst_ref_id) -> <<"214">>;
number_by_field(no_routing_ids) -> <<"215">>;
number_by_field(routing_type) -> <<"216">>;
number_by_field(routing_id) -> <<"217">>;
number_by_field(spread) -> <<"218">>;
number_by_field(benchmark) -> <<"219">>;
number_by_field(benchmark_curve_currency) -> <<"220">>;
number_by_field(benchmark_curve_name) -> <<"221">>;
number_by_field(benchmark_curve_point) -> <<"222">>;
number_by_field(coupon_rate) -> <<"223">>;
number_by_field(coupon_payment_date) -> <<"224">>;
number_by_field(issue_date) -> <<"225">>;
number_by_field(repurchase_term) -> <<"226">>;
number_by_field(repurchase_rate) -> <<"227">>;
number_by_field(factor) -> <<"228">>;
number_by_field(trade_origination_date) -> <<"229">>;
number_by_field(ex_date) -> <<"230">>;
number_by_field(contract_multiplier) -> <<"231">>;
number_by_field(no_stipulations) -> <<"232">>;
number_by_field(stipulation_type) -> <<"233">>;
number_by_field(stipulation_value) -> <<"234">>;
number_by_field(yield_type) -> <<"235">>;
number_by_field(yield) -> <<"236">>;
number_by_field(total_takedown) -> <<"237">>;
number_by_field(concession) -> <<"238">>;
number_by_field(repo_collateral_security_type) -> <<"239">>;
number_by_field(redemption_date) -> <<"240">>;
number_by_field(underlying_coupon_payment_date) -> <<"241">>;
number_by_field(underlying_issue_date) -> <<"242">>;
number_by_field(underlying_repo_collateral_security_type) -> <<"243">>;
number_by_field(underlying_repurchase_term) -> <<"244">>;
number_by_field(underlying_repurchase_rate) -> <<"245">>;
number_by_field(underlying_factor) -> <<"246">>;
number_by_field(underlying_redemption_date) -> <<"247">>;
number_by_field(leg_coupon_payment_date) -> <<"248">>;
number_by_field(leg_issue_date) -> <<"249">>;
number_by_field(leg_repo_collateral_security_type) -> <<"250">>;
number_by_field(leg_repurchase_term) -> <<"251">>;
number_by_field(leg_repurchase_rate) -> <<"252">>;
number_by_field(leg_factor) -> <<"253">>;
number_by_field(leg_redemption_date) -> <<"254">>;
number_by_field(credit_rating) -> <<"255">>;
number_by_field(underlying_credit_rating) -> <<"256">>;
number_by_field(leg_credit_rating) -> <<"257">>;
number_by_field(traded_flat_switch) -> <<"258">>;
number_by_field(basis_feature_date) -> <<"259">>;
number_by_field(basis_feature_price) -> <<"260">>;
number_by_field(md_req_id) -> <<"262">>;
number_by_field(subscription_request_type) -> <<"263">>;
number_by_field(market_depth) -> <<"264">>;
number_by_field(md_update_type) -> <<"265">>;
number_by_field(aggregated_book) -> <<"266">>;
number_by_field(no_md_entry_types) -> <<"267">>;
number_by_field(no_md_entries) -> <<"268">>;
number_by_field(md_entry_type) -> <<"269">>;
number_by_field(md_entry_px) -> <<"270">>;
number_by_field(md_entry_size) -> <<"271">>;
number_by_field(md_entry_date) -> <<"272">>;
number_by_field(md_entry_time) -> <<"273">>;
number_by_field(tick_direction) -> <<"274">>;
number_by_field(md_mkt) -> <<"275">>;
number_by_field(quote_condition) -> <<"276">>;
number_by_field(trade_condition) -> <<"277">>;
number_by_field(md_entry_id) -> <<"278">>;
number_by_field(md_update_action) -> <<"279">>;
number_by_field(md_entry_ref_id) -> <<"280">>;
number_by_field(md_req_rej_reason) -> <<"281">>;
number_by_field(md_entry_originator) -> <<"282">>;
number_by_field(location_id) -> <<"283">>;
number_by_field(desk_id) -> <<"284">>;
number_by_field(delete_reason) -> <<"285">>;
number_by_field(open_close_settl_flag) -> <<"286">>;
number_by_field(seller_days) -> <<"287">>;
number_by_field(md_entry_buyer) -> <<"288">>;
number_by_field(md_entry_seller) -> <<"289">>;
number_by_field(md_entry_position_no) -> <<"290">>;
number_by_field(financial_status) -> <<"291">>;
number_by_field(corporate_action) -> <<"292">>;
number_by_field(def_bid_size) -> <<"293">>;
number_by_field(def_offer_size) -> <<"294">>;
number_by_field(no_quote_entries) -> <<"295">>;
number_by_field(no_quote_sets) -> <<"296">>;
number_by_field(quote_status) -> <<"297">>;
number_by_field(quote_cancel_type) -> <<"298">>;
number_by_field(quote_entry_id) -> <<"299">>;
number_by_field(quote_reject_reason) -> <<"300">>;
number_by_field(quote_response_level) -> <<"301">>;
number_by_field(quote_set_id) -> <<"302">>;
number_by_field(quote_request_type) -> <<"303">>;
number_by_field(tot_no_quote_entries) -> <<"304">>;
number_by_field(underlying_security_id_source) -> <<"305">>;
number_by_field(underlying_issuer) -> <<"306">>;
number_by_field(underlying_security_desc) -> <<"307">>;
number_by_field(underlying_security_exchange) -> <<"308">>;
number_by_field(underlying_security_id) -> <<"309">>;
number_by_field(underlying_security_type) -> <<"310">>;
number_by_field(underlying_symbol) -> <<"311">>;
number_by_field(underlying_symbol_sfx) -> <<"312">>;
number_by_field(underlying_maturity_month_year) -> <<"313">>;
number_by_field(underlying_maturity_day) -> <<"314">>;
number_by_field(underlying_put_or_call) -> <<"315">>;
number_by_field(underlying_strike_price) -> <<"316">>;
number_by_field(underlying_opt_attribute) -> <<"317">>;
number_by_field(underlying_currency) -> <<"318">>;
number_by_field(ratio_qty) -> <<"319">>;
number_by_field(security_req_id) -> <<"320">>;
number_by_field(security_request_type) -> <<"321">>;
number_by_field(security_response_id) -> <<"322">>;
number_by_field(security_response_type) -> <<"323">>;
number_by_field(security_status_req_id) -> <<"324">>;
number_by_field(unsolicited_indicator) -> <<"325">>;
number_by_field(security_trading_status) -> <<"326">>;
number_by_field(halt_reason_char) -> <<"327">>;
number_by_field(in_view_of_common) -> <<"328">>;
number_by_field(due_to_related) -> <<"329">>;
number_by_field(buy_volume) -> <<"330">>;
number_by_field(sell_volume) -> <<"331">>;
number_by_field(high_px) -> <<"332">>;
number_by_field(low_px) -> <<"333">>;
number_by_field(adjustment) -> <<"334">>;
number_by_field(trad_ses_req_id) -> <<"335">>;
number_by_field(trading_session_id) -> <<"336">>;
number_by_field(contra_trader) -> <<"337">>;
number_by_field(trad_ses_method) -> <<"338">>;
number_by_field(trad_ses_mode) -> <<"339">>;
number_by_field(trad_ses_status) -> <<"340">>;
number_by_field(trad_ses_start_time) -> <<"341">>;
number_by_field(trad_ses_open_time) -> <<"342">>;
number_by_field(trad_ses_pre_close_time) -> <<"343">>;
number_by_field(trad_ses_close_time) -> <<"344">>;
number_by_field(trad_ses_end_time) -> <<"345">>;
number_by_field(number_of_orders) -> <<"346">>;
number_by_field(message_encoding) -> <<"347">>;
number_by_field(encoded_issuer_len) -> <<"348">>;
number_by_field(encoded_issuer) -> <<"349">>;
number_by_field(encoded_security_desc_len) -> <<"350">>;
number_by_field(encoded_security_desc) -> <<"351">>;
number_by_field(encoded_list_exec_inst_len) -> <<"352">>;
number_by_field(encoded_list_exec_inst) -> <<"353">>;
number_by_field(encoded_text_len) -> <<"354">>;
number_by_field(encoded_text) -> <<"355">>;
number_by_field(encoded_subject_len) -> <<"356">>;
number_by_field(encoded_subject) -> <<"357">>;
number_by_field(encoded_headline_len) -> <<"358">>;
number_by_field(encoded_headline) -> <<"359">>;
number_by_field(encoded_alloc_text_len) -> <<"360">>;
number_by_field(encoded_alloc_text) -> <<"361">>;
number_by_field(encoded_underlying_issuer_len) -> <<"362">>;
number_by_field(encoded_underlying_issuer) -> <<"363">>;
number_by_field(encoded_underlying_security_desc_len) -> <<"364">>;
number_by_field(encoded_underlying_security_desc) -> <<"365">>;
number_by_field(alloc_price) -> <<"366">>;
number_by_field(quote_set_valid_until_time) -> <<"367">>;
number_by_field(quote_entry_reject_reason) -> <<"368">>;
number_by_field(last_msg_seq_num_processed) -> <<"369">>;
number_by_field(on_behalf_of_sending_time) -> <<"370">>;
number_by_field(ref_tag_id) -> <<"371">>;
number_by_field(ref_msg_type) -> <<"372">>;
number_by_field(session_reject_reason) -> <<"373">>;
number_by_field(bid_request_trans_type) -> <<"374">>;
number_by_field(contra_broker) -> <<"375">>;
number_by_field(compliance_id) -> <<"376">>;
number_by_field(solicited_flag) -> <<"377">>;
number_by_field(exec_restatement_reason) -> <<"378">>;
number_by_field(business_reject_ref_id) -> <<"379">>;
number_by_field(business_reject_reason) -> <<"380">>;
number_by_field(gross_trade_amt) -> <<"381">>;
number_by_field(no_contra_brokers) -> <<"382">>;
number_by_field(max_message_size) -> <<"383">>;
number_by_field(no_msg_types) -> <<"384">>;
number_by_field(msg_direction) -> <<"385">>;
number_by_field(no_trading_sessions) -> <<"386">>;
number_by_field(total_volume_traded) -> <<"387">>;
number_by_field(discretion_inst) -> <<"388">>;
number_by_field(discretion_offset_value) -> <<"389">>;
number_by_field(bid_id) -> <<"390">>;
number_by_field(client_bid_id) -> <<"391">>;
number_by_field(list_name) -> <<"392">>;
number_by_field(tot_no_related_sym) -> <<"393">>;
number_by_field(bid_type) -> <<"394">>;
number_by_field(num_tickets) -> <<"395">>;
number_by_field(side_value1) -> <<"396">>;
number_by_field(side_value2) -> <<"397">>;
number_by_field(no_bid_descriptors) -> <<"398">>;
number_by_field(bid_descriptor_type) -> <<"399">>;
number_by_field(bid_descriptor) -> <<"400">>;
number_by_field(side_value_ind) -> <<"401">>;
number_by_field(liquidity_pct_low) -> <<"402">>;
number_by_field(liquidity_pct_high) -> <<"403">>;
number_by_field(liquidity_value) -> <<"404">>;
number_by_field(efp_tracking_error) -> <<"405">>;
number_by_field(fair_value) -> <<"406">>;
number_by_field(outside_index_pct) -> <<"407">>;
number_by_field(value_of_futures) -> <<"408">>;
number_by_field(liquidity_ind_type) -> <<"409">>;
number_by_field(wt_average_liquidity) -> <<"410">>;
number_by_field(exchange_for_physical) -> <<"411">>;
number_by_field(out_main_cntry_u_index) -> <<"412">>;
number_by_field(cross_percent) -> <<"413">>;
number_by_field(prog_rpt_reqs) -> <<"414">>;
number_by_field(prog_period_interval) -> <<"415">>;
number_by_field(inc_tax_ind) -> <<"416">>;
number_by_field(num_bidders) -> <<"417">>;
number_by_field(bid_trade_type) -> <<"418">>;
number_by_field(basis_px_type) -> <<"419">>;
number_by_field(no_bid_components) -> <<"420">>;
number_by_field(country) -> <<"421">>;
number_by_field(tot_no_strikes) -> <<"422">>;
number_by_field(price_type) -> <<"423">>;
number_by_field(day_order_qty) -> <<"424">>;
number_by_field(day_cum_qty) -> <<"425">>;
number_by_field(day_avg_px) -> <<"426">>;
number_by_field(gt_booking_inst) -> <<"427">>;
number_by_field(no_strikes) -> <<"428">>;
number_by_field(list_status_type) -> <<"429">>;
number_by_field(net_gross_ind) -> <<"430">>;
number_by_field(list_order_status) -> <<"431">>;
number_by_field(expire_date) -> <<"432">>;
number_by_field(list_exec_inst_type) -> <<"433">>;
number_by_field(cxl_rej_response_to) -> <<"434">>;
number_by_field(underlying_coupon_rate) -> <<"435">>;
number_by_field(underlying_contract_multiplier) -> <<"436">>;
number_by_field(contra_trade_qty) -> <<"437">>;
number_by_field(contra_trade_time) -> <<"438">>;
number_by_field(clearing_firm) -> <<"439">>;
number_by_field(clearing_account) -> <<"440">>;
number_by_field(liquidity_num_securities) -> <<"441">>;
number_by_field(multi_leg_reporting_type) -> <<"442">>;
number_by_field(strike_time) -> <<"443">>;
number_by_field(list_status_text) -> <<"444">>;
number_by_field(encoded_list_status_text_len) -> <<"445">>;
number_by_field(encoded_list_status_text) -> <<"446">>;
number_by_field(party_id_source) -> <<"447">>;
number_by_field(party_id) -> <<"448">>;
number_by_field(total_volume_traded_date) -> <<"449">>;
number_by_field(total_volume_traded_time) -> <<"450">>;
number_by_field(net_chg_prev_day) -> <<"451">>;
number_by_field(party_role) -> <<"452">>;
number_by_field(no_party_ids) -> <<"453">>;
number_by_field(no_security_alt_id) -> <<"454">>;
number_by_field(security_alt_id) -> <<"455">>;
number_by_field(security_alt_id_source) -> <<"456">>;
number_by_field(no_underlying_security_alt_id) -> <<"457">>;
number_by_field(underlying_security_alt_id) -> <<"458">>;
number_by_field(underlying_security_alt_id_source) -> <<"459">>;
number_by_field(product) -> <<"460">>;
number_by_field(cfi_code) -> <<"461">>;
number_by_field(underlying_product) -> <<"462">>;
number_by_field(underlying_cfi_code) -> <<"463">>;
number_by_field(test_message_indicator) -> <<"464">>;
number_by_field(quantity_type) -> <<"465">>;
number_by_field(booking_ref_id) -> <<"466">>;
number_by_field(individual_alloc_id) -> <<"467">>;
number_by_field(rounding_direction) -> <<"468">>;
number_by_field(rounding_modulus) -> <<"469">>;
number_by_field(country_of_issue) -> <<"470">>;
number_by_field(state_or_province_of_issue) -> <<"471">>;
number_by_field(locale_of_issue) -> <<"472">>;
number_by_field(no_regist_dtls) -> <<"473">>;
number_by_field(mailing_dtls) -> <<"474">>;
number_by_field(investor_country_of_residence) -> <<"475">>;
number_by_field(payment_ref) -> <<"476">>;
number_by_field(distrib_payment_method) -> <<"477">>;
number_by_field(cash_distrib_curr) -> <<"478">>;
number_by_field(comm_currency) -> <<"479">>;
number_by_field(cancellation_rights) -> <<"480">>;
number_by_field(money_laundering_status) -> <<"481">>;
number_by_field(mailing_inst) -> <<"482">>;
number_by_field(trans_bkd_time) -> <<"483">>;
number_by_field(exec_price_type) -> <<"484">>;
number_by_field(exec_price_adjustment) -> <<"485">>;
number_by_field(date_of_birth) -> <<"486">>;
number_by_field(trade_report_trans_type) -> <<"487">>;
number_by_field(card_holder_name) -> <<"488">>;
number_by_field(card_number) -> <<"489">>;
number_by_field(card_exp_date) -> <<"490">>;
number_by_field(card_iss_num) -> <<"491">>;
number_by_field(payment_method) -> <<"492">>;
number_by_field(regist_acct_type) -> <<"493">>;
number_by_field(designation) -> <<"494">>;
number_by_field(tax_advantage_type) -> <<"495">>;
number_by_field(regist_rej_reason_text) -> <<"496">>;
number_by_field(fund_renew_waiv) -> <<"497">>;
number_by_field(cash_distrib_agent_name) -> <<"498">>;
number_by_field(cash_distrib_agent_code) -> <<"499">>;
number_by_field(cash_distrib_agent_acct_number) -> <<"500">>;
number_by_field(cash_distrib_pay_ref) -> <<"501">>;
number_by_field(cash_distrib_agent_acct_name) -> <<"502">>;
number_by_field(card_start_date) -> <<"503">>;
number_by_field(payment_date) -> <<"504">>;
number_by_field(payment_remitter_id) -> <<"505">>;
number_by_field(regist_status) -> <<"506">>;
number_by_field(regist_rej_reason_code) -> <<"507">>;
number_by_field(regist_ref_id) -> <<"508">>;
number_by_field(regist_dtls) -> <<"509">>;
number_by_field(no_distrib_insts) -> <<"510">>;
number_by_field(regist_email) -> <<"511">>;
number_by_field(distrib_percentage) -> <<"512">>;
number_by_field(regist_id) -> <<"513">>;
number_by_field(regist_trans_type) -> <<"514">>;
number_by_field(exec_valuation_point) -> <<"515">>;
number_by_field(order_percent) -> <<"516">>;
number_by_field(ownership_type) -> <<"517">>;
number_by_field(no_cont_amts) -> <<"518">>;
number_by_field(cont_amt_type) -> <<"519">>;
number_by_field(cont_amt_value) -> <<"520">>;
number_by_field(cont_amt_curr) -> <<"521">>;
number_by_field(owner_type) -> <<"522">>;
number_by_field(party_sub_id) -> <<"523">>;
number_by_field(nested_party_id) -> <<"524">>;
number_by_field(nested_party_id_source) -> <<"525">>;
number_by_field(secondary_cl_ord_id) -> <<"526">>;
number_by_field(secondary_exec_id) -> <<"527">>;
number_by_field(order_capacity) -> <<"528">>;
number_by_field(order_restrictions) -> <<"529">>;
number_by_field(mass_cancel_request_type) -> <<"530">>;
number_by_field(mass_cancel_response) -> <<"531">>;
number_by_field(mass_cancel_reject_reason) -> <<"532">>;
number_by_field(total_affected_orders) -> <<"533">>;
number_by_field(no_affected_orders) -> <<"534">>;
number_by_field(affected_order_id) -> <<"535">>;
number_by_field(affected_secondary_order_id) -> <<"536">>;
number_by_field(quote_type) -> <<"537">>;
number_by_field(nested_party_role) -> <<"538">>;
number_by_field(no_nested_party_ids) -> <<"539">>;
number_by_field(total_accrued_interest_amt) -> <<"540">>;
number_by_field(maturity_date) -> <<"541">>;
number_by_field(underlying_maturity_date) -> <<"542">>;
number_by_field(instr_registry) -> <<"543">>;
number_by_field(cash_margin) -> <<"544">>;
number_by_field(nested_party_sub_id) -> <<"545">>;
number_by_field(scope) -> <<"546">>;
number_by_field(md_implicit_delete) -> <<"547">>;
number_by_field(cross_id) -> <<"548">>;
number_by_field(cross_type) -> <<"549">>;
number_by_field(cross_prioritization) -> <<"550">>;
number_by_field(orig_cross_id) -> <<"551">>;
number_by_field(no_sides) -> <<"552">>;
number_by_field(username) -> <<"553">>;
number_by_field(password) -> <<"554">>;
number_by_field(no_legs) -> <<"555">>;
number_by_field(leg_currency) -> <<"556">>;
number_by_field(tot_no_security_types) -> <<"557">>;
number_by_field(no_security_types) -> <<"558">>;
number_by_field(security_list_request_type) -> <<"559">>;
number_by_field(security_request_result) -> <<"560">>;
number_by_field(round_lot) -> <<"561">>;
number_by_field(min_trade_vol) -> <<"562">>;
number_by_field(multi_leg_rpt_type_req) -> <<"563">>;
number_by_field(leg_position_effect) -> <<"564">>;
number_by_field(leg_covered_or_uncovered) -> <<"565">>;
number_by_field(leg_price) -> <<"566">>;
number_by_field(trad_ses_status_rej_reason) -> <<"567">>;
number_by_field(trade_request_id) -> <<"568">>;
number_by_field(trade_request_type) -> <<"569">>;
number_by_field(previously_reported) -> <<"570">>;
number_by_field(trade_report_id) -> <<"571">>;
number_by_field(trade_report_ref_id) -> <<"572">>;
number_by_field(match_status) -> <<"573">>;
number_by_field(match_type) -> <<"574">>;
number_by_field(odd_lot) -> <<"575">>;
number_by_field(no_clearing_instructions) -> <<"576">>;
number_by_field(clearing_instruction) -> <<"577">>;
number_by_field(trade_input_source) -> <<"578">>;
number_by_field(trade_input_device) -> <<"579">>;
number_by_field(no_dates) -> <<"580">>;
number_by_field(account_type) -> <<"581">>;
number_by_field(cust_order_capacity) -> <<"582">>;
number_by_field(cl_ord_link_id) -> <<"583">>;
number_by_field(mass_status_req_id) -> <<"584">>;
number_by_field(mass_status_req_type) -> <<"585">>;
number_by_field(orig_ord_mod_time) -> <<"586">>;
number_by_field(leg_settl_type) -> <<"587">>;
number_by_field(leg_settl_date) -> <<"588">>;
number_by_field(day_booking_inst) -> <<"589">>;
number_by_field(booking_unit) -> <<"590">>;
number_by_field(prealloc_method) -> <<"591">>;
number_by_field(underlying_country_of_issue) -> <<"592">>;
number_by_field(underlying_state_or_province_of_issue) -> <<"593">>;
number_by_field(underlying_locale_of_issue) -> <<"594">>;
number_by_field(underlying_instr_registry) -> <<"595">>;
number_by_field(leg_country_of_issue) -> <<"596">>;
number_by_field(leg_state_or_province_of_issue) -> <<"597">>;
number_by_field(leg_locale_of_issue) -> <<"598">>;
number_by_field(leg_instr_registry) -> <<"599">>;
number_by_field(leg_symbol) -> <<"600">>;
number_by_field(leg_symbol_sfx) -> <<"601">>;
number_by_field(leg_security_id) -> <<"602">>;
number_by_field(leg_security_id_source) -> <<"603">>;
number_by_field(no_leg_security_alt_id) -> <<"604">>;
number_by_field(leg_security_alt_id) -> <<"605">>;
number_by_field(leg_security_alt_id_source) -> <<"606">>;
number_by_field(leg_product) -> <<"607">>;
number_by_field(leg_cfi_code) -> <<"608">>;
number_by_field(leg_security_type) -> <<"609">>;
number_by_field(leg_maturity_month_year) -> <<"610">>;
number_by_field(leg_maturity_date) -> <<"611">>;
number_by_field(leg_strike_price) -> <<"612">>;
number_by_field(leg_opt_attribute) -> <<"613">>;
number_by_field(leg_contract_multiplier) -> <<"614">>;
number_by_field(leg_coupon_rate) -> <<"615">>;
number_by_field(leg_security_exchange) -> <<"616">>;
number_by_field(leg_issuer) -> <<"617">>;
number_by_field(encoded_leg_issuer_len) -> <<"618">>;
number_by_field(encoded_leg_issuer) -> <<"619">>;
number_by_field(leg_security_desc) -> <<"620">>;
number_by_field(encoded_leg_security_desc_len) -> <<"621">>;
number_by_field(encoded_leg_security_desc) -> <<"622">>;
number_by_field(leg_ratio_qty) -> <<"623">>;
number_by_field(leg_side) -> <<"624">>;
number_by_field(trading_session_sub_id) -> <<"625">>;
number_by_field(alloc_type) -> <<"626">>;
number_by_field(no_hops) -> <<"627">>;
number_by_field(hop_comp_id) -> <<"628">>;
number_by_field(hop_sending_time) -> <<"629">>;
number_by_field(hop_ref_id) -> <<"630">>;
number_by_field(mid_px) -> <<"631">>;
number_by_field(bid_yield) -> <<"632">>;
number_by_field(mid_yield) -> <<"633">>;
number_by_field(offer_yield) -> <<"634">>;
number_by_field(clearing_fee_indicator) -> <<"635">>;
number_by_field(working_indicator) -> <<"636">>;
number_by_field(leg_last_px) -> <<"637">>;
number_by_field(priority_indicator) -> <<"638">>;
number_by_field(price_improvement) -> <<"639">>;
number_by_field(price2) -> <<"640">>;
number_by_field(last_forward_points2) -> <<"641">>;
number_by_field(bid_forward_points2) -> <<"642">>;
number_by_field(offer_forward_points2) -> <<"643">>;
number_by_field(rfq_req_id) -> <<"644">>;
number_by_field(mkt_bid_px) -> <<"645">>;
number_by_field(mkt_offer_px) -> <<"646">>;
number_by_field(min_bid_size) -> <<"647">>;
number_by_field(min_offer_size) -> <<"648">>;
number_by_field(quote_status_req_id) -> <<"649">>;
number_by_field(legal_confirm) -> <<"650">>;
number_by_field(underlying_last_px) -> <<"651">>;
number_by_field(underlying_last_qty) -> <<"652">>;
number_by_field(sec_def_status) -> <<"653">>;
number_by_field(leg_ref_id) -> <<"654">>;
number_by_field(contra_leg_ref_id) -> <<"655">>;
number_by_field(settl_curr_bid_fx_rate) -> <<"656">>;
number_by_field(settl_curr_offer_fx_rate) -> <<"657">>;
number_by_field(quote_request_reject_reason) -> <<"658">>;
number_by_field(side_compliance_id) -> <<"659">>;
number_by_field(acct_id_source) -> <<"660">>;
number_by_field(alloc_acct_id_source) -> <<"661">>;
number_by_field(benchmark_price) -> <<"662">>;
number_by_field(benchmark_price_type) -> <<"663">>;
number_by_field(confirm_id) -> <<"664">>;
number_by_field(confirm_status) -> <<"665">>;
number_by_field(confirm_trans_type) -> <<"666">>;
number_by_field(contract_settl_month) -> <<"667">>;
number_by_field(delivery_form) -> <<"668">>;
number_by_field(last_par_px) -> <<"669">>;
number_by_field(no_leg_allocs) -> <<"670">>;
number_by_field(leg_alloc_account) -> <<"671">>;
number_by_field(leg_individual_alloc_id) -> <<"672">>;
number_by_field(leg_alloc_qty) -> <<"673">>;
number_by_field(leg_alloc_acct_id_source) -> <<"674">>;
number_by_field(leg_settl_currency) -> <<"675">>;
number_by_field(leg_benchmark_curve_currency) -> <<"676">>;
number_by_field(leg_benchmark_curve_name) -> <<"677">>;
number_by_field(leg_benchmark_curve_point) -> <<"678">>;
number_by_field(leg_benchmark_price) -> <<"679">>;
number_by_field(leg_benchmark_price_type) -> <<"680">>;
number_by_field(leg_bid_px) -> <<"681">>;
number_by_field(leg_ioi_qty) -> <<"682">>;
number_by_field(no_leg_stipulations) -> <<"683">>;
number_by_field(leg_offer_px) -> <<"684">>;
number_by_field(leg_order_qty) -> <<"685">>;
number_by_field(leg_price_type) -> <<"686">>;
number_by_field(leg_qty) -> <<"687">>;
number_by_field(leg_stipulation_type) -> <<"688">>;
number_by_field(leg_stipulation_value) -> <<"689">>;
number_by_field(leg_swap_type) -> <<"690">>;
number_by_field(pool) -> <<"691">>;
number_by_field(quote_price_type) -> <<"692">>;
number_by_field(quote_resp_id) -> <<"693">>;
number_by_field(quote_resp_type) -> <<"694">>;
number_by_field(quote_qualifier) -> <<"695">>;
number_by_field(yield_redemption_date) -> <<"696">>;
number_by_field(yield_redemption_price) -> <<"697">>;
number_by_field(yield_redemption_price_type) -> <<"698">>;
number_by_field(benchmark_security_id) -> <<"699">>;
number_by_field(reversal_indicator) -> <<"700">>;
number_by_field(yield_calc_date) -> <<"701">>;
number_by_field(no_positions) -> <<"702">>;
number_by_field(pos_type) -> <<"703">>;
number_by_field(long_qty) -> <<"704">>;
number_by_field(short_qty) -> <<"705">>;
number_by_field(pos_qty_status) -> <<"706">>;
number_by_field(pos_amt_type) -> <<"707">>;
number_by_field(pos_amt) -> <<"708">>;
number_by_field(pos_trans_type) -> <<"709">>;
number_by_field(pos_req_id) -> <<"710">>;
number_by_field(no_underlyings) -> <<"711">>;
number_by_field(pos_maint_action) -> <<"712">>;
number_by_field(orig_pos_req_ref_id) -> <<"713">>;
number_by_field(pos_maint_rpt_ref_id) -> <<"714">>;
number_by_field(clearing_business_date) -> <<"715">>;
number_by_field(settl_sess_id) -> <<"716">>;
number_by_field(settl_sess_sub_id) -> <<"717">>;
number_by_field(adjustment_type) -> <<"718">>;
number_by_field(contrary_instruction_indicator) -> <<"719">>;
number_by_field(prior_spread_indicator) -> <<"720">>;
number_by_field(pos_maint_rpt_id) -> <<"721">>;
number_by_field(pos_maint_status) -> <<"722">>;
number_by_field(pos_maint_result) -> <<"723">>;
number_by_field(pos_req_type) -> <<"724">>;
number_by_field(response_transport_type) -> <<"725">>;
number_by_field(response_destination) -> <<"726">>;
number_by_field(total_num_pos_reports) -> <<"727">>;
number_by_field(pos_req_result) -> <<"728">>;
number_by_field(pos_req_status) -> <<"729">>;
number_by_field(settl_price) -> <<"730">>;
number_by_field(settl_price_type) -> <<"731">>;
number_by_field(underlying_settl_price) -> <<"732">>;
number_by_field(underlying_settl_price_type) -> <<"733">>;
number_by_field(prior_settl_price) -> <<"734">>;
number_by_field(no_quote_qualifiers) -> <<"735">>;
number_by_field(alloc_settl_currency) -> <<"736">>;
number_by_field(alloc_settl_curr_amt) -> <<"737">>;
number_by_field(interest_at_maturity) -> <<"738">>;
number_by_field(leg_dated_date) -> <<"739">>;
number_by_field(leg_pool) -> <<"740">>;
number_by_field(alloc_interest_at_maturity) -> <<"741">>;
number_by_field(alloc_accrued_interest_amt) -> <<"742">>;
number_by_field(delivery_date) -> <<"743">>;
number_by_field(assignment_method) -> <<"744">>;
number_by_field(assignment_unit) -> <<"745">>;
number_by_field(open_interest) -> <<"746">>;
number_by_field(exercise_method) -> <<"747">>;
number_by_field(tot_num_trade_reports) -> <<"748">>;
number_by_field(trade_request_result) -> <<"749">>;
number_by_field(trade_request_status) -> <<"750">>;
number_by_field(trade_report_reject_reason) -> <<"751">>;
number_by_field(side_multi_leg_reporting_type) -> <<"752">>;
number_by_field(no_pos_amt) -> <<"753">>;
number_by_field(auto_accept_indicator) -> <<"754">>;
number_by_field(alloc_report_id) -> <<"755">>;
number_by_field(no_nested2_party_ids) -> <<"756">>;
number_by_field(nested2_party_id) -> <<"757">>;
number_by_field(nested2_party_id_source) -> <<"758">>;
number_by_field(nested2_party_role) -> <<"759">>;
number_by_field(nested2_party_sub_id) -> <<"760">>;
number_by_field(benchmark_security_id_source) -> <<"761">>;
number_by_field(security_sub_type) -> <<"762">>;
number_by_field(underlying_security_sub_type) -> <<"763">>;
number_by_field(leg_security_sub_type) -> <<"764">>;
number_by_field(allowable_one_sidedness_pct) -> <<"765">>;
number_by_field(allowable_one_sidedness_value) -> <<"766">>;
number_by_field(allowable_one_sidedness_curr) -> <<"767">>;
number_by_field(no_trd_reg_timestamps) -> <<"768">>;
number_by_field(trd_reg_timestamp) -> <<"769">>;
number_by_field(trd_reg_timestamp_type) -> <<"770">>;
number_by_field(trd_reg_timestamp_origin) -> <<"771">>;
number_by_field(confirm_ref_id) -> <<"772">>;
number_by_field(confirm_type) -> <<"773">>;
number_by_field(confirm_rej_reason) -> <<"774">>;
number_by_field(booking_type) -> <<"775">>;
number_by_field(individual_alloc_rej_code) -> <<"776">>;
number_by_field(settl_inst_msg_id) -> <<"777">>;
number_by_field(no_settl_inst) -> <<"778">>;
number_by_field(last_update_time) -> <<"779">>;
number_by_field(alloc_settl_inst_type) -> <<"780">>;
number_by_field(no_settl_party_ids) -> <<"781">>;
number_by_field(settl_party_id) -> <<"782">>;
number_by_field(settl_party_id_source) -> <<"783">>;
number_by_field(settl_party_role) -> <<"784">>;
number_by_field(settl_party_sub_id) -> <<"785">>;
number_by_field(settl_party_sub_id_type) -> <<"786">>;
number_by_field(dlvy_inst_type) -> <<"787">>;
number_by_field(termination_type) -> <<"788">>;
number_by_field(next_expected_msg_seq_num) -> <<"789">>;
number_by_field(ord_status_req_id) -> <<"790">>;
number_by_field(settl_inst_req_id) -> <<"791">>;
number_by_field(settl_inst_req_rej_code) -> <<"792">>;
number_by_field(secondary_alloc_id) -> <<"793">>;
number_by_field(alloc_report_type) -> <<"794">>;
number_by_field(alloc_report_ref_id) -> <<"795">>;
number_by_field(alloc_canc_replace_reason) -> <<"796">>;
number_by_field(copy_msg_indicator) -> <<"797">>;
number_by_field(alloc_account_type) -> <<"798">>;
number_by_field(order_avg_px) -> <<"799">>;
number_by_field(order_booking_qty) -> <<"800">>;
number_by_field(no_settl_party_sub_ids) -> <<"801">>;
number_by_field(no_party_sub_ids) -> <<"802">>;
number_by_field(party_sub_id_type) -> <<"803">>;
number_by_field(no_nested_party_sub_ids) -> <<"804">>;
number_by_field(nested_party_sub_id_type) -> <<"805">>;
number_by_field(no_nested2_party_sub_ids) -> <<"806">>;
number_by_field(nested2_party_sub_id_type) -> <<"807">>;
number_by_field(alloc_intermed_req_type) -> <<"808">>;
number_by_field(underlying_px) -> <<"810">>;
number_by_field(price_delta) -> <<"811">>;
number_by_field(appl_queue_max) -> <<"812">>;
number_by_field(appl_queue_depth) -> <<"813">>;
number_by_field(appl_queue_resolution) -> <<"814">>;
number_by_field(appl_queue_action) -> <<"815">>;
number_by_field(no_alt_md_source) -> <<"816">>;
number_by_field(alt_md_source_id) -> <<"817">>;
number_by_field(secondary_trade_report_id) -> <<"818">>;
number_by_field(avg_px_indicator) -> <<"819">>;
number_by_field(trade_link_id) -> <<"820">>;
number_by_field(order_input_device) -> <<"821">>;
number_by_field(underlying_trading_session_id) -> <<"822">>;
number_by_field(underlying_trading_session_sub_id) -> <<"823">>;
number_by_field(trade_leg_ref_id) -> <<"824">>;
number_by_field(exchange_rule) -> <<"825">>;
number_by_field(trade_alloc_indicator) -> <<"826">>;
number_by_field(expiration_cycle) -> <<"827">>;
number_by_field(trd_type) -> <<"828">>;
number_by_field(trd_sub_type) -> <<"829">>;
number_by_field(transfer_reason) -> <<"830">>;
number_by_field(asgn_req_id) -> <<"831">>;
number_by_field(tot_num_assignment_reports) -> <<"832">>;
number_by_field(asgn_rpt_id) -> <<"833">>;
number_by_field(threshold_amount) -> <<"834">>;
number_by_field(peg_move_type) -> <<"835">>;
number_by_field(peg_offset_type) -> <<"836">>;
number_by_field(peg_limit_type) -> <<"837">>;
number_by_field(peg_round_direction) -> <<"838">>;
number_by_field(pegged_price) -> <<"839">>;
number_by_field(peg_scope) -> <<"840">>;
number_by_field(discretion_move_type) -> <<"841">>;
number_by_field(discretion_offset_type) -> <<"842">>;
number_by_field(discretion_limit_type) -> <<"843">>;
number_by_field(discretion_round_direction) -> <<"844">>;
number_by_field(discretion_price) -> <<"845">>;
number_by_field(discretion_scope) -> <<"846">>;
number_by_field(target_strategy) -> <<"847">>;
number_by_field(target_strategy_parameters) -> <<"848">>;
number_by_field(participation_rate) -> <<"849">>;
number_by_field(target_strategy_performance) -> <<"850">>;
number_by_field(last_liquidity_ind) -> <<"851">>;
number_by_field(publish_trd_indicator) -> <<"852">>;
number_by_field(short_sale_reason) -> <<"853">>;
number_by_field(qty_type) -> <<"854">>;
number_by_field(secondary_trd_type) -> <<"855">>;
number_by_field(trade_report_type) -> <<"856">>;
number_by_field(alloc_no_orders_type) -> <<"857">>;
number_by_field(shared_commission) -> <<"858">>;
number_by_field(confirm_req_id) -> <<"859">>;
number_by_field(avg_par_px) -> <<"860">>;
number_by_field(reported_px) -> <<"861">>;
number_by_field(no_capacities) -> <<"862">>;
number_by_field(order_capacity_qty) -> <<"863">>;
number_by_field(no_events) -> <<"864">>;
number_by_field(event_type) -> <<"865">>;
number_by_field(event_date) -> <<"866">>;
number_by_field(event_px) -> <<"867">>;
number_by_field(event_text) -> <<"868">>;
number_by_field(pct_at_risk) -> <<"869">>;
number_by_field(no_instr_attrib) -> <<"870">>;
number_by_field(instr_attrib_type) -> <<"871">>;
number_by_field(instr_attrib_value) -> <<"872">>;
number_by_field(dated_date) -> <<"873">>;
number_by_field(interest_accrual_date) -> <<"874">>;
number_by_field(cp_program) -> <<"875">>;
number_by_field(cp_reg_type) -> <<"876">>;
number_by_field(underlying_cp_program) -> <<"877">>;
number_by_field(underlying_cp_reg_type) -> <<"878">>;
number_by_field(underlying_qty) -> <<"879">>;
number_by_field(trd_match_id) -> <<"880">>;
number_by_field(secondary_trade_report_ref_id) -> <<"881">>;
number_by_field(underlying_dirty_price) -> <<"882">>;
number_by_field(underlying_end_price) -> <<"883">>;
number_by_field(underlying_start_value) -> <<"884">>;
number_by_field(underlying_current_value) -> <<"885">>;
number_by_field(underlying_end_value) -> <<"886">>;
number_by_field(no_underlying_stips) -> <<"887">>;
number_by_field(underlying_stip_type) -> <<"888">>;
number_by_field(underlying_stip_value) -> <<"889">>;
number_by_field(maturity_net_money) -> <<"890">>;
number_by_field(misc_fee_basis) -> <<"891">>;
number_by_field(tot_no_allocs) -> <<"892">>;
number_by_field(last_fragment) -> <<"893">>;
number_by_field(coll_req_id) -> <<"894">>;
number_by_field(coll_asgn_reason) -> <<"895">>;
number_by_field(coll_inquiry_qualifier) -> <<"896">>;
number_by_field(no_trades) -> <<"897">>;
number_by_field(margin_ratio) -> <<"898">>;
number_by_field(margin_excess) -> <<"899">>;
number_by_field(total_net_value) -> <<"900">>;
number_by_field(cash_outstanding) -> <<"901">>;
number_by_field(coll_asgn_id) -> <<"902">>;
number_by_field(coll_asgn_trans_type) -> <<"903">>;
number_by_field(coll_resp_id) -> <<"904">>;
number_by_field(coll_asgn_resp_type) -> <<"905">>;
number_by_field(coll_asgn_reject_reason) -> <<"906">>;
number_by_field(coll_asgn_ref_id) -> <<"907">>;
number_by_field(coll_rpt_id) -> <<"908">>;
number_by_field(coll_inquiry_id) -> <<"909">>;
number_by_field(coll_status) -> <<"910">>;
number_by_field(tot_num_reports) -> <<"911">>;
number_by_field(last_rpt_requested) -> <<"912">>;
number_by_field(agreement_desc) -> <<"913">>;
number_by_field(agreement_id) -> <<"914">>;
number_by_field(agreement_date) -> <<"915">>;
number_by_field(start_date) -> <<"916">>;
number_by_field(end_date) -> <<"917">>;
number_by_field(agreement_currency) -> <<"918">>;
number_by_field(delivery_type) -> <<"919">>;
number_by_field(end_accrued_interest_amt) -> <<"920">>;
number_by_field(start_cash) -> <<"921">>;
number_by_field(end_cash) -> <<"922">>;
number_by_field(user_request_id) -> <<"923">>;
number_by_field(user_request_type) -> <<"924">>;
number_by_field(new_password) -> <<"925">>;
number_by_field(user_status) -> <<"926">>;
number_by_field(user_status_text) -> <<"927">>;
number_by_field(status_value) -> <<"928">>;
number_by_field(status_text) -> <<"929">>;
number_by_field(ref_comp_id) -> <<"930">>;
number_by_field(ref_sub_id) -> <<"931">>;
number_by_field(network_response_id) -> <<"932">>;
number_by_field(network_request_id) -> <<"933">>;
number_by_field(last_network_response_id) -> <<"934">>;
number_by_field(network_request_type) -> <<"935">>;
number_by_field(no_comp_ids) -> <<"936">>;
number_by_field(network_status_response_type) -> <<"937">>;
number_by_field(no_coll_inquiry_qualifier) -> <<"938">>;
number_by_field(trd_rpt_status) -> <<"939">>;
number_by_field(affirm_status) -> <<"940">>;
number_by_field(underlying_strike_currency) -> <<"941">>;
number_by_field(leg_strike_currency) -> <<"942">>;
number_by_field(time_bracket) -> <<"943">>;
number_by_field(coll_action) -> <<"944">>;
number_by_field(coll_inquiry_status) -> <<"945">>;
number_by_field(coll_inquiry_result) -> <<"946">>;
number_by_field(strike_currency) -> <<"947">>;
number_by_field(no_nested3_party_ids) -> <<"948">>;
number_by_field(nested3_party_id) -> <<"949">>;
number_by_field(nested3_party_id_source) -> <<"950">>;
number_by_field(nested3_party_role) -> <<"951">>;
number_by_field(no_nested3_party_sub_ids) -> <<"952">>;
number_by_field(nested3_party_sub_id) -> <<"953">>;
number_by_field(nested3_party_sub_id_type) -> <<"954">>;
number_by_field(leg_contract_settl_month) -> <<"955">>;
number_by_field(leg_interest_accrual_date) -> <<"956">>;
number_by_field(Key) when is_binary(Key) -> Key.

message_by_number(<<"0">>) -> heartbeat;
message_by_number(<<"1">>) -> test_request;
message_by_number(<<"2">>) -> resend_request;
message_by_number(<<"3">>) -> reject;
message_by_number(<<"4">>) -> sequence_reset;
message_by_number(<<"5">>) -> logout;
message_by_number(<<"6">>) -> ioi;
message_by_number(<<"7">>) -> advertisement;
message_by_number(<<"8">>) -> execution_report;
message_by_number(<<"9">>) -> order_cancel_reject;
message_by_number(<<"A">>) -> logon;
message_by_number(<<"B">>) -> news;
message_by_number(<<"C">>) -> email;
message_by_number(<<"D">>) -> new_order_single;
message_by_number(<<"E">>) -> new_order_list;
message_by_number(<<"F">>) -> order_cancel_request;
message_by_number(<<"G">>) -> order_cancel_replace_request;
message_by_number(<<"H">>) -> order_status_request;
message_by_number(<<"J">>) -> allocation_instruction;
message_by_number(<<"K">>) -> list_cancel_request;
message_by_number(<<"L">>) -> list_execute;
message_by_number(<<"M">>) -> list_status_request;
message_by_number(<<"N">>) -> list_status;
message_by_number(<<"P">>) -> allocation_instruction_ack;
message_by_number(<<"Q">>) -> dont_know_trade;
message_by_number(<<"R">>) -> quote_request;
message_by_number(<<"S">>) -> quote;
message_by_number(<<"T">>) -> settlement_instructions;
message_by_number(<<"V">>) -> market_data_request;
message_by_number(<<"W">>) -> market_data_snapshot_full_refresh;
message_by_number(<<"X">>) -> market_data_incremental_refresh;
message_by_number(<<"Y">>) -> market_data_request_reject;
message_by_number(<<"Z">>) -> quote_cancel;
message_by_number(<<"a">>) -> quote_status_request;
message_by_number(<<"b">>) -> mass_quote_acknowledgement;
message_by_number(<<"c">>) -> security_definition_request;
message_by_number(<<"d">>) -> security_definition;
message_by_number(<<"e">>) -> security_status_request;
message_by_number(<<"f">>) -> security_status;
message_by_number(<<"g">>) -> trading_session_status_request;
message_by_number(<<"h">>) -> trading_session_status;
message_by_number(<<"i">>) -> mass_quote;
message_by_number(<<"j">>) -> business_message_reject;
message_by_number(<<"k">>) -> bid_request;
message_by_number(<<"l">>) -> bid_response;
message_by_number(<<"m">>) -> list_strike_price;
message_by_number(<<"o">>) -> registration_instructions;
message_by_number(<<"p">>) -> registration_instructions_response;
message_by_number(<<"q">>) -> order_mass_cancel_request;
message_by_number(<<"r">>) -> order_mass_cancel_report;
message_by_number(<<"s">>) -> new_order_cross;
message_by_number(<<"t">>) -> cross_order_cancel_replace_request;
message_by_number(<<"u">>) -> cross_order_cancel_request;
message_by_number(<<"v">>) -> security_type_request;
message_by_number(<<"w">>) -> security_types;
message_by_number(<<"x">>) -> security_list_request;
message_by_number(<<"y">>) -> security_list;
message_by_number(<<"z">>) -> derivative_security_list_request;
message_by_number(<<"AA">>) -> derivative_security_list;
message_by_number(<<"AB">>) -> new_order_multileg;
message_by_number(<<"AC">>) -> multileg_order_cancel_replace;
message_by_number(<<"AD">>) -> trade_capture_report_request;
message_by_number(<<"AE">>) -> trade_capture_report;
message_by_number(<<"AF">>) -> order_mass_status_request;
message_by_number(<<"AG">>) -> quote_request_reject;
message_by_number(<<"AH">>) -> rfq_request;
message_by_number(<<"AI">>) -> quote_status_report;
message_by_number(<<"AJ">>) -> quote_response;
message_by_number(<<"AK">>) -> confirmation;
message_by_number(<<"AL">>) -> position_maintenance_request;
message_by_number(<<"AM">>) -> position_maintenance_report;
message_by_number(<<"AN">>) -> request_for_positions;
message_by_number(<<"AO">>) -> request_for_positions_ack;
message_by_number(<<"AP">>) -> position_report;
message_by_number(<<"AQ">>) -> trade_capture_report_request_ack;
message_by_number(<<"AR">>) -> trade_capture_report_ack;
message_by_number(<<"AS">>) -> allocation_report;
message_by_number(<<"AT">>) -> allocation_report_ack;
message_by_number(<<"AU">>) -> confirmation_ack;
message_by_number(<<"AV">>) -> settlement_instruction_request;
message_by_number(<<"AW">>) -> assignment_report;
message_by_number(<<"AX">>) -> collateral_request;
message_by_number(<<"AY">>) -> collateral_assignment;
message_by_number(<<"AZ">>) -> collateral_response;
message_by_number(<<"BA">>) -> collateral_report;
message_by_number(<<"BB">>) -> collateral_inquiry;
message_by_number(<<"BC">>) -> network_counterparty_system_status_request;
message_by_number(<<"BD">>) -> network_counterparty_system_status_response;
message_by_number(<<"BE">>) -> user_request;
message_by_number(<<"BF">>) -> user_response;
message_by_number(<<"BG">>) -> collateral_inquiry_ack;
message_by_number(<<"BH">>) -> confirmation_request;
message_by_number(Type) when is_binary(Type) -> Type.

number_by_message(heartbeat) -> <<"0">>;
number_by_message(test_request) -> <<"1">>;
number_by_message(resend_request) -> <<"2">>;
number_by_message(reject) -> <<"3">>;
number_by_message(sequence_reset) -> <<"4">>;
number_by_message(logout) -> <<"5">>;
number_by_message(ioi) -> <<"6">>;
number_by_message(advertisement) -> <<"7">>;
number_by_message(execution_report) -> <<"8">>;
number_by_message(order_cancel_reject) -> <<"9">>;
number_by_message(logon) -> <<"A">>;
number_by_message(news) -> <<"B">>;
number_by_message(email) -> <<"C">>;
number_by_message(new_order_single) -> <<"D">>;
number_by_message(new_order_list) -> <<"E">>;
number_by_message(order_cancel_request) -> <<"F">>;
number_by_message(order_cancel_replace_request) -> <<"G">>;
number_by_message(order_status_request) -> <<"H">>;
number_by_message(allocation_instruction) -> <<"J">>;
number_by_message(list_cancel_request) -> <<"K">>;
number_by_message(list_execute) -> <<"L">>;
number_by_message(list_status_request) -> <<"M">>;
number_by_message(list_status) -> <<"N">>;
number_by_message(allocation_instruction_ack) -> <<"P">>;
number_by_message(dont_know_trade) -> <<"Q">>;
number_by_message(quote_request) -> <<"R">>;
number_by_message(quote) -> <<"S">>;
number_by_message(settlement_instructions) -> <<"T">>;
number_by_message(market_data_request) -> <<"V">>;
number_by_message(market_data_snapshot_full_refresh) -> <<"W">>;
number_by_message(market_data_incremental_refresh) -> <<"X">>;
number_by_message(market_data_request_reject) -> <<"Y">>;
number_by_message(quote_cancel) -> <<"Z">>;
number_by_message(quote_status_request) -> <<"a">>;
number_by_message(mass_quote_acknowledgement) -> <<"b">>;
number_by_message(security_definition_request) -> <<"c">>;
number_by_message(security_definition) -> <<"d">>;
number_by_message(security_status_request) -> <<"e">>;
number_by_message(security_status) -> <<"f">>;
number_by_message(trading_session_status_request) -> <<"g">>;
number_by_message(trading_session_status) -> <<"h">>;
number_by_message(mass_quote) -> <<"i">>;
number_by_message(business_message_reject) -> <<"j">>;
number_by_message(bid_request) -> <<"k">>;
number_by_message(bid_response) -> <<"l">>;
number_by_message(list_strike_price) -> <<"m">>;
number_by_message(registration_instructions) -> <<"o">>;
number_by_message(registration_instructions_response) -> <<"p">>;
number_by_message(order_mass_cancel_request) -> <<"q">>;
number_by_message(order_mass_cancel_report) -> <<"r">>;
number_by_message(new_order_cross) -> <<"s">>;
number_by_message(cross_order_cancel_replace_request) -> <<"t">>;
number_by_message(cross_order_cancel_request) -> <<"u">>;
number_by_message(security_type_request) -> <<"v">>;
number_by_message(security_types) -> <<"w">>;
number_by_message(security_list_request) -> <<"x">>;
number_by_message(security_list) -> <<"y">>;
number_by_message(derivative_security_list_request) -> <<"z">>;
number_by_message(derivative_security_list) -> <<"AA">>;
number_by_message(new_order_multileg) -> <<"AB">>;
number_by_message(multileg_order_cancel_replace) -> <<"AC">>;
number_by_message(trade_capture_report_request) -> <<"AD">>;
number_by_message(trade_capture_report) -> <<"AE">>;
number_by_message(order_mass_status_request) -> <<"AF">>;
number_by_message(quote_request_reject) -> <<"AG">>;
number_by_message(rfq_request) -> <<"AH">>;
number_by_message(quote_status_report) -> <<"AI">>;
number_by_message(quote_response) -> <<"AJ">>;
number_by_message(confirmation) -> <<"AK">>;
number_by_message(position_maintenance_request) -> <<"AL">>;
number_by_message(position_maintenance_report) -> <<"AM">>;
number_by_message(request_for_positions) -> <<"AN">>;
number_by_message(request_for_positions_ack) -> <<"AO">>;
number_by_message(position_report) -> <<"AP">>;
number_by_message(trade_capture_report_request_ack) -> <<"AQ">>;
number_by_message(trade_capture_report_ack) -> <<"AR">>;
number_by_message(allocation_report) -> <<"AS">>;
number_by_message(allocation_report_ack) -> <<"AT">>;
number_by_message(confirmation_ack) -> <<"AU">>;
number_by_message(settlement_instruction_request) -> <<"AV">>;
number_by_message(assignment_report) -> <<"AW">>;
number_by_message(collateral_request) -> <<"AX">>;
number_by_message(collateral_assignment) -> <<"AY">>;
number_by_message(collateral_response) -> <<"AZ">>;
number_by_message(collateral_report) -> <<"BA">>;
number_by_message(collateral_inquiry) -> <<"BB">>;
number_by_message(network_counterparty_system_status_request) -> <<"BC">>;
number_by_message(network_counterparty_system_status_response) -> <<"BD">>;
number_by_message(user_request) -> <<"BE">>;
number_by_message(user_response) -> <<"BF">>;
number_by_message(collateral_inquiry_ack) -> <<"BG">>;
number_by_message(confirmation_request) -> <<"BH">>;
number_by_message(Type) when is_binary(Type) -> Type.

