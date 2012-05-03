-module(fix_parser).
-include("../include/admin.hrl").
-include("../include/business.hrl").

-export([decode_message/1, field_by_number/1, number_by_field/1, decode_typed_field/2, encode_typed_field/2, message_by_number/1, number_by_message/1]).

decode_message(<<"35=0",1,Message/binary>>) -> % Heartbeat
  decode_fields(Message, #heartbeat{}, heartbeat, 8);

decode_message(<<"35=1",1,Message/binary>>) -> % TestRequest
  decode_fields(Message, #test_request{}, test_request, 8);

decode_message(<<"35=2",1,Message/binary>>) -> % ResendRequest
  decode_fields(Message, #resend_request{}, resend_request, 9);

decode_message(<<"35=3",1,Message/binary>>) -> % Reject
  decode_fields(Message, #reject{}, reject, 13);

decode_message(<<"35=4",1,Message/binary>>) -> % SequenceReset
  decode_fields(Message, #sequence_reset{}, sequence_reset, 9);

decode_message(<<"35=5",1,Message/binary>>) -> % Logout
  decode_fields(Message, #logout{}, logout, 9);

decode_message(<<"35=6",1,Message/binary>>) -> % IOI
  decode_fields(Message, #ioi{}, ioi, 27);

decode_message(<<"35=7",1,Message/binary>>) -> % Advertisement
  decode_fields(Message, #advertisement{}, advertisement, 25);

decode_message(<<"35=8",1,Message/binary>>) -> % ExecutionReport
  decode_fields(Message, #execution_report{}, execution_report, 134);

decode_message(<<"35=9",1,Message/binary>>) -> % OrderCancelReject
  decode_fields(Message, #order_cancel_reject{}, order_cancel_reject, 27);

decode_message(<<"35=A",1,Message/binary>>) -> % Logon
  decode_fields(Message, #logon{}, logon, 16);

decode_message(<<"35=B",1,Message/binary>>) -> % News
  decode_fields(Message, #news{}, news, 18);

decode_message(<<"35=C",1,Message/binary>>) -> % Email
  decode_fields(Message, #email{}, email, 20);

decode_message(<<"35=D",1,Message/binary>>) -> % NewOrderSingle
  decode_fields(Message, #new_order_single{}, new_order_single, 72);

decode_message(<<"35=E",1,Message/binary>>) -> % NewOrderList
  decode_fields(Message, #new_order_list{}, new_order_list, 75);

decode_message(<<"35=F",1,Message/binary>>) -> % OrderCancelRequest
  decode_fields(Message, #order_cancel_request{}, order_cancel_request, 23);

decode_message(<<"35=G",1,Message/binary>>) -> % OrderCancelReplaceRequest
  decode_fields(Message, #order_cancel_replace_request{}, order_cancel_replace_request, 72);

decode_message(<<"35=H",1,Message/binary>>) -> % OrderStatusRequest
  decode_fields(Message, #order_status_request{}, order_status_request, 16);

decode_message(<<"35=J",1,Message/binary>>) -> % AllocationInstruction
  decode_fields(Message, #allocation_instruction{}, allocation_instruction, 65);

decode_message(<<"35=K",1,Message/binary>>) -> % ListCancelRequest
  decode_fields(Message, #list_cancel_request{}, list_cancel_request, 13);

decode_message(<<"35=L",1,Message/binary>>) -> % ListExecute
  decode_fields(Message, #list_execute{}, list_execute, 13);

decode_message(<<"35=M",1,Message/binary>>) -> % ListStatusRequest
  decode_fields(Message, #list_status_request{}, list_status_request, 10);

decode_message(<<"35=N",1,Message/binary>>) -> % ListStatus
  decode_fields(Message, #list_status{}, list_status, 18);

decode_message(<<"35=P",1,Message/binary>>) -> % AllocationInstructionAck
  decode_fields(Message, #allocation_instruction_ack{}, allocation_instruction_ack, 21);

decode_message(<<"35=Q",1,Message/binary>>) -> % DontKnowTrade
  decode_fields(Message, #dont_know_trade{}, dont_know_trade, 18);

decode_message(<<"35=R",1,Message/binary>>) -> % QuoteRequest
  decode_fields(Message, #quote_request{}, quote_request, 40);

decode_message(<<"35=S",1,Message/binary>>) -> % Quote
  decode_fields(Message, #quote{}, quote, 58);

decode_message(<<"35=T",1,Message/binary>>) -> % SettlementInstructions
  decode_fields(Message, #settlement_instructions{}, settlement_instructions, 16);

decode_message(<<"35=V",1,Message/binary>>) -> % MarketDataRequest
  decode_fields(Message, #market_data_request{}, market_data_request, 21);

decode_message(<<"35=W",1,Message/binary>>) -> % MarketDataSnapshotFullRefresh
  decode_fields(Message, #market_data_snapshot_full_refresh{}, market_data_snapshot_full_refresh, 16);

decode_message(<<"35=X",1,Message/binary>>) -> % MarketDataIncrementalRefresh
  decode_fields(Message, #market_data_incremental_refresh{}, market_data_incremental_refresh, 46);

decode_message(<<"35=Y",1,Message/binary>>) -> % MarketDataRequestReject
  decode_fields(Message, #market_data_request_reject{}, market_data_request_reject, 12);

decode_message(<<"35=Z",1,Message/binary>>) -> % QuoteCancel
  decode_fields(Message, #quote_cancel{}, quote_cancel, 18);

decode_message(<<"35=a",1,Message/binary>>) -> % QuoteStatusRequest
  decode_fields(Message, #quote_status_request{}, quote_status_request, 17);

decode_message(<<"35=b",1,Message/binary>>) -> % MassQuoteAcknowledgement
  decode_fields(Message, #mass_quote_acknowledgement{}, mass_quote_acknowledgement, 43);

decode_message(<<"35=c",1,Message/binary>>) -> % SecurityDefinitionRequest
  decode_fields(Message, #security_definition_request{}, security_definition_request, 18);

decode_message(<<"35=d",1,Message/binary>>) -> % SecurityDefinition
  decode_fields(Message, #security_definition{}, security_definition, 20);

decode_message(<<"35=e",1,Message/binary>>) -> % SecurityStatusRequest
  decode_fields(Message, #security_status_request{}, security_status_request, 14);

decode_message(<<"35=f",1,Message/binary>>) -> % SecurityStatus
  decode_fields(Message, #security_status{}, security_status, 29);

decode_message(<<"35=g",1,Message/binary>>) -> % TradingSessionStatusRequest
  decode_fields(Message, #trading_session_status_request{}, trading_session_status_request, 13);

decode_message(<<"35=h",1,Message/binary>>) -> % TradingSessionStatus
  decode_fields(Message, #trading_session_status{}, trading_session_status, 23);

decode_message(<<"35=i",1,Message/binary>>) -> % MassQuote
  decode_fields(Message, #mass_quote{}, mass_quote, 40);

decode_message(<<"35=j",1,Message/binary>>) -> % BusinessMessageReject
  decode_fields(Message, #business_message_reject{}, business_message_reject, 13);

decode_message(<<"35=k",1,Message/binary>>) -> % BidRequest
  decode_fields(Message, #bid_request{}, bid_request, 35);

decode_message(<<"35=l",1,Message/binary>>) -> % BidResponse
  decode_fields(Message, #bid_response{}, bid_response, 10);

decode_message(<<"35=m",1,Message/binary>>) -> % ListStrikePrice
  decode_fields(Message, #list_strike_price{}, list_strike_price, 12);

decode_message(<<"35=o",1,Message/binary>>) -> % RegistrationInstructions
  decode_fields(Message, #registration_instructions{}, registration_instructions, 18);

decode_message(<<"35=p",1,Message/binary>>) -> % RegistrationInstructionsResponse
  decode_fields(Message, #registration_instructions_response{}, registration_instructions_response, 16);

decode_message(<<"35=q",1,Message/binary>>) -> % OrderMassCancelRequest
  decode_fields(Message, #order_mass_cancel_request{}, order_mass_cancel_request, 16);

decode_message(<<"35=r",1,Message/binary>>) -> % OrderMassCancelReport
  decode_fields(Message, #order_mass_cancel_report{}, order_mass_cancel_report, 22);

decode_message(<<"35=s",1,Message/binary>>) -> % NewOrderCross
  decode_fields(Message, #new_order_cross{}, new_order_cross, 61);

decode_message(<<"35=t",1,Message/binary>>) -> % CrossOrderCancelReplaceRequest
  decode_fields(Message, #cross_order_cancel_replace_request{}, cross_order_cancel_replace_request, 63);

decode_message(<<"35=u",1,Message/binary>>) -> % CrossOrderCancelRequest
  decode_fields(Message, #cross_order_cancel_request{}, cross_order_cancel_request, 16);

decode_message(<<"35=v",1,Message/binary>>) -> % SecurityTypeRequest
  decode_fields(Message, #security_type_request{}, security_type_request, 15);

decode_message(<<"35=w",1,Message/binary>>) -> % SecurityTypes
  decode_fields(Message, #security_types{}, security_types, 18);

decode_message(<<"35=x",1,Message/binary>>) -> % SecurityListRequest
  decode_fields(Message, #security_list_request{}, security_list_request, 17);

decode_message(<<"35=y",1,Message/binary>>) -> % SecurityList
  decode_fields(Message, #security_list{}, security_list, 22);

decode_message(<<"35=z",1,Message/binary>>) -> % DerivativeSecurityListRequest
  decode_fields(Message, #derivative_security_list_request{}, derivative_security_list_request, 16);

decode_message(<<"35=AA",1,Message/binary>>) -> % DerivativeSecurityList
  decode_fields(Message, #derivative_security_list{}, derivative_security_list, 17);

decode_message(<<"35=AB",1,Message/binary>>) -> % NewOrderMultileg
  decode_fields(Message, #new_order_multileg{}, new_order_multileg, 77);

decode_message(<<"35=AC",1,Message/binary>>) -> % MultilegOrderCancelReplace
  decode_fields(Message, #multileg_order_cancel_replace{}, multileg_order_cancel_replace, 80);

decode_message(<<"35=AD",1,Message/binary>>) -> % TradeCaptureReportRequest
  decode_fields(Message, #trade_capture_report_request{}, trade_capture_report_request, 38);

decode_message(<<"35=AE",1,Message/binary>>) -> % TradeCaptureReport
  decode_fields(Message, #trade_capture_report{}, trade_capture_report, 100);

decode_message(<<"35=AF",1,Message/binary>>) -> % OrderMassStatusRequest
  decode_fields(Message, #order_mass_status_request{}, order_mass_status_request, 14);

decode_message(<<"35=AG",1,Message/binary>>) -> % QuoteRequestReject
  decode_fields(Message, #quote_request_reject{}, quote_request_reject, 38);

decode_message(<<"35=AH",1,Message/binary>>) -> % RFQRequest
  decode_fields(Message, #rfq_request{}, rfq_request, 16);

decode_message(<<"35=AI",1,Message/binary>>) -> % QuoteStatusReport
  decode_fields(Message, #quote_status_report{}, quote_status_report, 60);

decode_message(<<"35=AJ",1,Message/binary>>) -> % QuoteResponse
  decode_fields(Message, #quote_response{}, quote_response, 60);

decode_message(<<"35=AK",1,Message/binary>>) -> % Confirmation
  decode_fields(Message, #confirmation{}, confirmation, 61);

decode_message(<<"35=AL",1,Message/binary>>) -> % PositionMaintenanceRequest
  decode_fields(Message, #position_maintenance_request{}, position_maintenance_request, 29);

decode_message(<<"35=AM",1,Message/binary>>) -> % PositionMaintenanceReport
  decode_fields(Message, #position_maintenance_report{}, position_maintenance_report, 29);

decode_message(<<"35=AN",1,Message/binary>>) -> % RequestForPositions
  decode_fields(Message, #request_for_positions{}, request_for_positions, 26);

decode_message(<<"35=AO",1,Message/binary>>) -> % RequestForPositionsAck
  decode_fields(Message, #request_for_positions_ack{}, request_for_positions_ack, 23);

decode_message(<<"35=AP",1,Message/binary>>) -> % PositionReport
  decode_fields(Message, #position_report{}, position_report, 30);

decode_message(<<"35=AQ",1,Message/binary>>) -> % TradeCaptureReportRequestAck
  decode_fields(Message, #trade_capture_report_request_ack{}, trade_capture_report_request_ack, 20);

decode_message(<<"35=AR",1,Message/binary>>) -> % TradeCaptureReportAck
  decode_fields(Message, #trade_capture_report_ack{}, trade_capture_report_ack, 41);

decode_message(<<"35=AS",1,Message/binary>>) -> % AllocationReport
  decode_fields(Message, #allocation_report{}, allocation_report, 69);

decode_message(<<"35=AT",1,Message/binary>>) -> % AllocationReportAck
  decode_fields(Message, #allocation_report_ack{}, allocation_report_ack, 22);

decode_message(<<"35=AU",1,Message/binary>>) -> % ConfirmationAck
  decode_fields(Message, #confirmation_ack{}, confirmation_ack, 15);

decode_message(<<"35=AV",1,Message/binary>>) -> % SettlementInstructionRequest
  decode_fields(Message, #settlement_instruction_request{}, settlement_instruction_request, 21);

decode_message(<<"35=AW",1,Message/binary>>) -> % AssignmentReport
  decode_fields(Message, #assignment_report{}, assignment_report, 29);

decode_message(<<"35=AX",1,Message/binary>>) -> % CollateralRequest
  decode_fields(Message, #collateral_request{}, collateral_request, 43);

decode_message(<<"35=AY",1,Message/binary>>) -> % CollateralAssignment
  decode_fields(Message, #collateral_assignment{}, collateral_assignment, 46);

decode_message(<<"35=AZ",1,Message/binary>>) -> % CollateralResponse
  decode_fields(Message, #collateral_response{}, collateral_response, 42);

decode_message(<<"35=BA",1,Message/binary>>) -> % CollateralReport
  decode_fields(Message, #collateral_report{}, collateral_report, 44);

decode_message(<<"35=BB",1,Message/binary>>) -> % CollateralInquiry
  decode_fields(Message, #collateral_inquiry{}, collateral_inquiry, 43);

decode_message(<<"35=BC",1,Message/binary>>) -> % NetworkCounterpartySystemStatusRequest
  decode_fields(Message, #network_counterparty_system_status_request{}, network_counterparty_system_status_request, 10);

decode_message(<<"35=BD",1,Message/binary>>) -> % NetworkCounterpartySystemStatusResponse
  decode_fields(Message, #network_counterparty_system_status_response{}, network_counterparty_system_status_response, 12);

decode_message(<<"35=BE",1,Message/binary>>) -> % UserRequest
  decode_fields(Message, #user_request{}, user_request, 13);

decode_message(<<"35=BF",1,Message/binary>>) -> % UserResponse
  decode_fields(Message, #user_response{}, user_response, 11);

decode_message(<<"35=BG",1,Message/binary>>) -> % CollateralInquiryAck
  decode_fields(Message, #collateral_inquiry_ack{}, collateral_inquiry_ack, 35);

decode_message(<<"35=BH",1,Message/binary>>) -> % ConfirmationRequest
  decode_fields(Message, #confirmation_request{}, confirmation_request, 19).

field_index(heartbeat, sender_comp_id) -> 2;
field_index(heartbeat, target_comp_id) -> 3;
field_index(heartbeat, msg_seq_num) -> 4;
field_index(heartbeat, sending_time) -> 5;
field_index(heartbeat, test_req_id) -> 6;
field_index(heartbeat, signature) -> 7;
field_index(test_request, sender_comp_id) -> 2;
field_index(test_request, target_comp_id) -> 3;
field_index(test_request, msg_seq_num) -> 4;
field_index(test_request, sending_time) -> 5;
field_index(test_request, test_req_id) -> 6;
field_index(test_request, signature) -> 7;
field_index(resend_request, sender_comp_id) -> 2;
field_index(resend_request, target_comp_id) -> 3;
field_index(resend_request, msg_seq_num) -> 4;
field_index(resend_request, sending_time) -> 5;
field_index(resend_request, begin_seq_no) -> 6;
field_index(resend_request, end_seq_no) -> 7;
field_index(resend_request, signature) -> 8;
field_index(reject, sender_comp_id) -> 2;
field_index(reject, target_comp_id) -> 3;
field_index(reject, msg_seq_num) -> 4;
field_index(reject, sending_time) -> 5;
field_index(reject, ref_seq_num) -> 6;
field_index(reject, ref_tag_id) -> 7;
field_index(reject, ref_msg_type) -> 8;
field_index(reject, session_reject_reason) -> 9;
field_index(reject, text) -> 10;
field_index(reject, encoded_text) -> 11;
field_index(reject, signature) -> 12;
field_index(sequence_reset, sender_comp_id) -> 2;
field_index(sequence_reset, target_comp_id) -> 3;
field_index(sequence_reset, msg_seq_num) -> 4;
field_index(sequence_reset, sending_time) -> 5;
field_index(sequence_reset, gap_fill_flag) -> 6;
field_index(sequence_reset, new_seq_no) -> 7;
field_index(sequence_reset, signature) -> 8;
field_index(logout, sender_comp_id) -> 2;
field_index(logout, target_comp_id) -> 3;
field_index(logout, msg_seq_num) -> 4;
field_index(logout, sending_time) -> 5;
field_index(logout, text) -> 6;
field_index(logout, encoded_text) -> 7;
field_index(logout, signature) -> 8;
field_index(ioi, sender_comp_id) -> 2;
field_index(ioi, target_comp_id) -> 3;
field_index(ioi, msg_seq_num) -> 4;
field_index(ioi, sending_time) -> 5;
field_index(ioi, ioi_id) -> 6;
field_index(ioi, ioi_trans_type) -> 7;
field_index(ioi, ioi_ref_id) -> 8;
field_index(ioi, side) -> 9;
field_index(ioi, qty_type) -> 10;
field_index(ioi, ioi_qty) -> 11;
field_index(ioi, currency) -> 12;
field_index(ioi, price_type) -> 13;
field_index(ioi, price) -> 14;
field_index(ioi, valid_until_time) -> 15;
field_index(ioi, ioi_qlty_ind) -> 16;
field_index(ioi, ioi_natural_flag) -> 17;
field_index(ioi, text) -> 18;
field_index(ioi, encoded_text) -> 19;
field_index(ioi, transact_time) -> 20;
field_index(ioi, url_link) -> 21;
field_index(ioi, signature) -> 22;
field_index(advertisement, sender_comp_id) -> 2;
field_index(advertisement, target_comp_id) -> 3;
field_index(advertisement, msg_seq_num) -> 4;
field_index(advertisement, sending_time) -> 5;
field_index(advertisement, adv_id) -> 6;
field_index(advertisement, adv_trans_type) -> 7;
field_index(advertisement, adv_ref_id) -> 8;
field_index(advertisement, adv_side) -> 9;
field_index(advertisement, quantity) -> 10;
field_index(advertisement, qty_type) -> 11;
field_index(advertisement, price) -> 12;
field_index(advertisement, currency) -> 13;
field_index(advertisement, trade_date) -> 14;
field_index(advertisement, transact_time) -> 15;
field_index(advertisement, text) -> 16;
field_index(advertisement, encoded_text) -> 17;
field_index(advertisement, url_link) -> 18;
field_index(advertisement, last_mkt) -> 19;
field_index(advertisement, trading_session_id) -> 20;
field_index(advertisement, trading_session_sub_id) -> 21;
field_index(advertisement, signature) -> 22;
field_index(execution_report, sender_comp_id) -> 2;
field_index(execution_report, target_comp_id) -> 3;
field_index(execution_report, msg_seq_num) -> 4;
field_index(execution_report, sending_time) -> 5;
field_index(execution_report, order_id) -> 6;
field_index(execution_report, secondary_order_id) -> 7;
field_index(execution_report, secondary_cl_ord_id) -> 8;
field_index(execution_report, secondary_exec_id) -> 9;
field_index(execution_report, cl_ord_id) -> 10;
field_index(execution_report, orig_cl_ord_id) -> 11;
field_index(execution_report, cl_ord_link_id) -> 12;
field_index(execution_report, quote_resp_id) -> 13;
field_index(execution_report, ord_status_req_id) -> 14;
field_index(execution_report, mass_status_req_id) -> 15;
field_index(execution_report, tot_num_reports) -> 16;
field_index(execution_report, last_rpt_requested) -> 17;
field_index(execution_report, trade_origination_date) -> 18;
field_index(execution_report, list_id) -> 19;
field_index(execution_report, cross_id) -> 20;
field_index(execution_report, orig_cross_id) -> 21;
field_index(execution_report, cross_type) -> 22;
field_index(execution_report, exec_id) -> 23;
field_index(execution_report, exec_ref_id) -> 24;
field_index(execution_report, exec_type) -> 25;
field_index(execution_report, ord_status) -> 26;
field_index(execution_report, working_indicator) -> 27;
field_index(execution_report, ord_rej_reason) -> 28;
field_index(execution_report, exec_restatement_reason) -> 29;
field_index(execution_report, account) -> 30;
field_index(execution_report, acct_id_source) -> 31;
field_index(execution_report, account_type) -> 32;
field_index(execution_report, day_booking_inst) -> 33;
field_index(execution_report, booking_unit) -> 34;
field_index(execution_report, prealloc_method) -> 35;
field_index(execution_report, settl_type) -> 36;
field_index(execution_report, settl_date) -> 37;
field_index(execution_report, cash_margin) -> 38;
field_index(execution_report, clearing_fee_indicator) -> 39;
field_index(execution_report, side) -> 40;
field_index(execution_report, qty_type) -> 41;
field_index(execution_report, ord_type) -> 42;
field_index(execution_report, price_type) -> 43;
field_index(execution_report, price) -> 44;
field_index(execution_report, stop_px) -> 45;
field_index(execution_report, pegged_price) -> 46;
field_index(execution_report, discretion_price) -> 47;
field_index(execution_report, target_strategy) -> 48;
field_index(execution_report, target_strategy_parameters) -> 49;
field_index(execution_report, participation_rate) -> 50;
field_index(execution_report, target_strategy_performance) -> 51;
field_index(execution_report, currency) -> 52;
field_index(execution_report, compliance_id) -> 53;
field_index(execution_report, solicited_flag) -> 54;
field_index(execution_report, time_in_force) -> 55;
field_index(execution_report, effective_time) -> 56;
field_index(execution_report, expire_date) -> 57;
field_index(execution_report, expire_time) -> 58;
field_index(execution_report, exec_inst) -> 59;
field_index(execution_report, order_capacity) -> 60;
field_index(execution_report, order_restrictions) -> 61;
field_index(execution_report, cust_order_capacity) -> 62;
field_index(execution_report, last_qty) -> 63;
field_index(execution_report, underlying_last_qty) -> 64;
field_index(execution_report, last_px) -> 65;
field_index(execution_report, underlying_last_px) -> 66;
field_index(execution_report, last_par_px) -> 67;
field_index(execution_report, last_spot_rate) -> 68;
field_index(execution_report, last_forward_points) -> 69;
field_index(execution_report, last_mkt) -> 70;
field_index(execution_report, trading_session_id) -> 71;
field_index(execution_report, trading_session_sub_id) -> 72;
field_index(execution_report, time_bracket) -> 73;
field_index(execution_report, last_capacity) -> 74;
field_index(execution_report, leaves_qty) -> 75;
field_index(execution_report, cum_qty) -> 76;
field_index(execution_report, avg_px) -> 77;
field_index(execution_report, day_order_qty) -> 78;
field_index(execution_report, day_cum_qty) -> 79;
field_index(execution_report, day_avg_px) -> 80;
field_index(execution_report, gt_booking_inst) -> 81;
field_index(execution_report, trade_date) -> 82;
field_index(execution_report, transact_time) -> 83;
field_index(execution_report, report_to_exch) -> 84;
field_index(execution_report, gross_trade_amt) -> 85;
field_index(execution_report, num_days_interest) -> 86;
field_index(execution_report, ex_date) -> 87;
field_index(execution_report, accrued_interest_rate) -> 88;
field_index(execution_report, accrued_interest_amt) -> 89;
field_index(execution_report, interest_at_maturity) -> 90;
field_index(execution_report, end_accrued_interest_amt) -> 91;
field_index(execution_report, start_cash) -> 92;
field_index(execution_report, end_cash) -> 93;
field_index(execution_report, traded_flat_switch) -> 94;
field_index(execution_report, basis_feature_date) -> 95;
field_index(execution_report, basis_feature_price) -> 96;
field_index(execution_report, concession) -> 97;
field_index(execution_report, total_takedown) -> 98;
field_index(execution_report, net_money) -> 99;
field_index(execution_report, settl_curr_amt) -> 100;
field_index(execution_report, settl_currency) -> 101;
field_index(execution_report, settl_curr_fx_rate) -> 102;
field_index(execution_report, settl_curr_fx_rate_calc) -> 103;
field_index(execution_report, handl_inst) -> 104;
field_index(execution_report, min_qty) -> 105;
field_index(execution_report, max_floor) -> 106;
field_index(execution_report, position_effect) -> 107;
field_index(execution_report, max_show) -> 108;
field_index(execution_report, booking_type) -> 109;
field_index(execution_report, text) -> 110;
field_index(execution_report, encoded_text) -> 111;
field_index(execution_report, settl_date2) -> 112;
field_index(execution_report, order_qty2) -> 113;
field_index(execution_report, last_forward_points2) -> 114;
field_index(execution_report, multi_leg_reporting_type) -> 115;
field_index(execution_report, cancellation_rights) -> 116;
field_index(execution_report, money_laundering_status) -> 117;
field_index(execution_report, regist_id) -> 118;
field_index(execution_report, designation) -> 119;
field_index(execution_report, trans_bkd_time) -> 120;
field_index(execution_report, exec_valuation_point) -> 121;
field_index(execution_report, exec_price_type) -> 122;
field_index(execution_report, exec_price_adjustment) -> 123;
field_index(execution_report, priority_indicator) -> 124;
field_index(execution_report, price_improvement) -> 125;
field_index(execution_report, last_liquidity_ind) -> 126;
field_index(execution_report, copy_msg_indicator) -> 127;
field_index(execution_report, signature) -> 128;
field_index(order_cancel_reject, sender_comp_id) -> 2;
field_index(order_cancel_reject, target_comp_id) -> 3;
field_index(order_cancel_reject, msg_seq_num) -> 4;
field_index(order_cancel_reject, sending_time) -> 5;
field_index(order_cancel_reject, order_id) -> 6;
field_index(order_cancel_reject, secondary_order_id) -> 7;
field_index(order_cancel_reject, secondary_cl_ord_id) -> 8;
field_index(order_cancel_reject, cl_ord_id) -> 9;
field_index(order_cancel_reject, cl_ord_link_id) -> 10;
field_index(order_cancel_reject, orig_cl_ord_id) -> 11;
field_index(order_cancel_reject, ord_status) -> 12;
field_index(order_cancel_reject, working_indicator) -> 13;
field_index(order_cancel_reject, orig_ord_mod_time) -> 14;
field_index(order_cancel_reject, list_id) -> 15;
field_index(order_cancel_reject, account) -> 16;
field_index(order_cancel_reject, acct_id_source) -> 17;
field_index(order_cancel_reject, account_type) -> 18;
field_index(order_cancel_reject, trade_origination_date) -> 19;
field_index(order_cancel_reject, trade_date) -> 20;
field_index(order_cancel_reject, transact_time) -> 21;
field_index(order_cancel_reject, cxl_rej_response_to) -> 22;
field_index(order_cancel_reject, cxl_rej_reason) -> 23;
field_index(order_cancel_reject, text) -> 24;
field_index(order_cancel_reject, encoded_text) -> 25;
field_index(order_cancel_reject, signature) -> 26;
field_index(logon, sender_comp_id) -> 2;
field_index(logon, target_comp_id) -> 3;
field_index(logon, msg_seq_num) -> 4;
field_index(logon, sending_time) -> 5;
field_index(logon, encrypt_method) -> 6;
field_index(logon, heart_bt_int) -> 7;
field_index(logon, raw_data) -> 8;
field_index(logon, reset_seq_num_flag) -> 9;
field_index(logon, next_expected_msg_seq_num) -> 10;
field_index(logon, test_message_indicator) -> 11;
field_index(logon, username) -> 12;
field_index(logon, password) -> 13;
field_index(logon, signature) -> 14;
field_index(news, sender_comp_id) -> 2;
field_index(news, target_comp_id) -> 3;
field_index(news, msg_seq_num) -> 4;
field_index(news, sending_time) -> 5;
field_index(news, orig_time) -> 6;
field_index(news, urgency) -> 7;
field_index(news, headline) -> 8;
field_index(news, encoded_headline) -> 9;
field_index(news, url_link) -> 10;
field_index(news, raw_data) -> 11;
field_index(news, signature) -> 12;
field_index(email, sender_comp_id) -> 2;
field_index(email, target_comp_id) -> 3;
field_index(email, msg_seq_num) -> 4;
field_index(email, sending_time) -> 5;
field_index(email, email_thread_id) -> 6;
field_index(email, email_type) -> 7;
field_index(email, orig_time) -> 8;
field_index(email, subject) -> 9;
field_index(email, encoded_subject) -> 10;
field_index(email, order_id) -> 11;
field_index(email, cl_ord_id) -> 12;
field_index(email, raw_data) -> 13;
field_index(email, signature) -> 14;
field_index(new_order_single, sender_comp_id) -> 2;
field_index(new_order_single, target_comp_id) -> 3;
field_index(new_order_single, msg_seq_num) -> 4;
field_index(new_order_single, sending_time) -> 5;
field_index(new_order_single, cl_ord_id) -> 6;
field_index(new_order_single, secondary_cl_ord_id) -> 7;
field_index(new_order_single, cl_ord_link_id) -> 8;
field_index(new_order_single, trade_origination_date) -> 9;
field_index(new_order_single, trade_date) -> 10;
field_index(new_order_single, account) -> 11;
field_index(new_order_single, acct_id_source) -> 12;
field_index(new_order_single, account_type) -> 13;
field_index(new_order_single, day_booking_inst) -> 14;
field_index(new_order_single, booking_unit) -> 15;
field_index(new_order_single, prealloc_method) -> 16;
field_index(new_order_single, alloc_id) -> 17;
field_index(new_order_single, settl_type) -> 18;
field_index(new_order_single, settl_date) -> 19;
field_index(new_order_single, cash_margin) -> 20;
field_index(new_order_single, clearing_fee_indicator) -> 21;
field_index(new_order_single, handl_inst) -> 22;
field_index(new_order_single, exec_inst) -> 23;
field_index(new_order_single, min_qty) -> 24;
field_index(new_order_single, max_floor) -> 25;
field_index(new_order_single, ex_destination) -> 26;
field_index(new_order_single, process_code) -> 27;
field_index(new_order_single, prev_close_px) -> 28;
field_index(new_order_single, side) -> 29;
field_index(new_order_single, locate_reqd) -> 30;
field_index(new_order_single, transact_time) -> 31;
field_index(new_order_single, qty_type) -> 32;
field_index(new_order_single, ord_type) -> 33;
field_index(new_order_single, price_type) -> 34;
field_index(new_order_single, price) -> 35;
field_index(new_order_single, stop_px) -> 36;
field_index(new_order_single, currency) -> 37;
field_index(new_order_single, compliance_id) -> 38;
field_index(new_order_single, solicited_flag) -> 39;
field_index(new_order_single, ioi_id) -> 40;
field_index(new_order_single, quote_id) -> 41;
field_index(new_order_single, time_in_force) -> 42;
field_index(new_order_single, effective_time) -> 43;
field_index(new_order_single, expire_date) -> 44;
field_index(new_order_single, expire_time) -> 45;
field_index(new_order_single, gt_booking_inst) -> 46;
field_index(new_order_single, order_capacity) -> 47;
field_index(new_order_single, order_restrictions) -> 48;
field_index(new_order_single, cust_order_capacity) -> 49;
field_index(new_order_single, forex_req) -> 50;
field_index(new_order_single, settl_currency) -> 51;
field_index(new_order_single, booking_type) -> 52;
field_index(new_order_single, text) -> 53;
field_index(new_order_single, encoded_text) -> 54;
field_index(new_order_single, settl_date2) -> 55;
field_index(new_order_single, order_qty2) -> 56;
field_index(new_order_single, price2) -> 57;
field_index(new_order_single, position_effect) -> 58;
field_index(new_order_single, covered_or_uncovered) -> 59;
field_index(new_order_single, max_show) -> 60;
field_index(new_order_single, target_strategy) -> 61;
field_index(new_order_single, target_strategy_parameters) -> 62;
field_index(new_order_single, participation_rate) -> 63;
field_index(new_order_single, cancellation_rights) -> 64;
field_index(new_order_single, money_laundering_status) -> 65;
field_index(new_order_single, regist_id) -> 66;
field_index(new_order_single, designation) -> 67;
field_index(new_order_single, signature) -> 68;
field_index(new_order_list, sender_comp_id) -> 2;
field_index(new_order_list, target_comp_id) -> 3;
field_index(new_order_list, msg_seq_num) -> 4;
field_index(new_order_list, sending_time) -> 5;
field_index(new_order_list, list_id) -> 6;
field_index(new_order_list, bid_id) -> 7;
field_index(new_order_list, client_bid_id) -> 8;
field_index(new_order_list, prog_rpt_reqs) -> 9;
field_index(new_order_list, bid_type) -> 10;
field_index(new_order_list, prog_period_interval) -> 11;
field_index(new_order_list, cancellation_rights) -> 12;
field_index(new_order_list, money_laundering_status) -> 13;
field_index(new_order_list, regist_id) -> 14;
field_index(new_order_list, list_exec_inst_type) -> 15;
field_index(new_order_list, list_exec_inst) -> 16;
field_index(new_order_list, encoded_list_exec_inst) -> 17;
field_index(new_order_list, allowable_one_sidedness_pct) -> 18;
field_index(new_order_list, allowable_one_sidedness_value) -> 19;
field_index(new_order_list, allowable_one_sidedness_curr) -> 20;
field_index(new_order_list, tot_no_orders) -> 21;
field_index(new_order_list, last_fragment) -> 22;
field_index(new_order_list, settl_type) -> 23;
field_index(new_order_list, settl_date) -> 24;
field_index(new_order_list, cash_margin) -> 25;
field_index(new_order_list, clearing_fee_indicator) -> 26;
field_index(new_order_list, handl_inst) -> 27;
field_index(new_order_list, exec_inst) -> 28;
field_index(new_order_list, min_qty) -> 29;
field_index(new_order_list, max_floor) -> 30;
field_index(new_order_list, ex_destination) -> 31;
field_index(new_order_list, process_code) -> 32;
field_index(new_order_list, prev_close_px) -> 33;
field_index(new_order_list, side) -> 34;
field_index(new_order_list, side_value_ind) -> 35;
field_index(new_order_list, locate_reqd) -> 36;
field_index(new_order_list, transact_time) -> 37;
field_index(new_order_list, qty_type) -> 38;
field_index(new_order_list, ord_type) -> 39;
field_index(new_order_list, price_type) -> 40;
field_index(new_order_list, price) -> 41;
field_index(new_order_list, stop_px) -> 42;
field_index(new_order_list, currency) -> 43;
field_index(new_order_list, compliance_id) -> 44;
field_index(new_order_list, solicited_flag) -> 45;
field_index(new_order_list, ioi_id) -> 46;
field_index(new_order_list, quote_id) -> 47;
field_index(new_order_list, time_in_force) -> 48;
field_index(new_order_list, effective_time) -> 49;
field_index(new_order_list, expire_date) -> 50;
field_index(new_order_list, expire_time) -> 51;
field_index(new_order_list, gt_booking_inst) -> 52;
field_index(new_order_list, order_capacity) -> 53;
field_index(new_order_list, order_restrictions) -> 54;
field_index(new_order_list, cust_order_capacity) -> 55;
field_index(new_order_list, forex_req) -> 56;
field_index(new_order_list, settl_currency) -> 57;
field_index(new_order_list, booking_type) -> 58;
field_index(new_order_list, text) -> 59;
field_index(new_order_list, encoded_text) -> 60;
field_index(new_order_list, settl_date2) -> 61;
field_index(new_order_list, order_qty2) -> 62;
field_index(new_order_list, price2) -> 63;
field_index(new_order_list, position_effect) -> 64;
field_index(new_order_list, covered_or_uncovered) -> 65;
field_index(new_order_list, max_show) -> 66;
field_index(new_order_list, target_strategy) -> 67;
field_index(new_order_list, target_strategy_parameters) -> 68;
field_index(new_order_list, participation_rate) -> 69;
field_index(new_order_list, designation) -> 70;
field_index(new_order_list, signature) -> 71;
field_index(order_cancel_request, sender_comp_id) -> 2;
field_index(order_cancel_request, target_comp_id) -> 3;
field_index(order_cancel_request, msg_seq_num) -> 4;
field_index(order_cancel_request, sending_time) -> 5;
field_index(order_cancel_request, orig_cl_ord_id) -> 6;
field_index(order_cancel_request, order_id) -> 7;
field_index(order_cancel_request, cl_ord_id) -> 8;
field_index(order_cancel_request, secondary_cl_ord_id) -> 9;
field_index(order_cancel_request, cl_ord_link_id) -> 10;
field_index(order_cancel_request, list_id) -> 11;
field_index(order_cancel_request, orig_ord_mod_time) -> 12;
field_index(order_cancel_request, account) -> 13;
field_index(order_cancel_request, acct_id_source) -> 14;
field_index(order_cancel_request, account_type) -> 15;
field_index(order_cancel_request, side) -> 16;
field_index(order_cancel_request, transact_time) -> 17;
field_index(order_cancel_request, compliance_id) -> 18;
field_index(order_cancel_request, text) -> 19;
field_index(order_cancel_request, encoded_text) -> 20;
field_index(order_cancel_request, signature) -> 21;
field_index(order_cancel_replace_request, sender_comp_id) -> 2;
field_index(order_cancel_replace_request, target_comp_id) -> 3;
field_index(order_cancel_replace_request, msg_seq_num) -> 4;
field_index(order_cancel_replace_request, sending_time) -> 5;
field_index(order_cancel_replace_request, order_id) -> 6;
field_index(order_cancel_replace_request, trade_origination_date) -> 7;
field_index(order_cancel_replace_request, trade_date) -> 8;
field_index(order_cancel_replace_request, orig_cl_ord_id) -> 9;
field_index(order_cancel_replace_request, cl_ord_id) -> 10;
field_index(order_cancel_replace_request, secondary_cl_ord_id) -> 11;
field_index(order_cancel_replace_request, cl_ord_link_id) -> 12;
field_index(order_cancel_replace_request, list_id) -> 13;
field_index(order_cancel_replace_request, orig_ord_mod_time) -> 14;
field_index(order_cancel_replace_request, account) -> 15;
field_index(order_cancel_replace_request, acct_id_source) -> 16;
field_index(order_cancel_replace_request, account_type) -> 17;
field_index(order_cancel_replace_request, day_booking_inst) -> 18;
field_index(order_cancel_replace_request, booking_unit) -> 19;
field_index(order_cancel_replace_request, prealloc_method) -> 20;
field_index(order_cancel_replace_request, alloc_id) -> 21;
field_index(order_cancel_replace_request, settl_type) -> 22;
field_index(order_cancel_replace_request, settl_date) -> 23;
field_index(order_cancel_replace_request, cash_margin) -> 24;
field_index(order_cancel_replace_request, clearing_fee_indicator) -> 25;
field_index(order_cancel_replace_request, handl_inst) -> 26;
field_index(order_cancel_replace_request, exec_inst) -> 27;
field_index(order_cancel_replace_request, min_qty) -> 28;
field_index(order_cancel_replace_request, max_floor) -> 29;
field_index(order_cancel_replace_request, ex_destination) -> 30;
field_index(order_cancel_replace_request, side) -> 31;
field_index(order_cancel_replace_request, transact_time) -> 32;
field_index(order_cancel_replace_request, qty_type) -> 33;
field_index(order_cancel_replace_request, ord_type) -> 34;
field_index(order_cancel_replace_request, price_type) -> 35;
field_index(order_cancel_replace_request, price) -> 36;
field_index(order_cancel_replace_request, stop_px) -> 37;
field_index(order_cancel_replace_request, target_strategy) -> 38;
field_index(order_cancel_replace_request, target_strategy_parameters) -> 39;
field_index(order_cancel_replace_request, participation_rate) -> 40;
field_index(order_cancel_replace_request, compliance_id) -> 41;
field_index(order_cancel_replace_request, solicited_flag) -> 42;
field_index(order_cancel_replace_request, currency) -> 43;
field_index(order_cancel_replace_request, time_in_force) -> 44;
field_index(order_cancel_replace_request, effective_time) -> 45;
field_index(order_cancel_replace_request, expire_date) -> 46;
field_index(order_cancel_replace_request, expire_time) -> 47;
field_index(order_cancel_replace_request, gt_booking_inst) -> 48;
field_index(order_cancel_replace_request, order_capacity) -> 49;
field_index(order_cancel_replace_request, order_restrictions) -> 50;
field_index(order_cancel_replace_request, cust_order_capacity) -> 51;
field_index(order_cancel_replace_request, forex_req) -> 52;
field_index(order_cancel_replace_request, settl_currency) -> 53;
field_index(order_cancel_replace_request, booking_type) -> 54;
field_index(order_cancel_replace_request, text) -> 55;
field_index(order_cancel_replace_request, encoded_text) -> 56;
field_index(order_cancel_replace_request, settl_date2) -> 57;
field_index(order_cancel_replace_request, order_qty2) -> 58;
field_index(order_cancel_replace_request, price2) -> 59;
field_index(order_cancel_replace_request, position_effect) -> 60;
field_index(order_cancel_replace_request, covered_or_uncovered) -> 61;
field_index(order_cancel_replace_request, max_show) -> 62;
field_index(order_cancel_replace_request, locate_reqd) -> 63;
field_index(order_cancel_replace_request, cancellation_rights) -> 64;
field_index(order_cancel_replace_request, money_laundering_status) -> 65;
field_index(order_cancel_replace_request, regist_id) -> 66;
field_index(order_cancel_replace_request, designation) -> 67;
field_index(order_cancel_replace_request, signature) -> 68;
field_index(order_status_request, sender_comp_id) -> 2;
field_index(order_status_request, target_comp_id) -> 3;
field_index(order_status_request, msg_seq_num) -> 4;
field_index(order_status_request, sending_time) -> 5;
field_index(order_status_request, order_id) -> 6;
field_index(order_status_request, cl_ord_id) -> 7;
field_index(order_status_request, secondary_cl_ord_id) -> 8;
field_index(order_status_request, cl_ord_link_id) -> 9;
field_index(order_status_request, ord_status_req_id) -> 10;
field_index(order_status_request, account) -> 11;
field_index(order_status_request, acct_id_source) -> 12;
field_index(order_status_request, side) -> 13;
field_index(order_status_request, signature) -> 14;
field_index(allocation_instruction, sender_comp_id) -> 2;
field_index(allocation_instruction, target_comp_id) -> 3;
field_index(allocation_instruction, msg_seq_num) -> 4;
field_index(allocation_instruction, sending_time) -> 5;
field_index(allocation_instruction, alloc_id) -> 6;
field_index(allocation_instruction, alloc_trans_type) -> 7;
field_index(allocation_instruction, alloc_type) -> 8;
field_index(allocation_instruction, secondary_alloc_id) -> 9;
field_index(allocation_instruction, ref_alloc_id) -> 10;
field_index(allocation_instruction, alloc_canc_replace_reason) -> 11;
field_index(allocation_instruction, alloc_intermed_req_type) -> 12;
field_index(allocation_instruction, alloc_link_id) -> 13;
field_index(allocation_instruction, alloc_link_type) -> 14;
field_index(allocation_instruction, booking_ref_id) -> 15;
field_index(allocation_instruction, alloc_no_orders_type) -> 16;
field_index(allocation_instruction, previously_reported) -> 17;
field_index(allocation_instruction, reversal_indicator) -> 18;
field_index(allocation_instruction, match_type) -> 19;
field_index(allocation_instruction, side) -> 20;
field_index(allocation_instruction, quantity) -> 21;
field_index(allocation_instruction, qty_type) -> 22;
field_index(allocation_instruction, last_mkt) -> 23;
field_index(allocation_instruction, trade_origination_date) -> 24;
field_index(allocation_instruction, trading_session_id) -> 25;
field_index(allocation_instruction, trading_session_sub_id) -> 26;
field_index(allocation_instruction, price_type) -> 27;
field_index(allocation_instruction, avg_px) -> 28;
field_index(allocation_instruction, avg_par_px) -> 29;
field_index(allocation_instruction, currency) -> 30;
field_index(allocation_instruction, avg_px_precision) -> 31;
field_index(allocation_instruction, trade_date) -> 32;
field_index(allocation_instruction, transact_time) -> 33;
field_index(allocation_instruction, settl_type) -> 34;
field_index(allocation_instruction, settl_date) -> 35;
field_index(allocation_instruction, booking_type) -> 36;
field_index(allocation_instruction, gross_trade_amt) -> 37;
field_index(allocation_instruction, concession) -> 38;
field_index(allocation_instruction, total_takedown) -> 39;
field_index(allocation_instruction, net_money) -> 40;
field_index(allocation_instruction, position_effect) -> 41;
field_index(allocation_instruction, auto_accept_indicator) -> 42;
field_index(allocation_instruction, text) -> 43;
field_index(allocation_instruction, encoded_text) -> 44;
field_index(allocation_instruction, num_days_interest) -> 45;
field_index(allocation_instruction, accrued_interest_rate) -> 46;
field_index(allocation_instruction, accrued_interest_amt) -> 47;
field_index(allocation_instruction, total_accrued_interest_amt) -> 48;
field_index(allocation_instruction, interest_at_maturity) -> 49;
field_index(allocation_instruction, end_accrued_interest_amt) -> 50;
field_index(allocation_instruction, start_cash) -> 51;
field_index(allocation_instruction, end_cash) -> 52;
field_index(allocation_instruction, legal_confirm) -> 53;
field_index(allocation_instruction, tot_no_allocs) -> 54;
field_index(allocation_instruction, last_fragment) -> 55;
field_index(allocation_instruction, clearing_fee_indicator) -> 56;
field_index(allocation_instruction, alloc_settl_inst_type) -> 57;
field_index(allocation_instruction, signature) -> 58;
field_index(list_cancel_request, sender_comp_id) -> 2;
field_index(list_cancel_request, target_comp_id) -> 3;
field_index(list_cancel_request, msg_seq_num) -> 4;
field_index(list_cancel_request, sending_time) -> 5;
field_index(list_cancel_request, list_id) -> 6;
field_index(list_cancel_request, transact_time) -> 7;
field_index(list_cancel_request, trade_origination_date) -> 8;
field_index(list_cancel_request, trade_date) -> 9;
field_index(list_cancel_request, text) -> 10;
field_index(list_cancel_request, encoded_text) -> 11;
field_index(list_cancel_request, signature) -> 12;
field_index(list_execute, sender_comp_id) -> 2;
field_index(list_execute, target_comp_id) -> 3;
field_index(list_execute, msg_seq_num) -> 4;
field_index(list_execute, sending_time) -> 5;
field_index(list_execute, list_id) -> 6;
field_index(list_execute, client_bid_id) -> 7;
field_index(list_execute, bid_id) -> 8;
field_index(list_execute, transact_time) -> 9;
field_index(list_execute, text) -> 10;
field_index(list_execute, encoded_text) -> 11;
field_index(list_execute, signature) -> 12;
field_index(list_status_request, sender_comp_id) -> 2;
field_index(list_status_request, target_comp_id) -> 3;
field_index(list_status_request, msg_seq_num) -> 4;
field_index(list_status_request, sending_time) -> 5;
field_index(list_status_request, list_id) -> 6;
field_index(list_status_request, text) -> 7;
field_index(list_status_request, encoded_text) -> 8;
field_index(list_status_request, signature) -> 9;
field_index(list_status, sender_comp_id) -> 2;
field_index(list_status, target_comp_id) -> 3;
field_index(list_status, msg_seq_num) -> 4;
field_index(list_status, sending_time) -> 5;
field_index(list_status, list_id) -> 6;
field_index(list_status, list_status_type) -> 7;
field_index(list_status, no_rpts) -> 8;
field_index(list_status, list_order_status) -> 9;
field_index(list_status, rpt_seq) -> 10;
field_index(list_status, list_status_text) -> 11;
field_index(list_status, encoded_list_status_text) -> 12;
field_index(list_status, transact_time) -> 13;
field_index(list_status, tot_no_orders) -> 14;
field_index(list_status, last_fragment) -> 15;
field_index(list_status, signature) -> 16;
field_index(allocation_instruction_ack, sender_comp_id) -> 2;
field_index(allocation_instruction_ack, target_comp_id) -> 3;
field_index(allocation_instruction_ack, msg_seq_num) -> 4;
field_index(allocation_instruction_ack, sending_time) -> 5;
field_index(allocation_instruction_ack, alloc_id) -> 6;
field_index(allocation_instruction_ack, secondary_alloc_id) -> 7;
field_index(allocation_instruction_ack, trade_date) -> 8;
field_index(allocation_instruction_ack, transact_time) -> 9;
field_index(allocation_instruction_ack, alloc_status) -> 10;
field_index(allocation_instruction_ack, alloc_rej_code) -> 11;
field_index(allocation_instruction_ack, alloc_type) -> 12;
field_index(allocation_instruction_ack, alloc_intermed_req_type) -> 13;
field_index(allocation_instruction_ack, match_status) -> 14;
field_index(allocation_instruction_ack, product) -> 15;
field_index(allocation_instruction_ack, security_type) -> 16;
field_index(allocation_instruction_ack, text) -> 17;
field_index(allocation_instruction_ack, encoded_text) -> 18;
field_index(allocation_instruction_ack, signature) -> 19;
field_index(dont_know_trade, sender_comp_id) -> 2;
field_index(dont_know_trade, target_comp_id) -> 3;
field_index(dont_know_trade, msg_seq_num) -> 4;
field_index(dont_know_trade, sending_time) -> 5;
field_index(dont_know_trade, order_id) -> 6;
field_index(dont_know_trade, secondary_order_id) -> 7;
field_index(dont_know_trade, exec_id) -> 8;
field_index(dont_know_trade, dk_reason) -> 9;
field_index(dont_know_trade, side) -> 10;
field_index(dont_know_trade, last_qty) -> 11;
field_index(dont_know_trade, last_px) -> 12;
field_index(dont_know_trade, text) -> 13;
field_index(dont_know_trade, encoded_text) -> 14;
field_index(dont_know_trade, signature) -> 15;
field_index(quote_request, sender_comp_id) -> 2;
field_index(quote_request, target_comp_id) -> 3;
field_index(quote_request, msg_seq_num) -> 4;
field_index(quote_request, sending_time) -> 5;
field_index(quote_request, quote_req_id) -> 6;
field_index(quote_request, rfq_req_id) -> 7;
field_index(quote_request, cl_ord_id) -> 8;
field_index(quote_request, order_capacity) -> 9;
field_index(quote_request, prev_close_px) -> 10;
field_index(quote_request, quote_request_type) -> 11;
field_index(quote_request, quote_type) -> 12;
field_index(quote_request, trading_session_id) -> 13;
field_index(quote_request, trading_session_sub_id) -> 14;
field_index(quote_request, trade_origination_date) -> 15;
field_index(quote_request, side) -> 16;
field_index(quote_request, qty_type) -> 17;
field_index(quote_request, settl_type) -> 18;
field_index(quote_request, settl_date) -> 19;
field_index(quote_request, settl_date2) -> 20;
field_index(quote_request, order_qty2) -> 21;
field_index(quote_request, currency) -> 22;
field_index(quote_request, account) -> 23;
field_index(quote_request, acct_id_source) -> 24;
field_index(quote_request, account_type) -> 25;
field_index(quote_request, quote_price_type) -> 26;
field_index(quote_request, ord_type) -> 27;
field_index(quote_request, valid_until_time) -> 28;
field_index(quote_request, expire_time) -> 29;
field_index(quote_request, transact_time) -> 30;
field_index(quote_request, price_type) -> 31;
field_index(quote_request, price) -> 32;
field_index(quote_request, price2) -> 33;
field_index(quote_request, text) -> 34;
field_index(quote_request, encoded_text) -> 35;
field_index(quote_request, signature) -> 36;
field_index(quote, sender_comp_id) -> 2;
field_index(quote, target_comp_id) -> 3;
field_index(quote, msg_seq_num) -> 4;
field_index(quote, sending_time) -> 5;
field_index(quote, quote_req_id) -> 6;
field_index(quote, quote_id) -> 7;
field_index(quote, quote_resp_id) -> 8;
field_index(quote, quote_type) -> 9;
field_index(quote, quote_response_level) -> 10;
field_index(quote, trading_session_id) -> 11;
field_index(quote, trading_session_sub_id) -> 12;
field_index(quote, side) -> 13;
field_index(quote, settl_type) -> 14;
field_index(quote, settl_date) -> 15;
field_index(quote, settl_date2) -> 16;
field_index(quote, order_qty2) -> 17;
field_index(quote, currency) -> 18;
field_index(quote, account) -> 19;
field_index(quote, acct_id_source) -> 20;
field_index(quote, account_type) -> 21;
field_index(quote, bid_px) -> 22;
field_index(quote, offer_px) -> 23;
field_index(quote, mkt_bid_px) -> 24;
field_index(quote, mkt_offer_px) -> 25;
field_index(quote, min_bid_size) -> 26;
field_index(quote, bid_size) -> 27;
field_index(quote, min_offer_size) -> 28;
field_index(quote, offer_size) -> 29;
field_index(quote, valid_until_time) -> 30;
field_index(quote, bid_spot_rate) -> 31;
field_index(quote, offer_spot_rate) -> 32;
field_index(quote, bid_forward_points) -> 33;
field_index(quote, offer_forward_points) -> 34;
field_index(quote, mid_px) -> 35;
field_index(quote, bid_yield) -> 36;
field_index(quote, mid_yield) -> 37;
field_index(quote, offer_yield) -> 38;
field_index(quote, transact_time) -> 39;
field_index(quote, ord_type) -> 40;
field_index(quote, bid_forward_points2) -> 41;
field_index(quote, offer_forward_points2) -> 42;
field_index(quote, settl_curr_bid_fx_rate) -> 43;
field_index(quote, settl_curr_offer_fx_rate) -> 44;
field_index(quote, settl_curr_fx_rate_calc) -> 45;
field_index(quote, comm_type) -> 46;
field_index(quote, commission) -> 47;
field_index(quote, cust_order_capacity) -> 48;
field_index(quote, ex_destination) -> 49;
field_index(quote, order_capacity) -> 50;
field_index(quote, price_type) -> 51;
field_index(quote, text) -> 52;
field_index(quote, encoded_text) -> 53;
field_index(quote, signature) -> 54;
field_index(settlement_instructions, sender_comp_id) -> 2;
field_index(settlement_instructions, target_comp_id) -> 3;
field_index(settlement_instructions, msg_seq_num) -> 4;
field_index(settlement_instructions, sending_time) -> 5;
field_index(settlement_instructions, settl_inst_msg_id) -> 6;
field_index(settlement_instructions, settl_inst_req_id) -> 7;
field_index(settlement_instructions, settl_inst_mode) -> 8;
field_index(settlement_instructions, settl_inst_req_rej_code) -> 9;
field_index(settlement_instructions, text) -> 10;
field_index(settlement_instructions, encoded_text) -> 11;
field_index(settlement_instructions, cl_ord_id) -> 12;
field_index(settlement_instructions, transact_time) -> 13;
field_index(settlement_instructions, signature) -> 14;
field_index(market_data_request, sender_comp_id) -> 2;
field_index(market_data_request, target_comp_id) -> 3;
field_index(market_data_request, msg_seq_num) -> 4;
field_index(market_data_request, sending_time) -> 5;
field_index(market_data_request, md_req_id) -> 6;
field_index(market_data_request, subscription_request_type) -> 7;
field_index(market_data_request, market_depth) -> 8;
field_index(market_data_request, md_update_type) -> 9;
field_index(market_data_request, aggregated_book) -> 10;
field_index(market_data_request, open_close_settl_flag) -> 11;
field_index(market_data_request, scope) -> 12;
field_index(market_data_request, md_implicit_delete) -> 13;
field_index(market_data_request, appl_queue_action) -> 14;
field_index(market_data_request, appl_queue_max) -> 15;
field_index(market_data_request, signature) -> 16;
field_index(market_data_snapshot_full_refresh, sender_comp_id) -> 2;
field_index(market_data_snapshot_full_refresh, target_comp_id) -> 3;
field_index(market_data_snapshot_full_refresh, msg_seq_num) -> 4;
field_index(market_data_snapshot_full_refresh, sending_time) -> 5;
field_index(market_data_snapshot_full_refresh, md_req_id) -> 6;
field_index(market_data_snapshot_full_refresh, financial_status) -> 7;
field_index(market_data_snapshot_full_refresh, corporate_action) -> 8;
field_index(market_data_snapshot_full_refresh, net_chg_prev_day) -> 9;
field_index(market_data_snapshot_full_refresh, appl_queue_depth) -> 10;
field_index(market_data_snapshot_full_refresh, appl_queue_resolution) -> 11;
field_index(market_data_snapshot_full_refresh, signature) -> 12;
field_index(market_data_incremental_refresh, sender_comp_id) -> 2;
field_index(market_data_incremental_refresh, target_comp_id) -> 3;
field_index(market_data_incremental_refresh, msg_seq_num) -> 4;
field_index(market_data_incremental_refresh, sending_time) -> 5;
field_index(market_data_incremental_refresh, md_req_id) -> 6;
field_index(market_data_incremental_refresh, financial_status) -> 7;
field_index(market_data_incremental_refresh, corporate_action) -> 8;
field_index(market_data_incremental_refresh, md_entry_px) -> 9;
field_index(market_data_incremental_refresh, currency) -> 10;
field_index(market_data_incremental_refresh, md_entry_size) -> 11;
field_index(market_data_incremental_refresh, md_entry_date) -> 12;
field_index(market_data_incremental_refresh, md_entry_time) -> 13;
field_index(market_data_incremental_refresh, tick_direction) -> 14;
field_index(market_data_incremental_refresh, md_mkt) -> 15;
field_index(market_data_incremental_refresh, trading_session_id) -> 16;
field_index(market_data_incremental_refresh, trading_session_sub_id) -> 17;
field_index(market_data_incremental_refresh, quote_condition) -> 18;
field_index(market_data_incremental_refresh, trade_condition) -> 19;
field_index(market_data_incremental_refresh, md_entry_originator) -> 20;
field_index(market_data_incremental_refresh, location_id) -> 21;
field_index(market_data_incremental_refresh, desk_id) -> 22;
field_index(market_data_incremental_refresh, open_close_settl_flag) -> 23;
field_index(market_data_incremental_refresh, time_in_force) -> 24;
field_index(market_data_incremental_refresh, expire_date) -> 25;
field_index(market_data_incremental_refresh, expire_time) -> 26;
field_index(market_data_incremental_refresh, min_qty) -> 27;
field_index(market_data_incremental_refresh, exec_inst) -> 28;
field_index(market_data_incremental_refresh, seller_days) -> 29;
field_index(market_data_incremental_refresh, order_id) -> 30;
field_index(market_data_incremental_refresh, quote_entry_id) -> 31;
field_index(market_data_incremental_refresh, md_entry_buyer) -> 32;
field_index(market_data_incremental_refresh, md_entry_seller) -> 33;
field_index(market_data_incremental_refresh, number_of_orders) -> 34;
field_index(market_data_incremental_refresh, md_entry_position_no) -> 35;
field_index(market_data_incremental_refresh, scope) -> 36;
field_index(market_data_incremental_refresh, price_delta) -> 37;
field_index(market_data_incremental_refresh, net_chg_prev_day) -> 38;
field_index(market_data_incremental_refresh, text) -> 39;
field_index(market_data_incremental_refresh, encoded_text) -> 40;
field_index(market_data_incremental_refresh, appl_queue_depth) -> 41;
field_index(market_data_incremental_refresh, appl_queue_resolution) -> 42;
field_index(market_data_incremental_refresh, signature) -> 43;
field_index(market_data_request_reject, sender_comp_id) -> 2;
field_index(market_data_request_reject, target_comp_id) -> 3;
field_index(market_data_request_reject, msg_seq_num) -> 4;
field_index(market_data_request_reject, sending_time) -> 5;
field_index(market_data_request_reject, md_req_id) -> 6;
field_index(market_data_request_reject, md_req_rej_reason) -> 7;
field_index(market_data_request_reject, text) -> 8;
field_index(market_data_request_reject, encoded_text) -> 9;
field_index(market_data_request_reject, signature) -> 10;
field_index(quote_cancel, sender_comp_id) -> 2;
field_index(quote_cancel, target_comp_id) -> 3;
field_index(quote_cancel, msg_seq_num) -> 4;
field_index(quote_cancel, sending_time) -> 5;
field_index(quote_cancel, quote_req_id) -> 6;
field_index(quote_cancel, quote_id) -> 7;
field_index(quote_cancel, quote_cancel_type) -> 8;
field_index(quote_cancel, quote_response_level) -> 9;
field_index(quote_cancel, account) -> 10;
field_index(quote_cancel, acct_id_source) -> 11;
field_index(quote_cancel, account_type) -> 12;
field_index(quote_cancel, trading_session_id) -> 13;
field_index(quote_cancel, trading_session_sub_id) -> 14;
field_index(quote_cancel, signature) -> 15;
field_index(quote_status_request, sender_comp_id) -> 2;
field_index(quote_status_request, target_comp_id) -> 3;
field_index(quote_status_request, msg_seq_num) -> 4;
field_index(quote_status_request, sending_time) -> 5;
field_index(quote_status_request, quote_status_req_id) -> 6;
field_index(quote_status_request, quote_id) -> 7;
field_index(quote_status_request, account) -> 8;
field_index(quote_status_request, acct_id_source) -> 9;
field_index(quote_status_request, account_type) -> 10;
field_index(quote_status_request, trading_session_id) -> 11;
field_index(quote_status_request, trading_session_sub_id) -> 12;
field_index(quote_status_request, subscription_request_type) -> 13;
field_index(quote_status_request, signature) -> 14;
field_index(mass_quote_acknowledgement, sender_comp_id) -> 2;
field_index(mass_quote_acknowledgement, target_comp_id) -> 3;
field_index(mass_quote_acknowledgement, msg_seq_num) -> 4;
field_index(mass_quote_acknowledgement, sending_time) -> 5;
field_index(mass_quote_acknowledgement, quote_req_id) -> 6;
field_index(mass_quote_acknowledgement, quote_id) -> 7;
field_index(mass_quote_acknowledgement, quote_status) -> 8;
field_index(mass_quote_acknowledgement, quote_reject_reason) -> 9;
field_index(mass_quote_acknowledgement, quote_response_level) -> 10;
field_index(mass_quote_acknowledgement, quote_type) -> 11;
field_index(mass_quote_acknowledgement, account) -> 12;
field_index(mass_quote_acknowledgement, acct_id_source) -> 13;
field_index(mass_quote_acknowledgement, account_type) -> 14;
field_index(mass_quote_acknowledgement, text) -> 15;
field_index(mass_quote_acknowledgement, encoded_text) -> 16;
field_index(mass_quote_acknowledgement, bid_px) -> 17;
field_index(mass_quote_acknowledgement, offer_px) -> 18;
field_index(mass_quote_acknowledgement, bid_size) -> 19;
field_index(mass_quote_acknowledgement, offer_size) -> 20;
field_index(mass_quote_acknowledgement, valid_until_time) -> 21;
field_index(mass_quote_acknowledgement, bid_spot_rate) -> 22;
field_index(mass_quote_acknowledgement, offer_spot_rate) -> 23;
field_index(mass_quote_acknowledgement, bid_forward_points) -> 24;
field_index(mass_quote_acknowledgement, offer_forward_points) -> 25;
field_index(mass_quote_acknowledgement, mid_px) -> 26;
field_index(mass_quote_acknowledgement, bid_yield) -> 27;
field_index(mass_quote_acknowledgement, mid_yield) -> 28;
field_index(mass_quote_acknowledgement, offer_yield) -> 29;
field_index(mass_quote_acknowledgement, transact_time) -> 30;
field_index(mass_quote_acknowledgement, trading_session_id) -> 31;
field_index(mass_quote_acknowledgement, trading_session_sub_id) -> 32;
field_index(mass_quote_acknowledgement, settl_date) -> 33;
field_index(mass_quote_acknowledgement, ord_type) -> 34;
field_index(mass_quote_acknowledgement, settl_date2) -> 35;
field_index(mass_quote_acknowledgement, order_qty2) -> 36;
field_index(mass_quote_acknowledgement, bid_forward_points2) -> 37;
field_index(mass_quote_acknowledgement, offer_forward_points2) -> 38;
field_index(mass_quote_acknowledgement, currency) -> 39;
field_index(mass_quote_acknowledgement, quote_entry_reject_reason) -> 40;
field_index(mass_quote_acknowledgement, signature) -> 41;
field_index(security_definition_request, sender_comp_id) -> 2;
field_index(security_definition_request, target_comp_id) -> 3;
field_index(security_definition_request, msg_seq_num) -> 4;
field_index(security_definition_request, sending_time) -> 5;
field_index(security_definition_request, security_req_id) -> 6;
field_index(security_definition_request, security_request_type) -> 7;
field_index(security_definition_request, currency) -> 8;
field_index(security_definition_request, text) -> 9;
field_index(security_definition_request, encoded_text) -> 10;
field_index(security_definition_request, trading_session_id) -> 11;
field_index(security_definition_request, trading_session_sub_id) -> 12;
field_index(security_definition_request, expiration_cycle) -> 13;
field_index(security_definition_request, subscription_request_type) -> 14;
field_index(security_definition_request, signature) -> 15;
field_index(security_definition, sender_comp_id) -> 2;
field_index(security_definition, target_comp_id) -> 3;
field_index(security_definition, msg_seq_num) -> 4;
field_index(security_definition, sending_time) -> 5;
field_index(security_definition, security_req_id) -> 6;
field_index(security_definition, security_response_id) -> 7;
field_index(security_definition, security_response_type) -> 8;
field_index(security_definition, currency) -> 9;
field_index(security_definition, trading_session_id) -> 10;
field_index(security_definition, trading_session_sub_id) -> 11;
field_index(security_definition, text) -> 12;
field_index(security_definition, encoded_text) -> 13;
field_index(security_definition, expiration_cycle) -> 14;
field_index(security_definition, round_lot) -> 15;
field_index(security_definition, min_trade_vol) -> 16;
field_index(security_definition, signature) -> 17;
field_index(security_status_request, sender_comp_id) -> 2;
field_index(security_status_request, target_comp_id) -> 3;
field_index(security_status_request, msg_seq_num) -> 4;
field_index(security_status_request, sending_time) -> 5;
field_index(security_status_request, security_status_req_id) -> 6;
field_index(security_status_request, currency) -> 7;
field_index(security_status_request, subscription_request_type) -> 8;
field_index(security_status_request, trading_session_id) -> 9;
field_index(security_status_request, trading_session_sub_id) -> 10;
field_index(security_status_request, signature) -> 11;
field_index(security_status, sender_comp_id) -> 2;
field_index(security_status, target_comp_id) -> 3;
field_index(security_status, msg_seq_num) -> 4;
field_index(security_status, sending_time) -> 5;
field_index(security_status, security_status_req_id) -> 6;
field_index(security_status, currency) -> 7;
field_index(security_status, trading_session_id) -> 8;
field_index(security_status, trading_session_sub_id) -> 9;
field_index(security_status, unsolicited_indicator) -> 10;
field_index(security_status, security_trading_status) -> 11;
field_index(security_status, financial_status) -> 12;
field_index(security_status, corporate_action) -> 13;
field_index(security_status, halt_reason_char) -> 14;
field_index(security_status, in_view_of_common) -> 15;
field_index(security_status, due_to_related) -> 16;
field_index(security_status, buy_volume) -> 17;
field_index(security_status, sell_volume) -> 18;
field_index(security_status, high_px) -> 19;
field_index(security_status, low_px) -> 20;
field_index(security_status, last_px) -> 21;
field_index(security_status, transact_time) -> 22;
field_index(security_status, adjustment) -> 23;
field_index(security_status, text) -> 24;
field_index(security_status, encoded_text) -> 25;
field_index(security_status, signature) -> 26;
field_index(trading_session_status_request, sender_comp_id) -> 2;
field_index(trading_session_status_request, target_comp_id) -> 3;
field_index(trading_session_status_request, msg_seq_num) -> 4;
field_index(trading_session_status_request, sending_time) -> 5;
field_index(trading_session_status_request, trad_ses_req_id) -> 6;
field_index(trading_session_status_request, trading_session_id) -> 7;
field_index(trading_session_status_request, trading_session_sub_id) -> 8;
field_index(trading_session_status_request, trad_ses_method) -> 9;
field_index(trading_session_status_request, trad_ses_mode) -> 10;
field_index(trading_session_status_request, subscription_request_type) -> 11;
field_index(trading_session_status_request, signature) -> 12;
field_index(trading_session_status, sender_comp_id) -> 2;
field_index(trading_session_status, target_comp_id) -> 3;
field_index(trading_session_status, msg_seq_num) -> 4;
field_index(trading_session_status, sending_time) -> 5;
field_index(trading_session_status, trad_ses_req_id) -> 6;
field_index(trading_session_status, trading_session_id) -> 7;
field_index(trading_session_status, trading_session_sub_id) -> 8;
field_index(trading_session_status, trad_ses_method) -> 9;
field_index(trading_session_status, trad_ses_mode) -> 10;
field_index(trading_session_status, unsolicited_indicator) -> 11;
field_index(trading_session_status, trad_ses_status) -> 12;
field_index(trading_session_status, trad_ses_status_rej_reason) -> 13;
field_index(trading_session_status, trad_ses_start_time) -> 14;
field_index(trading_session_status, trad_ses_open_time) -> 15;
field_index(trading_session_status, trad_ses_pre_close_time) -> 16;
field_index(trading_session_status, trad_ses_close_time) -> 17;
field_index(trading_session_status, trad_ses_end_time) -> 18;
field_index(trading_session_status, total_volume_traded) -> 19;
field_index(trading_session_status, text) -> 20;
field_index(trading_session_status, encoded_text) -> 21;
field_index(trading_session_status, signature) -> 22;
field_index(mass_quote, sender_comp_id) -> 2;
field_index(mass_quote, target_comp_id) -> 3;
field_index(mass_quote, msg_seq_num) -> 4;
field_index(mass_quote, sending_time) -> 5;
field_index(mass_quote, quote_req_id) -> 6;
field_index(mass_quote, quote_id) -> 7;
field_index(mass_quote, quote_type) -> 8;
field_index(mass_quote, quote_response_level) -> 9;
field_index(mass_quote, account) -> 10;
field_index(mass_quote, acct_id_source) -> 11;
field_index(mass_quote, account_type) -> 12;
field_index(mass_quote, def_bid_size) -> 13;
field_index(mass_quote, def_offer_size) -> 14;
field_index(mass_quote, bid_px) -> 15;
field_index(mass_quote, offer_px) -> 16;
field_index(mass_quote, bid_size) -> 17;
field_index(mass_quote, offer_size) -> 18;
field_index(mass_quote, valid_until_time) -> 19;
field_index(mass_quote, bid_spot_rate) -> 20;
field_index(mass_quote, offer_spot_rate) -> 21;
field_index(mass_quote, bid_forward_points) -> 22;
field_index(mass_quote, offer_forward_points) -> 23;
field_index(mass_quote, mid_px) -> 24;
field_index(mass_quote, bid_yield) -> 25;
field_index(mass_quote, mid_yield) -> 26;
field_index(mass_quote, offer_yield) -> 27;
field_index(mass_quote, transact_time) -> 28;
field_index(mass_quote, trading_session_id) -> 29;
field_index(mass_quote, trading_session_sub_id) -> 30;
field_index(mass_quote, settl_date) -> 31;
field_index(mass_quote, ord_type) -> 32;
field_index(mass_quote, settl_date2) -> 33;
field_index(mass_quote, order_qty2) -> 34;
field_index(mass_quote, bid_forward_points2) -> 35;
field_index(mass_quote, offer_forward_points2) -> 36;
field_index(mass_quote, currency) -> 37;
field_index(mass_quote, signature) -> 38;
field_index(business_message_reject, sender_comp_id) -> 2;
field_index(business_message_reject, target_comp_id) -> 3;
field_index(business_message_reject, msg_seq_num) -> 4;
field_index(business_message_reject, sending_time) -> 5;
field_index(business_message_reject, ref_seq_num) -> 6;
field_index(business_message_reject, ref_msg_type) -> 7;
field_index(business_message_reject, business_reject_ref_id) -> 8;
field_index(business_message_reject, business_reject_reason) -> 9;
field_index(business_message_reject, text) -> 10;
field_index(business_message_reject, encoded_text) -> 11;
field_index(business_message_reject, signature) -> 12;
field_index(bid_request, sender_comp_id) -> 2;
field_index(bid_request, target_comp_id) -> 3;
field_index(bid_request, msg_seq_num) -> 4;
field_index(bid_request, sending_time) -> 5;
field_index(bid_request, bid_id) -> 6;
field_index(bid_request, client_bid_id) -> 7;
field_index(bid_request, bid_request_trans_type) -> 8;
field_index(bid_request, list_name) -> 9;
field_index(bid_request, tot_no_related_sym) -> 10;
field_index(bid_request, bid_type) -> 11;
field_index(bid_request, num_tickets) -> 12;
field_index(bid_request, currency) -> 13;
field_index(bid_request, side_value1) -> 14;
field_index(bid_request, side_value2) -> 15;
field_index(bid_request, liquidity_ind_type) -> 16;
field_index(bid_request, wt_average_liquidity) -> 17;
field_index(bid_request, exchange_for_physical) -> 18;
field_index(bid_request, out_main_cntry_u_index) -> 19;
field_index(bid_request, cross_percent) -> 20;
field_index(bid_request, prog_rpt_reqs) -> 21;
field_index(bid_request, prog_period_interval) -> 22;
field_index(bid_request, inc_tax_ind) -> 23;
field_index(bid_request, forex_req) -> 24;
field_index(bid_request, num_bidders) -> 25;
field_index(bid_request, trade_date) -> 26;
field_index(bid_request, bid_trade_type) -> 27;
field_index(bid_request, basis_px_type) -> 28;
field_index(bid_request, strike_time) -> 29;
field_index(bid_request, text) -> 30;
field_index(bid_request, encoded_text) -> 31;
field_index(bid_request, signature) -> 32;
field_index(bid_response, sender_comp_id) -> 2;
field_index(bid_response, target_comp_id) -> 3;
field_index(bid_response, msg_seq_num) -> 4;
field_index(bid_response, sending_time) -> 5;
field_index(bid_response, bid_id) -> 6;
field_index(bid_response, client_bid_id) -> 7;
field_index(bid_response, signature) -> 8;
field_index(list_strike_price, sender_comp_id) -> 2;
field_index(list_strike_price, target_comp_id) -> 3;
field_index(list_strike_price, msg_seq_num) -> 4;
field_index(list_strike_price, sending_time) -> 5;
field_index(list_strike_price, list_id) -> 6;
field_index(list_strike_price, tot_no_strikes) -> 7;
field_index(list_strike_price, last_fragment) -> 8;
field_index(list_strike_price, signature) -> 9;
field_index(registration_instructions, sender_comp_id) -> 2;
field_index(registration_instructions, target_comp_id) -> 3;
field_index(registration_instructions, msg_seq_num) -> 4;
field_index(registration_instructions, sending_time) -> 5;
field_index(registration_instructions, regist_id) -> 6;
field_index(registration_instructions, regist_trans_type) -> 7;
field_index(registration_instructions, regist_ref_id) -> 8;
field_index(registration_instructions, cl_ord_id) -> 9;
field_index(registration_instructions, account) -> 10;
field_index(registration_instructions, acct_id_source) -> 11;
field_index(registration_instructions, regist_acct_type) -> 12;
field_index(registration_instructions, tax_advantage_type) -> 13;
field_index(registration_instructions, ownership_type) -> 14;
field_index(registration_instructions, signature) -> 15;
field_index(registration_instructions_response, sender_comp_id) -> 2;
field_index(registration_instructions_response, target_comp_id) -> 3;
field_index(registration_instructions_response, msg_seq_num) -> 4;
field_index(registration_instructions_response, sending_time) -> 5;
field_index(registration_instructions_response, regist_id) -> 6;
field_index(registration_instructions_response, regist_trans_type) -> 7;
field_index(registration_instructions_response, regist_ref_id) -> 8;
field_index(registration_instructions_response, cl_ord_id) -> 9;
field_index(registration_instructions_response, account) -> 10;
field_index(registration_instructions_response, acct_id_source) -> 11;
field_index(registration_instructions_response, regist_status) -> 12;
field_index(registration_instructions_response, regist_rej_reason_code) -> 13;
field_index(registration_instructions_response, regist_rej_reason_text) -> 14;
field_index(registration_instructions_response, signature) -> 15;
field_index(order_mass_cancel_request, sender_comp_id) -> 2;
field_index(order_mass_cancel_request, target_comp_id) -> 3;
field_index(order_mass_cancel_request, msg_seq_num) -> 4;
field_index(order_mass_cancel_request, sending_time) -> 5;
field_index(order_mass_cancel_request, cl_ord_id) -> 6;
field_index(order_mass_cancel_request, secondary_cl_ord_id) -> 7;
field_index(order_mass_cancel_request, mass_cancel_request_type) -> 8;
field_index(order_mass_cancel_request, trading_session_id) -> 9;
field_index(order_mass_cancel_request, trading_session_sub_id) -> 10;
field_index(order_mass_cancel_request, side) -> 11;
field_index(order_mass_cancel_request, transact_time) -> 12;
field_index(order_mass_cancel_request, text) -> 13;
field_index(order_mass_cancel_request, encoded_text) -> 14;
field_index(order_mass_cancel_request, signature) -> 15;
field_index(order_mass_cancel_report, sender_comp_id) -> 2;
field_index(order_mass_cancel_report, target_comp_id) -> 3;
field_index(order_mass_cancel_report, msg_seq_num) -> 4;
field_index(order_mass_cancel_report, sending_time) -> 5;
field_index(order_mass_cancel_report, cl_ord_id) -> 6;
field_index(order_mass_cancel_report, secondary_cl_ord_id) -> 7;
field_index(order_mass_cancel_report, order_id) -> 8;
field_index(order_mass_cancel_report, secondary_order_id) -> 9;
field_index(order_mass_cancel_report, mass_cancel_request_type) -> 10;
field_index(order_mass_cancel_report, mass_cancel_response) -> 11;
field_index(order_mass_cancel_report, mass_cancel_reject_reason) -> 12;
field_index(order_mass_cancel_report, total_affected_orders) -> 13;
field_index(order_mass_cancel_report, trading_session_id) -> 14;
field_index(order_mass_cancel_report, trading_session_sub_id) -> 15;
field_index(order_mass_cancel_report, side) -> 16;
field_index(order_mass_cancel_report, transact_time) -> 17;
field_index(order_mass_cancel_report, text) -> 18;
field_index(order_mass_cancel_report, encoded_text) -> 19;
field_index(order_mass_cancel_report, signature) -> 20;
field_index(new_order_cross, sender_comp_id) -> 2;
field_index(new_order_cross, target_comp_id) -> 3;
field_index(new_order_cross, msg_seq_num) -> 4;
field_index(new_order_cross, sending_time) -> 5;
field_index(new_order_cross, cross_id) -> 6;
field_index(new_order_cross, cross_type) -> 7;
field_index(new_order_cross, cross_prioritization) -> 8;
field_index(new_order_cross, qty_type) -> 9;
field_index(new_order_cross, order_capacity) -> 10;
field_index(new_order_cross, order_restrictions) -> 11;
field_index(new_order_cross, cust_order_capacity) -> 12;
field_index(new_order_cross, forex_req) -> 13;
field_index(new_order_cross, settl_currency) -> 14;
field_index(new_order_cross, booking_type) -> 15;
field_index(new_order_cross, text) -> 16;
field_index(new_order_cross, encoded_text) -> 17;
field_index(new_order_cross, position_effect) -> 18;
field_index(new_order_cross, covered_or_uncovered) -> 19;
field_index(new_order_cross, cash_margin) -> 20;
field_index(new_order_cross, clearing_fee_indicator) -> 21;
field_index(new_order_cross, solicited_flag) -> 22;
field_index(new_order_cross, side_compliance_id) -> 23;
field_index(new_order_cross, settl_type) -> 24;
field_index(new_order_cross, settl_date) -> 25;
field_index(new_order_cross, handl_inst) -> 26;
field_index(new_order_cross, exec_inst) -> 27;
field_index(new_order_cross, min_qty) -> 28;
field_index(new_order_cross, max_floor) -> 29;
field_index(new_order_cross, ex_destination) -> 30;
field_index(new_order_cross, process_code) -> 31;
field_index(new_order_cross, prev_close_px) -> 32;
field_index(new_order_cross, locate_reqd) -> 33;
field_index(new_order_cross, transact_time) -> 34;
field_index(new_order_cross, ord_type) -> 35;
field_index(new_order_cross, price_type) -> 36;
field_index(new_order_cross, price) -> 37;
field_index(new_order_cross, stop_px) -> 38;
field_index(new_order_cross, currency) -> 39;
field_index(new_order_cross, compliance_id) -> 40;
field_index(new_order_cross, ioi_id) -> 41;
field_index(new_order_cross, quote_id) -> 42;
field_index(new_order_cross, time_in_force) -> 43;
field_index(new_order_cross, effective_time) -> 44;
field_index(new_order_cross, expire_date) -> 45;
field_index(new_order_cross, expire_time) -> 46;
field_index(new_order_cross, gt_booking_inst) -> 47;
field_index(new_order_cross, max_show) -> 48;
field_index(new_order_cross, target_strategy) -> 49;
field_index(new_order_cross, target_strategy_parameters) -> 50;
field_index(new_order_cross, participation_rate) -> 51;
field_index(new_order_cross, cancellation_rights) -> 52;
field_index(new_order_cross, money_laundering_status) -> 53;
field_index(new_order_cross, regist_id) -> 54;
field_index(new_order_cross, designation) -> 55;
field_index(new_order_cross, signature) -> 56;
field_index(cross_order_cancel_replace_request, sender_comp_id) -> 2;
field_index(cross_order_cancel_replace_request, target_comp_id) -> 3;
field_index(cross_order_cancel_replace_request, msg_seq_num) -> 4;
field_index(cross_order_cancel_replace_request, sending_time) -> 5;
field_index(cross_order_cancel_replace_request, order_id) -> 6;
field_index(cross_order_cancel_replace_request, cross_id) -> 7;
field_index(cross_order_cancel_replace_request, orig_cross_id) -> 8;
field_index(cross_order_cancel_replace_request, cross_type) -> 9;
field_index(cross_order_cancel_replace_request, cross_prioritization) -> 10;
field_index(cross_order_cancel_replace_request, qty_type) -> 11;
field_index(cross_order_cancel_replace_request, order_capacity) -> 12;
field_index(cross_order_cancel_replace_request, order_restrictions) -> 13;
field_index(cross_order_cancel_replace_request, cust_order_capacity) -> 14;
field_index(cross_order_cancel_replace_request, forex_req) -> 15;
field_index(cross_order_cancel_replace_request, settl_currency) -> 16;
field_index(cross_order_cancel_replace_request, booking_type) -> 17;
field_index(cross_order_cancel_replace_request, text) -> 18;
field_index(cross_order_cancel_replace_request, encoded_text) -> 19;
field_index(cross_order_cancel_replace_request, position_effect) -> 20;
field_index(cross_order_cancel_replace_request, covered_or_uncovered) -> 21;
field_index(cross_order_cancel_replace_request, cash_margin) -> 22;
field_index(cross_order_cancel_replace_request, clearing_fee_indicator) -> 23;
field_index(cross_order_cancel_replace_request, solicited_flag) -> 24;
field_index(cross_order_cancel_replace_request, side_compliance_id) -> 25;
field_index(cross_order_cancel_replace_request, settl_type) -> 26;
field_index(cross_order_cancel_replace_request, settl_date) -> 27;
field_index(cross_order_cancel_replace_request, handl_inst) -> 28;
field_index(cross_order_cancel_replace_request, exec_inst) -> 29;
field_index(cross_order_cancel_replace_request, min_qty) -> 30;
field_index(cross_order_cancel_replace_request, max_floor) -> 31;
field_index(cross_order_cancel_replace_request, ex_destination) -> 32;
field_index(cross_order_cancel_replace_request, process_code) -> 33;
field_index(cross_order_cancel_replace_request, prev_close_px) -> 34;
field_index(cross_order_cancel_replace_request, locate_reqd) -> 35;
field_index(cross_order_cancel_replace_request, transact_time) -> 36;
field_index(cross_order_cancel_replace_request, ord_type) -> 37;
field_index(cross_order_cancel_replace_request, price_type) -> 38;
field_index(cross_order_cancel_replace_request, price) -> 39;
field_index(cross_order_cancel_replace_request, stop_px) -> 40;
field_index(cross_order_cancel_replace_request, currency) -> 41;
field_index(cross_order_cancel_replace_request, compliance_id) -> 42;
field_index(cross_order_cancel_replace_request, ioi_id) -> 43;
field_index(cross_order_cancel_replace_request, quote_id) -> 44;
field_index(cross_order_cancel_replace_request, time_in_force) -> 45;
field_index(cross_order_cancel_replace_request, effective_time) -> 46;
field_index(cross_order_cancel_replace_request, expire_date) -> 47;
field_index(cross_order_cancel_replace_request, expire_time) -> 48;
field_index(cross_order_cancel_replace_request, gt_booking_inst) -> 49;
field_index(cross_order_cancel_replace_request, max_show) -> 50;
field_index(cross_order_cancel_replace_request, target_strategy) -> 51;
field_index(cross_order_cancel_replace_request, target_strategy_parameters) -> 52;
field_index(cross_order_cancel_replace_request, participation_rate) -> 53;
field_index(cross_order_cancel_replace_request, cancellation_rights) -> 54;
field_index(cross_order_cancel_replace_request, money_laundering_status) -> 55;
field_index(cross_order_cancel_replace_request, regist_id) -> 56;
field_index(cross_order_cancel_replace_request, designation) -> 57;
field_index(cross_order_cancel_replace_request, signature) -> 58;
field_index(cross_order_cancel_request, sender_comp_id) -> 2;
field_index(cross_order_cancel_request, target_comp_id) -> 3;
field_index(cross_order_cancel_request, msg_seq_num) -> 4;
field_index(cross_order_cancel_request, sending_time) -> 5;
field_index(cross_order_cancel_request, order_id) -> 6;
field_index(cross_order_cancel_request, cross_id) -> 7;
field_index(cross_order_cancel_request, orig_cross_id) -> 8;
field_index(cross_order_cancel_request, cross_type) -> 9;
field_index(cross_order_cancel_request, cross_prioritization) -> 10;
field_index(cross_order_cancel_request, transact_time) -> 11;
field_index(cross_order_cancel_request, signature) -> 12;
field_index(security_type_request, sender_comp_id) -> 2;
field_index(security_type_request, target_comp_id) -> 3;
field_index(security_type_request, msg_seq_num) -> 4;
field_index(security_type_request, sending_time) -> 5;
field_index(security_type_request, security_req_id) -> 6;
field_index(security_type_request, text) -> 7;
field_index(security_type_request, encoded_text) -> 8;
field_index(security_type_request, trading_session_id) -> 9;
field_index(security_type_request, trading_session_sub_id) -> 10;
field_index(security_type_request, product) -> 11;
field_index(security_type_request, security_type) -> 12;
field_index(security_type_request, security_sub_type) -> 13;
field_index(security_type_request, signature) -> 14;
field_index(security_types, sender_comp_id) -> 2;
field_index(security_types, target_comp_id) -> 3;
field_index(security_types, msg_seq_num) -> 4;
field_index(security_types, sending_time) -> 5;
field_index(security_types, security_req_id) -> 6;
field_index(security_types, security_response_id) -> 7;
field_index(security_types, security_response_type) -> 8;
field_index(security_types, tot_no_security_types) -> 9;
field_index(security_types, last_fragment) -> 10;
field_index(security_types, text) -> 11;
field_index(security_types, encoded_text) -> 12;
field_index(security_types, trading_session_id) -> 13;
field_index(security_types, trading_session_sub_id) -> 14;
field_index(security_types, subscription_request_type) -> 15;
field_index(security_types, signature) -> 16;
field_index(security_list_request, sender_comp_id) -> 2;
field_index(security_list_request, target_comp_id) -> 3;
field_index(security_list_request, msg_seq_num) -> 4;
field_index(security_list_request, sending_time) -> 5;
field_index(security_list_request, security_req_id) -> 6;
field_index(security_list_request, security_list_request_type) -> 7;
field_index(security_list_request, currency) -> 8;
field_index(security_list_request, text) -> 9;
field_index(security_list_request, encoded_text) -> 10;
field_index(security_list_request, trading_session_id) -> 11;
field_index(security_list_request, trading_session_sub_id) -> 12;
field_index(security_list_request, subscription_request_type) -> 13;
field_index(security_list_request, signature) -> 14;
field_index(security_list, sender_comp_id) -> 2;
field_index(security_list, target_comp_id) -> 3;
field_index(security_list, msg_seq_num) -> 4;
field_index(security_list, sending_time) -> 5;
field_index(security_list, security_req_id) -> 6;
field_index(security_list, security_response_id) -> 7;
field_index(security_list, security_request_result) -> 8;
field_index(security_list, tot_no_related_sym) -> 9;
field_index(security_list, last_fragment) -> 10;
field_index(security_list, currency) -> 11;
field_index(security_list, round_lot) -> 12;
field_index(security_list, min_trade_vol) -> 13;
field_index(security_list, trading_session_id) -> 14;
field_index(security_list, trading_session_sub_id) -> 15;
field_index(security_list, expiration_cycle) -> 16;
field_index(security_list, text) -> 17;
field_index(security_list, encoded_text) -> 18;
field_index(security_list, signature) -> 19;
field_index(derivative_security_list_request, sender_comp_id) -> 2;
field_index(derivative_security_list_request, target_comp_id) -> 3;
field_index(derivative_security_list_request, msg_seq_num) -> 4;
field_index(derivative_security_list_request, sending_time) -> 5;
field_index(derivative_security_list_request, security_req_id) -> 6;
field_index(derivative_security_list_request, security_list_request_type) -> 7;
field_index(derivative_security_list_request, security_sub_type) -> 8;
field_index(derivative_security_list_request, currency) -> 9;
field_index(derivative_security_list_request, text) -> 10;
field_index(derivative_security_list_request, encoded_text) -> 11;
field_index(derivative_security_list_request, trading_session_id) -> 12;
field_index(derivative_security_list_request, trading_session_sub_id) -> 13;
field_index(derivative_security_list_request, subscription_request_type) -> 14;
field_index(derivative_security_list_request, signature) -> 15;
field_index(derivative_security_list, sender_comp_id) -> 2;
field_index(derivative_security_list, target_comp_id) -> 3;
field_index(derivative_security_list, msg_seq_num) -> 4;
field_index(derivative_security_list, sending_time) -> 5;
field_index(derivative_security_list, security_req_id) -> 6;
field_index(derivative_security_list, security_response_id) -> 7;
field_index(derivative_security_list, security_request_result) -> 8;
field_index(derivative_security_list, tot_no_related_sym) -> 9;
field_index(derivative_security_list, last_fragment) -> 10;
field_index(derivative_security_list, trading_session_id) -> 11;
field_index(derivative_security_list, trading_session_sub_id) -> 12;
field_index(derivative_security_list, text) -> 13;
field_index(derivative_security_list, encoded_text) -> 14;
field_index(derivative_security_list, signature) -> 15;
field_index(new_order_multileg, sender_comp_id) -> 2;
field_index(new_order_multileg, target_comp_id) -> 3;
field_index(new_order_multileg, msg_seq_num) -> 4;
field_index(new_order_multileg, sending_time) -> 5;
field_index(new_order_multileg, cl_ord_id) -> 6;
field_index(new_order_multileg, secondary_cl_ord_id) -> 7;
field_index(new_order_multileg, cl_ord_link_id) -> 8;
field_index(new_order_multileg, trade_origination_date) -> 9;
field_index(new_order_multileg, trade_date) -> 10;
field_index(new_order_multileg, account) -> 11;
field_index(new_order_multileg, acct_id_source) -> 12;
field_index(new_order_multileg, account_type) -> 13;
field_index(new_order_multileg, day_booking_inst) -> 14;
field_index(new_order_multileg, booking_unit) -> 15;
field_index(new_order_multileg, prealloc_method) -> 16;
field_index(new_order_multileg, alloc_id) -> 17;
field_index(new_order_multileg, settl_type) -> 18;
field_index(new_order_multileg, settl_date) -> 19;
field_index(new_order_multileg, cash_margin) -> 20;
field_index(new_order_multileg, clearing_fee_indicator) -> 21;
field_index(new_order_multileg, handl_inst) -> 22;
field_index(new_order_multileg, exec_inst) -> 23;
field_index(new_order_multileg, min_qty) -> 24;
field_index(new_order_multileg, max_floor) -> 25;
field_index(new_order_multileg, ex_destination) -> 26;
field_index(new_order_multileg, process_code) -> 27;
field_index(new_order_multileg, side) -> 28;
field_index(new_order_multileg, prev_close_px) -> 29;
field_index(new_order_multileg, leg_position_effect) -> 30;
field_index(new_order_multileg, leg_covered_or_uncovered) -> 31;
field_index(new_order_multileg, leg_ref_id) -> 32;
field_index(new_order_multileg, leg_price) -> 33;
field_index(new_order_multileg, leg_settl_type) -> 34;
field_index(new_order_multileg, leg_settl_date) -> 35;
field_index(new_order_multileg, locate_reqd) -> 36;
field_index(new_order_multileg, transact_time) -> 37;
field_index(new_order_multileg, qty_type) -> 38;
field_index(new_order_multileg, ord_type) -> 39;
field_index(new_order_multileg, price_type) -> 40;
field_index(new_order_multileg, price) -> 41;
field_index(new_order_multileg, stop_px) -> 42;
field_index(new_order_multileg, currency) -> 43;
field_index(new_order_multileg, compliance_id) -> 44;
field_index(new_order_multileg, solicited_flag) -> 45;
field_index(new_order_multileg, ioi_id) -> 46;
field_index(new_order_multileg, quote_id) -> 47;
field_index(new_order_multileg, time_in_force) -> 48;
field_index(new_order_multileg, effective_time) -> 49;
field_index(new_order_multileg, expire_date) -> 50;
field_index(new_order_multileg, expire_time) -> 51;
field_index(new_order_multileg, gt_booking_inst) -> 52;
field_index(new_order_multileg, order_capacity) -> 53;
field_index(new_order_multileg, order_restrictions) -> 54;
field_index(new_order_multileg, cust_order_capacity) -> 55;
field_index(new_order_multileg, forex_req) -> 56;
field_index(new_order_multileg, settl_currency) -> 57;
field_index(new_order_multileg, booking_type) -> 58;
field_index(new_order_multileg, text) -> 59;
field_index(new_order_multileg, encoded_text) -> 60;
field_index(new_order_multileg, position_effect) -> 61;
field_index(new_order_multileg, covered_or_uncovered) -> 62;
field_index(new_order_multileg, max_show) -> 63;
field_index(new_order_multileg, target_strategy) -> 64;
field_index(new_order_multileg, target_strategy_parameters) -> 65;
field_index(new_order_multileg, participation_rate) -> 66;
field_index(new_order_multileg, cancellation_rights) -> 67;
field_index(new_order_multileg, money_laundering_status) -> 68;
field_index(new_order_multileg, regist_id) -> 69;
field_index(new_order_multileg, designation) -> 70;
field_index(new_order_multileg, multi_leg_rpt_type_req) -> 71;
field_index(new_order_multileg, signature) -> 72;
field_index(multileg_order_cancel_replace, sender_comp_id) -> 2;
field_index(multileg_order_cancel_replace, target_comp_id) -> 3;
field_index(multileg_order_cancel_replace, msg_seq_num) -> 4;
field_index(multileg_order_cancel_replace, sending_time) -> 5;
field_index(multileg_order_cancel_replace, order_id) -> 6;
field_index(multileg_order_cancel_replace, orig_cl_ord_id) -> 7;
field_index(multileg_order_cancel_replace, cl_ord_id) -> 8;
field_index(multileg_order_cancel_replace, secondary_cl_ord_id) -> 9;
field_index(multileg_order_cancel_replace, cl_ord_link_id) -> 10;
field_index(multileg_order_cancel_replace, orig_ord_mod_time) -> 11;
field_index(multileg_order_cancel_replace, trade_origination_date) -> 12;
field_index(multileg_order_cancel_replace, trade_date) -> 13;
field_index(multileg_order_cancel_replace, account) -> 14;
field_index(multileg_order_cancel_replace, acct_id_source) -> 15;
field_index(multileg_order_cancel_replace, account_type) -> 16;
field_index(multileg_order_cancel_replace, day_booking_inst) -> 17;
field_index(multileg_order_cancel_replace, booking_unit) -> 18;
field_index(multileg_order_cancel_replace, prealloc_method) -> 19;
field_index(multileg_order_cancel_replace, alloc_id) -> 20;
field_index(multileg_order_cancel_replace, settl_type) -> 21;
field_index(multileg_order_cancel_replace, settl_date) -> 22;
field_index(multileg_order_cancel_replace, cash_margin) -> 23;
field_index(multileg_order_cancel_replace, clearing_fee_indicator) -> 24;
field_index(multileg_order_cancel_replace, handl_inst) -> 25;
field_index(multileg_order_cancel_replace, exec_inst) -> 26;
field_index(multileg_order_cancel_replace, min_qty) -> 27;
field_index(multileg_order_cancel_replace, max_floor) -> 28;
field_index(multileg_order_cancel_replace, ex_destination) -> 29;
field_index(multileg_order_cancel_replace, process_code) -> 30;
field_index(multileg_order_cancel_replace, side) -> 31;
field_index(multileg_order_cancel_replace, prev_close_px) -> 32;
field_index(multileg_order_cancel_replace, leg_position_effect) -> 33;
field_index(multileg_order_cancel_replace, leg_covered_or_uncovered) -> 34;
field_index(multileg_order_cancel_replace, leg_ref_id) -> 35;
field_index(multileg_order_cancel_replace, leg_price) -> 36;
field_index(multileg_order_cancel_replace, leg_settl_type) -> 37;
field_index(multileg_order_cancel_replace, leg_settl_date) -> 38;
field_index(multileg_order_cancel_replace, locate_reqd) -> 39;
field_index(multileg_order_cancel_replace, transact_time) -> 40;
field_index(multileg_order_cancel_replace, qty_type) -> 41;
field_index(multileg_order_cancel_replace, ord_type) -> 42;
field_index(multileg_order_cancel_replace, price_type) -> 43;
field_index(multileg_order_cancel_replace, price) -> 44;
field_index(multileg_order_cancel_replace, stop_px) -> 45;
field_index(multileg_order_cancel_replace, currency) -> 46;
field_index(multileg_order_cancel_replace, compliance_id) -> 47;
field_index(multileg_order_cancel_replace, solicited_flag) -> 48;
field_index(multileg_order_cancel_replace, ioi_id) -> 49;
field_index(multileg_order_cancel_replace, quote_id) -> 50;
field_index(multileg_order_cancel_replace, time_in_force) -> 51;
field_index(multileg_order_cancel_replace, effective_time) -> 52;
field_index(multileg_order_cancel_replace, expire_date) -> 53;
field_index(multileg_order_cancel_replace, expire_time) -> 54;
field_index(multileg_order_cancel_replace, gt_booking_inst) -> 55;
field_index(multileg_order_cancel_replace, order_capacity) -> 56;
field_index(multileg_order_cancel_replace, order_restrictions) -> 57;
field_index(multileg_order_cancel_replace, cust_order_capacity) -> 58;
field_index(multileg_order_cancel_replace, forex_req) -> 59;
field_index(multileg_order_cancel_replace, settl_currency) -> 60;
field_index(multileg_order_cancel_replace, booking_type) -> 61;
field_index(multileg_order_cancel_replace, text) -> 62;
field_index(multileg_order_cancel_replace, encoded_text) -> 63;
field_index(multileg_order_cancel_replace, position_effect) -> 64;
field_index(multileg_order_cancel_replace, covered_or_uncovered) -> 65;
field_index(multileg_order_cancel_replace, max_show) -> 66;
field_index(multileg_order_cancel_replace, target_strategy) -> 67;
field_index(multileg_order_cancel_replace, target_strategy_parameters) -> 68;
field_index(multileg_order_cancel_replace, participation_rate) -> 69;
field_index(multileg_order_cancel_replace, cancellation_rights) -> 70;
field_index(multileg_order_cancel_replace, money_laundering_status) -> 71;
field_index(multileg_order_cancel_replace, regist_id) -> 72;
field_index(multileg_order_cancel_replace, designation) -> 73;
field_index(multileg_order_cancel_replace, multi_leg_rpt_type_req) -> 74;
field_index(multileg_order_cancel_replace, signature) -> 75;
field_index(trade_capture_report_request, sender_comp_id) -> 2;
field_index(trade_capture_report_request, target_comp_id) -> 3;
field_index(trade_capture_report_request, msg_seq_num) -> 4;
field_index(trade_capture_report_request, sending_time) -> 5;
field_index(trade_capture_report_request, trade_request_id) -> 6;
field_index(trade_capture_report_request, trade_request_type) -> 7;
field_index(trade_capture_report_request, subscription_request_type) -> 8;
field_index(trade_capture_report_request, trade_report_id) -> 9;
field_index(trade_capture_report_request, secondary_trade_report_id) -> 10;
field_index(trade_capture_report_request, exec_id) -> 11;
field_index(trade_capture_report_request, exec_type) -> 12;
field_index(trade_capture_report_request, order_id) -> 13;
field_index(trade_capture_report_request, cl_ord_id) -> 14;
field_index(trade_capture_report_request, match_status) -> 15;
field_index(trade_capture_report_request, trd_type) -> 16;
field_index(trade_capture_report_request, trd_sub_type) -> 17;
field_index(trade_capture_report_request, transfer_reason) -> 18;
field_index(trade_capture_report_request, secondary_trd_type) -> 19;
field_index(trade_capture_report_request, trade_link_id) -> 20;
field_index(trade_capture_report_request, trd_match_id) -> 21;
field_index(trade_capture_report_request, clearing_business_date) -> 22;
field_index(trade_capture_report_request, trading_session_id) -> 23;
field_index(trade_capture_report_request, trading_session_sub_id) -> 24;
field_index(trade_capture_report_request, time_bracket) -> 25;
field_index(trade_capture_report_request, side) -> 26;
field_index(trade_capture_report_request, multi_leg_reporting_type) -> 27;
field_index(trade_capture_report_request, trade_input_source) -> 28;
field_index(trade_capture_report_request, trade_input_device) -> 29;
field_index(trade_capture_report_request, response_transport_type) -> 30;
field_index(trade_capture_report_request, response_destination) -> 31;
field_index(trade_capture_report_request, text) -> 32;
field_index(trade_capture_report_request, encoded_text) -> 33;
field_index(trade_capture_report_request, signature) -> 34;
field_index(trade_capture_report, sender_comp_id) -> 2;
field_index(trade_capture_report, target_comp_id) -> 3;
field_index(trade_capture_report, msg_seq_num) -> 4;
field_index(trade_capture_report, sending_time) -> 5;
field_index(trade_capture_report, trade_report_id) -> 6;
field_index(trade_capture_report, trade_report_trans_type) -> 7;
field_index(trade_capture_report, trade_report_type) -> 8;
field_index(trade_capture_report, trade_request_id) -> 9;
field_index(trade_capture_report, trd_type) -> 10;
field_index(trade_capture_report, trd_sub_type) -> 11;
field_index(trade_capture_report, secondary_trd_type) -> 12;
field_index(trade_capture_report, transfer_reason) -> 13;
field_index(trade_capture_report, exec_type) -> 14;
field_index(trade_capture_report, tot_num_trade_reports) -> 15;
field_index(trade_capture_report, last_rpt_requested) -> 16;
field_index(trade_capture_report, unsolicited_indicator) -> 17;
field_index(trade_capture_report, subscription_request_type) -> 18;
field_index(trade_capture_report, trade_report_ref_id) -> 19;
field_index(trade_capture_report, secondary_trade_report_ref_id) -> 20;
field_index(trade_capture_report, secondary_trade_report_id) -> 21;
field_index(trade_capture_report, trade_link_id) -> 22;
field_index(trade_capture_report, trd_match_id) -> 23;
field_index(trade_capture_report, exec_id) -> 24;
field_index(trade_capture_report, ord_status) -> 25;
field_index(trade_capture_report, secondary_exec_id) -> 26;
field_index(trade_capture_report, exec_restatement_reason) -> 27;
field_index(trade_capture_report, previously_reported) -> 28;
field_index(trade_capture_report, price_type) -> 29;
field_index(trade_capture_report, qty_type) -> 30;
field_index(trade_capture_report, underlying_trading_session_id) -> 31;
field_index(trade_capture_report, underlying_trading_session_sub_id) -> 32;
field_index(trade_capture_report, last_qty) -> 33;
field_index(trade_capture_report, last_px) -> 34;
field_index(trade_capture_report, last_par_px) -> 35;
field_index(trade_capture_report, last_spot_rate) -> 36;
field_index(trade_capture_report, last_forward_points) -> 37;
field_index(trade_capture_report, last_mkt) -> 38;
field_index(trade_capture_report, trade_date) -> 39;
field_index(trade_capture_report, clearing_business_date) -> 40;
field_index(trade_capture_report, avg_px) -> 41;
field_index(trade_capture_report, avg_px_indicator) -> 42;
field_index(trade_capture_report, multi_leg_reporting_type) -> 43;
field_index(trade_capture_report, trade_leg_ref_id) -> 44;
field_index(trade_capture_report, transact_time) -> 45;
field_index(trade_capture_report, settl_type) -> 46;
field_index(trade_capture_report, settl_date) -> 47;
field_index(trade_capture_report, match_status) -> 48;
field_index(trade_capture_report, match_type) -> 49;
field_index(trade_capture_report, clearing_fee_indicator) -> 50;
field_index(trade_capture_report, trade_input_source) -> 51;
field_index(trade_capture_report, trade_input_device) -> 52;
field_index(trade_capture_report, order_input_device) -> 53;
field_index(trade_capture_report, currency) -> 54;
field_index(trade_capture_report, compliance_id) -> 55;
field_index(trade_capture_report, solicited_flag) -> 56;
field_index(trade_capture_report, order_capacity) -> 57;
field_index(trade_capture_report, order_restrictions) -> 58;
field_index(trade_capture_report, cust_order_capacity) -> 59;
field_index(trade_capture_report, ord_type) -> 60;
field_index(trade_capture_report, exec_inst) -> 61;
field_index(trade_capture_report, trans_bkd_time) -> 62;
field_index(trade_capture_report, trading_session_id) -> 63;
field_index(trade_capture_report, trading_session_sub_id) -> 64;
field_index(trade_capture_report, time_bracket) -> 65;
field_index(trade_capture_report, gross_trade_amt) -> 66;
field_index(trade_capture_report, num_days_interest) -> 67;
field_index(trade_capture_report, ex_date) -> 68;
field_index(trade_capture_report, accrued_interest_rate) -> 69;
field_index(trade_capture_report, accrued_interest_amt) -> 70;
field_index(trade_capture_report, interest_at_maturity) -> 71;
field_index(trade_capture_report, end_accrued_interest_amt) -> 72;
field_index(trade_capture_report, start_cash) -> 73;
field_index(trade_capture_report, end_cash) -> 74;
field_index(trade_capture_report, concession) -> 75;
field_index(trade_capture_report, total_takedown) -> 76;
field_index(trade_capture_report, net_money) -> 77;
field_index(trade_capture_report, settl_curr_amt) -> 78;
field_index(trade_capture_report, settl_currency) -> 79;
field_index(trade_capture_report, settl_curr_fx_rate) -> 80;
field_index(trade_capture_report, settl_curr_fx_rate_calc) -> 81;
field_index(trade_capture_report, position_effect) -> 82;
field_index(trade_capture_report, text) -> 83;
field_index(trade_capture_report, encoded_text) -> 84;
field_index(trade_capture_report, side_multi_leg_reporting_type) -> 85;
field_index(trade_capture_report, exchange_rule) -> 86;
field_index(trade_capture_report, trade_alloc_indicator) -> 87;
field_index(trade_capture_report, prealloc_method) -> 88;
field_index(trade_capture_report, alloc_id) -> 89;
field_index(trade_capture_report, copy_msg_indicator) -> 90;
field_index(trade_capture_report, publish_trd_indicator) -> 91;
field_index(trade_capture_report, short_sale_reason) -> 92;
field_index(trade_capture_report, signature) -> 93;
field_index(order_mass_status_request, sender_comp_id) -> 2;
field_index(order_mass_status_request, target_comp_id) -> 3;
field_index(order_mass_status_request, msg_seq_num) -> 4;
field_index(order_mass_status_request, sending_time) -> 5;
field_index(order_mass_status_request, mass_status_req_id) -> 6;
field_index(order_mass_status_request, mass_status_req_type) -> 7;
field_index(order_mass_status_request, account) -> 8;
field_index(order_mass_status_request, acct_id_source) -> 9;
field_index(order_mass_status_request, trading_session_id) -> 10;
field_index(order_mass_status_request, trading_session_sub_id) -> 11;
field_index(order_mass_status_request, side) -> 12;
field_index(order_mass_status_request, signature) -> 13;
field_index(quote_request_reject, sender_comp_id) -> 2;
field_index(quote_request_reject, target_comp_id) -> 3;
field_index(quote_request_reject, msg_seq_num) -> 4;
field_index(quote_request_reject, sending_time) -> 5;
field_index(quote_request_reject, quote_req_id) -> 6;
field_index(quote_request_reject, rfq_req_id) -> 7;
field_index(quote_request_reject, quote_request_reject_reason) -> 8;
field_index(quote_request_reject, prev_close_px) -> 9;
field_index(quote_request_reject, quote_request_type) -> 10;
field_index(quote_request_reject, quote_type) -> 11;
field_index(quote_request_reject, trading_session_id) -> 12;
field_index(quote_request_reject, trading_session_sub_id) -> 13;
field_index(quote_request_reject, trade_origination_date) -> 14;
field_index(quote_request_reject, side) -> 15;
field_index(quote_request_reject, qty_type) -> 16;
field_index(quote_request_reject, settl_type) -> 17;
field_index(quote_request_reject, settl_date) -> 18;
field_index(quote_request_reject, settl_date2) -> 19;
field_index(quote_request_reject, order_qty2) -> 20;
field_index(quote_request_reject, currency) -> 21;
field_index(quote_request_reject, account) -> 22;
field_index(quote_request_reject, acct_id_source) -> 23;
field_index(quote_request_reject, account_type) -> 24;
field_index(quote_request_reject, quote_price_type) -> 25;
field_index(quote_request_reject, ord_type) -> 26;
field_index(quote_request_reject, expire_time) -> 27;
field_index(quote_request_reject, transact_time) -> 28;
field_index(quote_request_reject, price_type) -> 29;
field_index(quote_request_reject, price) -> 30;
field_index(quote_request_reject, price2) -> 31;
field_index(quote_request_reject, text) -> 32;
field_index(quote_request_reject, encoded_text) -> 33;
field_index(quote_request_reject, signature) -> 34;
field_index(rfq_request, sender_comp_id) -> 2;
field_index(rfq_request, target_comp_id) -> 3;
field_index(rfq_request, msg_seq_num) -> 4;
field_index(rfq_request, sending_time) -> 5;
field_index(rfq_request, rfq_req_id) -> 6;
field_index(rfq_request, prev_close_px) -> 7;
field_index(rfq_request, quote_request_type) -> 8;
field_index(rfq_request, quote_type) -> 9;
field_index(rfq_request, trading_session_id) -> 10;
field_index(rfq_request, trading_session_sub_id) -> 11;
field_index(rfq_request, subscription_request_type) -> 12;
field_index(rfq_request, signature) -> 13;
field_index(quote_status_report, sender_comp_id) -> 2;
field_index(quote_status_report, target_comp_id) -> 3;
field_index(quote_status_report, msg_seq_num) -> 4;
field_index(quote_status_report, sending_time) -> 5;
field_index(quote_status_report, quote_status_req_id) -> 6;
field_index(quote_status_report, quote_req_id) -> 7;
field_index(quote_status_report, quote_id) -> 8;
field_index(quote_status_report, quote_resp_id) -> 9;
field_index(quote_status_report, quote_type) -> 10;
field_index(quote_status_report, trading_session_id) -> 11;
field_index(quote_status_report, trading_session_sub_id) -> 12;
field_index(quote_status_report, side) -> 13;
field_index(quote_status_report, settl_type) -> 14;
field_index(quote_status_report, settl_date) -> 15;
field_index(quote_status_report, settl_date2) -> 16;
field_index(quote_status_report, order_qty2) -> 17;
field_index(quote_status_report, currency) -> 18;
field_index(quote_status_report, account) -> 19;
field_index(quote_status_report, acct_id_source) -> 20;
field_index(quote_status_report, account_type) -> 21;
field_index(quote_status_report, expire_time) -> 22;
field_index(quote_status_report, price) -> 23;
field_index(quote_status_report, price_type) -> 24;
field_index(quote_status_report, bid_px) -> 25;
field_index(quote_status_report, offer_px) -> 26;
field_index(quote_status_report, mkt_bid_px) -> 27;
field_index(quote_status_report, mkt_offer_px) -> 28;
field_index(quote_status_report, min_bid_size) -> 29;
field_index(quote_status_report, bid_size) -> 30;
field_index(quote_status_report, min_offer_size) -> 31;
field_index(quote_status_report, offer_size) -> 32;
field_index(quote_status_report, valid_until_time) -> 33;
field_index(quote_status_report, bid_spot_rate) -> 34;
field_index(quote_status_report, offer_spot_rate) -> 35;
field_index(quote_status_report, bid_forward_points) -> 36;
field_index(quote_status_report, offer_forward_points) -> 37;
field_index(quote_status_report, mid_px) -> 38;
field_index(quote_status_report, bid_yield) -> 39;
field_index(quote_status_report, mid_yield) -> 40;
field_index(quote_status_report, offer_yield) -> 41;
field_index(quote_status_report, transact_time) -> 42;
field_index(quote_status_report, ord_type) -> 43;
field_index(quote_status_report, bid_forward_points2) -> 44;
field_index(quote_status_report, offer_forward_points2) -> 45;
field_index(quote_status_report, settl_curr_bid_fx_rate) -> 46;
field_index(quote_status_report, settl_curr_offer_fx_rate) -> 47;
field_index(quote_status_report, settl_curr_fx_rate_calc) -> 48;
field_index(quote_status_report, comm_type) -> 49;
field_index(quote_status_report, commission) -> 50;
field_index(quote_status_report, cust_order_capacity) -> 51;
field_index(quote_status_report, ex_destination) -> 52;
field_index(quote_status_report, quote_status) -> 53;
field_index(quote_status_report, text) -> 54;
field_index(quote_status_report, encoded_text) -> 55;
field_index(quote_status_report, signature) -> 56;
field_index(quote_response, sender_comp_id) -> 2;
field_index(quote_response, target_comp_id) -> 3;
field_index(quote_response, msg_seq_num) -> 4;
field_index(quote_response, sending_time) -> 5;
field_index(quote_response, quote_resp_id) -> 6;
field_index(quote_response, quote_id) -> 7;
field_index(quote_response, quote_resp_type) -> 8;
field_index(quote_response, cl_ord_id) -> 9;
field_index(quote_response, order_capacity) -> 10;
field_index(quote_response, ioi_id) -> 11;
field_index(quote_response, quote_type) -> 12;
field_index(quote_response, trading_session_id) -> 13;
field_index(quote_response, trading_session_sub_id) -> 14;
field_index(quote_response, side) -> 15;
field_index(quote_response, settl_type) -> 16;
field_index(quote_response, settl_date) -> 17;
field_index(quote_response, settl_date2) -> 18;
field_index(quote_response, order_qty2) -> 19;
field_index(quote_response, currency) -> 20;
field_index(quote_response, account) -> 21;
field_index(quote_response, acct_id_source) -> 22;
field_index(quote_response, account_type) -> 23;
field_index(quote_response, bid_px) -> 24;
field_index(quote_response, offer_px) -> 25;
field_index(quote_response, mkt_bid_px) -> 26;
field_index(quote_response, mkt_offer_px) -> 27;
field_index(quote_response, min_bid_size) -> 28;
field_index(quote_response, bid_size) -> 29;
field_index(quote_response, min_offer_size) -> 30;
field_index(quote_response, offer_size) -> 31;
field_index(quote_response, valid_until_time) -> 32;
field_index(quote_response, bid_spot_rate) -> 33;
field_index(quote_response, offer_spot_rate) -> 34;
field_index(quote_response, bid_forward_points) -> 35;
field_index(quote_response, offer_forward_points) -> 36;
field_index(quote_response, mid_px) -> 37;
field_index(quote_response, bid_yield) -> 38;
field_index(quote_response, mid_yield) -> 39;
field_index(quote_response, offer_yield) -> 40;
field_index(quote_response, transact_time) -> 41;
field_index(quote_response, ord_type) -> 42;
field_index(quote_response, bid_forward_points2) -> 43;
field_index(quote_response, offer_forward_points2) -> 44;
field_index(quote_response, settl_curr_bid_fx_rate) -> 45;
field_index(quote_response, settl_curr_offer_fx_rate) -> 46;
field_index(quote_response, settl_curr_fx_rate_calc) -> 47;
field_index(quote_response, commission) -> 48;
field_index(quote_response, comm_type) -> 49;
field_index(quote_response, cust_order_capacity) -> 50;
field_index(quote_response, ex_destination) -> 51;
field_index(quote_response, text) -> 52;
field_index(quote_response, encoded_text) -> 53;
field_index(quote_response, price) -> 54;
field_index(quote_response, price_type) -> 55;
field_index(quote_response, signature) -> 56;
field_index(confirmation, sender_comp_id) -> 2;
field_index(confirmation, target_comp_id) -> 3;
field_index(confirmation, msg_seq_num) -> 4;
field_index(confirmation, sending_time) -> 5;
field_index(confirmation, confirm_id) -> 6;
field_index(confirmation, confirm_ref_id) -> 7;
field_index(confirmation, confirm_req_id) -> 8;
field_index(confirmation, confirm_trans_type) -> 9;
field_index(confirmation, confirm_type) -> 10;
field_index(confirmation, copy_msg_indicator) -> 11;
field_index(confirmation, legal_confirm) -> 12;
field_index(confirmation, confirm_status) -> 13;
field_index(confirmation, alloc_id) -> 14;
field_index(confirmation, secondary_alloc_id) -> 15;
field_index(confirmation, individual_alloc_id) -> 16;
field_index(confirmation, transact_time) -> 17;
field_index(confirmation, trade_date) -> 18;
field_index(confirmation, alloc_qty) -> 19;
field_index(confirmation, qty_type) -> 20;
field_index(confirmation, side) -> 21;
field_index(confirmation, currency) -> 22;
field_index(confirmation, last_mkt) -> 23;
field_index(confirmation, alloc_account) -> 24;
field_index(confirmation, alloc_acct_id_source) -> 25;
field_index(confirmation, alloc_account_type) -> 26;
field_index(confirmation, avg_px) -> 27;
field_index(confirmation, avg_px_precision) -> 28;
field_index(confirmation, price_type) -> 29;
field_index(confirmation, avg_par_px) -> 30;
field_index(confirmation, reported_px) -> 31;
field_index(confirmation, text) -> 32;
field_index(confirmation, encoded_text) -> 33;
field_index(confirmation, process_code) -> 34;
field_index(confirmation, gross_trade_amt) -> 35;
field_index(confirmation, num_days_interest) -> 36;
field_index(confirmation, ex_date) -> 37;
field_index(confirmation, accrued_interest_rate) -> 38;
field_index(confirmation, accrued_interest_amt) -> 39;
field_index(confirmation, interest_at_maturity) -> 40;
field_index(confirmation, end_accrued_interest_amt) -> 41;
field_index(confirmation, start_cash) -> 42;
field_index(confirmation, end_cash) -> 43;
field_index(confirmation, concession) -> 44;
field_index(confirmation, total_takedown) -> 45;
field_index(confirmation, net_money) -> 46;
field_index(confirmation, maturity_net_money) -> 47;
field_index(confirmation, settl_curr_amt) -> 48;
field_index(confirmation, settl_currency) -> 49;
field_index(confirmation, settl_curr_fx_rate) -> 50;
field_index(confirmation, settl_curr_fx_rate_calc) -> 51;
field_index(confirmation, settl_type) -> 52;
field_index(confirmation, settl_date) -> 53;
field_index(confirmation, shared_commission) -> 54;
field_index(confirmation, signature) -> 55;
field_index(position_maintenance_request, sender_comp_id) -> 2;
field_index(position_maintenance_request, target_comp_id) -> 3;
field_index(position_maintenance_request, msg_seq_num) -> 4;
field_index(position_maintenance_request, sending_time) -> 5;
field_index(position_maintenance_request, pos_req_id) -> 6;
field_index(position_maintenance_request, pos_trans_type) -> 7;
field_index(position_maintenance_request, pos_maint_action) -> 8;
field_index(position_maintenance_request, orig_pos_req_ref_id) -> 9;
field_index(position_maintenance_request, pos_maint_rpt_ref_id) -> 10;
field_index(position_maintenance_request, clearing_business_date) -> 11;
field_index(position_maintenance_request, settl_sess_id) -> 12;
field_index(position_maintenance_request, settl_sess_sub_id) -> 13;
field_index(position_maintenance_request, account) -> 14;
field_index(position_maintenance_request, acct_id_source) -> 15;
field_index(position_maintenance_request, account_type) -> 16;
field_index(position_maintenance_request, currency) -> 17;
field_index(position_maintenance_request, transact_time) -> 18;
field_index(position_maintenance_request, adjustment_type) -> 19;
field_index(position_maintenance_request, contrary_instruction_indicator) -> 20;
field_index(position_maintenance_request, prior_spread_indicator) -> 21;
field_index(position_maintenance_request, threshold_amount) -> 22;
field_index(position_maintenance_request, text) -> 23;
field_index(position_maintenance_request, encoded_text) -> 24;
field_index(position_maintenance_request, signature) -> 25;
field_index(position_maintenance_report, sender_comp_id) -> 2;
field_index(position_maintenance_report, target_comp_id) -> 3;
field_index(position_maintenance_report, msg_seq_num) -> 4;
field_index(position_maintenance_report, sending_time) -> 5;
field_index(position_maintenance_report, pos_maint_rpt_id) -> 6;
field_index(position_maintenance_report, pos_trans_type) -> 7;
field_index(position_maintenance_report, pos_req_id) -> 8;
field_index(position_maintenance_report, pos_maint_action) -> 9;
field_index(position_maintenance_report, orig_pos_req_ref_id) -> 10;
field_index(position_maintenance_report, pos_maint_status) -> 11;
field_index(position_maintenance_report, pos_maint_result) -> 12;
field_index(position_maintenance_report, clearing_business_date) -> 13;
field_index(position_maintenance_report, settl_sess_id) -> 14;
field_index(position_maintenance_report, settl_sess_sub_id) -> 15;
field_index(position_maintenance_report, account) -> 16;
field_index(position_maintenance_report, acct_id_source) -> 17;
field_index(position_maintenance_report, account_type) -> 18;
field_index(position_maintenance_report, currency) -> 19;
field_index(position_maintenance_report, transact_time) -> 20;
field_index(position_maintenance_report, adjustment_type) -> 21;
field_index(position_maintenance_report, threshold_amount) -> 22;
field_index(position_maintenance_report, text) -> 23;
field_index(position_maintenance_report, encoded_text) -> 24;
field_index(position_maintenance_report, signature) -> 25;
field_index(request_for_positions, sender_comp_id) -> 2;
field_index(request_for_positions, target_comp_id) -> 3;
field_index(request_for_positions, msg_seq_num) -> 4;
field_index(request_for_positions, sending_time) -> 5;
field_index(request_for_positions, pos_req_id) -> 6;
field_index(request_for_positions, pos_req_type) -> 7;
field_index(request_for_positions, match_status) -> 8;
field_index(request_for_positions, subscription_request_type) -> 9;
field_index(request_for_positions, account) -> 10;
field_index(request_for_positions, acct_id_source) -> 11;
field_index(request_for_positions, account_type) -> 12;
field_index(request_for_positions, currency) -> 13;
field_index(request_for_positions, clearing_business_date) -> 14;
field_index(request_for_positions, settl_sess_id) -> 15;
field_index(request_for_positions, settl_sess_sub_id) -> 16;
field_index(request_for_positions, transact_time) -> 17;
field_index(request_for_positions, response_transport_type) -> 18;
field_index(request_for_positions, response_destination) -> 19;
field_index(request_for_positions, text) -> 20;
field_index(request_for_positions, encoded_text) -> 21;
field_index(request_for_positions, signature) -> 22;
field_index(request_for_positions_ack, sender_comp_id) -> 2;
field_index(request_for_positions_ack, target_comp_id) -> 3;
field_index(request_for_positions_ack, msg_seq_num) -> 4;
field_index(request_for_positions_ack, sending_time) -> 5;
field_index(request_for_positions_ack, pos_maint_rpt_id) -> 6;
field_index(request_for_positions_ack, pos_req_id) -> 7;
field_index(request_for_positions_ack, total_num_pos_reports) -> 8;
field_index(request_for_positions_ack, unsolicited_indicator) -> 9;
field_index(request_for_positions_ack, pos_req_result) -> 10;
field_index(request_for_positions_ack, pos_req_status) -> 11;
field_index(request_for_positions_ack, account) -> 12;
field_index(request_for_positions_ack, acct_id_source) -> 13;
field_index(request_for_positions_ack, account_type) -> 14;
field_index(request_for_positions_ack, currency) -> 15;
field_index(request_for_positions_ack, response_transport_type) -> 16;
field_index(request_for_positions_ack, response_destination) -> 17;
field_index(request_for_positions_ack, text) -> 18;
field_index(request_for_positions_ack, encoded_text) -> 19;
field_index(request_for_positions_ack, signature) -> 20;
field_index(position_report, sender_comp_id) -> 2;
field_index(position_report, target_comp_id) -> 3;
field_index(position_report, msg_seq_num) -> 4;
field_index(position_report, sending_time) -> 5;
field_index(position_report, pos_maint_rpt_id) -> 6;
field_index(position_report, pos_req_id) -> 7;
field_index(position_report, pos_req_type) -> 8;
field_index(position_report, subscription_request_type) -> 9;
field_index(position_report, total_num_pos_reports) -> 10;
field_index(position_report, unsolicited_indicator) -> 11;
field_index(position_report, pos_req_result) -> 12;
field_index(position_report, clearing_business_date) -> 13;
field_index(position_report, settl_sess_id) -> 14;
field_index(position_report, settl_sess_sub_id) -> 15;
field_index(position_report, account) -> 16;
field_index(position_report, acct_id_source) -> 17;
field_index(position_report, account_type) -> 18;
field_index(position_report, currency) -> 19;
field_index(position_report, settl_price) -> 20;
field_index(position_report, settl_price_type) -> 21;
field_index(position_report, prior_settl_price) -> 22;
field_index(position_report, regist_status) -> 23;
field_index(position_report, delivery_date) -> 24;
field_index(position_report, text) -> 25;
field_index(position_report, encoded_text) -> 26;
field_index(position_report, signature) -> 27;
field_index(trade_capture_report_request_ack, sender_comp_id) -> 2;
field_index(trade_capture_report_request_ack, target_comp_id) -> 3;
field_index(trade_capture_report_request_ack, msg_seq_num) -> 4;
field_index(trade_capture_report_request_ack, sending_time) -> 5;
field_index(trade_capture_report_request_ack, trade_request_id) -> 6;
field_index(trade_capture_report_request_ack, trade_request_type) -> 7;
field_index(trade_capture_report_request_ack, subscription_request_type) -> 8;
field_index(trade_capture_report_request_ack, tot_num_trade_reports) -> 9;
field_index(trade_capture_report_request_ack, trade_request_result) -> 10;
field_index(trade_capture_report_request_ack, trade_request_status) -> 11;
field_index(trade_capture_report_request_ack, multi_leg_reporting_type) -> 12;
field_index(trade_capture_report_request_ack, response_transport_type) -> 13;
field_index(trade_capture_report_request_ack, response_destination) -> 14;
field_index(trade_capture_report_request_ack, text) -> 15;
field_index(trade_capture_report_request_ack, encoded_text) -> 16;
field_index(trade_capture_report_request_ack, signature) -> 17;
field_index(trade_capture_report_ack, sender_comp_id) -> 2;
field_index(trade_capture_report_ack, target_comp_id) -> 3;
field_index(trade_capture_report_ack, msg_seq_num) -> 4;
field_index(trade_capture_report_ack, sending_time) -> 5;
field_index(trade_capture_report_ack, trade_report_id) -> 6;
field_index(trade_capture_report_ack, trade_report_trans_type) -> 7;
field_index(trade_capture_report_ack, trade_report_type) -> 8;
field_index(trade_capture_report_ack, trd_type) -> 9;
field_index(trade_capture_report_ack, trd_sub_type) -> 10;
field_index(trade_capture_report_ack, secondary_trd_type) -> 11;
field_index(trade_capture_report_ack, transfer_reason) -> 12;
field_index(trade_capture_report_ack, exec_type) -> 13;
field_index(trade_capture_report_ack, trade_report_ref_id) -> 14;
field_index(trade_capture_report_ack, secondary_trade_report_ref_id) -> 15;
field_index(trade_capture_report_ack, trd_rpt_status) -> 16;
field_index(trade_capture_report_ack, trade_report_reject_reason) -> 17;
field_index(trade_capture_report_ack, secondary_trade_report_id) -> 18;
field_index(trade_capture_report_ack, subscription_request_type) -> 19;
field_index(trade_capture_report_ack, trade_link_id) -> 20;
field_index(trade_capture_report_ack, trd_match_id) -> 21;
field_index(trade_capture_report_ack, exec_id) -> 22;
field_index(trade_capture_report_ack, secondary_exec_id) -> 23;
field_index(trade_capture_report_ack, transact_time) -> 24;
field_index(trade_capture_report_ack, response_transport_type) -> 25;
field_index(trade_capture_report_ack, response_destination) -> 26;
field_index(trade_capture_report_ack, text) -> 27;
field_index(trade_capture_report_ack, encoded_text) -> 28;
field_index(trade_capture_report_ack, clearing_fee_indicator) -> 29;
field_index(trade_capture_report_ack, order_capacity) -> 30;
field_index(trade_capture_report_ack, order_restrictions) -> 31;
field_index(trade_capture_report_ack, cust_order_capacity) -> 32;
field_index(trade_capture_report_ack, account) -> 33;
field_index(trade_capture_report_ack, acct_id_source) -> 34;
field_index(trade_capture_report_ack, account_type) -> 35;
field_index(trade_capture_report_ack, position_effect) -> 36;
field_index(trade_capture_report_ack, prealloc_method) -> 37;
field_index(trade_capture_report_ack, signature) -> 38;
field_index(allocation_report, sender_comp_id) -> 2;
field_index(allocation_report, target_comp_id) -> 3;
field_index(allocation_report, msg_seq_num) -> 4;
field_index(allocation_report, sending_time) -> 5;
field_index(allocation_report, alloc_report_id) -> 6;
field_index(allocation_report, alloc_id) -> 7;
field_index(allocation_report, alloc_trans_type) -> 8;
field_index(allocation_report, alloc_report_ref_id) -> 9;
field_index(allocation_report, alloc_canc_replace_reason) -> 10;
field_index(allocation_report, secondary_alloc_id) -> 11;
field_index(allocation_report, alloc_report_type) -> 12;
field_index(allocation_report, alloc_status) -> 13;
field_index(allocation_report, alloc_rej_code) -> 14;
field_index(allocation_report, ref_alloc_id) -> 15;
field_index(allocation_report, alloc_intermed_req_type) -> 16;
field_index(allocation_report, alloc_link_id) -> 17;
field_index(allocation_report, alloc_link_type) -> 18;
field_index(allocation_report, booking_ref_id) -> 19;
field_index(allocation_report, alloc_no_orders_type) -> 20;
field_index(allocation_report, previously_reported) -> 21;
field_index(allocation_report, reversal_indicator) -> 22;
field_index(allocation_report, match_type) -> 23;
field_index(allocation_report, side) -> 24;
field_index(allocation_report, quantity) -> 25;
field_index(allocation_report, qty_type) -> 26;
field_index(allocation_report, last_mkt) -> 27;
field_index(allocation_report, trade_origination_date) -> 28;
field_index(allocation_report, trading_session_id) -> 29;
field_index(allocation_report, trading_session_sub_id) -> 30;
field_index(allocation_report, price_type) -> 31;
field_index(allocation_report, avg_px) -> 32;
field_index(allocation_report, avg_par_px) -> 33;
field_index(allocation_report, currency) -> 34;
field_index(allocation_report, avg_px_precision) -> 35;
field_index(allocation_report, trade_date) -> 36;
field_index(allocation_report, transact_time) -> 37;
field_index(allocation_report, settl_type) -> 38;
field_index(allocation_report, settl_date) -> 39;
field_index(allocation_report, booking_type) -> 40;
field_index(allocation_report, gross_trade_amt) -> 41;
field_index(allocation_report, concession) -> 42;
field_index(allocation_report, total_takedown) -> 43;
field_index(allocation_report, net_money) -> 44;
field_index(allocation_report, position_effect) -> 45;
field_index(allocation_report, auto_accept_indicator) -> 46;
field_index(allocation_report, text) -> 47;
field_index(allocation_report, encoded_text) -> 48;
field_index(allocation_report, num_days_interest) -> 49;
field_index(allocation_report, accrued_interest_rate) -> 50;
field_index(allocation_report, accrued_interest_amt) -> 51;
field_index(allocation_report, total_accrued_interest_amt) -> 52;
field_index(allocation_report, interest_at_maturity) -> 53;
field_index(allocation_report, end_accrued_interest_amt) -> 54;
field_index(allocation_report, start_cash) -> 55;
field_index(allocation_report, end_cash) -> 56;
field_index(allocation_report, legal_confirm) -> 57;
field_index(allocation_report, tot_no_allocs) -> 58;
field_index(allocation_report, last_fragment) -> 59;
field_index(allocation_report, clearing_fee_indicator) -> 60;
field_index(allocation_report, alloc_settl_inst_type) -> 61;
field_index(allocation_report, signature) -> 62;
field_index(allocation_report_ack, sender_comp_id) -> 2;
field_index(allocation_report_ack, target_comp_id) -> 3;
field_index(allocation_report_ack, msg_seq_num) -> 4;
field_index(allocation_report_ack, sending_time) -> 5;
field_index(allocation_report_ack, alloc_report_id) -> 6;
field_index(allocation_report_ack, alloc_id) -> 7;
field_index(allocation_report_ack, secondary_alloc_id) -> 8;
field_index(allocation_report_ack, trade_date) -> 9;
field_index(allocation_report_ack, transact_time) -> 10;
field_index(allocation_report_ack, alloc_status) -> 11;
field_index(allocation_report_ack, alloc_rej_code) -> 12;
field_index(allocation_report_ack, alloc_report_type) -> 13;
field_index(allocation_report_ack, alloc_intermed_req_type) -> 14;
field_index(allocation_report_ack, match_status) -> 15;
field_index(allocation_report_ack, product) -> 16;
field_index(allocation_report_ack, security_type) -> 17;
field_index(allocation_report_ack, text) -> 18;
field_index(allocation_report_ack, encoded_text) -> 19;
field_index(allocation_report_ack, signature) -> 20;
field_index(confirmation_ack, sender_comp_id) -> 2;
field_index(confirmation_ack, target_comp_id) -> 3;
field_index(confirmation_ack, msg_seq_num) -> 4;
field_index(confirmation_ack, sending_time) -> 5;
field_index(confirmation_ack, confirm_id) -> 6;
field_index(confirmation_ack, trade_date) -> 7;
field_index(confirmation_ack, transact_time) -> 8;
field_index(confirmation_ack, affirm_status) -> 9;
field_index(confirmation_ack, confirm_rej_reason) -> 10;
field_index(confirmation_ack, match_status) -> 11;
field_index(confirmation_ack, text) -> 12;
field_index(confirmation_ack, encoded_text) -> 13;
field_index(confirmation_ack, signature) -> 14;
field_index(settlement_instruction_request, sender_comp_id) -> 2;
field_index(settlement_instruction_request, target_comp_id) -> 3;
field_index(settlement_instruction_request, msg_seq_num) -> 4;
field_index(settlement_instruction_request, sending_time) -> 5;
field_index(settlement_instruction_request, settl_inst_req_id) -> 6;
field_index(settlement_instruction_request, transact_time) -> 7;
field_index(settlement_instruction_request, alloc_account) -> 8;
field_index(settlement_instruction_request, alloc_acct_id_source) -> 9;
field_index(settlement_instruction_request, side) -> 10;
field_index(settlement_instruction_request, product) -> 11;
field_index(settlement_instruction_request, security_type) -> 12;
field_index(settlement_instruction_request, cfi_code) -> 13;
field_index(settlement_instruction_request, effective_time) -> 14;
field_index(settlement_instruction_request, expire_time) -> 15;
field_index(settlement_instruction_request, last_update_time) -> 16;
field_index(settlement_instruction_request, stand_inst_db_type) -> 17;
field_index(settlement_instruction_request, stand_inst_db_name) -> 18;
field_index(settlement_instruction_request, stand_inst_db_id) -> 19;
field_index(settlement_instruction_request, signature) -> 20;
field_index(assignment_report, sender_comp_id) -> 2;
field_index(assignment_report, target_comp_id) -> 3;
field_index(assignment_report, msg_seq_num) -> 4;
field_index(assignment_report, sending_time) -> 5;
field_index(assignment_report, asgn_rpt_id) -> 6;
field_index(assignment_report, tot_num_assignment_reports) -> 7;
field_index(assignment_report, last_rpt_requested) -> 8;
field_index(assignment_report, account) -> 9;
field_index(assignment_report, account_type) -> 10;
field_index(assignment_report, currency) -> 11;
field_index(assignment_report, threshold_amount) -> 12;
field_index(assignment_report, settl_price) -> 13;
field_index(assignment_report, settl_price_type) -> 14;
field_index(assignment_report, underlying_settl_price) -> 15;
field_index(assignment_report, expire_date) -> 16;
field_index(assignment_report, assignment_method) -> 17;
field_index(assignment_report, assignment_unit) -> 18;
field_index(assignment_report, open_interest) -> 19;
field_index(assignment_report, exercise_method) -> 20;
field_index(assignment_report, settl_sess_id) -> 21;
field_index(assignment_report, settl_sess_sub_id) -> 22;
field_index(assignment_report, clearing_business_date) -> 23;
field_index(assignment_report, text) -> 24;
field_index(assignment_report, encoded_text) -> 25;
field_index(assignment_report, signature) -> 26;
field_index(collateral_request, sender_comp_id) -> 2;
field_index(collateral_request, target_comp_id) -> 3;
field_index(collateral_request, msg_seq_num) -> 4;
field_index(collateral_request, sending_time) -> 5;
field_index(collateral_request, coll_req_id) -> 6;
field_index(collateral_request, coll_asgn_reason) -> 7;
field_index(collateral_request, transact_time) -> 8;
field_index(collateral_request, expire_time) -> 9;
field_index(collateral_request, account) -> 10;
field_index(collateral_request, account_type) -> 11;
field_index(collateral_request, cl_ord_id) -> 12;
field_index(collateral_request, order_id) -> 13;
field_index(collateral_request, secondary_order_id) -> 14;
field_index(collateral_request, secondary_cl_ord_id) -> 15;
field_index(collateral_request, settl_date) -> 16;
field_index(collateral_request, quantity) -> 17;
field_index(collateral_request, qty_type) -> 18;
field_index(collateral_request, currency) -> 19;
field_index(collateral_request, margin_excess) -> 20;
field_index(collateral_request, total_net_value) -> 21;
field_index(collateral_request, cash_outstanding) -> 22;
field_index(collateral_request, side) -> 23;
field_index(collateral_request, price) -> 24;
field_index(collateral_request, price_type) -> 25;
field_index(collateral_request, accrued_interest_amt) -> 26;
field_index(collateral_request, end_accrued_interest_amt) -> 27;
field_index(collateral_request, start_cash) -> 28;
field_index(collateral_request, end_cash) -> 29;
field_index(collateral_request, trading_session_id) -> 30;
field_index(collateral_request, trading_session_sub_id) -> 31;
field_index(collateral_request, settl_sess_id) -> 32;
field_index(collateral_request, settl_sess_sub_id) -> 33;
field_index(collateral_request, clearing_business_date) -> 34;
field_index(collateral_request, text) -> 35;
field_index(collateral_request, encoded_text) -> 36;
field_index(collateral_request, signature) -> 37;
field_index(collateral_assignment, sender_comp_id) -> 2;
field_index(collateral_assignment, target_comp_id) -> 3;
field_index(collateral_assignment, msg_seq_num) -> 4;
field_index(collateral_assignment, sending_time) -> 5;
field_index(collateral_assignment, coll_asgn_id) -> 6;
field_index(collateral_assignment, coll_req_id) -> 7;
field_index(collateral_assignment, coll_asgn_reason) -> 8;
field_index(collateral_assignment, coll_asgn_trans_type) -> 9;
field_index(collateral_assignment, coll_asgn_ref_id) -> 10;
field_index(collateral_assignment, transact_time) -> 11;
field_index(collateral_assignment, expire_time) -> 12;
field_index(collateral_assignment, account) -> 13;
field_index(collateral_assignment, account_type) -> 14;
field_index(collateral_assignment, cl_ord_id) -> 15;
field_index(collateral_assignment, order_id) -> 16;
field_index(collateral_assignment, secondary_order_id) -> 17;
field_index(collateral_assignment, secondary_cl_ord_id) -> 18;
field_index(collateral_assignment, settl_date) -> 19;
field_index(collateral_assignment, quantity) -> 20;
field_index(collateral_assignment, qty_type) -> 21;
field_index(collateral_assignment, currency) -> 22;
field_index(collateral_assignment, margin_excess) -> 23;
field_index(collateral_assignment, total_net_value) -> 24;
field_index(collateral_assignment, cash_outstanding) -> 25;
field_index(collateral_assignment, side) -> 26;
field_index(collateral_assignment, price) -> 27;
field_index(collateral_assignment, price_type) -> 28;
field_index(collateral_assignment, accrued_interest_amt) -> 29;
field_index(collateral_assignment, end_accrued_interest_amt) -> 30;
field_index(collateral_assignment, start_cash) -> 31;
field_index(collateral_assignment, end_cash) -> 32;
field_index(collateral_assignment, trading_session_id) -> 33;
field_index(collateral_assignment, trading_session_sub_id) -> 34;
field_index(collateral_assignment, settl_sess_id) -> 35;
field_index(collateral_assignment, settl_sess_sub_id) -> 36;
field_index(collateral_assignment, clearing_business_date) -> 37;
field_index(collateral_assignment, text) -> 38;
field_index(collateral_assignment, encoded_text) -> 39;
field_index(collateral_assignment, signature) -> 40;
field_index(collateral_response, sender_comp_id) -> 2;
field_index(collateral_response, target_comp_id) -> 3;
field_index(collateral_response, msg_seq_num) -> 4;
field_index(collateral_response, sending_time) -> 5;
field_index(collateral_response, coll_resp_id) -> 6;
field_index(collateral_response, coll_asgn_id) -> 7;
field_index(collateral_response, coll_req_id) -> 8;
field_index(collateral_response, coll_asgn_reason) -> 9;
field_index(collateral_response, coll_asgn_trans_type) -> 10;
field_index(collateral_response, coll_asgn_resp_type) -> 11;
field_index(collateral_response, coll_asgn_reject_reason) -> 12;
field_index(collateral_response, transact_time) -> 13;
field_index(collateral_response, account) -> 14;
field_index(collateral_response, account_type) -> 15;
field_index(collateral_response, cl_ord_id) -> 16;
field_index(collateral_response, order_id) -> 17;
field_index(collateral_response, secondary_order_id) -> 18;
field_index(collateral_response, secondary_cl_ord_id) -> 19;
field_index(collateral_response, settl_date) -> 20;
field_index(collateral_response, quantity) -> 21;
field_index(collateral_response, qty_type) -> 22;
field_index(collateral_response, currency) -> 23;
field_index(collateral_response, margin_excess) -> 24;
field_index(collateral_response, total_net_value) -> 25;
field_index(collateral_response, cash_outstanding) -> 26;
field_index(collateral_response, side) -> 27;
field_index(collateral_response, price) -> 28;
field_index(collateral_response, price_type) -> 29;
field_index(collateral_response, accrued_interest_amt) -> 30;
field_index(collateral_response, end_accrued_interest_amt) -> 31;
field_index(collateral_response, start_cash) -> 32;
field_index(collateral_response, end_cash) -> 33;
field_index(collateral_response, text) -> 34;
field_index(collateral_response, encoded_text) -> 35;
field_index(collateral_response, signature) -> 36;
field_index(collateral_report, sender_comp_id) -> 2;
field_index(collateral_report, target_comp_id) -> 3;
field_index(collateral_report, msg_seq_num) -> 4;
field_index(collateral_report, sending_time) -> 5;
field_index(collateral_report, coll_rpt_id) -> 6;
field_index(collateral_report, coll_inquiry_id) -> 7;
field_index(collateral_report, coll_status) -> 8;
field_index(collateral_report, tot_num_reports) -> 9;
field_index(collateral_report, last_rpt_requested) -> 10;
field_index(collateral_report, account) -> 11;
field_index(collateral_report, account_type) -> 12;
field_index(collateral_report, cl_ord_id) -> 13;
field_index(collateral_report, order_id) -> 14;
field_index(collateral_report, secondary_order_id) -> 15;
field_index(collateral_report, secondary_cl_ord_id) -> 16;
field_index(collateral_report, settl_date) -> 17;
field_index(collateral_report, quantity) -> 18;
field_index(collateral_report, qty_type) -> 19;
field_index(collateral_report, currency) -> 20;
field_index(collateral_report, margin_excess) -> 21;
field_index(collateral_report, total_net_value) -> 22;
field_index(collateral_report, cash_outstanding) -> 23;
field_index(collateral_report, side) -> 24;
field_index(collateral_report, price) -> 25;
field_index(collateral_report, price_type) -> 26;
field_index(collateral_report, accrued_interest_amt) -> 27;
field_index(collateral_report, end_accrued_interest_amt) -> 28;
field_index(collateral_report, start_cash) -> 29;
field_index(collateral_report, end_cash) -> 30;
field_index(collateral_report, trading_session_id) -> 31;
field_index(collateral_report, trading_session_sub_id) -> 32;
field_index(collateral_report, settl_sess_id) -> 33;
field_index(collateral_report, settl_sess_sub_id) -> 34;
field_index(collateral_report, clearing_business_date) -> 35;
field_index(collateral_report, text) -> 36;
field_index(collateral_report, encoded_text) -> 37;
field_index(collateral_report, signature) -> 38;
field_index(collateral_inquiry, sender_comp_id) -> 2;
field_index(collateral_inquiry, target_comp_id) -> 3;
field_index(collateral_inquiry, msg_seq_num) -> 4;
field_index(collateral_inquiry, sending_time) -> 5;
field_index(collateral_inquiry, coll_inquiry_id) -> 6;
field_index(collateral_inquiry, subscription_request_type) -> 7;
field_index(collateral_inquiry, response_transport_type) -> 8;
field_index(collateral_inquiry, response_destination) -> 9;
field_index(collateral_inquiry, account) -> 10;
field_index(collateral_inquiry, account_type) -> 11;
field_index(collateral_inquiry, cl_ord_id) -> 12;
field_index(collateral_inquiry, order_id) -> 13;
field_index(collateral_inquiry, secondary_order_id) -> 14;
field_index(collateral_inquiry, secondary_cl_ord_id) -> 15;
field_index(collateral_inquiry, settl_date) -> 16;
field_index(collateral_inquiry, quantity) -> 17;
field_index(collateral_inquiry, qty_type) -> 18;
field_index(collateral_inquiry, currency) -> 19;
field_index(collateral_inquiry, margin_excess) -> 20;
field_index(collateral_inquiry, total_net_value) -> 21;
field_index(collateral_inquiry, cash_outstanding) -> 22;
field_index(collateral_inquiry, side) -> 23;
field_index(collateral_inquiry, price) -> 24;
field_index(collateral_inquiry, price_type) -> 25;
field_index(collateral_inquiry, accrued_interest_amt) -> 26;
field_index(collateral_inquiry, end_accrued_interest_amt) -> 27;
field_index(collateral_inquiry, start_cash) -> 28;
field_index(collateral_inquiry, end_cash) -> 29;
field_index(collateral_inquiry, trading_session_id) -> 30;
field_index(collateral_inquiry, trading_session_sub_id) -> 31;
field_index(collateral_inquiry, settl_sess_id) -> 32;
field_index(collateral_inquiry, settl_sess_sub_id) -> 33;
field_index(collateral_inquiry, clearing_business_date) -> 34;
field_index(collateral_inquiry, text) -> 35;
field_index(collateral_inquiry, encoded_text) -> 36;
field_index(collateral_inquiry, signature) -> 37;
field_index(network_counterparty_system_status_request, sender_comp_id) -> 2;
field_index(network_counterparty_system_status_request, target_comp_id) -> 3;
field_index(network_counterparty_system_status_request, msg_seq_num) -> 4;
field_index(network_counterparty_system_status_request, sending_time) -> 5;
field_index(network_counterparty_system_status_request, network_request_type) -> 6;
field_index(network_counterparty_system_status_request, network_request_id) -> 7;
field_index(network_counterparty_system_status_request, signature) -> 8;
field_index(network_counterparty_system_status_response, sender_comp_id) -> 2;
field_index(network_counterparty_system_status_response, target_comp_id) -> 3;
field_index(network_counterparty_system_status_response, msg_seq_num) -> 4;
field_index(network_counterparty_system_status_response, sending_time) -> 5;
field_index(network_counterparty_system_status_response, network_status_response_type) -> 6;
field_index(network_counterparty_system_status_response, network_request_id) -> 7;
field_index(network_counterparty_system_status_response, network_response_id) -> 8;
field_index(network_counterparty_system_status_response, last_network_response_id) -> 9;
field_index(network_counterparty_system_status_response, signature) -> 10;
field_index(user_request, sender_comp_id) -> 2;
field_index(user_request, target_comp_id) -> 3;
field_index(user_request, msg_seq_num) -> 4;
field_index(user_request, sending_time) -> 5;
field_index(user_request, user_request_id) -> 6;
field_index(user_request, user_request_type) -> 7;
field_index(user_request, username) -> 8;
field_index(user_request, password) -> 9;
field_index(user_request, new_password) -> 10;
field_index(user_request, raw_data) -> 11;
field_index(user_request, signature) -> 12;
field_index(user_response, sender_comp_id) -> 2;
field_index(user_response, target_comp_id) -> 3;
field_index(user_response, msg_seq_num) -> 4;
field_index(user_response, sending_time) -> 5;
field_index(user_response, user_request_id) -> 6;
field_index(user_response, username) -> 7;
field_index(user_response, user_status) -> 8;
field_index(user_response, user_status_text) -> 9;
field_index(user_response, signature) -> 10;
field_index(collateral_inquiry_ack, sender_comp_id) -> 2;
field_index(collateral_inquiry_ack, target_comp_id) -> 3;
field_index(collateral_inquiry_ack, msg_seq_num) -> 4;
field_index(collateral_inquiry_ack, sending_time) -> 5;
field_index(collateral_inquiry_ack, coll_inquiry_id) -> 6;
field_index(collateral_inquiry_ack, coll_inquiry_status) -> 7;
field_index(collateral_inquiry_ack, coll_inquiry_result) -> 8;
field_index(collateral_inquiry_ack, tot_num_reports) -> 9;
field_index(collateral_inquiry_ack, account) -> 10;
field_index(collateral_inquiry_ack, account_type) -> 11;
field_index(collateral_inquiry_ack, cl_ord_id) -> 12;
field_index(collateral_inquiry_ack, order_id) -> 13;
field_index(collateral_inquiry_ack, secondary_order_id) -> 14;
field_index(collateral_inquiry_ack, secondary_cl_ord_id) -> 15;
field_index(collateral_inquiry_ack, settl_date) -> 16;
field_index(collateral_inquiry_ack, quantity) -> 17;
field_index(collateral_inquiry_ack, qty_type) -> 18;
field_index(collateral_inquiry_ack, currency) -> 19;
field_index(collateral_inquiry_ack, trading_session_id) -> 20;
field_index(collateral_inquiry_ack, trading_session_sub_id) -> 21;
field_index(collateral_inquiry_ack, settl_sess_id) -> 22;
field_index(collateral_inquiry_ack, settl_sess_sub_id) -> 23;
field_index(collateral_inquiry_ack, clearing_business_date) -> 24;
field_index(collateral_inquiry_ack, response_transport_type) -> 25;
field_index(collateral_inquiry_ack, response_destination) -> 26;
field_index(collateral_inquiry_ack, text) -> 27;
field_index(collateral_inquiry_ack, encoded_text) -> 28;
field_index(collateral_inquiry_ack, signature) -> 29;
field_index(confirmation_request, sender_comp_id) -> 2;
field_index(confirmation_request, target_comp_id) -> 3;
field_index(confirmation_request, msg_seq_num) -> 4;
field_index(confirmation_request, sending_time) -> 5;
field_index(confirmation_request, confirm_req_id) -> 6;
field_index(confirmation_request, confirm_type) -> 7;
field_index(confirmation_request, alloc_id) -> 8;
field_index(confirmation_request, secondary_alloc_id) -> 9;
field_index(confirmation_request, individual_alloc_id) -> 10;
field_index(confirmation_request, transact_time) -> 11;
field_index(confirmation_request, alloc_account) -> 12;
field_index(confirmation_request, alloc_acct_id_source) -> 13;
field_index(confirmation_request, alloc_account_type) -> 14;
field_index(confirmation_request, text) -> 15;
field_index(confirmation_request, encoded_text) -> 16;
field_index(confirmation_request, signature) -> 17;
field_index(_,_) -> undefined.

decode_fields(<<"1=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, account) of
    undefined -> erlang:setelement(Default, Record, [{account,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"2=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, adv_id) of
    undefined -> erlang:setelement(Default, Record, [{adv_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"3=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, adv_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{adv_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"4=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, adv_side) of
    undefined -> erlang:setelement(Default, Record, [{adv_side,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"5=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, adv_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{adv_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"6=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, avg_px) of
    undefined -> erlang:setelement(Default, Record, [{avg_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"7=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, begin_seq_no) of
    undefined -> erlang:setelement(Default, Record, [{begin_seq_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"8=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, begin_string) of
    undefined -> erlang:setelement(Default, Record, [{begin_string,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"10=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, check_sum) of
    undefined -> erlang:setelement(Default, Record, [{check_sum,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"11=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cl_ord_id) of
    undefined -> erlang:setelement(Default, Record, [{cl_ord_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"12=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, commission) of
    undefined -> erlang:setelement(Default, Record, [{commission,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"13=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, comm_type) of
    undefined -> erlang:setelement(Default, Record, [{comm_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"14=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cum_qty) of
    undefined -> erlang:setelement(Default, Record, [{cum_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"15=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, currency) of
    undefined -> erlang:setelement(Default, Record, [{currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"16=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, end_seq_no) of
    undefined -> erlang:setelement(Default, Record, [{end_seq_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"17=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_id) of
    undefined -> erlang:setelement(Default, Record, [{exec_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"18=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_inst) of
    undefined -> erlang:setelement(Default, Record, [{exec_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"19=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{exec_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"20=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{exec_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"21=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, handl_inst) of
    undefined -> erlang:setelement(Default, Record, [{handl_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"22=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_id_source) of
    undefined -> erlang:setelement(Default, Record, [{security_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"23=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_id) of
    undefined -> erlang:setelement(Default, Record, [{ioi_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"24=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_oth_svc) of
    undefined -> erlang:setelement(Default, Record, [{ioi_oth_svc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"25=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_qlty_ind) of
    undefined -> erlang:setelement(Default, Record, [{ioi_qlty_ind,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"26=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{ioi_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"27=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_qty) of
    undefined -> erlang:setelement(Default, Record, [{ioi_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"28=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{ioi_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"29=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_capacity) of
    undefined -> erlang:setelement(Default, Record, [{last_capacity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"30=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_mkt) of
    undefined -> erlang:setelement(Default, Record, [{last_mkt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"31=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_px) of
    undefined -> erlang:setelement(Default, Record, [{last_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"32=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_qty) of
    undefined -> erlang:setelement(Default, Record, [{last_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"33=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_lines_of_text) of
    undefined -> erlang:setelement(Default, Record, [{no_lines_of_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"34=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, msg_seq_num) of
    undefined -> erlang:setelement(Default, Record, [{msg_seq_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"35=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, msg_type) of
    undefined -> erlang:setelement(Default, Record, [{msg_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"36=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, new_seq_no) of
    undefined -> erlang:setelement(Default, Record, [{new_seq_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"37=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, order_id) of
    undefined -> erlang:setelement(Default, Record, [{order_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"38=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, order_qty) of
    undefined -> erlang:setelement(Default, Record, [{order_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"39=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ord_status) of
    undefined -> erlang:setelement(Default, Record, [{ord_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"40=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ord_type) of
    undefined -> erlang:setelement(Default, Record, [{ord_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"41=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_cl_ord_id) of
    undefined -> erlang:setelement(Default, Record, [{orig_cl_ord_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"42=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_time) of
    undefined -> erlang:setelement(Default, Record, [{orig_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"43=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, poss_dup_flag) of
    undefined -> erlang:setelement(Default, Record, [{poss_dup_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"44=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, price) of
    undefined -> erlang:setelement(Default, Record, [{price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"45=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, ref_seq_num) of
    undefined -> erlang:setelement(Default, Record, [{ref_seq_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"46=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, relatd_sym) of
    undefined -> erlang:setelement(Default, Record, [{relatd_sym,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"47=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, rule80a) of
    undefined -> erlang:setelement(Default, Record, [{rule80a,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"48=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_id) of
    undefined -> erlang:setelement(Default, Record, [{security_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"49=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, sender_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{sender_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"50=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, sender_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{sender_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"51=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, sending_date) of
    undefined -> erlang:setelement(Default, Record, [{sending_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"52=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, sending_time) of
    undefined -> erlang:setelement(Default, Record, [{sending_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"53=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quantity) of
    undefined -> erlang:setelement(Default, Record, [{quantity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"54=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, side) of
    undefined -> erlang:setelement(Default, Record, [{side,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"55=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, symbol) of
    undefined -> erlang:setelement(Default, Record, [{symbol,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"56=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, target_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{target_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"57=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, target_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{target_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"58=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, text) of
    undefined -> erlang:setelement(Default, Record, [{text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"59=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, time_in_force) of
    undefined -> erlang:setelement(Default, Record, [{time_in_force,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"60=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, transact_time) of
    undefined -> erlang:setelement(Default, Record, [{transact_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"61=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, urgency) of
    undefined -> erlang:setelement(Default, Record, [{urgency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"62=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, valid_until_time) of
    undefined -> erlang:setelement(Default, Record, [{valid_until_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"63=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_type) of
    undefined -> erlang:setelement(Default, Record, [{settl_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"64=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_date) of
    undefined -> erlang:setelement(Default, Record, [{settl_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"65=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, symbol_sfx) of
    undefined -> erlang:setelement(Default, Record, [{symbol_sfx,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"66=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, list_id) of
    undefined -> erlang:setelement(Default, Record, [{list_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"67=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, list_seq_no) of
    undefined -> erlang:setelement(Default, Record, [{list_seq_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"68=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_orders) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_orders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"69=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, list_exec_inst) of
    undefined -> erlang:setelement(Default, Record, [{list_exec_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"70=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_id) of
    undefined -> erlang:setelement(Default, Record, [{alloc_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"71=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"72=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ref_alloc_id) of
    undefined -> erlang:setelement(Default, Record, [{ref_alloc_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"73=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_orders) of
    undefined -> erlang:setelement(Default, Record, [{no_orders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"74=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, avg_px_precision) of
    undefined -> erlang:setelement(Default, Record, [{avg_px_precision,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"75=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_date) of
    undefined -> erlang:setelement(Default, Record, [{trade_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"76=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_broker) of
    undefined -> erlang:setelement(Default, Record, [{exec_broker,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"77=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, position_effect) of
    undefined -> erlang:setelement(Default, Record, [{position_effect,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"78=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_allocs) of
    undefined -> erlang:setelement(Default, Record, [{no_allocs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"79=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_account) of
    undefined -> erlang:setelement(Default, Record, [{alloc_account,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"80=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_qty) of
    undefined -> erlang:setelement(Default, Record, [{alloc_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"81=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, process_code) of
    undefined -> erlang:setelement(Default, Record, [{process_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"82=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_rpts) of
    undefined -> erlang:setelement(Default, Record, [{no_rpts,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"83=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, rpt_seq) of
    undefined -> erlang:setelement(Default, Record, [{rpt_seq,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"84=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cxl_qty) of
    undefined -> erlang:setelement(Default, Record, [{cxl_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"85=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_dlvy_inst) of
    undefined -> erlang:setelement(Default, Record, [{no_dlvy_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"86=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, dlvy_inst) of
    undefined -> erlang:setelement(Default, Record, [{dlvy_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"87=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_status) of
    undefined -> erlang:setelement(Default, Record, [{alloc_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"88=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_rej_code) of
    undefined -> erlang:setelement(Default, Record, [{alloc_rej_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"92=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, broker_of_credit) of
    undefined -> erlang:setelement(Default, Record, [{broker_of_credit,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"94=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, email_type) of
    undefined -> erlang:setelement(Default, Record, [{email_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"97=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, poss_resend) of
    undefined -> erlang:setelement(Default, Record, [{poss_resend,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"98=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, encrypt_method) of
    undefined -> erlang:setelement(Default, Record, [{encrypt_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"99=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, stop_px) of
    undefined -> erlang:setelement(Default, Record, [{stop_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"100=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ex_destination) of
    undefined -> erlang:setelement(Default, Record, [{ex_destination,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"102=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cxl_rej_reason) of
    undefined -> erlang:setelement(Default, Record, [{cxl_rej_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"103=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, ord_rej_reason) of
    undefined -> erlang:setelement(Default, Record, [{ord_rej_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"104=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ioi_qualifier) of
    undefined -> erlang:setelement(Default, Record, [{ioi_qualifier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"105=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, wave_no) of
    undefined -> erlang:setelement(Default, Record, [{wave_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"106=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, issuer) of
    undefined -> erlang:setelement(Default, Record, [{issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"107=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_desc) of
    undefined -> erlang:setelement(Default, Record, [{security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"108=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, heart_bt_int) of
    undefined -> erlang:setelement(Default, Record, [{heart_bt_int,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"109=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, client_id) of
    undefined -> erlang:setelement(Default, Record, [{client_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"110=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, min_qty) of
    undefined -> erlang:setelement(Default, Record, [{min_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"111=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, max_floor) of
    undefined -> erlang:setelement(Default, Record, [{max_floor,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"112=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, test_req_id) of
    undefined -> erlang:setelement(Default, Record, [{test_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"113=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, report_to_exch) of
    undefined -> erlang:setelement(Default, Record, [{report_to_exch,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"114=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, locate_reqd) of
    undefined -> erlang:setelement(Default, Record, [{locate_reqd,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"115=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, on_behalf_of_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{on_behalf_of_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"116=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, on_behalf_of_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{on_behalf_of_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"117=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"118=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, net_money) of
    undefined -> erlang:setelement(Default, Record, [{net_money,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"119=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_curr_amt) of
    undefined -> erlang:setelement(Default, Record, [{settl_curr_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"120=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_currency) of
    undefined -> erlang:setelement(Default, Record, [{settl_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"121=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, forex_req) of
    undefined -> erlang:setelement(Default, Record, [{forex_req,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"122=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_sending_time) of
    undefined -> erlang:setelement(Default, Record, [{orig_sending_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"123=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, gap_fill_flag) of
    undefined -> erlang:setelement(Default, Record, [{gap_fill_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"124=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_execs) of
    undefined -> erlang:setelement(Default, Record, [{no_execs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"125=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cxl_type) of
    undefined -> erlang:setelement(Default, Record, [{cxl_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"126=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, expire_time) of
    undefined -> erlang:setelement(Default, Record, [{expire_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"127=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, dk_reason) of
    undefined -> erlang:setelement(Default, Record, [{dk_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"128=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, deliver_to_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{deliver_to_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"129=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, deliver_to_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{deliver_to_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"130=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, ioi_natural_flag) of
    undefined -> erlang:setelement(Default, Record, [{ioi_natural_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"131=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_req_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"132=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, bid_px) of
    undefined -> erlang:setelement(Default, Record, [{bid_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"133=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, offer_px) of
    undefined -> erlang:setelement(Default, Record, [{offer_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"134=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, bid_size) of
    undefined -> erlang:setelement(Default, Record, [{bid_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"135=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, offer_size) of
    undefined -> erlang:setelement(Default, Record, [{offer_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"136=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_misc_fees) of
    undefined -> erlang:setelement(Default, Record, [{no_misc_fees,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"137=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, misc_fee_amt) of
    undefined -> erlang:setelement(Default, Record, [{misc_fee_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"138=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, misc_fee_curr) of
    undefined -> erlang:setelement(Default, Record, [{misc_fee_curr,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"139=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, misc_fee_type) of
    undefined -> erlang:setelement(Default, Record, [{misc_fee_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"140=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, prev_close_px) of
    undefined -> erlang:setelement(Default, Record, [{prev_close_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"141=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, reset_seq_num_flag) of
    undefined -> erlang:setelement(Default, Record, [{reset_seq_num_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"142=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, sender_location_id) of
    undefined -> erlang:setelement(Default, Record, [{sender_location_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"143=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, target_location_id) of
    undefined -> erlang:setelement(Default, Record, [{target_location_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"144=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, on_behalf_of_location_id) of
    undefined -> erlang:setelement(Default, Record, [{on_behalf_of_location_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"145=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, deliver_to_location_id) of
    undefined -> erlang:setelement(Default, Record, [{deliver_to_location_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"146=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_related_sym) of
    undefined -> erlang:setelement(Default, Record, [{no_related_sym,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"147=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, subject) of
    undefined -> erlang:setelement(Default, Record, [{subject,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"148=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, headline) of
    undefined -> erlang:setelement(Default, Record, [{headline,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"149=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, url_link) of
    undefined -> erlang:setelement(Default, Record, [{url_link,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"150=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_type) of
    undefined -> erlang:setelement(Default, Record, [{exec_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"151=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leaves_qty) of
    undefined -> erlang:setelement(Default, Record, [{leaves_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"152=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cash_order_qty) of
    undefined -> erlang:setelement(Default, Record, [{cash_order_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"153=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_avg_px) of
    undefined -> erlang:setelement(Default, Record, [{alloc_avg_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"154=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_net_money) of
    undefined -> erlang:setelement(Default, Record, [{alloc_net_money,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"155=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_curr_fx_rate) of
    undefined -> erlang:setelement(Default, Record, [{settl_curr_fx_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"156=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_curr_fx_rate_calc) of
    undefined -> erlang:setelement(Default, Record, [{settl_curr_fx_rate_calc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"157=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, num_days_interest) of
    undefined -> erlang:setelement(Default, Record, [{num_days_interest,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"158=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, accrued_interest_rate) of
    undefined -> erlang:setelement(Default, Record, [{accrued_interest_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"159=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, accrued_interest_amt) of
    undefined -> erlang:setelement(Default, Record, [{accrued_interest_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"160=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_mode) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_mode,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"161=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_text) of
    undefined -> erlang:setelement(Default, Record, [{alloc_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"162=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"163=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"164=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, email_thread_id) of
    undefined -> erlang:setelement(Default, Record, [{email_thread_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"165=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_source) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"166=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_location) of
    undefined -> erlang:setelement(Default, Record, [{settl_location,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"167=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_type) of
    undefined -> erlang:setelement(Default, Record, [{security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"168=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, effective_time) of
    undefined -> erlang:setelement(Default, Record, [{effective_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"169=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, stand_inst_db_type) of
    undefined -> erlang:setelement(Default, Record, [{stand_inst_db_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"170=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, stand_inst_db_name) of
    undefined -> erlang:setelement(Default, Record, [{stand_inst_db_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"171=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, stand_inst_db_id) of
    undefined -> erlang:setelement(Default, Record, [{stand_inst_db_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"172=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_delivery_type) of
    undefined -> erlang:setelement(Default, Record, [{settl_delivery_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"173=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_depository_code) of
    undefined -> erlang:setelement(Default, Record, [{settl_depository_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"174=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_brkr_code) of
    undefined -> erlang:setelement(Default, Record, [{settl_brkr_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"175=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_code) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"176=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_name) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"177=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_code) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"178=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_acct_num) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_acct_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"179=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_acct_name) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_acct_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"180=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_contact_name) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_contact_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"181=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_settl_agent_contact_phone) of
    undefined -> erlang:setelement(Default, Record, [{security_settl_agent_contact_phone,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"182=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_name) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"183=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_code) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"184=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_acct_num) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_acct_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"185=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_acct_name) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_acct_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"186=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_contact_name) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_contact_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"187=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_settl_agent_contact_phone) of
    undefined -> erlang:setelement(Default, Record, [{cash_settl_agent_contact_phone,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"188=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, bid_spot_rate) of
    undefined -> erlang:setelement(Default, Record, [{bid_spot_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"189=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_forward_points) of
    undefined -> erlang:setelement(Default, Record, [{bid_forward_points,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"190=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, offer_spot_rate) of
    undefined -> erlang:setelement(Default, Record, [{offer_spot_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"191=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, offer_forward_points) of
    undefined -> erlang:setelement(Default, Record, [{offer_forward_points,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"192=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, order_qty2) of
    undefined -> erlang:setelement(Default, Record, [{order_qty2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"193=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_date2) of
    undefined -> erlang:setelement(Default, Record, [{settl_date2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"194=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_spot_rate) of
    undefined -> erlang:setelement(Default, Record, [{last_spot_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"195=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_forward_points) of
    undefined -> erlang:setelement(Default, Record, [{last_forward_points,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"196=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_link_id) of
    undefined -> erlang:setelement(Default, Record, [{alloc_link_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"197=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_link_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_link_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"198=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_order_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_order_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"199=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_ioi_qualifiers) of
    undefined -> erlang:setelement(Default, Record, [{no_ioi_qualifiers,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"200=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, maturity_month_year) of
    undefined -> erlang:setelement(Default, Record, [{maturity_month_year,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"201=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, put_or_call) of
    undefined -> erlang:setelement(Default, Record, [{put_or_call,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"202=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, strike_price) of
    undefined -> erlang:setelement(Default, Record, [{strike_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"203=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, covered_or_uncovered) of
    undefined -> erlang:setelement(Default, Record, [{covered_or_uncovered,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"204=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, customer_or_firm) of
    undefined -> erlang:setelement(Default, Record, [{customer_or_firm,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"205=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, maturity_day) of
    undefined -> erlang:setelement(Default, Record, [{maturity_day,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"206=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, opt_attribute) of
    undefined -> erlang:setelement(Default, Record, [{opt_attribute,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"207=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_exchange) of
    undefined -> erlang:setelement(Default, Record, [{security_exchange,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"208=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, notify_broker_of_credit) of
    undefined -> erlang:setelement(Default, Record, [{notify_broker_of_credit,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"209=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_handl_inst) of
    undefined -> erlang:setelement(Default, Record, [{alloc_handl_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"210=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, max_show) of
    undefined -> erlang:setelement(Default, Record, [{max_show,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"211=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, peg_offset_value) of
    undefined -> erlang:setelement(Default, Record, [{peg_offset_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"214=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"215=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_routing_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_routing_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"216=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, routing_type) of
    undefined -> erlang:setelement(Default, Record, [{routing_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"217=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, routing_id) of
    undefined -> erlang:setelement(Default, Record, [{routing_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"218=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, spread) of
    undefined -> erlang:setelement(Default, Record, [{spread,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"219=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark) of
    undefined -> erlang:setelement(Default, Record, [{benchmark,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"220=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark_curve_currency) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_curve_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"221=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark_curve_name) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_curve_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"222=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark_curve_point) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_curve_point,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"223=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coupon_rate) of
    undefined -> erlang:setelement(Default, Record, [{coupon_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"224=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coupon_payment_date) of
    undefined -> erlang:setelement(Default, Record, [{coupon_payment_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"225=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, issue_date) of
    undefined -> erlang:setelement(Default, Record, [{issue_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"226=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, repurchase_term) of
    undefined -> erlang:setelement(Default, Record, [{repurchase_term,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"227=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, repurchase_rate) of
    undefined -> erlang:setelement(Default, Record, [{repurchase_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"228=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, factor) of
    undefined -> erlang:setelement(Default, Record, [{factor,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"229=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_origination_date) of
    undefined -> erlang:setelement(Default, Record, [{trade_origination_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"230=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ex_date) of
    undefined -> erlang:setelement(Default, Record, [{ex_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"231=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contract_multiplier) of
    undefined -> erlang:setelement(Default, Record, [{contract_multiplier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"232=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_stipulations) of
    undefined -> erlang:setelement(Default, Record, [{no_stipulations,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"233=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, stipulation_type) of
    undefined -> erlang:setelement(Default, Record, [{stipulation_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"234=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, stipulation_value) of
    undefined -> erlang:setelement(Default, Record, [{stipulation_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"235=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, yield_type) of
    undefined -> erlang:setelement(Default, Record, [{yield_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"236=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, yield) of
    undefined -> erlang:setelement(Default, Record, [{yield,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"237=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, total_takedown) of
    undefined -> erlang:setelement(Default, Record, [{total_takedown,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"238=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, concession) of
    undefined -> erlang:setelement(Default, Record, [{concession,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"239=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, repo_collateral_security_type) of
    undefined -> erlang:setelement(Default, Record, [{repo_collateral_security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"240=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, redemption_date) of
    undefined -> erlang:setelement(Default, Record, [{redemption_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"241=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_coupon_payment_date) of
    undefined -> erlang:setelement(Default, Record, [{underlying_coupon_payment_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"242=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_issue_date) of
    undefined -> erlang:setelement(Default, Record, [{underlying_issue_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"243=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_repo_collateral_security_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_repo_collateral_security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"244=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_repurchase_term) of
    undefined -> erlang:setelement(Default, Record, [{underlying_repurchase_term,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"245=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_repurchase_rate) of
    undefined -> erlang:setelement(Default, Record, [{underlying_repurchase_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"246=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_factor) of
    undefined -> erlang:setelement(Default, Record, [{underlying_factor,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"247=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_redemption_date) of
    undefined -> erlang:setelement(Default, Record, [{underlying_redemption_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"248=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_coupon_payment_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_coupon_payment_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"249=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_issue_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_issue_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"250=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_repo_collateral_security_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_repo_collateral_security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"251=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_repurchase_term) of
    undefined -> erlang:setelement(Default, Record, [{leg_repurchase_term,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"252=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_repurchase_rate) of
    undefined -> erlang:setelement(Default, Record, [{leg_repurchase_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"253=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_factor) of
    undefined -> erlang:setelement(Default, Record, [{leg_factor,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"254=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_redemption_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_redemption_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"255=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, credit_rating) of
    undefined -> erlang:setelement(Default, Record, [{credit_rating,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"256=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_credit_rating) of
    undefined -> erlang:setelement(Default, Record, [{underlying_credit_rating,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"257=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_credit_rating) of
    undefined -> erlang:setelement(Default, Record, [{leg_credit_rating,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"258=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, traded_flat_switch) of
    undefined -> erlang:setelement(Default, Record, [{traded_flat_switch,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"259=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, basis_feature_date) of
    undefined -> erlang:setelement(Default, Record, [{basis_feature_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"260=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, basis_feature_price) of
    undefined -> erlang:setelement(Default, Record, [{basis_feature_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"262=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_req_id) of
    undefined -> erlang:setelement(Default, Record, [{md_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"263=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, subscription_request_type) of
    undefined -> erlang:setelement(Default, Record, [{subscription_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"264=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, market_depth) of
    undefined -> erlang:setelement(Default, Record, [{market_depth,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"265=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, md_update_type) of
    undefined -> erlang:setelement(Default, Record, [{md_update_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"266=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, aggregated_book) of
    undefined -> erlang:setelement(Default, Record, [{aggregated_book,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"267=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_md_entry_types) of
    undefined -> erlang:setelement(Default, Record, [{no_md_entry_types,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"268=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_md_entries) of
    undefined -> erlang:setelement(Default, Record, [{no_md_entries,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"269=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_type) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"270=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, md_entry_px) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"271=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, md_entry_size) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"272=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_date) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"273=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_time) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"274=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, tick_direction) of
    undefined -> erlang:setelement(Default, Record, [{tick_direction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"275=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_mkt) of
    undefined -> erlang:setelement(Default, Record, [{md_mkt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"276=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_condition) of
    undefined -> erlang:setelement(Default, Record, [{quote_condition,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"277=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_condition) of
    undefined -> erlang:setelement(Default, Record, [{trade_condition,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"278=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_id) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"279=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_update_action) of
    undefined -> erlang:setelement(Default, Record, [{md_update_action,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"280=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"281=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_req_rej_reason) of
    undefined -> erlang:setelement(Default, Record, [{md_req_rej_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"282=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_originator) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_originator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"283=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, location_id) of
    undefined -> erlang:setelement(Default, Record, [{location_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"284=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, desk_id) of
    undefined -> erlang:setelement(Default, Record, [{desk_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"285=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, delete_reason) of
    undefined -> erlang:setelement(Default, Record, [{delete_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"286=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, open_close_settl_flag) of
    undefined -> erlang:setelement(Default, Record, [{open_close_settl_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"287=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, seller_days) of
    undefined -> erlang:setelement(Default, Record, [{seller_days,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"288=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_buyer) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_buyer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"289=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, md_entry_seller) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_seller,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"290=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, md_entry_position_no) of
    undefined -> erlang:setelement(Default, Record, [{md_entry_position_no,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"291=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, financial_status) of
    undefined -> erlang:setelement(Default, Record, [{financial_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"292=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, corporate_action) of
    undefined -> erlang:setelement(Default, Record, [{corporate_action,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"293=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, def_bid_size) of
    undefined -> erlang:setelement(Default, Record, [{def_bid_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"294=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, def_offer_size) of
    undefined -> erlang:setelement(Default, Record, [{def_offer_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"295=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_quote_entries) of
    undefined -> erlang:setelement(Default, Record, [{no_quote_entries,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"296=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_quote_sets) of
    undefined -> erlang:setelement(Default, Record, [{no_quote_sets,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"297=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_status) of
    undefined -> erlang:setelement(Default, Record, [{quote_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"298=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_cancel_type) of
    undefined -> erlang:setelement(Default, Record, [{quote_cancel_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"299=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_entry_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_entry_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"300=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{quote_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"301=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_response_level) of
    undefined -> erlang:setelement(Default, Record, [{quote_response_level,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"302=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_set_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_set_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"303=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_request_type) of
    undefined -> erlang:setelement(Default, Record, [{quote_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"304=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_quote_entries) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_quote_entries,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"305=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_id_source) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"306=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_issuer) of
    undefined -> erlang:setelement(Default, Record, [{underlying_issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"307=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_desc) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"308=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_exchange) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_exchange,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"309=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_id) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"310=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"311=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_symbol) of
    undefined -> erlang:setelement(Default, Record, [{underlying_symbol,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"312=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_symbol_sfx) of
    undefined -> erlang:setelement(Default, Record, [{underlying_symbol_sfx,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"313=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_maturity_month_year) of
    undefined -> erlang:setelement(Default, Record, [{underlying_maturity_month_year,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"314=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_maturity_day) of
    undefined -> erlang:setelement(Default, Record, [{underlying_maturity_day,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"315=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_put_or_call) of
    undefined -> erlang:setelement(Default, Record, [{underlying_put_or_call,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"316=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_strike_price) of
    undefined -> erlang:setelement(Default, Record, [{underlying_strike_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"317=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_opt_attribute) of
    undefined -> erlang:setelement(Default, Record, [{underlying_opt_attribute,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"318=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_currency) of
    undefined -> erlang:setelement(Default, Record, [{underlying_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"319=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, ratio_qty) of
    undefined -> erlang:setelement(Default, Record, [{ratio_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"320=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_req_id) of
    undefined -> erlang:setelement(Default, Record, [{security_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"321=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, security_request_type) of
    undefined -> erlang:setelement(Default, Record, [{security_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"322=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_response_id) of
    undefined -> erlang:setelement(Default, Record, [{security_response_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"323=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, security_response_type) of
    undefined -> erlang:setelement(Default, Record, [{security_response_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"324=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_status_req_id) of
    undefined -> erlang:setelement(Default, Record, [{security_status_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"325=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, unsolicited_indicator) of
    undefined -> erlang:setelement(Default, Record, [{unsolicited_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"326=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, security_trading_status) of
    undefined -> erlang:setelement(Default, Record, [{security_trading_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"327=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, halt_reason_char) of
    undefined -> erlang:setelement(Default, Record, [{halt_reason_char,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"328=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, in_view_of_common) of
    undefined -> erlang:setelement(Default, Record, [{in_view_of_common,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"329=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, due_to_related) of
    undefined -> erlang:setelement(Default, Record, [{due_to_related,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"330=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, buy_volume) of
    undefined -> erlang:setelement(Default, Record, [{buy_volume,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"331=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, sell_volume) of
    undefined -> erlang:setelement(Default, Record, [{sell_volume,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"332=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, high_px) of
    undefined -> erlang:setelement(Default, Record, [{high_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"333=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, low_px) of
    undefined -> erlang:setelement(Default, Record, [{low_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"334=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, adjustment) of
    undefined -> erlang:setelement(Default, Record, [{adjustment,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"335=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_req_id) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"336=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trading_session_id) of
    undefined -> erlang:setelement(Default, Record, [{trading_session_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"337=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contra_trader) of
    undefined -> erlang:setelement(Default, Record, [{contra_trader,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"338=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trad_ses_method) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"339=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trad_ses_mode) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_mode,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"340=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trad_ses_status) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"341=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_start_time) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_start_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"342=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_open_time) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_open_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"343=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_pre_close_time) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_pre_close_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"344=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_close_time) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_close_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"345=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trad_ses_end_time) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_end_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"346=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, number_of_orders) of
    undefined -> erlang:setelement(Default, Record, [{number_of_orders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"347=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, message_encoding) of
    undefined -> erlang:setelement(Default, Record, [{message_encoding,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"366=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_price) of
    undefined -> erlang:setelement(Default, Record, [{alloc_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"367=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_set_valid_until_time) of
    undefined -> erlang:setelement(Default, Record, [{quote_set_valid_until_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"368=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_entry_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{quote_entry_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"369=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_msg_seq_num_processed) of
    undefined -> erlang:setelement(Default, Record, [{last_msg_seq_num_processed,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"370=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, on_behalf_of_sending_time) of
    undefined -> erlang:setelement(Default, Record, [{on_behalf_of_sending_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"371=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, ref_tag_id) of
    undefined -> erlang:setelement(Default, Record, [{ref_tag_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"372=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ref_msg_type) of
    undefined -> erlang:setelement(Default, Record, [{ref_msg_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"373=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, session_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{session_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"374=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_request_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{bid_request_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"375=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contra_broker) of
    undefined -> erlang:setelement(Default, Record, [{contra_broker,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"376=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, compliance_id) of
    undefined -> erlang:setelement(Default, Record, [{compliance_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"377=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, solicited_flag) of
    undefined -> erlang:setelement(Default, Record, [{solicited_flag,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"378=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, exec_restatement_reason) of
    undefined -> erlang:setelement(Default, Record, [{exec_restatement_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"379=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, business_reject_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{business_reject_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"380=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, business_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{business_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"381=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, gross_trade_amt) of
    undefined -> erlang:setelement(Default, Record, [{gross_trade_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"382=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_contra_brokers) of
    undefined -> erlang:setelement(Default, Record, [{no_contra_brokers,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"384=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_msg_types) of
    undefined -> erlang:setelement(Default, Record, [{no_msg_types,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"385=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, msg_direction) of
    undefined -> erlang:setelement(Default, Record, [{msg_direction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"386=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_trading_sessions) of
    undefined -> erlang:setelement(Default, Record, [{no_trading_sessions,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"387=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, total_volume_traded) of
    undefined -> erlang:setelement(Default, Record, [{total_volume_traded,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"388=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, discretion_inst) of
    undefined -> erlang:setelement(Default, Record, [{discretion_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"389=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, discretion_offset_value) of
    undefined -> erlang:setelement(Default, Record, [{discretion_offset_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"390=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_id) of
    undefined -> erlang:setelement(Default, Record, [{bid_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"391=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, client_bid_id) of
    undefined -> erlang:setelement(Default, Record, [{client_bid_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"392=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, list_name) of
    undefined -> erlang:setelement(Default, Record, [{list_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"393=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_related_sym) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_related_sym,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"394=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, bid_type) of
    undefined -> erlang:setelement(Default, Record, [{bid_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"395=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, num_tickets) of
    undefined -> erlang:setelement(Default, Record, [{num_tickets,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"396=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, side_value1) of
    undefined -> erlang:setelement(Default, Record, [{side_value1,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"397=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, side_value2) of
    undefined -> erlang:setelement(Default, Record, [{side_value2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"398=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_bid_descriptors) of
    undefined -> erlang:setelement(Default, Record, [{no_bid_descriptors,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"399=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, bid_descriptor_type) of
    undefined -> erlang:setelement(Default, Record, [{bid_descriptor_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"400=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_descriptor) of
    undefined -> erlang:setelement(Default, Record, [{bid_descriptor,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"401=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, side_value_ind) of
    undefined -> erlang:setelement(Default, Record, [{side_value_ind,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"402=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, liquidity_pct_low) of
    undefined -> erlang:setelement(Default, Record, [{liquidity_pct_low,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"403=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, liquidity_pct_high) of
    undefined -> erlang:setelement(Default, Record, [{liquidity_pct_high,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"404=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, liquidity_value) of
    undefined -> erlang:setelement(Default, Record, [{liquidity_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"405=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, efp_tracking_error) of
    undefined -> erlang:setelement(Default, Record, [{efp_tracking_error,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"406=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, fair_value) of
    undefined -> erlang:setelement(Default, Record, [{fair_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"407=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, outside_index_pct) of
    undefined -> erlang:setelement(Default, Record, [{outside_index_pct,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"408=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, value_of_futures) of
    undefined -> erlang:setelement(Default, Record, [{value_of_futures,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"409=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, liquidity_ind_type) of
    undefined -> erlang:setelement(Default, Record, [{liquidity_ind_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"410=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, wt_average_liquidity) of
    undefined -> erlang:setelement(Default, Record, [{wt_average_liquidity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"411=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, exchange_for_physical) of
    undefined -> erlang:setelement(Default, Record, [{exchange_for_physical,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"412=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, out_main_cntry_u_index) of
    undefined -> erlang:setelement(Default, Record, [{out_main_cntry_u_index,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"413=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cross_percent) of
    undefined -> erlang:setelement(Default, Record, [{cross_percent,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"414=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, prog_rpt_reqs) of
    undefined -> erlang:setelement(Default, Record, [{prog_rpt_reqs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"415=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, prog_period_interval) of
    undefined -> erlang:setelement(Default, Record, [{prog_period_interval,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"416=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, inc_tax_ind) of
    undefined -> erlang:setelement(Default, Record, [{inc_tax_ind,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"417=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, num_bidders) of
    undefined -> erlang:setelement(Default, Record, [{num_bidders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"418=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_trade_type) of
    undefined -> erlang:setelement(Default, Record, [{bid_trade_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"419=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, basis_px_type) of
    undefined -> erlang:setelement(Default, Record, [{basis_px_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"420=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_bid_components) of
    undefined -> erlang:setelement(Default, Record, [{no_bid_components,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"421=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, country) of
    undefined -> erlang:setelement(Default, Record, [{country,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"422=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_strikes) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_strikes,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"423=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, price_type) of
    undefined -> erlang:setelement(Default, Record, [{price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"424=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, day_order_qty) of
    undefined -> erlang:setelement(Default, Record, [{day_order_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"425=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, day_cum_qty) of
    undefined -> erlang:setelement(Default, Record, [{day_cum_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"426=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, day_avg_px) of
    undefined -> erlang:setelement(Default, Record, [{day_avg_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"427=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, gt_booking_inst) of
    undefined -> erlang:setelement(Default, Record, [{gt_booking_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"428=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_strikes) of
    undefined -> erlang:setelement(Default, Record, [{no_strikes,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"429=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, list_status_type) of
    undefined -> erlang:setelement(Default, Record, [{list_status_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"430=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, net_gross_ind) of
    undefined -> erlang:setelement(Default, Record, [{net_gross_ind,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"431=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, list_order_status) of
    undefined -> erlang:setelement(Default, Record, [{list_order_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"432=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, expire_date) of
    undefined -> erlang:setelement(Default, Record, [{expire_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"433=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, list_exec_inst_type) of
    undefined -> erlang:setelement(Default, Record, [{list_exec_inst_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"434=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cxl_rej_response_to) of
    undefined -> erlang:setelement(Default, Record, [{cxl_rej_response_to,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"435=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_coupon_rate) of
    undefined -> erlang:setelement(Default, Record, [{underlying_coupon_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"436=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_contract_multiplier) of
    undefined -> erlang:setelement(Default, Record, [{underlying_contract_multiplier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"437=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, contra_trade_qty) of
    undefined -> erlang:setelement(Default, Record, [{contra_trade_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"438=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contra_trade_time) of
    undefined -> erlang:setelement(Default, Record, [{contra_trade_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"439=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, clearing_firm) of
    undefined -> erlang:setelement(Default, Record, [{clearing_firm,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"440=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, clearing_account) of
    undefined -> erlang:setelement(Default, Record, [{clearing_account,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"441=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, liquidity_num_securities) of
    undefined -> erlang:setelement(Default, Record, [{liquidity_num_securities,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"442=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, multi_leg_reporting_type) of
    undefined -> erlang:setelement(Default, Record, [{multi_leg_reporting_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"443=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, strike_time) of
    undefined -> erlang:setelement(Default, Record, [{strike_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"444=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, list_status_text) of
    undefined -> erlang:setelement(Default, Record, [{list_status_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"447=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, party_id_source) of
    undefined -> erlang:setelement(Default, Record, [{party_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"448=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, party_id) of
    undefined -> erlang:setelement(Default, Record, [{party_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"449=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, total_volume_traded_date) of
    undefined -> erlang:setelement(Default, Record, [{total_volume_traded_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"450=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, total_volume_traded_time) of
    undefined -> erlang:setelement(Default, Record, [{total_volume_traded_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"451=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, net_chg_prev_day) of
    undefined -> erlang:setelement(Default, Record, [{net_chg_prev_day,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"452=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, party_role) of
    undefined -> erlang:setelement(Default, Record, [{party_role,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"453=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_party_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_party_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"454=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{no_security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"455=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"456=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_alt_id_source) of
    undefined -> erlang:setelement(Default, Record, [{security_alt_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"457=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_underlying_security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{no_underlying_security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"458=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"459=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_alt_id_source) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_alt_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"460=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, product) of
    undefined -> erlang:setelement(Default, Record, [{product,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"461=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cfi_code) of
    undefined -> erlang:setelement(Default, Record, [{cfi_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"462=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_product) of
    undefined -> erlang:setelement(Default, Record, [{underlying_product,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"463=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_cfi_code) of
    undefined -> erlang:setelement(Default, Record, [{underlying_cfi_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"464=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, test_message_indicator) of
    undefined -> erlang:setelement(Default, Record, [{test_message_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"465=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quantity_type) of
    undefined -> erlang:setelement(Default, Record, [{quantity_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"466=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, booking_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{booking_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"467=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, individual_alloc_id) of
    undefined -> erlang:setelement(Default, Record, [{individual_alloc_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"468=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, rounding_direction) of
    undefined -> erlang:setelement(Default, Record, [{rounding_direction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"469=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, rounding_modulus) of
    undefined -> erlang:setelement(Default, Record, [{rounding_modulus,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"470=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, country_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{country_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"471=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, state_or_province_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{state_or_province_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"472=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, locale_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{locale_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"473=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_regist_dtls) of
    undefined -> erlang:setelement(Default, Record, [{no_regist_dtls,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"474=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mailing_dtls) of
    undefined -> erlang:setelement(Default, Record, [{mailing_dtls,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"475=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, investor_country_of_residence) of
    undefined -> erlang:setelement(Default, Record, [{investor_country_of_residence,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"476=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, payment_ref) of
    undefined -> erlang:setelement(Default, Record, [{payment_ref,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"477=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, distrib_payment_method) of
    undefined -> erlang:setelement(Default, Record, [{distrib_payment_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"478=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_curr) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_curr,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"479=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, comm_currency) of
    undefined -> erlang:setelement(Default, Record, [{comm_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"480=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cancellation_rights) of
    undefined -> erlang:setelement(Default, Record, [{cancellation_rights,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"481=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, money_laundering_status) of
    undefined -> erlang:setelement(Default, Record, [{money_laundering_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"482=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mailing_inst) of
    undefined -> erlang:setelement(Default, Record, [{mailing_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"483=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trans_bkd_time) of
    undefined -> erlang:setelement(Default, Record, [{trans_bkd_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"484=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_price_type) of
    undefined -> erlang:setelement(Default, Record, [{exec_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"485=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_price_adjustment) of
    undefined -> erlang:setelement(Default, Record, [{exec_price_adjustment,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"486=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, date_of_birth) of
    undefined -> erlang:setelement(Default, Record, [{date_of_birth,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"487=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_report_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{trade_report_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"488=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, card_holder_name) of
    undefined -> erlang:setelement(Default, Record, [{card_holder_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"489=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, card_number) of
    undefined -> erlang:setelement(Default, Record, [{card_number,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"490=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, card_exp_date) of
    undefined -> erlang:setelement(Default, Record, [{card_exp_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"491=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, card_iss_num) of
    undefined -> erlang:setelement(Default, Record, [{card_iss_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"492=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, payment_method) of
    undefined -> erlang:setelement(Default, Record, [{payment_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"493=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_acct_type) of
    undefined -> erlang:setelement(Default, Record, [{regist_acct_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"494=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, designation) of
    undefined -> erlang:setelement(Default, Record, [{designation,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"495=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tax_advantage_type) of
    undefined -> erlang:setelement(Default, Record, [{tax_advantage_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"496=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_rej_reason_text) of
    undefined -> erlang:setelement(Default, Record, [{regist_rej_reason_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"497=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, fund_renew_waiv) of
    undefined -> erlang:setelement(Default, Record, [{fund_renew_waiv,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"498=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_agent_name) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_agent_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"499=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_agent_code) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_agent_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"500=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_agent_acct_number) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_agent_acct_number,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"501=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_pay_ref) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_pay_ref,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"502=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_distrib_agent_acct_name) of
    undefined -> erlang:setelement(Default, Record, [{cash_distrib_agent_acct_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"503=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, card_start_date) of
    undefined -> erlang:setelement(Default, Record, [{card_start_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"504=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, payment_date) of
    undefined -> erlang:setelement(Default, Record, [{payment_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"505=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, payment_remitter_id) of
    undefined -> erlang:setelement(Default, Record, [{payment_remitter_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"506=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_status) of
    undefined -> erlang:setelement(Default, Record, [{regist_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"507=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, regist_rej_reason_code) of
    undefined -> erlang:setelement(Default, Record, [{regist_rej_reason_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"508=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{regist_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"509=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_dtls) of
    undefined -> erlang:setelement(Default, Record, [{regist_dtls,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"510=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_distrib_insts) of
    undefined -> erlang:setelement(Default, Record, [{no_distrib_insts,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"511=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_email) of
    undefined -> erlang:setelement(Default, Record, [{regist_email,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"512=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, distrib_percentage) of
    undefined -> erlang:setelement(Default, Record, [{distrib_percentage,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"513=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_id) of
    undefined -> erlang:setelement(Default, Record, [{regist_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"514=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, regist_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{regist_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"515=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exec_valuation_point) of
    undefined -> erlang:setelement(Default, Record, [{exec_valuation_point,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"516=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, order_percent) of
    undefined -> erlang:setelement(Default, Record, [{order_percent,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"517=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ownership_type) of
    undefined -> erlang:setelement(Default, Record, [{ownership_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"518=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_cont_amts) of
    undefined -> erlang:setelement(Default, Record, [{no_cont_amts,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"519=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cont_amt_type) of
    undefined -> erlang:setelement(Default, Record, [{cont_amt_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"520=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cont_amt_value) of
    undefined -> erlang:setelement(Default, Record, [{cont_amt_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"521=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cont_amt_curr) of
    undefined -> erlang:setelement(Default, Record, [{cont_amt_curr,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"522=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, owner_type) of
    undefined -> erlang:setelement(Default, Record, [{owner_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"523=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, party_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{party_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"524=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested_party_id) of
    undefined -> erlang:setelement(Default, Record, [{nested_party_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"525=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested_party_id_source) of
    undefined -> erlang:setelement(Default, Record, [{nested_party_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"526=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_cl_ord_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_cl_ord_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"527=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_exec_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_exec_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"528=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, order_capacity) of
    undefined -> erlang:setelement(Default, Record, [{order_capacity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"529=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, order_restrictions) of
    undefined -> erlang:setelement(Default, Record, [{order_restrictions,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"530=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mass_cancel_request_type) of
    undefined -> erlang:setelement(Default, Record, [{mass_cancel_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"531=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mass_cancel_response) of
    undefined -> erlang:setelement(Default, Record, [{mass_cancel_response,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"532=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mass_cancel_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{mass_cancel_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"533=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, total_affected_orders) of
    undefined -> erlang:setelement(Default, Record, [{total_affected_orders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"534=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_affected_orders) of
    undefined -> erlang:setelement(Default, Record, [{no_affected_orders,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"535=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, affected_order_id) of
    undefined -> erlang:setelement(Default, Record, [{affected_order_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"536=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, affected_secondary_order_id) of
    undefined -> erlang:setelement(Default, Record, [{affected_secondary_order_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"537=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_type) of
    undefined -> erlang:setelement(Default, Record, [{quote_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"538=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested_party_role) of
    undefined -> erlang:setelement(Default, Record, [{nested_party_role,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"539=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested_party_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested_party_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"540=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, total_accrued_interest_amt) of
    undefined -> erlang:setelement(Default, Record, [{total_accrued_interest_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"541=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, maturity_date) of
    undefined -> erlang:setelement(Default, Record, [{maturity_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"542=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_maturity_date) of
    undefined -> erlang:setelement(Default, Record, [{underlying_maturity_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"543=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, instr_registry) of
    undefined -> erlang:setelement(Default, Record, [{instr_registry,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"544=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_margin) of
    undefined -> erlang:setelement(Default, Record, [{cash_margin,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"545=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested_party_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{nested_party_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"546=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, scope) of
    undefined -> erlang:setelement(Default, Record, [{scope,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"547=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, md_implicit_delete) of
    undefined -> erlang:setelement(Default, Record, [{md_implicit_delete,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"548=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cross_id) of
    undefined -> erlang:setelement(Default, Record, [{cross_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"549=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cross_type) of
    undefined -> erlang:setelement(Default, Record, [{cross_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"550=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cross_prioritization) of
    undefined -> erlang:setelement(Default, Record, [{cross_prioritization,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"551=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_cross_id) of
    undefined -> erlang:setelement(Default, Record, [{orig_cross_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"552=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_sides) of
    undefined -> erlang:setelement(Default, Record, [{no_sides,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"553=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, username) of
    undefined -> erlang:setelement(Default, Record, [{username,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"554=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, password) of
    undefined -> erlang:setelement(Default, Record, [{password,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"555=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_legs) of
    undefined -> erlang:setelement(Default, Record, [{no_legs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"556=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_currency) of
    undefined -> erlang:setelement(Default, Record, [{leg_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"557=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_security_types) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_security_types,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"558=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_security_types) of
    undefined -> erlang:setelement(Default, Record, [{no_security_types,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"559=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, security_list_request_type) of
    undefined -> erlang:setelement(Default, Record, [{security_list_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"560=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, security_request_result) of
    undefined -> erlang:setelement(Default, Record, [{security_request_result,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"561=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, round_lot) of
    undefined -> erlang:setelement(Default, Record, [{round_lot,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"562=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, min_trade_vol) of
    undefined -> erlang:setelement(Default, Record, [{min_trade_vol,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"563=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, multi_leg_rpt_type_req) of
    undefined -> erlang:setelement(Default, Record, [{multi_leg_rpt_type_req,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"564=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_position_effect) of
    undefined -> erlang:setelement(Default, Record, [{leg_position_effect,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"565=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_covered_or_uncovered) of
    undefined -> erlang:setelement(Default, Record, [{leg_covered_or_uncovered,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"566=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_price) of
    undefined -> erlang:setelement(Default, Record, [{leg_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"567=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trad_ses_status_rej_reason) of
    undefined -> erlang:setelement(Default, Record, [{trad_ses_status_rej_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"568=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_request_id) of
    undefined -> erlang:setelement(Default, Record, [{trade_request_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"569=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_request_type) of
    undefined -> erlang:setelement(Default, Record, [{trade_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"570=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, previously_reported) of
    undefined -> erlang:setelement(Default, Record, [{previously_reported,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"571=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_report_id) of
    undefined -> erlang:setelement(Default, Record, [{trade_report_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"572=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_report_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{trade_report_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"573=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, match_status) of
    undefined -> erlang:setelement(Default, Record, [{match_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"574=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, match_type) of
    undefined -> erlang:setelement(Default, Record, [{match_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"575=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, odd_lot) of
    undefined -> erlang:setelement(Default, Record, [{odd_lot,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"576=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_clearing_instructions) of
    undefined -> erlang:setelement(Default, Record, [{no_clearing_instructions,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"577=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, clearing_instruction) of
    undefined -> erlang:setelement(Default, Record, [{clearing_instruction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"578=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_input_source) of
    undefined -> erlang:setelement(Default, Record, [{trade_input_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"579=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_input_device) of
    undefined -> erlang:setelement(Default, Record, [{trade_input_device,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"580=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_dates) of
    undefined -> erlang:setelement(Default, Record, [{no_dates,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"581=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, account_type) of
    undefined -> erlang:setelement(Default, Record, [{account_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"582=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cust_order_capacity) of
    undefined -> erlang:setelement(Default, Record, [{cust_order_capacity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"583=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cl_ord_link_id) of
    undefined -> erlang:setelement(Default, Record, [{cl_ord_link_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"584=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mass_status_req_id) of
    undefined -> erlang:setelement(Default, Record, [{mass_status_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"585=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, mass_status_req_type) of
    undefined -> erlang:setelement(Default, Record, [{mass_status_req_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"586=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_ord_mod_time) of
    undefined -> erlang:setelement(Default, Record, [{orig_ord_mod_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"587=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_settl_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_settl_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"588=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_settl_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_settl_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"589=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, day_booking_inst) of
    undefined -> erlang:setelement(Default, Record, [{day_booking_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"590=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, booking_unit) of
    undefined -> erlang:setelement(Default, Record, [{booking_unit,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"591=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, prealloc_method) of
    undefined -> erlang:setelement(Default, Record, [{prealloc_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"592=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_country_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{underlying_country_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"593=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_state_or_province_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{underlying_state_or_province_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"594=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_locale_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{underlying_locale_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"595=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_instr_registry) of
    undefined -> erlang:setelement(Default, Record, [{underlying_instr_registry,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"596=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_country_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{leg_country_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"597=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_state_or_province_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{leg_state_or_province_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"598=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_locale_of_issue) of
    undefined -> erlang:setelement(Default, Record, [{leg_locale_of_issue,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"599=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_instr_registry) of
    undefined -> erlang:setelement(Default, Record, [{leg_instr_registry,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"600=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_symbol) of
    undefined -> erlang:setelement(Default, Record, [{leg_symbol,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"601=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_symbol_sfx) of
    undefined -> erlang:setelement(Default, Record, [{leg_symbol_sfx,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"602=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_id) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"603=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_id_source) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"604=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, no_leg_security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{no_leg_security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"605=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_alt_id) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_alt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"606=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_alt_id_source) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_alt_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"607=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_product) of
    undefined -> erlang:setelement(Default, Record, [{leg_product,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"608=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_cfi_code) of
    undefined -> erlang:setelement(Default, Record, [{leg_cfi_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"609=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"610=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_maturity_month_year) of
    undefined -> erlang:setelement(Default, Record, [{leg_maturity_month_year,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"611=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_maturity_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_maturity_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"612=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_strike_price) of
    undefined -> erlang:setelement(Default, Record, [{leg_strike_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"613=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_opt_attribute) of
    undefined -> erlang:setelement(Default, Record, [{leg_opt_attribute,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"614=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_contract_multiplier) of
    undefined -> erlang:setelement(Default, Record, [{leg_contract_multiplier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"615=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_coupon_rate) of
    undefined -> erlang:setelement(Default, Record, [{leg_coupon_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"616=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_exchange) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_exchange,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"617=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_issuer) of
    undefined -> erlang:setelement(Default, Record, [{leg_issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"620=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_desc) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"623=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_ratio_qty) of
    undefined -> erlang:setelement(Default, Record, [{leg_ratio_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"624=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_side) of
    undefined -> erlang:setelement(Default, Record, [{leg_side,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"625=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trading_session_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{trading_session_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"626=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"627=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_hops) of
    undefined -> erlang:setelement(Default, Record, [{no_hops,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"628=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, hop_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{hop_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"629=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, hop_sending_time) of
    undefined -> erlang:setelement(Default, Record, [{hop_sending_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"630=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, hop_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{hop_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"631=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, mid_px) of
    undefined -> erlang:setelement(Default, Record, [{mid_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"632=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_yield) of
    undefined -> erlang:setelement(Default, Record, [{bid_yield,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"633=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, mid_yield) of
    undefined -> erlang:setelement(Default, Record, [{mid_yield,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"634=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, offer_yield) of
    undefined -> erlang:setelement(Default, Record, [{offer_yield,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"635=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, clearing_fee_indicator) of
    undefined -> erlang:setelement(Default, Record, [{clearing_fee_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"636=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, working_indicator) of
    undefined -> erlang:setelement(Default, Record, [{working_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"637=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_last_px) of
    undefined -> erlang:setelement(Default, Record, [{leg_last_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"638=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, priority_indicator) of
    undefined -> erlang:setelement(Default, Record, [{priority_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"639=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, price_improvement) of
    undefined -> erlang:setelement(Default, Record, [{price_improvement,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"640=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, price2) of
    undefined -> erlang:setelement(Default, Record, [{price2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"641=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_forward_points2) of
    undefined -> erlang:setelement(Default, Record, [{last_forward_points2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"642=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, bid_forward_points2) of
    undefined -> erlang:setelement(Default, Record, [{bid_forward_points2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"643=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, offer_forward_points2) of
    undefined -> erlang:setelement(Default, Record, [{offer_forward_points2,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"644=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, rfq_req_id) of
    undefined -> erlang:setelement(Default, Record, [{rfq_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"645=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, mkt_bid_px) of
    undefined -> erlang:setelement(Default, Record, [{mkt_bid_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"646=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, mkt_offer_px) of
    undefined -> erlang:setelement(Default, Record, [{mkt_offer_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"647=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, min_bid_size) of
    undefined -> erlang:setelement(Default, Record, [{min_bid_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"648=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, min_offer_size) of
    undefined -> erlang:setelement(Default, Record, [{min_offer_size,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"649=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_status_req_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_status_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"650=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, legal_confirm) of
    undefined -> erlang:setelement(Default, Record, [{legal_confirm,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"651=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_last_px) of
    undefined -> erlang:setelement(Default, Record, [{underlying_last_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"652=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_last_qty) of
    undefined -> erlang:setelement(Default, Record, [{underlying_last_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"653=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, sec_def_status) of
    undefined -> erlang:setelement(Default, Record, [{sec_def_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"654=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{leg_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"655=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contra_leg_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{contra_leg_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"656=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_curr_bid_fx_rate) of
    undefined -> erlang:setelement(Default, Record, [{settl_curr_bid_fx_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"657=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_curr_offer_fx_rate) of
    undefined -> erlang:setelement(Default, Record, [{settl_curr_offer_fx_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"658=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_request_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{quote_request_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"659=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, side_compliance_id) of
    undefined -> erlang:setelement(Default, Record, [{side_compliance_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"660=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, acct_id_source) of
    undefined -> erlang:setelement(Default, Record, [{acct_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"661=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_acct_id_source) of
    undefined -> erlang:setelement(Default, Record, [{alloc_acct_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"662=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, benchmark_price) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"663=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, benchmark_price_type) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"664=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, confirm_id) of
    undefined -> erlang:setelement(Default, Record, [{confirm_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"665=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, confirm_status) of
    undefined -> erlang:setelement(Default, Record, [{confirm_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"666=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, confirm_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{confirm_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"667=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, contract_settl_month) of
    undefined -> erlang:setelement(Default, Record, [{contract_settl_month,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"668=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, delivery_form) of
    undefined -> erlang:setelement(Default, Record, [{delivery_form,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"669=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_par_px) of
    undefined -> erlang:setelement(Default, Record, [{last_par_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"670=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_leg_allocs) of
    undefined -> erlang:setelement(Default, Record, [{no_leg_allocs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"671=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_alloc_account) of
    undefined -> erlang:setelement(Default, Record, [{leg_alloc_account,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"672=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_individual_alloc_id) of
    undefined -> erlang:setelement(Default, Record, [{leg_individual_alloc_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"673=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_alloc_qty) of
    undefined -> erlang:setelement(Default, Record, [{leg_alloc_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"674=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_alloc_acct_id_source) of
    undefined -> erlang:setelement(Default, Record, [{leg_alloc_acct_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"675=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_settl_currency) of
    undefined -> erlang:setelement(Default, Record, [{leg_settl_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"676=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_benchmark_curve_currency) of
    undefined -> erlang:setelement(Default, Record, [{leg_benchmark_curve_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"677=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_benchmark_curve_name) of
    undefined -> erlang:setelement(Default, Record, [{leg_benchmark_curve_name,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"678=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_benchmark_curve_point) of
    undefined -> erlang:setelement(Default, Record, [{leg_benchmark_curve_point,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"679=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_benchmark_price) of
    undefined -> erlang:setelement(Default, Record, [{leg_benchmark_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"680=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_benchmark_price_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_benchmark_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"681=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_bid_px) of
    undefined -> erlang:setelement(Default, Record, [{leg_bid_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"682=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_ioi_qty) of
    undefined -> erlang:setelement(Default, Record, [{leg_ioi_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"683=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_leg_stipulations) of
    undefined -> erlang:setelement(Default, Record, [{no_leg_stipulations,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"684=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_offer_px) of
    undefined -> erlang:setelement(Default, Record, [{leg_offer_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"685=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_order_qty) of
    undefined -> erlang:setelement(Default, Record, [{leg_order_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"686=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_price_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"687=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_qty) of
    undefined -> erlang:setelement(Default, Record, [{leg_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"688=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_stipulation_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_stipulation_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"689=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_stipulation_value) of
    undefined -> erlang:setelement(Default, Record, [{leg_stipulation_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"690=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, leg_swap_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_swap_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"691=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pool) of
    undefined -> erlang:setelement(Default, Record, [{pool,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"692=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_price_type) of
    undefined -> erlang:setelement(Default, Record, [{quote_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"693=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_resp_id) of
    undefined -> erlang:setelement(Default, Record, [{quote_resp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"694=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, quote_resp_type) of
    undefined -> erlang:setelement(Default, Record, [{quote_resp_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"695=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, quote_qualifier) of
    undefined -> erlang:setelement(Default, Record, [{quote_qualifier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"696=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, yield_redemption_date) of
    undefined -> erlang:setelement(Default, Record, [{yield_redemption_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"697=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, yield_redemption_price) of
    undefined -> erlang:setelement(Default, Record, [{yield_redemption_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"698=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, yield_redemption_price_type) of
    undefined -> erlang:setelement(Default, Record, [{yield_redemption_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"699=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark_security_id) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_security_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"700=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, reversal_indicator) of
    undefined -> erlang:setelement(Default, Record, [{reversal_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"701=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, yield_calc_date) of
    undefined -> erlang:setelement(Default, Record, [{yield_calc_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"702=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_positions) of
    undefined -> erlang:setelement(Default, Record, [{no_positions,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"703=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_type) of
    undefined -> erlang:setelement(Default, Record, [{pos_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"704=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, long_qty) of
    undefined -> erlang:setelement(Default, Record, [{long_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"705=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, short_qty) of
    undefined -> erlang:setelement(Default, Record, [{short_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"706=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_qty_status) of
    undefined -> erlang:setelement(Default, Record, [{pos_qty_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"707=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_amt_type) of
    undefined -> erlang:setelement(Default, Record, [{pos_amt_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"708=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_amt) of
    undefined -> erlang:setelement(Default, Record, [{pos_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"709=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{pos_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"710=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_req_id) of
    undefined -> erlang:setelement(Default, Record, [{pos_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"711=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_underlyings) of
    undefined -> erlang:setelement(Default, Record, [{no_underlyings,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"712=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_maint_action) of
    undefined -> erlang:setelement(Default, Record, [{pos_maint_action,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"713=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, orig_pos_req_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{orig_pos_req_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"714=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_maint_rpt_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{pos_maint_rpt_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"715=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, clearing_business_date) of
    undefined -> erlang:setelement(Default, Record, [{clearing_business_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"716=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_sess_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_sess_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"717=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_sess_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_sess_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"718=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, adjustment_type) of
    undefined -> erlang:setelement(Default, Record, [{adjustment_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"719=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, contrary_instruction_indicator) of
    undefined -> erlang:setelement(Default, Record, [{contrary_instruction_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"720=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, prior_spread_indicator) of
    undefined -> erlang:setelement(Default, Record, [{prior_spread_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"721=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pos_maint_rpt_id) of
    undefined -> erlang:setelement(Default, Record, [{pos_maint_rpt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"722=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_maint_status) of
    undefined -> erlang:setelement(Default, Record, [{pos_maint_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"723=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_maint_result) of
    undefined -> erlang:setelement(Default, Record, [{pos_maint_result,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"724=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_req_type) of
    undefined -> erlang:setelement(Default, Record, [{pos_req_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"725=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, response_transport_type) of
    undefined -> erlang:setelement(Default, Record, [{response_transport_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"726=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, response_destination) of
    undefined -> erlang:setelement(Default, Record, [{response_destination,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"727=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, total_num_pos_reports) of
    undefined -> erlang:setelement(Default, Record, [{total_num_pos_reports,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"728=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_req_result) of
    undefined -> erlang:setelement(Default, Record, [{pos_req_result,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"729=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pos_req_status) of
    undefined -> erlang:setelement(Default, Record, [{pos_req_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"730=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_price) of
    undefined -> erlang:setelement(Default, Record, [{settl_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"731=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_price_type) of
    undefined -> erlang:setelement(Default, Record, [{settl_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"732=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_settl_price) of
    undefined -> erlang:setelement(Default, Record, [{underlying_settl_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"733=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_settl_price_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_settl_price_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"734=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, prior_settl_price) of
    undefined -> erlang:setelement(Default, Record, [{prior_settl_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"735=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_quote_qualifiers) of
    undefined -> erlang:setelement(Default, Record, [{no_quote_qualifiers,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"736=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_settl_currency) of
    undefined -> erlang:setelement(Default, Record, [{alloc_settl_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"737=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_settl_curr_amt) of
    undefined -> erlang:setelement(Default, Record, [{alloc_settl_curr_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"738=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, interest_at_maturity) of
    undefined -> erlang:setelement(Default, Record, [{interest_at_maturity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"739=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_dated_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_dated_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"740=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_pool) of
    undefined -> erlang:setelement(Default, Record, [{leg_pool,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"741=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_interest_at_maturity) of
    undefined -> erlang:setelement(Default, Record, [{alloc_interest_at_maturity,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"742=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_accrued_interest_amt) of
    undefined -> erlang:setelement(Default, Record, [{alloc_accrued_interest_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"743=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, delivery_date) of
    undefined -> erlang:setelement(Default, Record, [{delivery_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"744=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, assignment_method) of
    undefined -> erlang:setelement(Default, Record, [{assignment_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"745=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, assignment_unit) of
    undefined -> erlang:setelement(Default, Record, [{assignment_unit,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"746=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, open_interest) of
    undefined -> erlang:setelement(Default, Record, [{open_interest,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"747=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exercise_method) of
    undefined -> erlang:setelement(Default, Record, [{exercise_method,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"748=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_num_trade_reports) of
    undefined -> erlang:setelement(Default, Record, [{tot_num_trade_reports,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"749=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_request_result) of
    undefined -> erlang:setelement(Default, Record, [{trade_request_result,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"750=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_request_status) of
    undefined -> erlang:setelement(Default, Record, [{trade_request_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"751=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_report_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{trade_report_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"752=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, side_multi_leg_reporting_type) of
    undefined -> erlang:setelement(Default, Record, [{side_multi_leg_reporting_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"753=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_pos_amt) of
    undefined -> erlang:setelement(Default, Record, [{no_pos_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"754=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, auto_accept_indicator) of
    undefined -> erlang:setelement(Default, Record, [{auto_accept_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"755=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_report_id) of
    undefined -> erlang:setelement(Default, Record, [{alloc_report_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"756=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested2_party_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested2_party_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"757=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested2_party_id) of
    undefined -> erlang:setelement(Default, Record, [{nested2_party_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"758=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested2_party_id_source) of
    undefined -> erlang:setelement(Default, Record, [{nested2_party_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"759=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested2_party_role) of
    undefined -> erlang:setelement(Default, Record, [{nested2_party_role,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"760=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested2_party_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{nested2_party_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"761=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, benchmark_security_id_source) of
    undefined -> erlang:setelement(Default, Record, [{benchmark_security_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"762=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, security_sub_type) of
    undefined -> erlang:setelement(Default, Record, [{security_sub_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"763=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_security_sub_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_security_sub_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"764=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_security_sub_type) of
    undefined -> erlang:setelement(Default, Record, [{leg_security_sub_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"765=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, allowable_one_sidedness_pct) of
    undefined -> erlang:setelement(Default, Record, [{allowable_one_sidedness_pct,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"766=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, allowable_one_sidedness_value) of
    undefined -> erlang:setelement(Default, Record, [{allowable_one_sidedness_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"767=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, allowable_one_sidedness_curr) of
    undefined -> erlang:setelement(Default, Record, [{allowable_one_sidedness_curr,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"768=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_trd_reg_timestamps) of
    undefined -> erlang:setelement(Default, Record, [{no_trd_reg_timestamps,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"769=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trd_reg_timestamp) of
    undefined -> erlang:setelement(Default, Record, [{trd_reg_timestamp,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"770=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trd_reg_timestamp_type) of
    undefined -> erlang:setelement(Default, Record, [{trd_reg_timestamp_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"771=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trd_reg_timestamp_origin) of
    undefined -> erlang:setelement(Default, Record, [{trd_reg_timestamp_origin,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"772=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, confirm_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{confirm_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"773=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, confirm_type) of
    undefined -> erlang:setelement(Default, Record, [{confirm_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"774=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, confirm_rej_reason) of
    undefined -> erlang:setelement(Default, Record, [{confirm_rej_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"775=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, booking_type) of
    undefined -> erlang:setelement(Default, Record, [{booking_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"776=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, individual_alloc_rej_code) of
    undefined -> erlang:setelement(Default, Record, [{individual_alloc_rej_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"777=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_msg_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_msg_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"778=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_settl_inst) of
    undefined -> erlang:setelement(Default, Record, [{no_settl_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"779=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_update_time) of
    undefined -> erlang:setelement(Default, Record, [{last_update_time,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"780=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_settl_inst_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_settl_inst_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"781=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_settl_party_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_settl_party_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"782=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_party_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_party_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"783=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_party_id_source) of
    undefined -> erlang:setelement(Default, Record, [{settl_party_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"784=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_party_role) of
    undefined -> erlang:setelement(Default, Record, [{settl_party_role,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"785=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_party_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_party_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"786=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_party_sub_id_type) of
    undefined -> erlang:setelement(Default, Record, [{settl_party_sub_id_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"787=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, dlvy_inst_type) of
    undefined -> erlang:setelement(Default, Record, [{dlvy_inst_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"788=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, termination_type) of
    undefined -> erlang:setelement(Default, Record, [{termination_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"789=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, next_expected_msg_seq_num) of
    undefined -> erlang:setelement(Default, Record, [{next_expected_msg_seq_num,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"790=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ord_status_req_id) of
    undefined -> erlang:setelement(Default, Record, [{ord_status_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"791=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, settl_inst_req_id) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"792=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, settl_inst_req_rej_code) of
    undefined -> erlang:setelement(Default, Record, [{settl_inst_req_rej_code,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"793=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_alloc_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_alloc_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"794=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_report_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_report_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"795=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alloc_report_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{alloc_report_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"796=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_canc_replace_reason) of
    undefined -> erlang:setelement(Default, Record, [{alloc_canc_replace_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"797=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, copy_msg_indicator) of
    undefined -> erlang:setelement(Default, Record, [{copy_msg_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"798=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_account_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_account_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"799=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, order_avg_px) of
    undefined -> erlang:setelement(Default, Record, [{order_avg_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"800=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, order_booking_qty) of
    undefined -> erlang:setelement(Default, Record, [{order_booking_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"801=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_settl_party_sub_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_settl_party_sub_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"802=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_party_sub_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_party_sub_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"803=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, party_sub_id_type) of
    undefined -> erlang:setelement(Default, Record, [{party_sub_id_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"804=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested_party_sub_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested_party_sub_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"805=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested_party_sub_id_type) of
    undefined -> erlang:setelement(Default, Record, [{nested_party_sub_id_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"806=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested2_party_sub_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested2_party_sub_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"807=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested2_party_sub_id_type) of
    undefined -> erlang:setelement(Default, Record, [{nested2_party_sub_id_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"808=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_intermed_req_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_intermed_req_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"810=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_px) of
    undefined -> erlang:setelement(Default, Record, [{underlying_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"811=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, price_delta) of
    undefined -> erlang:setelement(Default, Record, [{price_delta,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"812=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, appl_queue_max) of
    undefined -> erlang:setelement(Default, Record, [{appl_queue_max,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"813=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, appl_queue_depth) of
    undefined -> erlang:setelement(Default, Record, [{appl_queue_depth,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"814=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, appl_queue_resolution) of
    undefined -> erlang:setelement(Default, Record, [{appl_queue_resolution,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"815=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, appl_queue_action) of
    undefined -> erlang:setelement(Default, Record, [{appl_queue_action,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"816=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_alt_md_source) of
    undefined -> erlang:setelement(Default, Record, [{no_alt_md_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"817=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, alt_md_source_id) of
    undefined -> erlang:setelement(Default, Record, [{alt_md_source_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"818=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_trade_report_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_trade_report_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"819=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, avg_px_indicator) of
    undefined -> erlang:setelement(Default, Record, [{avg_px_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"820=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_link_id) of
    undefined -> erlang:setelement(Default, Record, [{trade_link_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"821=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, order_input_device) of
    undefined -> erlang:setelement(Default, Record, [{order_input_device,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"822=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_trading_session_id) of
    undefined -> erlang:setelement(Default, Record, [{underlying_trading_session_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"823=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_trading_session_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{underlying_trading_session_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"824=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trade_leg_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{trade_leg_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"825=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, exchange_rule) of
    undefined -> erlang:setelement(Default, Record, [{exchange_rule,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"826=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_alloc_indicator) of
    undefined -> erlang:setelement(Default, Record, [{trade_alloc_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"827=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, expiration_cycle) of
    undefined -> erlang:setelement(Default, Record, [{expiration_cycle,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"828=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trd_type) of
    undefined -> erlang:setelement(Default, Record, [{trd_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"829=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trd_sub_type) of
    undefined -> erlang:setelement(Default, Record, [{trd_sub_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"830=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, transfer_reason) of
    undefined -> erlang:setelement(Default, Record, [{transfer_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"831=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, asgn_req_id) of
    undefined -> erlang:setelement(Default, Record, [{asgn_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"832=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_num_assignment_reports) of
    undefined -> erlang:setelement(Default, Record, [{tot_num_assignment_reports,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"833=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, asgn_rpt_id) of
    undefined -> erlang:setelement(Default, Record, [{asgn_rpt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"834=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, threshold_amount) of
    undefined -> erlang:setelement(Default, Record, [{threshold_amount,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"835=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, peg_move_type) of
    undefined -> erlang:setelement(Default, Record, [{peg_move_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"836=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, peg_offset_type) of
    undefined -> erlang:setelement(Default, Record, [{peg_offset_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"837=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, peg_limit_type) of
    undefined -> erlang:setelement(Default, Record, [{peg_limit_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"838=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, peg_round_direction) of
    undefined -> erlang:setelement(Default, Record, [{peg_round_direction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"839=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, pegged_price) of
    undefined -> erlang:setelement(Default, Record, [{pegged_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"840=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, peg_scope) of
    undefined -> erlang:setelement(Default, Record, [{peg_scope,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"841=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_move_type) of
    undefined -> erlang:setelement(Default, Record, [{discretion_move_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"842=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_offset_type) of
    undefined -> erlang:setelement(Default, Record, [{discretion_offset_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"843=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_limit_type) of
    undefined -> erlang:setelement(Default, Record, [{discretion_limit_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"844=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_round_direction) of
    undefined -> erlang:setelement(Default, Record, [{discretion_round_direction,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"845=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_price) of
    undefined -> erlang:setelement(Default, Record, [{discretion_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"846=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, discretion_scope) of
    undefined -> erlang:setelement(Default, Record, [{discretion_scope,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"847=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, target_strategy) of
    undefined -> erlang:setelement(Default, Record, [{target_strategy,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"848=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, target_strategy_parameters) of
    undefined -> erlang:setelement(Default, Record, [{target_strategy_parameters,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"849=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, participation_rate) of
    undefined -> erlang:setelement(Default, Record, [{participation_rate,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"850=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, target_strategy_performance) of
    undefined -> erlang:setelement(Default, Record, [{target_strategy_performance,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"851=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, last_liquidity_ind) of
    undefined -> erlang:setelement(Default, Record, [{last_liquidity_ind,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"852=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, publish_trd_indicator) of
    undefined -> erlang:setelement(Default, Record, [{publish_trd_indicator,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"853=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, short_sale_reason) of
    undefined -> erlang:setelement(Default, Record, [{short_sale_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"854=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, qty_type) of
    undefined -> erlang:setelement(Default, Record, [{qty_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"855=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, secondary_trd_type) of
    undefined -> erlang:setelement(Default, Record, [{secondary_trd_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"856=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trade_report_type) of
    undefined -> erlang:setelement(Default, Record, [{trade_report_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"857=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, alloc_no_orders_type) of
    undefined -> erlang:setelement(Default, Record, [{alloc_no_orders_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"858=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, shared_commission) of
    undefined -> erlang:setelement(Default, Record, [{shared_commission,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"859=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, confirm_req_id) of
    undefined -> erlang:setelement(Default, Record, [{confirm_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"860=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, avg_par_px) of
    undefined -> erlang:setelement(Default, Record, [{avg_par_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"861=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, reported_px) of
    undefined -> erlang:setelement(Default, Record, [{reported_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"862=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_capacities) of
    undefined -> erlang:setelement(Default, Record, [{no_capacities,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"863=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, order_capacity_qty) of
    undefined -> erlang:setelement(Default, Record, [{order_capacity_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"864=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_events) of
    undefined -> erlang:setelement(Default, Record, [{no_events,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"865=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, event_type) of
    undefined -> erlang:setelement(Default, Record, [{event_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"866=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, event_date) of
    undefined -> erlang:setelement(Default, Record, [{event_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"867=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, event_px) of
    undefined -> erlang:setelement(Default, Record, [{event_px,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"868=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, event_text) of
    undefined -> erlang:setelement(Default, Record, [{event_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"869=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, pct_at_risk) of
    undefined -> erlang:setelement(Default, Record, [{pct_at_risk,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"870=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_instr_attrib) of
    undefined -> erlang:setelement(Default, Record, [{no_instr_attrib,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"871=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, instr_attrib_type) of
    undefined -> erlang:setelement(Default, Record, [{instr_attrib_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"872=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, instr_attrib_value) of
    undefined -> erlang:setelement(Default, Record, [{instr_attrib_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"873=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, dated_date) of
    undefined -> erlang:setelement(Default, Record, [{dated_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"874=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, interest_accrual_date) of
    undefined -> erlang:setelement(Default, Record, [{interest_accrual_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"875=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, cp_program) of
    undefined -> erlang:setelement(Default, Record, [{cp_program,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"876=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cp_reg_type) of
    undefined -> erlang:setelement(Default, Record, [{cp_reg_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"877=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_cp_program) of
    undefined -> erlang:setelement(Default, Record, [{underlying_cp_program,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"878=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_cp_reg_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_cp_reg_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"879=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_qty) of
    undefined -> erlang:setelement(Default, Record, [{underlying_qty,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"880=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, trd_match_id) of
    undefined -> erlang:setelement(Default, Record, [{trd_match_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"881=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, secondary_trade_report_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{secondary_trade_report_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"882=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_dirty_price) of
    undefined -> erlang:setelement(Default, Record, [{underlying_dirty_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"883=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, underlying_end_price) of
    undefined -> erlang:setelement(Default, Record, [{underlying_end_price,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"884=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_start_value) of
    undefined -> erlang:setelement(Default, Record, [{underlying_start_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"885=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_current_value) of
    undefined -> erlang:setelement(Default, Record, [{underlying_current_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"886=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_end_value) of
    undefined -> erlang:setelement(Default, Record, [{underlying_end_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"887=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_underlying_stips) of
    undefined -> erlang:setelement(Default, Record, [{no_underlying_stips,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"888=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_stip_type) of
    undefined -> erlang:setelement(Default, Record, [{underlying_stip_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"889=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_stip_value) of
    undefined -> erlang:setelement(Default, Record, [{underlying_stip_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"890=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, maturity_net_money) of
    undefined -> erlang:setelement(Default, Record, [{maturity_net_money,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"891=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, misc_fee_basis) of
    undefined -> erlang:setelement(Default, Record, [{misc_fee_basis,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"892=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_no_allocs) of
    undefined -> erlang:setelement(Default, Record, [{tot_no_allocs,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"893=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, last_fragment) of
    undefined -> erlang:setelement(Default, Record, [{last_fragment,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"894=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_req_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_req_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"895=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_asgn_reason) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"896=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_inquiry_qualifier) of
    undefined -> erlang:setelement(Default, Record, [{coll_inquiry_qualifier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"897=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_trades) of
    undefined -> erlang:setelement(Default, Record, [{no_trades,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"898=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, margin_ratio) of
    undefined -> erlang:setelement(Default, Record, [{margin_ratio,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"899=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, margin_excess) of
    undefined -> erlang:setelement(Default, Record, [{margin_excess,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"900=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, total_net_value) of
    undefined -> erlang:setelement(Default, Record, [{total_net_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"901=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, cash_outstanding) of
    undefined -> erlang:setelement(Default, Record, [{cash_outstanding,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"902=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_asgn_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"903=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_asgn_trans_type) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_trans_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"904=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_resp_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_resp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"905=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_asgn_resp_type) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_resp_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"906=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_asgn_reject_reason) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_reject_reason,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"907=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_asgn_ref_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_asgn_ref_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"908=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_rpt_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_rpt_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"909=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, coll_inquiry_id) of
    undefined -> erlang:setelement(Default, Record, [{coll_inquiry_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"910=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_status) of
    undefined -> erlang:setelement(Default, Record, [{coll_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"911=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, tot_num_reports) of
    undefined -> erlang:setelement(Default, Record, [{tot_num_reports,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"912=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case field_index(RecordName, last_rpt_requested) of
    undefined -> erlang:setelement(Default, Record, [{last_rpt_requested,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"913=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, agreement_desc) of
    undefined -> erlang:setelement(Default, Record, [{agreement_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"914=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, agreement_id) of
    undefined -> erlang:setelement(Default, Record, [{agreement_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"915=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, agreement_date) of
    undefined -> erlang:setelement(Default, Record, [{agreement_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"916=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, start_date) of
    undefined -> erlang:setelement(Default, Record, [{start_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"917=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, end_date) of
    undefined -> erlang:setelement(Default, Record, [{end_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"918=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, agreement_currency) of
    undefined -> erlang:setelement(Default, Record, [{agreement_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"919=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, delivery_type) of
    undefined -> erlang:setelement(Default, Record, [{delivery_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"920=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, end_accrued_interest_amt) of
    undefined -> erlang:setelement(Default, Record, [{end_accrued_interest_amt,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"921=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, start_cash) of
    undefined -> erlang:setelement(Default, Record, [{start_cash,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"922=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, end_cash) of
    undefined -> erlang:setelement(Default, Record, [{end_cash,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"923=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, user_request_id) of
    undefined -> erlang:setelement(Default, Record, [{user_request_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"924=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, user_request_type) of
    undefined -> erlang:setelement(Default, Record, [{user_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"925=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, new_password) of
    undefined -> erlang:setelement(Default, Record, [{new_password,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"926=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, user_status) of
    undefined -> erlang:setelement(Default, Record, [{user_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"927=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, user_status_text) of
    undefined -> erlang:setelement(Default, Record, [{user_status_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"928=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, status_value) of
    undefined -> erlang:setelement(Default, Record, [{status_value,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"929=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, status_text) of
    undefined -> erlang:setelement(Default, Record, [{status_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"930=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ref_comp_id) of
    undefined -> erlang:setelement(Default, Record, [{ref_comp_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"931=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, ref_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{ref_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"932=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, network_response_id) of
    undefined -> erlang:setelement(Default, Record, [{network_response_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"933=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, network_request_id) of
    undefined -> erlang:setelement(Default, Record, [{network_request_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"934=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, last_network_response_id) of
    undefined -> erlang:setelement(Default, Record, [{last_network_response_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"935=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, network_request_type) of
    undefined -> erlang:setelement(Default, Record, [{network_request_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"936=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_comp_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_comp_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"937=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, network_status_response_type) of
    undefined -> erlang:setelement(Default, Record, [{network_status_response_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"938=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_coll_inquiry_qualifier) of
    undefined -> erlang:setelement(Default, Record, [{no_coll_inquiry_qualifier,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"939=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, trd_rpt_status) of
    undefined -> erlang:setelement(Default, Record, [{trd_rpt_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"940=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, affirm_status) of
    undefined -> erlang:setelement(Default, Record, [{affirm_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"941=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, underlying_strike_currency) of
    undefined -> erlang:setelement(Default, Record, [{underlying_strike_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"942=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_strike_currency) of
    undefined -> erlang:setelement(Default, Record, [{leg_strike_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"943=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, time_bracket) of
    undefined -> erlang:setelement(Default, Record, [{time_bracket,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"944=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_action) of
    undefined -> erlang:setelement(Default, Record, [{coll_action,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"945=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_inquiry_status) of
    undefined -> erlang:setelement(Default, Record, [{coll_inquiry_status,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"946=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, coll_inquiry_result) of
    undefined -> erlang:setelement(Default, Record, [{coll_inquiry_result,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"947=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, strike_currency) of
    undefined -> erlang:setelement(Default, Record, [{strike_currency,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"948=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested3_party_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested3_party_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"949=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested3_party_id) of
    undefined -> erlang:setelement(Default, Record, [{nested3_party_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"950=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested3_party_id_source) of
    undefined -> erlang:setelement(Default, Record, [{nested3_party_id_source,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"951=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested3_party_role) of
    undefined -> erlang:setelement(Default, Record, [{nested3_party_role,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"952=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, no_nested3_party_sub_ids) of
    undefined -> erlang:setelement(Default, Record, [{no_nested3_party_sub_ids,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"953=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, nested3_party_sub_id) of
    undefined -> erlang:setelement(Default, Record, [{nested3_party_sub_id,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"954=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = parse_num(RawValue),
  Record1 = case field_index(RecordName, nested3_party_sub_id_type) of
    undefined -> erlang:setelement(Default, Record, [{nested3_party_sub_id_type,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"955=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_contract_settl_month) of
    undefined -> erlang:setelement(Default, Record, [{leg_contract_settl_month,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"956=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case field_index(RecordName, leg_interest_accrual_date) of
    undefined -> erlang:setelement(Default, Record, [{leg_interest_accrual_date,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_fields(<<"9=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"90=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"93=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"95=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"212=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"348=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"350=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"352=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"354=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"356=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"358=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"360=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"362=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"364=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"383=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"445=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"618=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<"621=", Message/binary>>, Record, RecordName, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, RecordName, Default);

decode_fields(<<>>, Record, _RecordName, Default) ->
  erlang:setelement(Default, Record, lists:reverse(erlang:element(Default,Record))).

decode_data_field(<<"89=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, signature) of
    undefined -> erlang:setelement(Default, Record, [{signature,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"91=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, secure_data) of
    undefined -> erlang:setelement(Default, Record, [{secure_data,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"96=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, raw_data) of
    undefined -> erlang:setelement(Default, Record, [{raw_data,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"213=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, xml_data) of
    undefined -> erlang:setelement(Default, Record, [{xml_data,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"349=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_issuer) of
    undefined -> erlang:setelement(Default, Record, [{encoded_issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"351=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_security_desc) of
    undefined -> erlang:setelement(Default, Record, [{encoded_security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"353=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_list_exec_inst) of
    undefined -> erlang:setelement(Default, Record, [{encoded_list_exec_inst,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"355=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_text) of
    undefined -> erlang:setelement(Default, Record, [{encoded_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"357=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_subject) of
    undefined -> erlang:setelement(Default, Record, [{encoded_subject,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"359=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_headline) of
    undefined -> erlang:setelement(Default, Record, [{encoded_headline,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"361=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_alloc_text) of
    undefined -> erlang:setelement(Default, Record, [{encoded_alloc_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"363=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_underlying_issuer) of
    undefined -> erlang:setelement(Default, Record, [{encoded_underlying_issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"365=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_underlying_security_desc) of
    undefined -> erlang:setelement(Default, Record, [{encoded_underlying_security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"446=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_list_status_text) of
    undefined -> erlang:setelement(Default, Record, [{encoded_list_status_text,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"619=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_leg_issuer) of
    undefined -> erlang:setelement(Default, Record, [{encoded_leg_issuer,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default);

decode_data_field(<<"622=", Message/binary>>, DataLength, Record, RecordName, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case field_index(RecordName, encoded_leg_security_desc) of
    undefined -> erlang:setelement(Default, Record, [{encoded_leg_security_desc,Value}|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, RecordName, Default).

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
decode_typed_field(avg_px, V) -> parse_num(V)*1.0;
decode_typed_field(begin_seq_no, V) -> parse_num(V);
decode_typed_field(begin_string, V) -> V;
decode_typed_field(body_length, V) -> parse_num(V);
decode_typed_field(check_sum, V) -> V;
decode_typed_field(cl_ord_id, V) -> V;
decode_typed_field(commission, V) -> V;
decode_typed_field(comm_type, V) -> V;
decode_typed_field(cum_qty, V) -> parse_num(V);
decode_typed_field(currency, V) -> V;
decode_typed_field(end_seq_no, V) -> parse_num(V);
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
decode_typed_field(last_px, V) -> parse_num(V)*1.0;
decode_typed_field(last_qty, V) -> parse_num(V);
decode_typed_field(no_lines_of_text, V) -> parse_num(V);
decode_typed_field(msg_seq_num, V) -> parse_num(V);
decode_typed_field(msg_type, V) -> message_by_number(V);
decode_typed_field(new_seq_no, V) -> parse_num(V);
decode_typed_field(order_id, V) -> V;
decode_typed_field(order_qty, V) -> parse_num(V);
decode_typed_field(ord_status, V) -> V;
decode_typed_field(ord_type, V) -> V;
decode_typed_field(orig_cl_ord_id, V) -> V;
decode_typed_field(orig_time, V) -> V;
decode_typed_field(poss_dup_flag, V) -> V == <<"Y">>;
decode_typed_field(price, V) -> parse_num(V)*1.0;
decode_typed_field(ref_seq_num, V) -> parse_num(V);
decode_typed_field(relatd_sym, V) -> V;
decode_typed_field(rule80a, V) -> V;
decode_typed_field(security_id, V) -> V;
decode_typed_field(sender_comp_id, V) -> V;
decode_typed_field(sender_sub_id, V) -> V;
decode_typed_field(sending_date, V) -> V;
decode_typed_field(sending_time, V) -> V;
decode_typed_field(quantity, V) -> parse_num(V);
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
decode_typed_field(list_seq_no, V) -> parse_num(V);
decode_typed_field(tot_no_orders, V) -> parse_num(V);
decode_typed_field(list_exec_inst, V) -> V;
decode_typed_field(alloc_id, V) -> V;
decode_typed_field(alloc_trans_type, V) -> V;
decode_typed_field(ref_alloc_id, V) -> V;
decode_typed_field(no_orders, V) -> parse_num(V);
decode_typed_field(avg_px_precision, V) -> parse_num(V);
decode_typed_field(trade_date, V) -> V;
decode_typed_field(exec_broker, V) -> V;
decode_typed_field(position_effect, V) -> V;
decode_typed_field(no_allocs, V) -> parse_num(V);
decode_typed_field(alloc_account, V) -> V;
decode_typed_field(alloc_qty, V) -> parse_num(V);
decode_typed_field(process_code, V) -> V;
decode_typed_field(no_rpts, V) -> parse_num(V);
decode_typed_field(rpt_seq, V) -> parse_num(V);
decode_typed_field(cxl_qty, V) -> parse_num(V);
decode_typed_field(no_dlvy_inst, V) -> parse_num(V);
decode_typed_field(dlvy_inst, V) -> V;
decode_typed_field(alloc_status, V) -> parse_num(V);
decode_typed_field(alloc_rej_code, V) -> parse_num(V);
decode_typed_field(signature, V) -> V;
decode_typed_field(secure_data_len, V) -> parse_num(V);
decode_typed_field(secure_data, V) -> V;
decode_typed_field(broker_of_credit, V) -> V;
decode_typed_field(signature_length, V) -> parse_num(V);
decode_typed_field(email_type, V) -> V;
decode_typed_field(raw_data_length, V) -> parse_num(V);
decode_typed_field(raw_data, V) -> V;
decode_typed_field(poss_resend, V) -> V == <<"Y">>;
decode_typed_field(encrypt_method, V) -> parse_num(V);
decode_typed_field(stop_px, V) -> parse_num(V)*1.0;
decode_typed_field(ex_destination, V) -> V;
decode_typed_field(cxl_rej_reason, V) -> parse_num(V);
decode_typed_field(ord_rej_reason, V) -> parse_num(V);
decode_typed_field(ioi_qualifier, V) -> V;
decode_typed_field(wave_no, V) -> V;
decode_typed_field(issuer, V) -> V;
decode_typed_field(security_desc, V) -> V;
decode_typed_field(heart_bt_int, V) -> parse_num(V);
decode_typed_field(client_id, V) -> V;
decode_typed_field(min_qty, V) -> parse_num(V);
decode_typed_field(max_floor, V) -> parse_num(V);
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
decode_typed_field(no_execs, V) -> parse_num(V);
decode_typed_field(cxl_type, V) -> V;
decode_typed_field(expire_time, V) -> V;
decode_typed_field(dk_reason, V) -> V;
decode_typed_field(deliver_to_comp_id, V) -> V;
decode_typed_field(deliver_to_sub_id, V) -> V;
decode_typed_field(ioi_natural_flag, V) -> V == <<"Y">>;
decode_typed_field(quote_req_id, V) -> V;
decode_typed_field(bid_px, V) -> parse_num(V)*1.0;
decode_typed_field(offer_px, V) -> parse_num(V)*1.0;
decode_typed_field(bid_size, V) -> parse_num(V);
decode_typed_field(offer_size, V) -> parse_num(V);
decode_typed_field(no_misc_fees, V) -> parse_num(V);
decode_typed_field(misc_fee_amt, V) -> V;
decode_typed_field(misc_fee_curr, V) -> V;
decode_typed_field(misc_fee_type, V) -> V;
decode_typed_field(prev_close_px, V) -> parse_num(V)*1.0;
decode_typed_field(reset_seq_num_flag, V) -> V == <<"Y">>;
decode_typed_field(sender_location_id, V) -> V;
decode_typed_field(target_location_id, V) -> V;
decode_typed_field(on_behalf_of_location_id, V) -> V;
decode_typed_field(deliver_to_location_id, V) -> V;
decode_typed_field(no_related_sym, V) -> parse_num(V);
decode_typed_field(subject, V) -> V;
decode_typed_field(headline, V) -> V;
decode_typed_field(url_link, V) -> V;
decode_typed_field(exec_type, V) -> V;
decode_typed_field(leaves_qty, V) -> parse_num(V);
decode_typed_field(cash_order_qty, V) -> parse_num(V);
decode_typed_field(alloc_avg_px, V) -> parse_num(V)*1.0;
decode_typed_field(alloc_net_money, V) -> V;
decode_typed_field(settl_curr_fx_rate, V) -> V;
decode_typed_field(settl_curr_fx_rate_calc, V) -> V;
decode_typed_field(num_days_interest, V) -> parse_num(V);
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
decode_typed_field(stand_inst_db_type, V) -> parse_num(V);
decode_typed_field(stand_inst_db_name, V) -> V;
decode_typed_field(stand_inst_db_id, V) -> V;
decode_typed_field(settl_delivery_type, V) -> parse_num(V);
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
decode_typed_field(bid_spot_rate, V) -> parse_num(V)*1.0;
decode_typed_field(bid_forward_points, V) -> V;
decode_typed_field(offer_spot_rate, V) -> parse_num(V)*1.0;
decode_typed_field(offer_forward_points, V) -> V;
decode_typed_field(order_qty2, V) -> parse_num(V);
decode_typed_field(settl_date2, V) -> V;
decode_typed_field(last_spot_rate, V) -> parse_num(V)*1.0;
decode_typed_field(last_forward_points, V) -> V;
decode_typed_field(alloc_link_id, V) -> V;
decode_typed_field(alloc_link_type, V) -> parse_num(V);
decode_typed_field(secondary_order_id, V) -> V;
decode_typed_field(no_ioi_qualifiers, V) -> parse_num(V);
decode_typed_field(maturity_month_year, V) -> V;
decode_typed_field(put_or_call, V) -> parse_num(V);
decode_typed_field(strike_price, V) -> parse_num(V)*1.0;
decode_typed_field(covered_or_uncovered, V) -> parse_num(V);
decode_typed_field(customer_or_firm, V) -> parse_num(V);
decode_typed_field(maturity_day, V) -> V;
decode_typed_field(opt_attribute, V) -> V;
decode_typed_field(security_exchange, V) -> V;
decode_typed_field(notify_broker_of_credit, V) -> V == <<"Y">>;
decode_typed_field(alloc_handl_inst, V) -> parse_num(V);
decode_typed_field(max_show, V) -> parse_num(V);
decode_typed_field(peg_offset_value, V) -> V;
decode_typed_field(xml_data_len, V) -> parse_num(V);
decode_typed_field(xml_data, V) -> V;
decode_typed_field(settl_inst_ref_id, V) -> V;
decode_typed_field(no_routing_ids, V) -> parse_num(V);
decode_typed_field(routing_type, V) -> parse_num(V);
decode_typed_field(routing_id, V) -> V;
decode_typed_field(spread, V) -> V;
decode_typed_field(benchmark, V) -> V;
decode_typed_field(benchmark_curve_currency, V) -> V;
decode_typed_field(benchmark_curve_name, V) -> V;
decode_typed_field(benchmark_curve_point, V) -> V;
decode_typed_field(coupon_rate, V) -> V;
decode_typed_field(coupon_payment_date, V) -> V;
decode_typed_field(issue_date, V) -> V;
decode_typed_field(repurchase_term, V) -> parse_num(V);
decode_typed_field(repurchase_rate, V) -> V;
decode_typed_field(factor, V) -> V;
decode_typed_field(trade_origination_date, V) -> V;
decode_typed_field(ex_date, V) -> V;
decode_typed_field(contract_multiplier, V) -> V;
decode_typed_field(no_stipulations, V) -> parse_num(V);
decode_typed_field(stipulation_type, V) -> V;
decode_typed_field(stipulation_value, V) -> V;
decode_typed_field(yield_type, V) -> V;
decode_typed_field(yield, V) -> V;
decode_typed_field(total_takedown, V) -> V;
decode_typed_field(concession, V) -> V;
decode_typed_field(repo_collateral_security_type, V) -> parse_num(V);
decode_typed_field(redemption_date, V) -> V;
decode_typed_field(underlying_coupon_payment_date, V) -> V;
decode_typed_field(underlying_issue_date, V) -> V;
decode_typed_field(underlying_repo_collateral_security_type, V) -> parse_num(V);
decode_typed_field(underlying_repurchase_term, V) -> parse_num(V);
decode_typed_field(underlying_repurchase_rate, V) -> V;
decode_typed_field(underlying_factor, V) -> V;
decode_typed_field(underlying_redemption_date, V) -> V;
decode_typed_field(leg_coupon_payment_date, V) -> V;
decode_typed_field(leg_issue_date, V) -> V;
decode_typed_field(leg_repo_collateral_security_type, V) -> parse_num(V);
decode_typed_field(leg_repurchase_term, V) -> parse_num(V);
decode_typed_field(leg_repurchase_rate, V) -> V;
decode_typed_field(leg_factor, V) -> V;
decode_typed_field(leg_redemption_date, V) -> V;
decode_typed_field(credit_rating, V) -> V;
decode_typed_field(underlying_credit_rating, V) -> V;
decode_typed_field(leg_credit_rating, V) -> V;
decode_typed_field(traded_flat_switch, V) -> V == <<"Y">>;
decode_typed_field(basis_feature_date, V) -> V;
decode_typed_field(basis_feature_price, V) -> parse_num(V)*1.0;
decode_typed_field(md_req_id, V) -> V;
decode_typed_field(subscription_request_type, V) -> V;
decode_typed_field(market_depth, V) -> parse_num(V);
decode_typed_field(md_update_type, V) -> parse_num(V);
decode_typed_field(aggregated_book, V) -> V == <<"Y">>;
decode_typed_field(no_md_entry_types, V) -> parse_num(V);
decode_typed_field(no_md_entries, V) -> parse_num(V);
decode_typed_field(md_entry_type, V) -> V;
decode_typed_field(md_entry_px, V) -> parse_num(V)*1.0;
decode_typed_field(md_entry_size, V) -> parse_num(V);
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
decode_typed_field(seller_days, V) -> parse_num(V);
decode_typed_field(md_entry_buyer, V) -> V;
decode_typed_field(md_entry_seller, V) -> V;
decode_typed_field(md_entry_position_no, V) -> parse_num(V);
decode_typed_field(financial_status, V) -> V;
decode_typed_field(corporate_action, V) -> V;
decode_typed_field(def_bid_size, V) -> parse_num(V);
decode_typed_field(def_offer_size, V) -> parse_num(V);
decode_typed_field(no_quote_entries, V) -> parse_num(V);
decode_typed_field(no_quote_sets, V) -> parse_num(V);
decode_typed_field(quote_status, V) -> parse_num(V);
decode_typed_field(quote_cancel_type, V) -> parse_num(V);
decode_typed_field(quote_entry_id, V) -> V;
decode_typed_field(quote_reject_reason, V) -> parse_num(V);
decode_typed_field(quote_response_level, V) -> parse_num(V);
decode_typed_field(quote_set_id, V) -> V;
decode_typed_field(quote_request_type, V) -> parse_num(V);
decode_typed_field(tot_no_quote_entries, V) -> parse_num(V);
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
decode_typed_field(underlying_put_or_call, V) -> parse_num(V);
decode_typed_field(underlying_strike_price, V) -> parse_num(V)*1.0;
decode_typed_field(underlying_opt_attribute, V) -> V;
decode_typed_field(underlying_currency, V) -> V;
decode_typed_field(ratio_qty, V) -> parse_num(V);
decode_typed_field(security_req_id, V) -> V;
decode_typed_field(security_request_type, V) -> parse_num(V);
decode_typed_field(security_response_id, V) -> V;
decode_typed_field(security_response_type, V) -> parse_num(V);
decode_typed_field(security_status_req_id, V) -> V;
decode_typed_field(unsolicited_indicator, V) -> V == <<"Y">>;
decode_typed_field(security_trading_status, V) -> parse_num(V);
decode_typed_field(halt_reason_char, V) -> V;
decode_typed_field(in_view_of_common, V) -> V == <<"Y">>;
decode_typed_field(due_to_related, V) -> V == <<"Y">>;
decode_typed_field(buy_volume, V) -> parse_num(V);
decode_typed_field(sell_volume, V) -> parse_num(V);
decode_typed_field(high_px, V) -> parse_num(V)*1.0;
decode_typed_field(low_px, V) -> parse_num(V)*1.0;
decode_typed_field(adjustment, V) -> parse_num(V);
decode_typed_field(trad_ses_req_id, V) -> V;
decode_typed_field(trading_session_id, V) -> V;
decode_typed_field(contra_trader, V) -> V;
decode_typed_field(trad_ses_method, V) -> parse_num(V);
decode_typed_field(trad_ses_mode, V) -> parse_num(V);
decode_typed_field(trad_ses_status, V) -> parse_num(V);
decode_typed_field(trad_ses_start_time, V) -> V;
decode_typed_field(trad_ses_open_time, V) -> V;
decode_typed_field(trad_ses_pre_close_time, V) -> V;
decode_typed_field(trad_ses_close_time, V) -> V;
decode_typed_field(trad_ses_end_time, V) -> V;
decode_typed_field(number_of_orders, V) -> parse_num(V);
decode_typed_field(message_encoding, V) -> V;
decode_typed_field(encoded_issuer_len, V) -> parse_num(V);
decode_typed_field(encoded_issuer, V) -> V;
decode_typed_field(encoded_security_desc_len, V) -> parse_num(V);
decode_typed_field(encoded_security_desc, V) -> V;
decode_typed_field(encoded_list_exec_inst_len, V) -> parse_num(V);
decode_typed_field(encoded_list_exec_inst, V) -> V;
decode_typed_field(encoded_text_len, V) -> parse_num(V);
decode_typed_field(encoded_text, V) -> V;
decode_typed_field(encoded_subject_len, V) -> parse_num(V);
decode_typed_field(encoded_subject, V) -> V;
decode_typed_field(encoded_headline_len, V) -> parse_num(V);
decode_typed_field(encoded_headline, V) -> V;
decode_typed_field(encoded_alloc_text_len, V) -> parse_num(V);
decode_typed_field(encoded_alloc_text, V) -> V;
decode_typed_field(encoded_underlying_issuer_len, V) -> parse_num(V);
decode_typed_field(encoded_underlying_issuer, V) -> V;
decode_typed_field(encoded_underlying_security_desc_len, V) -> parse_num(V);
decode_typed_field(encoded_underlying_security_desc, V) -> V;
decode_typed_field(alloc_price, V) -> parse_num(V)*1.0;
decode_typed_field(quote_set_valid_until_time, V) -> V;
decode_typed_field(quote_entry_reject_reason, V) -> parse_num(V);
decode_typed_field(last_msg_seq_num_processed, V) -> parse_num(V);
decode_typed_field(on_behalf_of_sending_time, V) -> V;
decode_typed_field(ref_tag_id, V) -> parse_num(V);
decode_typed_field(ref_msg_type, V) -> V;
decode_typed_field(session_reject_reason, V) -> parse_num(V);
decode_typed_field(bid_request_trans_type, V) -> V;
decode_typed_field(contra_broker, V) -> V;
decode_typed_field(compliance_id, V) -> V;
decode_typed_field(solicited_flag, V) -> V == <<"Y">>;
decode_typed_field(exec_restatement_reason, V) -> parse_num(V);
decode_typed_field(business_reject_ref_id, V) -> V;
decode_typed_field(business_reject_reason, V) -> parse_num(V);
decode_typed_field(gross_trade_amt, V) -> V;
decode_typed_field(no_contra_brokers, V) -> parse_num(V);
decode_typed_field(max_message_size, V) -> parse_num(V);
decode_typed_field(no_msg_types, V) -> parse_num(V);
decode_typed_field(msg_direction, V) -> V;
decode_typed_field(no_trading_sessions, V) -> parse_num(V);
decode_typed_field(total_volume_traded, V) -> parse_num(V);
decode_typed_field(discretion_inst, V) -> V;
decode_typed_field(discretion_offset_value, V) -> V;
decode_typed_field(bid_id, V) -> V;
decode_typed_field(client_bid_id, V) -> V;
decode_typed_field(list_name, V) -> V;
decode_typed_field(tot_no_related_sym, V) -> parse_num(V);
decode_typed_field(bid_type, V) -> parse_num(V);
decode_typed_field(num_tickets, V) -> parse_num(V);
decode_typed_field(side_value1, V) -> V;
decode_typed_field(side_value2, V) -> V;
decode_typed_field(no_bid_descriptors, V) -> parse_num(V);
decode_typed_field(bid_descriptor_type, V) -> parse_num(V);
decode_typed_field(bid_descriptor, V) -> V;
decode_typed_field(side_value_ind, V) -> parse_num(V);
decode_typed_field(liquidity_pct_low, V) -> V;
decode_typed_field(liquidity_pct_high, V) -> V;
decode_typed_field(liquidity_value, V) -> V;
decode_typed_field(efp_tracking_error, V) -> V;
decode_typed_field(fair_value, V) -> V;
decode_typed_field(outside_index_pct, V) -> V;
decode_typed_field(value_of_futures, V) -> V;
decode_typed_field(liquidity_ind_type, V) -> parse_num(V);
decode_typed_field(wt_average_liquidity, V) -> V;
decode_typed_field(exchange_for_physical, V) -> V == <<"Y">>;
decode_typed_field(out_main_cntry_u_index, V) -> V;
decode_typed_field(cross_percent, V) -> V;
decode_typed_field(prog_rpt_reqs, V) -> parse_num(V);
decode_typed_field(prog_period_interval, V) -> parse_num(V);
decode_typed_field(inc_tax_ind, V) -> parse_num(V);
decode_typed_field(num_bidders, V) -> parse_num(V);
decode_typed_field(bid_trade_type, V) -> V;
decode_typed_field(basis_px_type, V) -> V;
decode_typed_field(no_bid_components, V) -> parse_num(V);
decode_typed_field(country, V) -> V;
decode_typed_field(tot_no_strikes, V) -> parse_num(V);
decode_typed_field(price_type, V) -> parse_num(V);
decode_typed_field(day_order_qty, V) -> parse_num(V);
decode_typed_field(day_cum_qty, V) -> parse_num(V);
decode_typed_field(day_avg_px, V) -> parse_num(V)*1.0;
decode_typed_field(gt_booking_inst, V) -> parse_num(V);
decode_typed_field(no_strikes, V) -> parse_num(V);
decode_typed_field(list_status_type, V) -> parse_num(V);
decode_typed_field(net_gross_ind, V) -> parse_num(V);
decode_typed_field(list_order_status, V) -> parse_num(V);
decode_typed_field(expire_date, V) -> V;
decode_typed_field(list_exec_inst_type, V) -> V;
decode_typed_field(cxl_rej_response_to, V) -> V;
decode_typed_field(underlying_coupon_rate, V) -> V;
decode_typed_field(underlying_contract_multiplier, V) -> V;
decode_typed_field(contra_trade_qty, V) -> parse_num(V);
decode_typed_field(contra_trade_time, V) -> V;
decode_typed_field(clearing_firm, V) -> V;
decode_typed_field(clearing_account, V) -> V;
decode_typed_field(liquidity_num_securities, V) -> parse_num(V);
decode_typed_field(multi_leg_reporting_type, V) -> V;
decode_typed_field(strike_time, V) -> V;
decode_typed_field(list_status_text, V) -> V;
decode_typed_field(encoded_list_status_text_len, V) -> parse_num(V);
decode_typed_field(encoded_list_status_text, V) -> V;
decode_typed_field(party_id_source, V) -> V;
decode_typed_field(party_id, V) -> V;
decode_typed_field(total_volume_traded_date, V) -> V;
decode_typed_field(total_volume_traded_time, V) -> V;
decode_typed_field(net_chg_prev_day, V) -> V;
decode_typed_field(party_role, V) -> parse_num(V);
decode_typed_field(no_party_ids, V) -> parse_num(V);
decode_typed_field(no_security_alt_id, V) -> parse_num(V);
decode_typed_field(security_alt_id, V) -> V;
decode_typed_field(security_alt_id_source, V) -> V;
decode_typed_field(no_underlying_security_alt_id, V) -> parse_num(V);
decode_typed_field(underlying_security_alt_id, V) -> V;
decode_typed_field(underlying_security_alt_id_source, V) -> V;
decode_typed_field(product, V) -> parse_num(V);
decode_typed_field(cfi_code, V) -> V;
decode_typed_field(underlying_product, V) -> parse_num(V);
decode_typed_field(underlying_cfi_code, V) -> V;
decode_typed_field(test_message_indicator, V) -> V == <<"Y">>;
decode_typed_field(quantity_type, V) -> parse_num(V);
decode_typed_field(booking_ref_id, V) -> V;
decode_typed_field(individual_alloc_id, V) -> V;
decode_typed_field(rounding_direction, V) -> V;
decode_typed_field(rounding_modulus, V) -> V;
decode_typed_field(country_of_issue, V) -> V;
decode_typed_field(state_or_province_of_issue, V) -> V;
decode_typed_field(locale_of_issue, V) -> V;
decode_typed_field(no_regist_dtls, V) -> parse_num(V);
decode_typed_field(mailing_dtls, V) -> V;
decode_typed_field(investor_country_of_residence, V) -> V;
decode_typed_field(payment_ref, V) -> V;
decode_typed_field(distrib_payment_method, V) -> parse_num(V);
decode_typed_field(cash_distrib_curr, V) -> V;
decode_typed_field(comm_currency, V) -> V;
decode_typed_field(cancellation_rights, V) -> V;
decode_typed_field(money_laundering_status, V) -> V;
decode_typed_field(mailing_inst, V) -> V;
decode_typed_field(trans_bkd_time, V) -> V;
decode_typed_field(exec_price_type, V) -> V;
decode_typed_field(exec_price_adjustment, V) -> V;
decode_typed_field(date_of_birth, V) -> V;
decode_typed_field(trade_report_trans_type, V) -> parse_num(V);
decode_typed_field(card_holder_name, V) -> V;
decode_typed_field(card_number, V) -> V;
decode_typed_field(card_exp_date, V) -> V;
decode_typed_field(card_iss_num, V) -> V;
decode_typed_field(payment_method, V) -> parse_num(V);
decode_typed_field(regist_acct_type, V) -> V;
decode_typed_field(designation, V) -> V;
decode_typed_field(tax_advantage_type, V) -> parse_num(V);
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
decode_typed_field(regist_rej_reason_code, V) -> parse_num(V);
decode_typed_field(regist_ref_id, V) -> V;
decode_typed_field(regist_dtls, V) -> V;
decode_typed_field(no_distrib_insts, V) -> parse_num(V);
decode_typed_field(regist_email, V) -> V;
decode_typed_field(distrib_percentage, V) -> V;
decode_typed_field(regist_id, V) -> V;
decode_typed_field(regist_trans_type, V) -> V;
decode_typed_field(exec_valuation_point, V) -> V;
decode_typed_field(order_percent, V) -> V;
decode_typed_field(ownership_type, V) -> V;
decode_typed_field(no_cont_amts, V) -> parse_num(V);
decode_typed_field(cont_amt_type, V) -> parse_num(V);
decode_typed_field(cont_amt_value, V) -> V;
decode_typed_field(cont_amt_curr, V) -> V;
decode_typed_field(owner_type, V) -> parse_num(V);
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
decode_typed_field(total_affected_orders, V) -> parse_num(V);
decode_typed_field(no_affected_orders, V) -> parse_num(V);
decode_typed_field(affected_order_id, V) -> V;
decode_typed_field(affected_secondary_order_id, V) -> V;
decode_typed_field(quote_type, V) -> parse_num(V);
decode_typed_field(nested_party_role, V) -> parse_num(V);
decode_typed_field(no_nested_party_ids, V) -> parse_num(V);
decode_typed_field(total_accrued_interest_amt, V) -> V;
decode_typed_field(maturity_date, V) -> V;
decode_typed_field(underlying_maturity_date, V) -> V;
decode_typed_field(instr_registry, V) -> V;
decode_typed_field(cash_margin, V) -> V;
decode_typed_field(nested_party_sub_id, V) -> V;
decode_typed_field(scope, V) -> V;
decode_typed_field(md_implicit_delete, V) -> V == <<"Y">>;
decode_typed_field(cross_id, V) -> V;
decode_typed_field(cross_type, V) -> parse_num(V);
decode_typed_field(cross_prioritization, V) -> parse_num(V);
decode_typed_field(orig_cross_id, V) -> V;
decode_typed_field(no_sides, V) -> parse_num(V);
decode_typed_field(username, V) -> V;
decode_typed_field(password, V) -> V;
decode_typed_field(no_legs, V) -> parse_num(V);
decode_typed_field(leg_currency, V) -> V;
decode_typed_field(tot_no_security_types, V) -> parse_num(V);
decode_typed_field(no_security_types, V) -> parse_num(V);
decode_typed_field(security_list_request_type, V) -> parse_num(V);
decode_typed_field(security_request_result, V) -> parse_num(V);
decode_typed_field(round_lot, V) -> parse_num(V);
decode_typed_field(min_trade_vol, V) -> parse_num(V);
decode_typed_field(multi_leg_rpt_type_req, V) -> parse_num(V);
decode_typed_field(leg_position_effect, V) -> V;
decode_typed_field(leg_covered_or_uncovered, V) -> parse_num(V);
decode_typed_field(leg_price, V) -> parse_num(V)*1.0;
decode_typed_field(trad_ses_status_rej_reason, V) -> parse_num(V);
decode_typed_field(trade_request_id, V) -> V;
decode_typed_field(trade_request_type, V) -> parse_num(V);
decode_typed_field(previously_reported, V) -> V == <<"Y">>;
decode_typed_field(trade_report_id, V) -> V;
decode_typed_field(trade_report_ref_id, V) -> V;
decode_typed_field(match_status, V) -> V;
decode_typed_field(match_type, V) -> V;
decode_typed_field(odd_lot, V) -> V == <<"Y">>;
decode_typed_field(no_clearing_instructions, V) -> parse_num(V);
decode_typed_field(clearing_instruction, V) -> parse_num(V);
decode_typed_field(trade_input_source, V) -> V;
decode_typed_field(trade_input_device, V) -> V;
decode_typed_field(no_dates, V) -> parse_num(V);
decode_typed_field(account_type, V) -> parse_num(V);
decode_typed_field(cust_order_capacity, V) -> parse_num(V);
decode_typed_field(cl_ord_link_id, V) -> V;
decode_typed_field(mass_status_req_id, V) -> V;
decode_typed_field(mass_status_req_type, V) -> parse_num(V);
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
decode_typed_field(leg_product, V) -> parse_num(V);
decode_typed_field(leg_cfi_code, V) -> V;
decode_typed_field(leg_security_type, V) -> V;
decode_typed_field(leg_maturity_month_year, V) -> V;
decode_typed_field(leg_maturity_date, V) -> V;
decode_typed_field(leg_strike_price, V) -> parse_num(V)*1.0;
decode_typed_field(leg_opt_attribute, V) -> V;
decode_typed_field(leg_contract_multiplier, V) -> V;
decode_typed_field(leg_coupon_rate, V) -> V;
decode_typed_field(leg_security_exchange, V) -> V;
decode_typed_field(leg_issuer, V) -> V;
decode_typed_field(encoded_leg_issuer_len, V) -> parse_num(V);
decode_typed_field(encoded_leg_issuer, V) -> V;
decode_typed_field(leg_security_desc, V) -> V;
decode_typed_field(encoded_leg_security_desc_len, V) -> parse_num(V);
decode_typed_field(encoded_leg_security_desc, V) -> V;
decode_typed_field(leg_ratio_qty, V) -> V;
decode_typed_field(leg_side, V) -> V;
decode_typed_field(trading_session_sub_id, V) -> V;
decode_typed_field(alloc_type, V) -> parse_num(V);
decode_typed_field(no_hops, V) -> parse_num(V);
decode_typed_field(hop_comp_id, V) -> V;
decode_typed_field(hop_sending_time, V) -> V;
decode_typed_field(hop_ref_id, V) -> parse_num(V);
decode_typed_field(mid_px, V) -> parse_num(V)*1.0;
decode_typed_field(bid_yield, V) -> V;
decode_typed_field(mid_yield, V) -> V;
decode_typed_field(offer_yield, V) -> V;
decode_typed_field(clearing_fee_indicator, V) -> V;
decode_typed_field(working_indicator, V) -> V == <<"Y">>;
decode_typed_field(leg_last_px, V) -> parse_num(V)*1.0;
decode_typed_field(priority_indicator, V) -> parse_num(V);
decode_typed_field(price_improvement, V) -> V;
decode_typed_field(price2, V) -> parse_num(V)*1.0;
decode_typed_field(last_forward_points2, V) -> V;
decode_typed_field(bid_forward_points2, V) -> V;
decode_typed_field(offer_forward_points2, V) -> V;
decode_typed_field(rfq_req_id, V) -> V;
decode_typed_field(mkt_bid_px, V) -> parse_num(V)*1.0;
decode_typed_field(mkt_offer_px, V) -> parse_num(V)*1.0;
decode_typed_field(min_bid_size, V) -> parse_num(V);
decode_typed_field(min_offer_size, V) -> parse_num(V);
decode_typed_field(quote_status_req_id, V) -> V;
decode_typed_field(legal_confirm, V) -> V == <<"Y">>;
decode_typed_field(underlying_last_px, V) -> parse_num(V)*1.0;
decode_typed_field(underlying_last_qty, V) -> parse_num(V);
decode_typed_field(sec_def_status, V) -> parse_num(V);
decode_typed_field(leg_ref_id, V) -> V;
decode_typed_field(contra_leg_ref_id, V) -> V;
decode_typed_field(settl_curr_bid_fx_rate, V) -> V;
decode_typed_field(settl_curr_offer_fx_rate, V) -> V;
decode_typed_field(quote_request_reject_reason, V) -> parse_num(V);
decode_typed_field(side_compliance_id, V) -> V;
decode_typed_field(acct_id_source, V) -> parse_num(V);
decode_typed_field(alloc_acct_id_source, V) -> parse_num(V);
decode_typed_field(benchmark_price, V) -> parse_num(V)*1.0;
decode_typed_field(benchmark_price_type, V) -> parse_num(V);
decode_typed_field(confirm_id, V) -> V;
decode_typed_field(confirm_status, V) -> parse_num(V);
decode_typed_field(confirm_trans_type, V) -> parse_num(V);
decode_typed_field(contract_settl_month, V) -> V;
decode_typed_field(delivery_form, V) -> parse_num(V);
decode_typed_field(last_par_px, V) -> parse_num(V)*1.0;
decode_typed_field(no_leg_allocs, V) -> parse_num(V);
decode_typed_field(leg_alloc_account, V) -> V;
decode_typed_field(leg_individual_alloc_id, V) -> V;
decode_typed_field(leg_alloc_qty, V) -> parse_num(V);
decode_typed_field(leg_alloc_acct_id_source, V) -> V;
decode_typed_field(leg_settl_currency, V) -> V;
decode_typed_field(leg_benchmark_curve_currency, V) -> V;
decode_typed_field(leg_benchmark_curve_name, V) -> V;
decode_typed_field(leg_benchmark_curve_point, V) -> V;
decode_typed_field(leg_benchmark_price, V) -> parse_num(V)*1.0;
decode_typed_field(leg_benchmark_price_type, V) -> parse_num(V);
decode_typed_field(leg_bid_px, V) -> parse_num(V)*1.0;
decode_typed_field(leg_ioi_qty, V) -> V;
decode_typed_field(no_leg_stipulations, V) -> parse_num(V);
decode_typed_field(leg_offer_px, V) -> parse_num(V)*1.0;
decode_typed_field(leg_order_qty, V) -> parse_num(V);
decode_typed_field(leg_price_type, V) -> parse_num(V);
decode_typed_field(leg_qty, V) -> parse_num(V);
decode_typed_field(leg_stipulation_type, V) -> V;
decode_typed_field(leg_stipulation_value, V) -> V;
decode_typed_field(leg_swap_type, V) -> parse_num(V);
decode_typed_field(pool, V) -> V;
decode_typed_field(quote_price_type, V) -> parse_num(V);
decode_typed_field(quote_resp_id, V) -> V;
decode_typed_field(quote_resp_type, V) -> parse_num(V);
decode_typed_field(quote_qualifier, V) -> V;
decode_typed_field(yield_redemption_date, V) -> V;
decode_typed_field(yield_redemption_price, V) -> parse_num(V)*1.0;
decode_typed_field(yield_redemption_price_type, V) -> parse_num(V);
decode_typed_field(benchmark_security_id, V) -> V;
decode_typed_field(reversal_indicator, V) -> V == <<"Y">>;
decode_typed_field(yield_calc_date, V) -> V;
decode_typed_field(no_positions, V) -> parse_num(V);
decode_typed_field(pos_type, V) -> V;
decode_typed_field(long_qty, V) -> parse_num(V);
decode_typed_field(short_qty, V) -> parse_num(V);
decode_typed_field(pos_qty_status, V) -> parse_num(V);
decode_typed_field(pos_amt_type, V) -> V;
decode_typed_field(pos_amt, V) -> V;
decode_typed_field(pos_trans_type, V) -> parse_num(V);
decode_typed_field(pos_req_id, V) -> V;
decode_typed_field(no_underlyings, V) -> parse_num(V);
decode_typed_field(pos_maint_action, V) -> parse_num(V);
decode_typed_field(orig_pos_req_ref_id, V) -> V;
decode_typed_field(pos_maint_rpt_ref_id, V) -> V;
decode_typed_field(clearing_business_date, V) -> V;
decode_typed_field(settl_sess_id, V) -> V;
decode_typed_field(settl_sess_sub_id, V) -> V;
decode_typed_field(adjustment_type, V) -> parse_num(V);
decode_typed_field(contrary_instruction_indicator, V) -> V == <<"Y">>;
decode_typed_field(prior_spread_indicator, V) -> V == <<"Y">>;
decode_typed_field(pos_maint_rpt_id, V) -> V;
decode_typed_field(pos_maint_status, V) -> parse_num(V);
decode_typed_field(pos_maint_result, V) -> parse_num(V);
decode_typed_field(pos_req_type, V) -> parse_num(V);
decode_typed_field(response_transport_type, V) -> parse_num(V);
decode_typed_field(response_destination, V) -> V;
decode_typed_field(total_num_pos_reports, V) -> parse_num(V);
decode_typed_field(pos_req_result, V) -> parse_num(V);
decode_typed_field(pos_req_status, V) -> parse_num(V);
decode_typed_field(settl_price, V) -> parse_num(V)*1.0;
decode_typed_field(settl_price_type, V) -> parse_num(V);
decode_typed_field(underlying_settl_price, V) -> parse_num(V)*1.0;
decode_typed_field(underlying_settl_price_type, V) -> parse_num(V);
decode_typed_field(prior_settl_price, V) -> parse_num(V)*1.0;
decode_typed_field(no_quote_qualifiers, V) -> parse_num(V);
decode_typed_field(alloc_settl_currency, V) -> V;
decode_typed_field(alloc_settl_curr_amt, V) -> V;
decode_typed_field(interest_at_maturity, V) -> V;
decode_typed_field(leg_dated_date, V) -> V;
decode_typed_field(leg_pool, V) -> V;
decode_typed_field(alloc_interest_at_maturity, V) -> V;
decode_typed_field(alloc_accrued_interest_amt, V) -> V;
decode_typed_field(delivery_date, V) -> V;
decode_typed_field(assignment_method, V) -> V;
decode_typed_field(assignment_unit, V) -> parse_num(V);
decode_typed_field(open_interest, V) -> V;
decode_typed_field(exercise_method, V) -> V;
decode_typed_field(tot_num_trade_reports, V) -> parse_num(V);
decode_typed_field(trade_request_result, V) -> parse_num(V);
decode_typed_field(trade_request_status, V) -> parse_num(V);
decode_typed_field(trade_report_reject_reason, V) -> parse_num(V);
decode_typed_field(side_multi_leg_reporting_type, V) -> parse_num(V);
decode_typed_field(no_pos_amt, V) -> parse_num(V);
decode_typed_field(auto_accept_indicator, V) -> V == <<"Y">>;
decode_typed_field(alloc_report_id, V) -> V;
decode_typed_field(no_nested2_party_ids, V) -> parse_num(V);
decode_typed_field(nested2_party_id, V) -> V;
decode_typed_field(nested2_party_id_source, V) -> V;
decode_typed_field(nested2_party_role, V) -> parse_num(V);
decode_typed_field(nested2_party_sub_id, V) -> V;
decode_typed_field(benchmark_security_id_source, V) -> V;
decode_typed_field(security_sub_type, V) -> V;
decode_typed_field(underlying_security_sub_type, V) -> V;
decode_typed_field(leg_security_sub_type, V) -> V;
decode_typed_field(allowable_one_sidedness_pct, V) -> V;
decode_typed_field(allowable_one_sidedness_value, V) -> V;
decode_typed_field(allowable_one_sidedness_curr, V) -> V;
decode_typed_field(no_trd_reg_timestamps, V) -> parse_num(V);
decode_typed_field(trd_reg_timestamp, V) -> V;
decode_typed_field(trd_reg_timestamp_type, V) -> parse_num(V);
decode_typed_field(trd_reg_timestamp_origin, V) -> V;
decode_typed_field(confirm_ref_id, V) -> V;
decode_typed_field(confirm_type, V) -> parse_num(V);
decode_typed_field(confirm_rej_reason, V) -> parse_num(V);
decode_typed_field(booking_type, V) -> parse_num(V);
decode_typed_field(individual_alloc_rej_code, V) -> parse_num(V);
decode_typed_field(settl_inst_msg_id, V) -> V;
decode_typed_field(no_settl_inst, V) -> parse_num(V);
decode_typed_field(last_update_time, V) -> V;
decode_typed_field(alloc_settl_inst_type, V) -> parse_num(V);
decode_typed_field(no_settl_party_ids, V) -> parse_num(V);
decode_typed_field(settl_party_id, V) -> V;
decode_typed_field(settl_party_id_source, V) -> V;
decode_typed_field(settl_party_role, V) -> parse_num(V);
decode_typed_field(settl_party_sub_id, V) -> V;
decode_typed_field(settl_party_sub_id_type, V) -> parse_num(V);
decode_typed_field(dlvy_inst_type, V) -> V;
decode_typed_field(termination_type, V) -> parse_num(V);
decode_typed_field(next_expected_msg_seq_num, V) -> parse_num(V);
decode_typed_field(ord_status_req_id, V) -> V;
decode_typed_field(settl_inst_req_id, V) -> V;
decode_typed_field(settl_inst_req_rej_code, V) -> parse_num(V);
decode_typed_field(secondary_alloc_id, V) -> V;
decode_typed_field(alloc_report_type, V) -> parse_num(V);
decode_typed_field(alloc_report_ref_id, V) -> V;
decode_typed_field(alloc_canc_replace_reason, V) -> parse_num(V);
decode_typed_field(copy_msg_indicator, V) -> V == <<"Y">>;
decode_typed_field(alloc_account_type, V) -> parse_num(V);
decode_typed_field(order_avg_px, V) -> parse_num(V)*1.0;
decode_typed_field(order_booking_qty, V) -> parse_num(V);
decode_typed_field(no_settl_party_sub_ids, V) -> parse_num(V);
decode_typed_field(no_party_sub_ids, V) -> parse_num(V);
decode_typed_field(party_sub_id_type, V) -> parse_num(V);
decode_typed_field(no_nested_party_sub_ids, V) -> parse_num(V);
decode_typed_field(nested_party_sub_id_type, V) -> parse_num(V);
decode_typed_field(no_nested2_party_sub_ids, V) -> parse_num(V);
decode_typed_field(nested2_party_sub_id_type, V) -> parse_num(V);
decode_typed_field(alloc_intermed_req_type, V) -> parse_num(V);
decode_typed_field(underlying_px, V) -> parse_num(V)*1.0;
decode_typed_field(price_delta, V) -> V;
decode_typed_field(appl_queue_max, V) -> parse_num(V);
decode_typed_field(appl_queue_depth, V) -> parse_num(V);
decode_typed_field(appl_queue_resolution, V) -> parse_num(V);
decode_typed_field(appl_queue_action, V) -> parse_num(V);
decode_typed_field(no_alt_md_source, V) -> parse_num(V);
decode_typed_field(alt_md_source_id, V) -> V;
decode_typed_field(secondary_trade_report_id, V) -> V;
decode_typed_field(avg_px_indicator, V) -> parse_num(V);
decode_typed_field(trade_link_id, V) -> V;
decode_typed_field(order_input_device, V) -> V;
decode_typed_field(underlying_trading_session_id, V) -> V;
decode_typed_field(underlying_trading_session_sub_id, V) -> V;
decode_typed_field(trade_leg_ref_id, V) -> V;
decode_typed_field(exchange_rule, V) -> V;
decode_typed_field(trade_alloc_indicator, V) -> parse_num(V);
decode_typed_field(expiration_cycle, V) -> parse_num(V);
decode_typed_field(trd_type, V) -> parse_num(V);
decode_typed_field(trd_sub_type, V) -> parse_num(V);
decode_typed_field(transfer_reason, V) -> V;
decode_typed_field(asgn_req_id, V) -> V;
decode_typed_field(tot_num_assignment_reports, V) -> parse_num(V);
decode_typed_field(asgn_rpt_id, V) -> V;
decode_typed_field(threshold_amount, V) -> V;
decode_typed_field(peg_move_type, V) -> parse_num(V);
decode_typed_field(peg_offset_type, V) -> parse_num(V);
decode_typed_field(peg_limit_type, V) -> parse_num(V);
decode_typed_field(peg_round_direction, V) -> parse_num(V);
decode_typed_field(pegged_price, V) -> parse_num(V)*1.0;
decode_typed_field(peg_scope, V) -> parse_num(V);
decode_typed_field(discretion_move_type, V) -> parse_num(V);
decode_typed_field(discretion_offset_type, V) -> parse_num(V);
decode_typed_field(discretion_limit_type, V) -> parse_num(V);
decode_typed_field(discretion_round_direction, V) -> parse_num(V);
decode_typed_field(discretion_price, V) -> parse_num(V)*1.0;
decode_typed_field(discretion_scope, V) -> parse_num(V);
decode_typed_field(target_strategy, V) -> parse_num(V);
decode_typed_field(target_strategy_parameters, V) -> V;
decode_typed_field(participation_rate, V) -> V;
decode_typed_field(target_strategy_performance, V) -> V;
decode_typed_field(last_liquidity_ind, V) -> parse_num(V);
decode_typed_field(publish_trd_indicator, V) -> V == <<"Y">>;
decode_typed_field(short_sale_reason, V) -> parse_num(V);
decode_typed_field(qty_type, V) -> parse_num(V);
decode_typed_field(secondary_trd_type, V) -> parse_num(V);
decode_typed_field(trade_report_type, V) -> parse_num(V);
decode_typed_field(alloc_no_orders_type, V) -> parse_num(V);
decode_typed_field(shared_commission, V) -> V;
decode_typed_field(confirm_req_id, V) -> V;
decode_typed_field(avg_par_px, V) -> parse_num(V)*1.0;
decode_typed_field(reported_px, V) -> parse_num(V)*1.0;
decode_typed_field(no_capacities, V) -> parse_num(V);
decode_typed_field(order_capacity_qty, V) -> parse_num(V);
decode_typed_field(no_events, V) -> parse_num(V);
decode_typed_field(event_type, V) -> parse_num(V);
decode_typed_field(event_date, V) -> V;
decode_typed_field(event_px, V) -> parse_num(V)*1.0;
decode_typed_field(event_text, V) -> V;
decode_typed_field(pct_at_risk, V) -> V;
decode_typed_field(no_instr_attrib, V) -> parse_num(V);
decode_typed_field(instr_attrib_type, V) -> parse_num(V);
decode_typed_field(instr_attrib_value, V) -> V;
decode_typed_field(dated_date, V) -> V;
decode_typed_field(interest_accrual_date, V) -> V;
decode_typed_field(cp_program, V) -> parse_num(V);
decode_typed_field(cp_reg_type, V) -> V;
decode_typed_field(underlying_cp_program, V) -> V;
decode_typed_field(underlying_cp_reg_type, V) -> V;
decode_typed_field(underlying_qty, V) -> parse_num(V);
decode_typed_field(trd_match_id, V) -> V;
decode_typed_field(secondary_trade_report_ref_id, V) -> V;
decode_typed_field(underlying_dirty_price, V) -> parse_num(V)*1.0;
decode_typed_field(underlying_end_price, V) -> parse_num(V)*1.0;
decode_typed_field(underlying_start_value, V) -> V;
decode_typed_field(underlying_current_value, V) -> V;
decode_typed_field(underlying_end_value, V) -> V;
decode_typed_field(no_underlying_stips, V) -> parse_num(V);
decode_typed_field(underlying_stip_type, V) -> V;
decode_typed_field(underlying_stip_value, V) -> V;
decode_typed_field(maturity_net_money, V) -> V;
decode_typed_field(misc_fee_basis, V) -> parse_num(V);
decode_typed_field(tot_no_allocs, V) -> parse_num(V);
decode_typed_field(last_fragment, V) -> V == <<"Y">>;
decode_typed_field(coll_req_id, V) -> V;
decode_typed_field(coll_asgn_reason, V) -> parse_num(V);
decode_typed_field(coll_inquiry_qualifier, V) -> parse_num(V);
decode_typed_field(no_trades, V) -> parse_num(V);
decode_typed_field(margin_ratio, V) -> V;
decode_typed_field(margin_excess, V) -> V;
decode_typed_field(total_net_value, V) -> V;
decode_typed_field(cash_outstanding, V) -> V;
decode_typed_field(coll_asgn_id, V) -> V;
decode_typed_field(coll_asgn_trans_type, V) -> parse_num(V);
decode_typed_field(coll_resp_id, V) -> V;
decode_typed_field(coll_asgn_resp_type, V) -> parse_num(V);
decode_typed_field(coll_asgn_reject_reason, V) -> parse_num(V);
decode_typed_field(coll_asgn_ref_id, V) -> V;
decode_typed_field(coll_rpt_id, V) -> V;
decode_typed_field(coll_inquiry_id, V) -> V;
decode_typed_field(coll_status, V) -> parse_num(V);
decode_typed_field(tot_num_reports, V) -> parse_num(V);
decode_typed_field(last_rpt_requested, V) -> V == <<"Y">>;
decode_typed_field(agreement_desc, V) -> V;
decode_typed_field(agreement_id, V) -> V;
decode_typed_field(agreement_date, V) -> V;
decode_typed_field(start_date, V) -> V;
decode_typed_field(end_date, V) -> V;
decode_typed_field(agreement_currency, V) -> V;
decode_typed_field(delivery_type, V) -> parse_num(V);
decode_typed_field(end_accrued_interest_amt, V) -> V;
decode_typed_field(start_cash, V) -> V;
decode_typed_field(end_cash, V) -> V;
decode_typed_field(user_request_id, V) -> V;
decode_typed_field(user_request_type, V) -> parse_num(V);
decode_typed_field(new_password, V) -> V;
decode_typed_field(user_status, V) -> parse_num(V);
decode_typed_field(user_status_text, V) -> V;
decode_typed_field(status_value, V) -> parse_num(V);
decode_typed_field(status_text, V) -> V;
decode_typed_field(ref_comp_id, V) -> V;
decode_typed_field(ref_sub_id, V) -> V;
decode_typed_field(network_response_id, V) -> V;
decode_typed_field(network_request_id, V) -> V;
decode_typed_field(last_network_response_id, V) -> V;
decode_typed_field(network_request_type, V) -> parse_num(V);
decode_typed_field(no_comp_ids, V) -> parse_num(V);
decode_typed_field(network_status_response_type, V) -> parse_num(V);
decode_typed_field(no_coll_inquiry_qualifier, V) -> parse_num(V);
decode_typed_field(trd_rpt_status, V) -> parse_num(V);
decode_typed_field(affirm_status, V) -> parse_num(V);
decode_typed_field(underlying_strike_currency, V) -> V;
decode_typed_field(leg_strike_currency, V) -> V;
decode_typed_field(time_bracket, V) -> V;
decode_typed_field(coll_action, V) -> parse_num(V);
decode_typed_field(coll_inquiry_status, V) -> parse_num(V);
decode_typed_field(coll_inquiry_result, V) -> parse_num(V);
decode_typed_field(strike_currency, V) -> V;
decode_typed_field(no_nested3_party_ids, V) -> parse_num(V);
decode_typed_field(nested3_party_id, V) -> V;
decode_typed_field(nested3_party_id_source, V) -> V;
decode_typed_field(nested3_party_role, V) -> parse_num(V);
decode_typed_field(no_nested3_party_sub_ids, V) -> parse_num(V);
decode_typed_field(nested3_party_sub_id, V) -> V;
decode_typed_field(nested3_party_sub_id_type, V) -> parse_num(V);
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

parse_num(Bin) -> parse_num_erl(Bin).

parse_num_erl(Bin) -> parse_num(Bin, 0, 0).
parse_num(<<$., Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*1.0, 0.1);
parse_num(<<X, Bin/binary>>, Acc, 0) -> parse_num(Bin, Acc*10 + X - $0, 0);
parse_num(<<X, Bin/binary>>, Acc, Coeff) -> parse_num(Bin, Acc + (X - $0)*Coeff, Coeff*0.1);
parse_num(<<>>, Acc, _) -> Acc.

