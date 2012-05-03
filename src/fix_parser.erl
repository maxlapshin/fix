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
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{test_req_id, 6},{signature, 7}], 8),
  Message1.

decode_message_test_request(Message, #test_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{test_req_id, 6},{signature, 7}], 8),
  Message1.

decode_message_resend_request(Message, #resend_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{begin_seq_no, 6},{end_seq_no, 7},{signature, 8}], 9),
  Message1.

decode_message_reject(Message, #reject{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{ref_seq_num, 6},{ref_tag_id, 7},{ref_msg_type, 8},{session_reject_reason, 9},{text, 10},{encoded_text, 11},{signature, 12}], 13),
  Message1.

decode_message_sequence_reset(Message, #sequence_reset{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{gap_fill_flag, 6},{new_seq_no, 7},{signature, 8}], 9),
  Message1.

decode_message_logout(Message, #logout{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{text, 6},{encoded_text, 7},{signature, 8}], 9),
  Message1.

decode_message_ioi(Message, #ioi{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{ioi_id, 6},{ioi_trans_type, 7},{ioi_ref_id, 8},{side, 9},{qty_type, 10},{ioi_qty, 11},{currency, 12},{leg_ioi_qty, 13},{price_type, 14},{price, 15},{valid_until_time, 16},{ioi_qlty_ind, 17},{ioi_natural_flag, 18},{ioi_qualifier, 19},{text, 20},{encoded_text, 21},{transact_time, 22},{url_link, 23},{routing_type, 24},{routing_id, 25},{signature, 26}], 27),
  Message1.

decode_message_advertisement(Message, #advertisement{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{adv_id, 6},{adv_trans_type, 7},{adv_ref_id, 8},{adv_side, 9},{quantity, 10},{qty_type, 11},{price, 12},{currency, 13},{trade_date, 14},{transact_time, 15},{text, 16},{encoded_text, 17},{url_link, 18},{last_mkt, 19},{trading_session_id, 20},{trading_session_sub_id, 21},{signature, 22}], 23),
  Message1.

decode_message_execution_report(Message, #execution_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{secondary_order_id, 7},{secondary_cl_ord_id, 8},{secondary_exec_id, 9},{cl_ord_id, 10},{orig_cl_ord_id, 11},{cl_ord_link_id, 12},{quote_resp_id, 13},{ord_status_req_id, 14},{mass_status_req_id, 15},{tot_num_reports, 16},{last_rpt_requested, 17},{trade_origination_date, 18},{contra_broker, 19},{contra_trader, 20},{contra_trade_qty, 21},{contra_trade_time, 22},{contra_leg_ref_id, 23},{list_id, 24},{cross_id, 25},{orig_cross_id, 26},{cross_type, 27},{exec_id, 28},{exec_ref_id, 29},{exec_type, 30},{ord_status, 31},{working_indicator, 32},{ord_rej_reason, 33},{exec_restatement_reason, 34},{account, 35},{acct_id_source, 36},{account_type, 37},{day_booking_inst, 38},{booking_unit, 39},{prealloc_method, 40},{settl_type, 41},{settl_date, 42},{cash_margin, 43},{clearing_fee_indicator, 44},{side, 45},{qty_type, 46},{ord_type, 47},{price_type, 48},{price, 49},{stop_px, 50},{pegged_price, 51},{discretion_price, 52},{target_strategy, 53},{target_strategy_parameters, 54},{participation_rate, 55},{target_strategy_performance, 56},{currency, 57},{compliance_id, 58},{solicited_flag, 59},{time_in_force, 60},{effective_time, 61},{expire_date, 62},{expire_time, 63},{exec_inst, 64},{order_capacity, 65},{order_restrictions, 66},{cust_order_capacity, 67},{last_qty, 68},{underlying_last_qty, 69},{last_px, 70},{underlying_last_px, 71},{last_par_px, 72},{last_spot_rate, 73},{last_forward_points, 74},{last_mkt, 75},{trading_session_id, 76},{trading_session_sub_id, 77},{time_bracket, 78},{last_capacity, 79},{leaves_qty, 80},{cum_qty, 81},{avg_px, 82},{day_order_qty, 83},{day_cum_qty, 84},{day_avg_px, 85},{gt_booking_inst, 86},{trade_date, 87},{transact_time, 88},{report_to_exch, 89},{gross_trade_amt, 90},{num_days_interest, 91},{ex_date, 92},{accrued_interest_rate, 93},{accrued_interest_amt, 94},{interest_at_maturity, 95},{end_accrued_interest_amt, 96},{start_cash, 97},{end_cash, 98},{traded_flat_switch, 99},{basis_feature_date, 100},{basis_feature_price, 101},{concession, 102},{total_takedown, 103},{net_money, 104},{settl_curr_amt, 105},{settl_currency, 106},{settl_curr_fx_rate, 107},{settl_curr_fx_rate_calc, 108},{handl_inst, 109},{min_qty, 110},{max_floor, 111},{position_effect, 112},{max_show, 113},{booking_type, 114},{text, 115},{encoded_text, 116},{settl_date2, 117},{order_qty2, 118},{last_forward_points2, 119},{multi_leg_reporting_type, 120},{cancellation_rights, 121},{money_laundering_status, 122},{regist_id, 123},{designation, 124},{trans_bkd_time, 125},{exec_valuation_point, 126},{exec_price_type, 127},{exec_price_adjustment, 128},{priority_indicator, 129},{price_improvement, 130},{last_liquidity_ind, 131},{cont_amt_type, 132},{cont_amt_value, 133},{cont_amt_curr, 134},{leg_qty, 135},{leg_swap_type, 136},{leg_position_effect, 137},{leg_covered_or_uncovered, 138},{leg_ref_id, 139},{leg_price, 140},{leg_settl_type, 141},{leg_settl_date, 142},{leg_last_px, 143},{copy_msg_indicator, 144},{misc_fee_amt, 145},{misc_fee_curr, 146},{misc_fee_type, 147},{misc_fee_basis, 148},{signature, 149}], 150),
  Message1.

decode_message_order_cancel_reject(Message, #order_cancel_reject{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{secondary_order_id, 7},{secondary_cl_ord_id, 8},{cl_ord_id, 9},{cl_ord_link_id, 10},{orig_cl_ord_id, 11},{ord_status, 12},{working_indicator, 13},{orig_ord_mod_time, 14},{list_id, 15},{account, 16},{acct_id_source, 17},{account_type, 18},{trade_origination_date, 19},{trade_date, 20},{transact_time, 21},{cxl_rej_response_to, 22},{cxl_rej_reason, 23},{text, 24},{encoded_text, 25},{signature, 26}], 27),
  Message1.

decode_message_logon(Message, #logon{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{encrypt_method, 6},{heart_bt_int, 7},{raw_data, 8},{reset_seq_num_flag, 9},{next_expected_msg_seq_num, 10},{ref_msg_type, 11},{msg_direction, 12},{test_message_indicator, 13},{username, 14},{password, 15},{signature, 16}], 17),
  Message1.

decode_message_news(Message, #news{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{orig_time, 6},{urgency, 7},{headline, 8},{encoded_headline, 9},{routing_type, 10},{routing_id, 11},{text, 12},{encoded_text, 13},{url_link, 14},{raw_data, 15},{signature, 16}], 17),
  Message1.

decode_message_email(Message, #email{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{email_thread_id, 6},{email_type, 7},{orig_time, 8},{subject, 9},{encoded_subject, 10},{routing_type, 11},{routing_id, 12},{order_id, 13},{cl_ord_id, 14},{text, 15},{encoded_text, 16},{raw_data, 17},{signature, 18}], 19),
  Message1.

decode_message_new_order_single(Message, #new_order_single{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{cl_ord_id, 6},{secondary_cl_ord_id, 7},{cl_ord_link_id, 8},{trade_origination_date, 9},{trade_date, 10},{account, 11},{acct_id_source, 12},{account_type, 13},{day_booking_inst, 14},{booking_unit, 15},{prealloc_method, 16},{alloc_id, 17},{alloc_account, 18},{alloc_acct_id_source, 19},{alloc_settl_currency, 20},{individual_alloc_id, 21},{alloc_qty, 22},{settl_type, 23},{settl_date, 24},{cash_margin, 25},{clearing_fee_indicator, 26},{handl_inst, 27},{exec_inst, 28},{min_qty, 29},{max_floor, 30},{ex_destination, 31},{trading_session_id, 32},{trading_session_sub_id, 33},{process_code, 34},{prev_close_px, 35},{side, 36},{locate_reqd, 37},{transact_time, 38},{qty_type, 39},{ord_type, 40},{price_type, 41},{price, 42},{stop_px, 43},{currency, 44},{compliance_id, 45},{solicited_flag, 46},{ioi_id, 47},{quote_id, 48},{time_in_force, 49},{effective_time, 50},{expire_date, 51},{expire_time, 52},{gt_booking_inst, 53},{order_capacity, 54},{order_restrictions, 55},{cust_order_capacity, 56},{forex_req, 57},{settl_currency, 58},{booking_type, 59},{text, 60},{encoded_text, 61},{settl_date2, 62},{order_qty2, 63},{price2, 64},{position_effect, 65},{covered_or_uncovered, 66},{max_show, 67},{target_strategy, 68},{target_strategy_parameters, 69},{participation_rate, 70},{cancellation_rights, 71},{money_laundering_status, 72},{regist_id, 73},{designation, 74},{signature, 75}], 76),
  Message1.

decode_message_new_order_list(Message, #new_order_list{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{bid_id, 7},{client_bid_id, 8},{prog_rpt_reqs, 9},{bid_type, 10},{prog_period_interval, 11},{cancellation_rights, 12},{money_laundering_status, 13},{regist_id, 14},{list_exec_inst_type, 15},{list_exec_inst, 16},{encoded_list_exec_inst, 17},{allowable_one_sidedness_pct, 18},{allowable_one_sidedness_value, 19},{allowable_one_sidedness_curr, 20},{tot_no_orders, 21},{last_fragment, 22},{cl_ord_id, 23},{secondary_cl_ord_id, 24},{list_seq_no, 25},{cl_ord_link_id, 26},{settl_inst_mode, 27},{trade_origination_date, 28},{trade_date, 29},{account, 30},{acct_id_source, 31},{account_type, 32},{day_booking_inst, 33},{booking_unit, 34},{alloc_id, 35},{prealloc_method, 36},{alloc_account, 37},{alloc_acct_id_source, 38},{alloc_settl_currency, 39},{individual_alloc_id, 40},{alloc_qty, 41},{settl_type, 42},{settl_date, 43},{cash_margin, 44},{clearing_fee_indicator, 45},{handl_inst, 46},{exec_inst, 47},{min_qty, 48},{max_floor, 49},{ex_destination, 50},{trading_session_id, 51},{trading_session_sub_id, 52},{process_code, 53},{prev_close_px, 54},{side, 55},{side_value_ind, 56},{locate_reqd, 57},{transact_time, 58},{qty_type, 59},{ord_type, 60},{price_type, 61},{price, 62},{stop_px, 63},{currency, 64},{compliance_id, 65},{solicited_flag, 66},{ioi_id, 67},{quote_id, 68},{time_in_force, 69},{effective_time, 70},{expire_date, 71},{expire_time, 72},{gt_booking_inst, 73},{order_capacity, 74},{order_restrictions, 75},{cust_order_capacity, 76},{forex_req, 77},{settl_currency, 78},{booking_type, 79},{text, 80},{encoded_text, 81},{settl_date2, 82},{order_qty2, 83},{price2, 84},{position_effect, 85},{covered_or_uncovered, 86},{max_show, 87},{target_strategy, 88},{target_strategy_parameters, 89},{participation_rate, 90},{designation, 91},{signature, 92}], 93),
  Message1.

decode_message_order_cancel_request(Message, #order_cancel_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{orig_cl_ord_id, 6},{order_id, 7},{cl_ord_id, 8},{secondary_cl_ord_id, 9},{cl_ord_link_id, 10},{list_id, 11},{orig_ord_mod_time, 12},{account, 13},{acct_id_source, 14},{account_type, 15},{side, 16},{transact_time, 17},{compliance_id, 18},{text, 19},{encoded_text, 20},{signature, 21}], 22),
  Message1.

decode_message_order_cancel_replace_request(Message, #order_cancel_replace_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{trade_origination_date, 7},{trade_date, 8},{orig_cl_ord_id, 9},{cl_ord_id, 10},{secondary_cl_ord_id, 11},{cl_ord_link_id, 12},{list_id, 13},{orig_ord_mod_time, 14},{account, 15},{acct_id_source, 16},{account_type, 17},{day_booking_inst, 18},{booking_unit, 19},{prealloc_method, 20},{alloc_id, 21},{alloc_account, 22},{alloc_acct_id_source, 23},{alloc_settl_currency, 24},{individual_alloc_id, 25},{alloc_qty, 26},{settl_type, 27},{settl_date, 28},{cash_margin, 29},{clearing_fee_indicator, 30},{handl_inst, 31},{exec_inst, 32},{min_qty, 33},{max_floor, 34},{ex_destination, 35},{trading_session_id, 36},{trading_session_sub_id, 37},{side, 38},{transact_time, 39},{qty_type, 40},{ord_type, 41},{price_type, 42},{price, 43},{stop_px, 44},{target_strategy, 45},{target_strategy_parameters, 46},{participation_rate, 47},{compliance_id, 48},{solicited_flag, 49},{currency, 50},{time_in_force, 51},{effective_time, 52},{expire_date, 53},{expire_time, 54},{gt_booking_inst, 55},{order_capacity, 56},{order_restrictions, 57},{cust_order_capacity, 58},{forex_req, 59},{settl_currency, 60},{booking_type, 61},{text, 62},{encoded_text, 63},{settl_date2, 64},{order_qty2, 65},{price2, 66},{position_effect, 67},{covered_or_uncovered, 68},{max_show, 69},{locate_reqd, 70},{cancellation_rights, 71},{money_laundering_status, 72},{regist_id, 73},{designation, 74},{signature, 75}], 76),
  Message1.

decode_message_order_status_request(Message, #order_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{cl_ord_id, 7},{secondary_cl_ord_id, 8},{cl_ord_link_id, 9},{ord_status_req_id, 10},{account, 11},{acct_id_source, 12},{side, 13},{signature, 14}], 15),
  Message1.

decode_message_allocation_instruction(Message, #allocation_instruction{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{alloc_id, 6},{alloc_trans_type, 7},{alloc_type, 8},{secondary_alloc_id, 9},{ref_alloc_id, 10},{alloc_canc_replace_reason, 11},{alloc_intermed_req_type, 12},{alloc_link_id, 13},{alloc_link_type, 14},{booking_ref_id, 15},{alloc_no_orders_type, 16},{cl_ord_id, 17},{order_id, 18},{secondary_order_id, 19},{secondary_cl_ord_id, 20},{list_id, 21},{order_qty, 22},{order_avg_px, 23},{order_booking_qty, 24},{last_qty, 25},{exec_id, 26},{secondary_exec_id, 27},{last_px, 28},{last_par_px, 29},{last_capacity, 30},{previously_reported, 31},{reversal_indicator, 32},{match_type, 33},{side, 34},{quantity, 35},{qty_type, 36},{last_mkt, 37},{trade_origination_date, 38},{trading_session_id, 39},{trading_session_sub_id, 40},{price_type, 41},{avg_px, 42},{avg_par_px, 43},{currency, 44},{avg_px_precision, 45},{trade_date, 46},{transact_time, 47},{settl_type, 48},{settl_date, 49},{booking_type, 50},{gross_trade_amt, 51},{concession, 52},{total_takedown, 53},{net_money, 54},{position_effect, 55},{auto_accept_indicator, 56},{text, 57},{encoded_text, 58},{num_days_interest, 59},{accrued_interest_rate, 60},{accrued_interest_amt, 61},{total_accrued_interest_amt, 62},{interest_at_maturity, 63},{end_accrued_interest_amt, 64},{start_cash, 65},{end_cash, 66},{legal_confirm, 67},{tot_no_allocs, 68},{last_fragment, 69},{alloc_account, 70},{alloc_acct_id_source, 71},{match_status, 72},{alloc_price, 73},{alloc_qty, 74},{individual_alloc_id, 75},{process_code, 76},{notify_broker_of_credit, 77},{alloc_handl_inst, 78},{alloc_text, 79},{encoded_alloc_text, 80},{alloc_avg_px, 81},{alloc_net_money, 82},{settl_curr_amt, 83},{alloc_settl_curr_amt, 84},{settl_currency, 85},{alloc_settl_currency, 86},{settl_curr_fx_rate, 87},{settl_curr_fx_rate_calc, 88},{alloc_accrued_interest_amt, 89},{alloc_interest_at_maturity, 90},{misc_fee_amt, 91},{misc_fee_curr, 92},{misc_fee_type, 93},{misc_fee_basis, 94},{clearing_instruction, 95},{clearing_fee_indicator, 96},{alloc_settl_inst_type, 97},{signature, 98}], 99),
  Message1.

decode_message_list_cancel_request(Message, #list_cancel_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{transact_time, 7},{trade_origination_date, 8},{trade_date, 9},{text, 10},{encoded_text, 11},{signature, 12}], 13),
  Message1.

decode_message_list_execute(Message, #list_execute{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{client_bid_id, 7},{bid_id, 8},{transact_time, 9},{text, 10},{encoded_text, 11},{signature, 12}], 13),
  Message1.

decode_message_list_status_request(Message, #list_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{text, 7},{encoded_text, 8},{signature, 9}], 10),
  Message1.

decode_message_list_status(Message, #list_status{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{list_status_type, 7},{no_rpts, 8},{list_order_status, 9},{rpt_seq, 10},{list_status_text, 11},{encoded_list_status_text, 12},{transact_time, 13},{tot_no_orders, 14},{last_fragment, 15},{cl_ord_id, 16},{secondary_cl_ord_id, 17},{cum_qty, 18},{ord_status, 19},{working_indicator, 20},{leaves_qty, 21},{cxl_qty, 22},{avg_px, 23},{ord_rej_reason, 24},{text, 25},{encoded_text, 26},{signature, 27}], 28),
  Message1.

decode_message_allocation_instruction_ack(Message, #allocation_instruction_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{alloc_id, 6},{secondary_alloc_id, 7},{trade_date, 8},{transact_time, 9},{alloc_status, 10},{alloc_rej_code, 11},{alloc_type, 12},{alloc_intermed_req_type, 13},{match_status, 14},{product, 15},{security_type, 16},{text, 17},{encoded_text, 18},{alloc_account, 19},{alloc_acct_id_source, 20},{alloc_price, 21},{individual_alloc_id, 22},{individual_alloc_rej_code, 23},{alloc_text, 24},{encoded_alloc_text, 25},{signature, 26}], 27),
  Message1.

decode_message_dont_know_trade(Message, #dont_know_trade{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{secondary_order_id, 7},{exec_id, 8},{dk_reason, 9},{side, 10},{last_qty, 11},{last_px, 12},{text, 13},{encoded_text, 14},{signature, 15}], 16),
  Message1.

decode_message_quote_request(Message, #quote_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{rfq_req_id, 7},{cl_ord_id, 8},{order_capacity, 9},{prev_close_px, 10},{quote_request_type, 11},{quote_type, 12},{trading_session_id, 13},{trading_session_sub_id, 14},{trade_origination_date, 15},{side, 16},{qty_type, 17},{settl_type, 18},{settl_date, 19},{settl_date2, 20},{order_qty2, 21},{currency, 22},{account, 23},{acct_id_source, 24},{account_type, 25},{leg_qty, 26},{leg_swap_type, 27},{leg_settl_type, 28},{leg_settl_date, 29},{quote_qualifier, 30},{quote_price_type, 31},{ord_type, 32},{valid_until_time, 33},{expire_time, 34},{transact_time, 35},{price_type, 36},{price, 37},{price2, 38},{text, 39},{encoded_text, 40},{signature, 41}], 42),
  Message1.

decode_message_quote(Message, #quote{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{quote_id, 7},{quote_resp_id, 8},{quote_type, 9},{quote_qualifier, 10},{quote_response_level, 11},{trading_session_id, 12},{trading_session_sub_id, 13},{side, 14},{settl_type, 15},{settl_date, 16},{settl_date2, 17},{order_qty2, 18},{currency, 19},{account, 20},{acct_id_source, 21},{account_type, 22},{leg_qty, 23},{leg_swap_type, 24},{leg_settl_type, 25},{leg_settl_date, 26},{leg_price_type, 27},{leg_bid_px, 28},{leg_offer_px, 29},{bid_px, 30},{offer_px, 31},{mkt_bid_px, 32},{mkt_offer_px, 33},{min_bid_size, 34},{bid_size, 35},{min_offer_size, 36},{offer_size, 37},{valid_until_time, 38},{bid_spot_rate, 39},{offer_spot_rate, 40},{bid_forward_points, 41},{offer_forward_points, 42},{mid_px, 43},{bid_yield, 44},{mid_yield, 45},{offer_yield, 46},{transact_time, 47},{ord_type, 48},{bid_forward_points2, 49},{offer_forward_points2, 50},{settl_curr_bid_fx_rate, 51},{settl_curr_offer_fx_rate, 52},{settl_curr_fx_rate_calc, 53},{comm_type, 54},{commission, 55},{cust_order_capacity, 56},{ex_destination, 57},{order_capacity, 58},{price_type, 59},{text, 60},{encoded_text, 61},{signature, 62}], 63),
  Message1.

decode_message_settlement_instructions(Message, #settlement_instructions{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{settl_inst_msg_id, 6},{settl_inst_req_id, 7},{settl_inst_mode, 8},{settl_inst_req_rej_code, 9},{text, 10},{encoded_text, 11},{cl_ord_id, 12},{transact_time, 13},{settl_inst_id, 14},{settl_inst_trans_type, 15},{settl_inst_ref_id, 16},{side, 17},{product, 18},{security_type, 19},{cfi_code, 20},{effective_time, 21},{expire_time, 22},{last_update_time, 23},{payment_method, 24},{payment_ref, 25},{card_holder_name, 26},{card_number, 27},{card_start_date, 28},{card_exp_date, 29},{card_iss_num, 30},{payment_date, 31},{payment_remitter_id, 32},{signature, 33}], 34),
  Message1.

decode_message_market_data_request(Message, #market_data_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{md_req_id, 6},{subscription_request_type, 7},{market_depth, 8},{md_update_type, 9},{aggregated_book, 10},{open_close_settl_flag, 11},{scope, 12},{md_implicit_delete, 13},{md_entry_type, 14},{trading_session_id, 15},{trading_session_sub_id, 16},{appl_queue_action, 17},{appl_queue_max, 18},{signature, 19}], 20),
  Message1.

decode_message_market_data_snapshot_full_refresh(Message, #market_data_snapshot_full_refresh{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{md_req_id, 6},{financial_status, 7},{corporate_action, 8},{net_chg_prev_day, 9},{md_entry_type, 10},{md_entry_px, 11},{currency, 12},{md_entry_size, 13},{md_entry_date, 14},{md_entry_time, 15},{tick_direction, 16},{md_mkt, 17},{trading_session_id, 18},{trading_session_sub_id, 19},{quote_condition, 20},{trade_condition, 21},{md_entry_originator, 22},{location_id, 23},{desk_id, 24},{open_close_settl_flag, 25},{time_in_force, 26},{expire_date, 27},{expire_time, 28},{min_qty, 29},{exec_inst, 30},{seller_days, 31},{order_id, 32},{quote_entry_id, 33},{md_entry_buyer, 34},{md_entry_seller, 35},{number_of_orders, 36},{md_entry_position_no, 37},{scope, 38},{price_delta, 39},{text, 40},{encoded_text, 41},{appl_queue_depth, 42},{appl_queue_resolution, 43},{signature, 44}], 45),
  Message1.

decode_message_market_data_incremental_refresh(Message, #market_data_incremental_refresh{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{md_req_id, 6},{md_update_action, 7},{delete_reason, 8},{md_entry_type, 9},{md_entry_id, 10},{md_entry_ref_id, 11},{financial_status, 12},{corporate_action, 13},{md_entry_px, 14},{currency, 15},{md_entry_size, 16},{md_entry_date, 17},{md_entry_time, 18},{tick_direction, 19},{md_mkt, 20},{trading_session_id, 21},{trading_session_sub_id, 22},{quote_condition, 23},{trade_condition, 24},{md_entry_originator, 25},{location_id, 26},{desk_id, 27},{open_close_settl_flag, 28},{time_in_force, 29},{expire_date, 30},{expire_time, 31},{min_qty, 32},{exec_inst, 33},{seller_days, 34},{order_id, 35},{quote_entry_id, 36},{md_entry_buyer, 37},{md_entry_seller, 38},{number_of_orders, 39},{md_entry_position_no, 40},{scope, 41},{price_delta, 42},{net_chg_prev_day, 43},{text, 44},{encoded_text, 45},{appl_queue_depth, 46},{appl_queue_resolution, 47},{signature, 48}], 49),
  Message1.

decode_message_market_data_request_reject(Message, #market_data_request_reject{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{md_req_id, 6},{md_req_rej_reason, 7},{alt_md_source_id, 8},{text, 9},{encoded_text, 10},{signature, 11}], 12),
  Message1.

decode_message_quote_cancel(Message, #quote_cancel{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{quote_id, 7},{quote_cancel_type, 8},{quote_response_level, 9},{account, 10},{acct_id_source, 11},{account_type, 12},{trading_session_id, 13},{trading_session_sub_id, 14},{signature, 15}], 16),
  Message1.

decode_message_quote_status_request(Message, #quote_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_status_req_id, 6},{quote_id, 7},{account, 8},{acct_id_source, 9},{account_type, 10},{trading_session_id, 11},{trading_session_sub_id, 12},{subscription_request_type, 13},{signature, 14}], 15),
  Message1.

decode_message_mass_quote_acknowledgement(Message, #mass_quote_acknowledgement{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{quote_id, 7},{quote_status, 8},{quote_reject_reason, 9},{quote_response_level, 10},{quote_type, 11},{account, 12},{acct_id_source, 13},{account_type, 14},{text, 15},{encoded_text, 16},{quote_set_id, 17},{tot_no_quote_entries, 18},{last_fragment, 19},{quote_entry_id, 20},{bid_px, 21},{offer_px, 22},{bid_size, 23},{offer_size, 24},{valid_until_time, 25},{bid_spot_rate, 26},{offer_spot_rate, 27},{bid_forward_points, 28},{offer_forward_points, 29},{mid_px, 30},{bid_yield, 31},{mid_yield, 32},{offer_yield, 33},{transact_time, 34},{trading_session_id, 35},{trading_session_sub_id, 36},{settl_date, 37},{ord_type, 38},{settl_date2, 39},{order_qty2, 40},{bid_forward_points2, 41},{offer_forward_points2, 42},{currency, 43},{quote_entry_reject_reason, 44},{signature, 45}], 46),
  Message1.

decode_message_security_definition_request(Message, #security_definition_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_request_type, 7},{currency, 8},{text, 9},{encoded_text, 10},{trading_session_id, 11},{trading_session_sub_id, 12},{expiration_cycle, 13},{subscription_request_type, 14},{signature, 15}], 16),
  Message1.

decode_message_security_definition(Message, #security_definition{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_response_id, 7},{security_response_type, 8},{currency, 9},{trading_session_id, 10},{trading_session_sub_id, 11},{text, 12},{encoded_text, 13},{expiration_cycle, 14},{round_lot, 15},{min_trade_vol, 16},{signature, 17}], 18),
  Message1.

decode_message_security_status_request(Message, #security_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_status_req_id, 6},{currency, 7},{subscription_request_type, 8},{trading_session_id, 9},{trading_session_sub_id, 10},{signature, 11}], 12),
  Message1.

decode_message_security_status(Message, #security_status{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_status_req_id, 6},{currency, 7},{trading_session_id, 8},{trading_session_sub_id, 9},{unsolicited_indicator, 10},{security_trading_status, 11},{financial_status, 12},{corporate_action, 13},{halt_reason_char, 14},{in_view_of_common, 15},{due_to_related, 16},{buy_volume, 17},{sell_volume, 18},{high_px, 19},{low_px, 20},{last_px, 21},{transact_time, 22},{adjustment, 23},{text, 24},{encoded_text, 25},{signature, 26}], 27),
  Message1.

decode_message_trading_session_status_request(Message, #trading_session_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trad_ses_req_id, 6},{trading_session_id, 7},{trading_session_sub_id, 8},{trad_ses_method, 9},{trad_ses_mode, 10},{subscription_request_type, 11},{signature, 12}], 13),
  Message1.

decode_message_trading_session_status(Message, #trading_session_status{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trad_ses_req_id, 6},{trading_session_id, 7},{trading_session_sub_id, 8},{trad_ses_method, 9},{trad_ses_mode, 10},{unsolicited_indicator, 11},{trad_ses_status, 12},{trad_ses_status_rej_reason, 13},{trad_ses_start_time, 14},{trad_ses_open_time, 15},{trad_ses_pre_close_time, 16},{trad_ses_close_time, 17},{trad_ses_end_time, 18},{total_volume_traded, 19},{text, 20},{encoded_text, 21},{signature, 22}], 23),
  Message1.

decode_message_mass_quote(Message, #mass_quote{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{quote_id, 7},{quote_type, 8},{quote_response_level, 9},{account, 10},{acct_id_source, 11},{account_type, 12},{def_bid_size, 13},{def_offer_size, 14},{quote_set_id, 15},{quote_set_valid_until_time, 16},{tot_no_quote_entries, 17},{last_fragment, 18},{quote_entry_id, 19},{bid_px, 20},{offer_px, 21},{bid_size, 22},{offer_size, 23},{valid_until_time, 24},{bid_spot_rate, 25},{offer_spot_rate, 26},{bid_forward_points, 27},{offer_forward_points, 28},{mid_px, 29},{bid_yield, 30},{mid_yield, 31},{offer_yield, 32},{transact_time, 33},{trading_session_id, 34},{trading_session_sub_id, 35},{settl_date, 36},{ord_type, 37},{settl_date2, 38},{order_qty2, 39},{bid_forward_points2, 40},{offer_forward_points2, 41},{currency, 42},{signature, 43}], 44),
  Message1.

decode_message_business_message_reject(Message, #business_message_reject{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{ref_seq_num, 6},{ref_msg_type, 7},{business_reject_ref_id, 8},{business_reject_reason, 9},{text, 10},{encoded_text, 11},{signature, 12}], 13),
  Message1.

decode_message_bid_request(Message, #bid_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{bid_id, 6},{client_bid_id, 7},{bid_request_trans_type, 8},{list_name, 9},{tot_no_related_sym, 10},{bid_type, 11},{num_tickets, 12},{currency, 13},{side_value1, 14},{side_value2, 15},{bid_descriptor_type, 16},{bid_descriptor, 17},{side_value_ind, 18},{liquidity_value, 19},{liquidity_num_securities, 20},{liquidity_pct_low, 21},{liquidity_pct_high, 22},{efp_tracking_error, 23},{fair_value, 24},{outside_index_pct, 25},{value_of_futures, 26},{list_id, 27},{side, 28},{trading_session_id, 29},{trading_session_sub_id, 30},{net_gross_ind, 31},{settl_type, 32},{settl_date, 33},{account, 34},{acct_id_source, 35},{liquidity_ind_type, 36},{wt_average_liquidity, 37},{exchange_for_physical, 38},{out_main_cntry_u_index, 39},{cross_percent, 40},{prog_rpt_reqs, 41},{prog_period_interval, 42},{inc_tax_ind, 43},{forex_req, 44},{num_bidders, 45},{trade_date, 46},{bid_trade_type, 47},{basis_px_type, 48},{strike_time, 49},{text, 50},{encoded_text, 51},{signature, 52}], 53),
  Message1.

decode_message_bid_response(Message, #bid_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{bid_id, 6},{client_bid_id, 7},{list_id, 8},{country, 9},{side, 10},{price, 11},{price_type, 12},{fair_value, 13},{net_gross_ind, 14},{settl_type, 15},{settl_date, 16},{trading_session_id, 17},{trading_session_sub_id, 18},{text, 19},{encoded_text, 20},{signature, 21}], 22),
  Message1.

decode_message_list_strike_price(Message, #list_strike_price{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{list_id, 6},{tot_no_strikes, 7},{last_fragment, 8},{prev_close_px, 9},{cl_ord_id, 10},{secondary_cl_ord_id, 11},{side, 12},{price, 13},{currency, 14},{text, 15},{encoded_text, 16},{signature, 17}], 18),
  Message1.

decode_message_registration_instructions(Message, #registration_instructions{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{regist_id, 6},{regist_trans_type, 7},{regist_ref_id, 8},{cl_ord_id, 9},{account, 10},{acct_id_source, 11},{regist_acct_type, 12},{tax_advantage_type, 13},{ownership_type, 14},{regist_dtls, 15},{regist_email, 16},{mailing_dtls, 17},{mailing_inst, 18},{owner_type, 19},{date_of_birth, 20},{investor_country_of_residence, 21},{distrib_payment_method, 22},{distrib_percentage, 23},{cash_distrib_curr, 24},{cash_distrib_agent_name, 25},{cash_distrib_agent_code, 26},{cash_distrib_agent_acct_number, 27},{cash_distrib_pay_ref, 28},{cash_distrib_agent_acct_name, 29},{signature, 30}], 31),
  Message1.

decode_message_registration_instructions_response(Message, #registration_instructions_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{regist_id, 6},{regist_trans_type, 7},{regist_ref_id, 8},{cl_ord_id, 9},{account, 10},{acct_id_source, 11},{regist_status, 12},{regist_rej_reason_code, 13},{regist_rej_reason_text, 14},{signature, 15}], 16),
  Message1.

decode_message_order_mass_cancel_request(Message, #order_mass_cancel_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{cl_ord_id, 6},{secondary_cl_ord_id, 7},{mass_cancel_request_type, 8},{trading_session_id, 9},{trading_session_sub_id, 10},{side, 11},{transact_time, 12},{text, 13},{encoded_text, 14},{signature, 15}], 16),
  Message1.

decode_message_order_mass_cancel_report(Message, #order_mass_cancel_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{cl_ord_id, 6},{secondary_cl_ord_id, 7},{order_id, 8},{secondary_order_id, 9},{mass_cancel_request_type, 10},{mass_cancel_response, 11},{mass_cancel_reject_reason, 12},{total_affected_orders, 13},{orig_cl_ord_id, 14},{affected_order_id, 15},{affected_secondary_order_id, 16},{trading_session_id, 17},{trading_session_sub_id, 18},{side, 19},{transact_time, 20},{text, 21},{encoded_text, 22},{signature, 23}], 24),
  Message1.

decode_message_new_order_cross(Message, #new_order_cross{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{cross_id, 6},{cross_type, 7},{cross_prioritization, 8},{side, 9},{cl_ord_id, 10},{secondary_cl_ord_id, 11},{cl_ord_link_id, 12},{trade_origination_date, 13},{trade_date, 14},{account, 15},{acct_id_source, 16},{account_type, 17},{day_booking_inst, 18},{booking_unit, 19},{prealloc_method, 20},{alloc_id, 21},{alloc_account, 22},{alloc_acct_id_source, 23},{alloc_settl_currency, 24},{individual_alloc_id, 25},{alloc_qty, 26},{qty_type, 27},{order_capacity, 28},{order_restrictions, 29},{cust_order_capacity, 30},{forex_req, 31},{settl_currency, 32},{booking_type, 33},{text, 34},{encoded_text, 35},{position_effect, 36},{covered_or_uncovered, 37},{cash_margin, 38},{clearing_fee_indicator, 39},{solicited_flag, 40},{side_compliance_id, 41},{settl_type, 42},{settl_date, 43},{handl_inst, 44},{exec_inst, 45},{min_qty, 46},{max_floor, 47},{ex_destination, 48},{trading_session_id, 49},{trading_session_sub_id, 50},{process_code, 51},{prev_close_px, 52},{locate_reqd, 53},{transact_time, 54},{ord_type, 55},{price_type, 56},{price, 57},{stop_px, 58},{currency, 59},{compliance_id, 60},{ioi_id, 61},{quote_id, 62},{time_in_force, 63},{effective_time, 64},{expire_date, 65},{expire_time, 66},{gt_booking_inst, 67},{max_show, 68},{target_strategy, 69},{target_strategy_parameters, 70},{participation_rate, 71},{cancellation_rights, 72},{money_laundering_status, 73},{regist_id, 74},{designation, 75},{signature, 76}], 77),
  Message1.

decode_message_cross_order_cancel_replace_request(Message, #cross_order_cancel_replace_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{cross_id, 7},{orig_cross_id, 8},{cross_type, 9},{cross_prioritization, 10},{side, 11},{orig_cl_ord_id, 12},{cl_ord_id, 13},{secondary_cl_ord_id, 14},{cl_ord_link_id, 15},{orig_ord_mod_time, 16},{trade_origination_date, 17},{trade_date, 18},{account, 19},{acct_id_source, 20},{account_type, 21},{day_booking_inst, 22},{booking_unit, 23},{prealloc_method, 24},{alloc_id, 25},{alloc_account, 26},{alloc_acct_id_source, 27},{alloc_settl_currency, 28},{individual_alloc_id, 29},{alloc_qty, 30},{qty_type, 31},{order_capacity, 32},{order_restrictions, 33},{cust_order_capacity, 34},{forex_req, 35},{settl_currency, 36},{booking_type, 37},{text, 38},{encoded_text, 39},{position_effect, 40},{covered_or_uncovered, 41},{cash_margin, 42},{clearing_fee_indicator, 43},{solicited_flag, 44},{side_compliance_id, 45},{settl_type, 46},{settl_date, 47},{handl_inst, 48},{exec_inst, 49},{min_qty, 50},{max_floor, 51},{ex_destination, 52},{trading_session_id, 53},{trading_session_sub_id, 54},{process_code, 55},{prev_close_px, 56},{locate_reqd, 57},{transact_time, 58},{ord_type, 59},{price_type, 60},{price, 61},{stop_px, 62},{currency, 63},{compliance_id, 64},{ioi_id, 65},{quote_id, 66},{time_in_force, 67},{effective_time, 68},{expire_date, 69},{expire_time, 70},{gt_booking_inst, 71},{max_show, 72},{target_strategy, 73},{target_strategy_parameters, 74},{participation_rate, 75},{cancellation_rights, 76},{money_laundering_status, 77},{regist_id, 78},{designation, 79},{signature, 80}], 81),
  Message1.

decode_message_cross_order_cancel_request(Message, #cross_order_cancel_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{cross_id, 7},{orig_cross_id, 8},{cross_type, 9},{cross_prioritization, 10},{side, 11},{orig_cl_ord_id, 12},{cl_ord_id, 13},{secondary_cl_ord_id, 14},{cl_ord_link_id, 15},{orig_ord_mod_time, 16},{trade_origination_date, 17},{trade_date, 18},{compliance_id, 19},{text, 20},{encoded_text, 21},{transact_time, 22},{signature, 23}], 24),
  Message1.

decode_message_security_type_request(Message, #security_type_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{text, 7},{encoded_text, 8},{trading_session_id, 9},{trading_session_sub_id, 10},{product, 11},{security_type, 12},{security_sub_type, 13},{signature, 14}], 15),
  Message1.

decode_message_security_types(Message, #security_types{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_response_id, 7},{security_response_type, 8},{tot_no_security_types, 9},{last_fragment, 10},{security_type, 11},{security_sub_type, 12},{product, 13},{cfi_code, 14},{text, 15},{encoded_text, 16},{trading_session_id, 17},{trading_session_sub_id, 18},{subscription_request_type, 19},{signature, 20}], 21),
  Message1.

decode_message_security_list_request(Message, #security_list_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_list_request_type, 7},{currency, 8},{text, 9},{encoded_text, 10},{trading_session_id, 11},{trading_session_sub_id, 12},{subscription_request_type, 13},{signature, 14}], 15),
  Message1.

decode_message_security_list(Message, #security_list{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_response_id, 7},{security_request_result, 8},{tot_no_related_sym, 9},{last_fragment, 10},{currency, 11},{leg_swap_type, 12},{leg_settl_type, 13},{round_lot, 14},{min_trade_vol, 15},{trading_session_id, 16},{trading_session_sub_id, 17},{expiration_cycle, 18},{text, 19},{encoded_text, 20},{signature, 21}], 22),
  Message1.

decode_message_derivative_security_list_request(Message, #derivative_security_list_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_list_request_type, 7},{security_sub_type, 8},{currency, 9},{text, 10},{encoded_text, 11},{trading_session_id, 12},{trading_session_sub_id, 13},{subscription_request_type, 14},{signature, 15}], 16),
  Message1.

decode_message_derivative_security_list(Message, #derivative_security_list{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{security_req_id, 6},{security_response_id, 7},{security_request_result, 8},{tot_no_related_sym, 9},{last_fragment, 10},{currency, 11},{expiration_cycle, 12},{trading_session_id, 13},{trading_session_sub_id, 14},{text, 15},{encoded_text, 16},{signature, 17}], 18),
  Message1.

decode_message_new_order_multileg(Message, #new_order_multileg{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{cl_ord_id, 6},{secondary_cl_ord_id, 7},{cl_ord_link_id, 8},{trade_origination_date, 9},{trade_date, 10},{account, 11},{acct_id_source, 12},{account_type, 13},{day_booking_inst, 14},{booking_unit, 15},{prealloc_method, 16},{alloc_id, 17},{alloc_account, 18},{alloc_acct_id_source, 19},{alloc_settl_currency, 20},{individual_alloc_id, 21},{alloc_qty, 22},{settl_type, 23},{settl_date, 24},{cash_margin, 25},{clearing_fee_indicator, 26},{handl_inst, 27},{exec_inst, 28},{min_qty, 29},{max_floor, 30},{ex_destination, 31},{trading_session_id, 32},{trading_session_sub_id, 33},{process_code, 34},{side, 35},{prev_close_px, 36},{leg_qty, 37},{leg_swap_type, 38},{leg_alloc_account, 39},{leg_individual_alloc_id, 40},{leg_alloc_qty, 41},{leg_alloc_acct_id_source, 42},{leg_settl_currency, 43},{leg_position_effect, 44},{leg_covered_or_uncovered, 45},{leg_ref_id, 46},{leg_price, 47},{leg_settl_type, 48},{leg_settl_date, 49},{locate_reqd, 50},{transact_time, 51},{qty_type, 52},{ord_type, 53},{price_type, 54},{price, 55},{stop_px, 56},{currency, 57},{compliance_id, 58},{solicited_flag, 59},{ioi_id, 60},{quote_id, 61},{time_in_force, 62},{effective_time, 63},{expire_date, 64},{expire_time, 65},{gt_booking_inst, 66},{order_capacity, 67},{order_restrictions, 68},{cust_order_capacity, 69},{forex_req, 70},{settl_currency, 71},{booking_type, 72},{text, 73},{encoded_text, 74},{position_effect, 75},{covered_or_uncovered, 76},{max_show, 77},{target_strategy, 78},{target_strategy_parameters, 79},{participation_rate, 80},{cancellation_rights, 81},{money_laundering_status, 82},{regist_id, 83},{designation, 84},{multi_leg_rpt_type_req, 85},{signature, 86}], 87),
  Message1.

decode_message_multileg_order_cancel_replace(Message, #multileg_order_cancel_replace{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{order_id, 6},{orig_cl_ord_id, 7},{cl_ord_id, 8},{secondary_cl_ord_id, 9},{cl_ord_link_id, 10},{orig_ord_mod_time, 11},{trade_origination_date, 12},{trade_date, 13},{account, 14},{acct_id_source, 15},{account_type, 16},{day_booking_inst, 17},{booking_unit, 18},{prealloc_method, 19},{alloc_id, 20},{alloc_account, 21},{alloc_acct_id_source, 22},{alloc_settl_currency, 23},{individual_alloc_id, 24},{alloc_qty, 25},{settl_type, 26},{settl_date, 27},{cash_margin, 28},{clearing_fee_indicator, 29},{handl_inst, 30},{exec_inst, 31},{min_qty, 32},{max_floor, 33},{ex_destination, 34},{trading_session_id, 35},{trading_session_sub_id, 36},{process_code, 37},{side, 38},{prev_close_px, 39},{leg_qty, 40},{leg_swap_type, 41},{leg_alloc_account, 42},{leg_individual_alloc_id, 43},{leg_alloc_qty, 44},{leg_alloc_acct_id_source, 45},{leg_settl_currency, 46},{leg_position_effect, 47},{leg_covered_or_uncovered, 48},{leg_ref_id, 49},{leg_price, 50},{leg_settl_type, 51},{leg_settl_date, 52},{locate_reqd, 53},{transact_time, 54},{qty_type, 55},{ord_type, 56},{price_type, 57},{price, 58},{stop_px, 59},{currency, 60},{compliance_id, 61},{solicited_flag, 62},{ioi_id, 63},{quote_id, 64},{time_in_force, 65},{effective_time, 66},{expire_date, 67},{expire_time, 68},{gt_booking_inst, 69},{order_capacity, 70},{order_restrictions, 71},{cust_order_capacity, 72},{forex_req, 73},{settl_currency, 74},{booking_type, 75},{text, 76},{encoded_text, 77},{position_effect, 78},{covered_or_uncovered, 79},{max_show, 80},{target_strategy, 81},{target_strategy_parameters, 82},{participation_rate, 83},{cancellation_rights, 84},{money_laundering_status, 85},{regist_id, 86},{designation, 87},{multi_leg_rpt_type_req, 88},{signature, 89}], 90),
  Message1.

decode_message_trade_capture_report_request(Message, #trade_capture_report_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trade_request_id, 6},{trade_request_type, 7},{subscription_request_type, 8},{trade_report_id, 9},{secondary_trade_report_id, 10},{exec_id, 11},{exec_type, 12},{order_id, 13},{cl_ord_id, 14},{match_status, 15},{trd_type, 16},{trd_sub_type, 17},{transfer_reason, 18},{secondary_trd_type, 19},{trade_link_id, 20},{trd_match_id, 21},{trade_date, 22},{transact_time, 23},{clearing_business_date, 24},{trading_session_id, 25},{trading_session_sub_id, 26},{time_bracket, 27},{side, 28},{multi_leg_reporting_type, 29},{trade_input_source, 30},{trade_input_device, 31},{response_transport_type, 32},{response_destination, 33},{text, 34},{encoded_text, 35},{signature, 36}], 37),
  Message1.

decode_message_trade_capture_report(Message, #trade_capture_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trade_report_id, 6},{trade_report_trans_type, 7},{trade_report_type, 8},{trade_request_id, 9},{trd_type, 10},{trd_sub_type, 11},{secondary_trd_type, 12},{transfer_reason, 13},{exec_type, 14},{tot_num_trade_reports, 15},{last_rpt_requested, 16},{unsolicited_indicator, 17},{subscription_request_type, 18},{trade_report_ref_id, 19},{secondary_trade_report_ref_id, 20},{secondary_trade_report_id, 21},{trade_link_id, 22},{trd_match_id, 23},{exec_id, 24},{ord_status, 25},{secondary_exec_id, 26},{exec_restatement_reason, 27},{previously_reported, 28},{price_type, 29},{qty_type, 30},{underlying_trading_session_id, 31},{underlying_trading_session_sub_id, 32},{last_qty, 33},{last_px, 34},{last_par_px, 35},{last_spot_rate, 36},{last_forward_points, 37},{last_mkt, 38},{trade_date, 39},{clearing_business_date, 40},{avg_px, 41},{avg_px_indicator, 42},{multi_leg_reporting_type, 43},{trade_leg_ref_id, 44},{leg_qty, 45},{leg_swap_type, 46},{leg_position_effect, 47},{leg_covered_or_uncovered, 48},{leg_ref_id, 49},{leg_price, 50},{leg_settl_type, 51},{leg_settl_date, 52},{leg_last_px, 53},{transact_time, 54},{settl_type, 55},{settl_date, 56},{match_status, 57},{match_type, 58},{side, 59},{order_id, 60},{secondary_order_id, 61},{cl_ord_id, 62},{secondary_cl_ord_id, 63},{list_id, 64},{account, 65},{acct_id_source, 66},{account_type, 67},{process_code, 68},{odd_lot, 69},{clearing_instruction, 70},{clearing_fee_indicator, 71},{trade_input_source, 72},{trade_input_device, 73},{order_input_device, 74},{currency, 75},{compliance_id, 76},{solicited_flag, 77},{order_capacity, 78},{order_restrictions, 79},{cust_order_capacity, 80},{ord_type, 81},{exec_inst, 82},{trans_bkd_time, 83},{trading_session_id, 84},{trading_session_sub_id, 85},{time_bracket, 86},{gross_trade_amt, 87},{num_days_interest, 88},{ex_date, 89},{accrued_interest_rate, 90},{accrued_interest_amt, 91},{interest_at_maturity, 92},{end_accrued_interest_amt, 93},{start_cash, 94},{end_cash, 95},{concession, 96},{total_takedown, 97},{net_money, 98},{settl_curr_amt, 99},{settl_currency, 100},{settl_curr_fx_rate, 101},{settl_curr_fx_rate_calc, 102},{position_effect, 103},{text, 104},{encoded_text, 105},{side_multi_leg_reporting_type, 106},{cont_amt_type, 107},{cont_amt_value, 108},{cont_amt_curr, 109},{misc_fee_amt, 110},{misc_fee_curr, 111},{misc_fee_type, 112},{misc_fee_basis, 113},{exchange_rule, 114},{trade_alloc_indicator, 115},{prealloc_method, 116},{alloc_id, 117},{alloc_account, 118},{alloc_acct_id_source, 119},{alloc_settl_currency, 120},{individual_alloc_id, 121},{alloc_qty, 122},{copy_msg_indicator, 123},{publish_trd_indicator, 124},{short_sale_reason, 125},{signature, 126}], 127),
  Message1.

decode_message_order_mass_status_request(Message, #order_mass_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{mass_status_req_id, 6},{mass_status_req_type, 7},{account, 8},{acct_id_source, 9},{trading_session_id, 10},{trading_session_sub_id, 11},{side, 12},{signature, 13}], 14),
  Message1.

decode_message_quote_request_reject(Message, #quote_request_reject{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_req_id, 6},{rfq_req_id, 7},{quote_request_reject_reason, 8},{prev_close_px, 9},{quote_request_type, 10},{quote_type, 11},{trading_session_id, 12},{trading_session_sub_id, 13},{trade_origination_date, 14},{side, 15},{qty_type, 16},{settl_type, 17},{settl_date, 18},{settl_date2, 19},{order_qty2, 20},{currency, 21},{account, 22},{acct_id_source, 23},{account_type, 24},{leg_qty, 25},{leg_swap_type, 26},{leg_settl_type, 27},{leg_settl_date, 28},{quote_qualifier, 29},{quote_price_type, 30},{ord_type, 31},{expire_time, 32},{transact_time, 33},{price_type, 34},{price, 35},{price2, 36},{text, 37},{encoded_text, 38},{signature, 39}], 40),
  Message1.

decode_message_rfq_request(Message, #rfq_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{rfq_req_id, 6},{prev_close_px, 7},{quote_request_type, 8},{quote_type, 9},{trading_session_id, 10},{trading_session_sub_id, 11},{subscription_request_type, 12},{signature, 13}], 14),
  Message1.

decode_message_quote_status_report(Message, #quote_status_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_status_req_id, 6},{quote_req_id, 7},{quote_id, 8},{quote_resp_id, 9},{quote_type, 10},{trading_session_id, 11},{trading_session_sub_id, 12},{side, 13},{settl_type, 14},{settl_date, 15},{settl_date2, 16},{order_qty2, 17},{currency, 18},{account, 19},{acct_id_source, 20},{account_type, 21},{leg_qty, 22},{leg_swap_type, 23},{leg_settl_type, 24},{leg_settl_date, 25},{quote_qualifier, 26},{expire_time, 27},{price, 28},{price_type, 29},{bid_px, 30},{offer_px, 31},{mkt_bid_px, 32},{mkt_offer_px, 33},{min_bid_size, 34},{bid_size, 35},{min_offer_size, 36},{offer_size, 37},{valid_until_time, 38},{bid_spot_rate, 39},{offer_spot_rate, 40},{bid_forward_points, 41},{offer_forward_points, 42},{mid_px, 43},{bid_yield, 44},{mid_yield, 45},{offer_yield, 46},{transact_time, 47},{ord_type, 48},{bid_forward_points2, 49},{offer_forward_points2, 50},{settl_curr_bid_fx_rate, 51},{settl_curr_offer_fx_rate, 52},{settl_curr_fx_rate_calc, 53},{comm_type, 54},{commission, 55},{cust_order_capacity, 56},{ex_destination, 57},{quote_status, 58},{text, 59},{encoded_text, 60},{signature, 61}], 62),
  Message1.

decode_message_quote_response(Message, #quote_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{quote_resp_id, 6},{quote_id, 7},{quote_resp_type, 8},{cl_ord_id, 9},{order_capacity, 10},{ioi_id, 11},{quote_type, 12},{quote_qualifier, 13},{trading_session_id, 14},{trading_session_sub_id, 15},{side, 16},{settl_type, 17},{settl_date, 18},{settl_date2, 19},{order_qty2, 20},{currency, 21},{account, 22},{acct_id_source, 23},{account_type, 24},{leg_qty, 25},{leg_swap_type, 26},{leg_settl_type, 27},{leg_settl_date, 28},{leg_price_type, 29},{leg_bid_px, 30},{leg_offer_px, 31},{bid_px, 32},{offer_px, 33},{mkt_bid_px, 34},{mkt_offer_px, 35},{min_bid_size, 36},{bid_size, 37},{min_offer_size, 38},{offer_size, 39},{valid_until_time, 40},{bid_spot_rate, 41},{offer_spot_rate, 42},{bid_forward_points, 43},{offer_forward_points, 44},{mid_px, 45},{bid_yield, 46},{mid_yield, 47},{offer_yield, 48},{transact_time, 49},{ord_type, 50},{bid_forward_points2, 51},{offer_forward_points2, 52},{settl_curr_bid_fx_rate, 53},{settl_curr_offer_fx_rate, 54},{settl_curr_fx_rate_calc, 55},{commission, 56},{comm_type, 57},{cust_order_capacity, 58},{ex_destination, 59},{text, 60},{encoded_text, 61},{price, 62},{price_type, 63},{signature, 64}], 65),
  Message1.

decode_message_confirmation(Message, #confirmation{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{confirm_id, 6},{confirm_ref_id, 7},{confirm_req_id, 8},{confirm_trans_type, 9},{confirm_type, 10},{copy_msg_indicator, 11},{legal_confirm, 12},{confirm_status, 13},{cl_ord_id, 14},{order_id, 15},{secondary_order_id, 16},{secondary_cl_ord_id, 17},{list_id, 18},{order_qty, 19},{order_avg_px, 20},{order_booking_qty, 21},{alloc_id, 22},{secondary_alloc_id, 23},{individual_alloc_id, 24},{transact_time, 25},{trade_date, 26},{alloc_qty, 27},{qty_type, 28},{side, 29},{currency, 30},{last_mkt, 31},{order_capacity, 32},{order_restrictions, 33},{order_capacity_qty, 34},{alloc_account, 35},{alloc_acct_id_source, 36},{alloc_account_type, 37},{avg_px, 38},{avg_px_precision, 39},{price_type, 40},{avg_par_px, 41},{reported_px, 42},{text, 43},{encoded_text, 44},{process_code, 45},{gross_trade_amt, 46},{num_days_interest, 47},{ex_date, 48},{accrued_interest_rate, 49},{accrued_interest_amt, 50},{interest_at_maturity, 51},{end_accrued_interest_amt, 52},{start_cash, 53},{end_cash, 54},{concession, 55},{total_takedown, 56},{net_money, 57},{maturity_net_money, 58},{settl_curr_amt, 59},{settl_currency, 60},{settl_curr_fx_rate, 61},{settl_curr_fx_rate_calc, 62},{settl_type, 63},{settl_date, 64},{shared_commission, 65},{misc_fee_amt, 66},{misc_fee_curr, 67},{misc_fee_type, 68},{misc_fee_basis, 69},{signature, 70}], 71),
  Message1.

decode_message_position_maintenance_request(Message, #position_maintenance_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{pos_req_id, 6},{pos_trans_type, 7},{pos_maint_action, 8},{orig_pos_req_ref_id, 9},{pos_maint_rpt_ref_id, 10},{clearing_business_date, 11},{settl_sess_id, 12},{settl_sess_sub_id, 13},{account, 14},{acct_id_source, 15},{account_type, 16},{currency, 17},{trading_session_id, 18},{trading_session_sub_id, 19},{transact_time, 20},{adjustment_type, 21},{contrary_instruction_indicator, 22},{prior_spread_indicator, 23},{threshold_amount, 24},{text, 25},{encoded_text, 26},{signature, 27}], 28),
  Message1.

decode_message_position_maintenance_report(Message, #position_maintenance_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{pos_maint_rpt_id, 6},{pos_trans_type, 7},{pos_req_id, 8},{pos_maint_action, 9},{orig_pos_req_ref_id, 10},{pos_maint_status, 11},{pos_maint_result, 12},{clearing_business_date, 13},{settl_sess_id, 14},{settl_sess_sub_id, 15},{account, 16},{acct_id_source, 17},{account_type, 18},{currency, 19},{trading_session_id, 20},{trading_session_sub_id, 21},{transact_time, 22},{adjustment_type, 23},{threshold_amount, 24},{text, 25},{encoded_text, 26},{signature, 27}], 28),
  Message1.

decode_message_request_for_positions(Message, #request_for_positions{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{pos_req_id, 6},{pos_req_type, 7},{match_status, 8},{subscription_request_type, 9},{account, 10},{acct_id_source, 11},{account_type, 12},{currency, 13},{clearing_business_date, 14},{settl_sess_id, 15},{settl_sess_sub_id, 16},{trading_session_id, 17},{trading_session_sub_id, 18},{transact_time, 19},{response_transport_type, 20},{response_destination, 21},{text, 22},{encoded_text, 23},{signature, 24}], 25),
  Message1.

decode_message_request_for_positions_ack(Message, #request_for_positions_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{pos_maint_rpt_id, 6},{pos_req_id, 7},{total_num_pos_reports, 8},{unsolicited_indicator, 9},{pos_req_result, 10},{pos_req_status, 11},{account, 12},{acct_id_source, 13},{account_type, 14},{currency, 15},{response_transport_type, 16},{response_destination, 17},{text, 18},{encoded_text, 19},{signature, 20}], 21),
  Message1.

decode_message_position_report(Message, #position_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{pos_maint_rpt_id, 6},{pos_req_id, 7},{pos_req_type, 8},{subscription_request_type, 9},{total_num_pos_reports, 10},{unsolicited_indicator, 11},{pos_req_result, 12},{clearing_business_date, 13},{settl_sess_id, 14},{settl_sess_sub_id, 15},{account, 16},{acct_id_source, 17},{account_type, 18},{currency, 19},{settl_price, 20},{settl_price_type, 21},{prior_settl_price, 22},{underlying_settl_price, 23},{underlying_settl_price_type, 24},{regist_status, 25},{delivery_date, 26},{text, 27},{encoded_text, 28},{signature, 29}], 30),
  Message1.

decode_message_trade_capture_report_request_ack(Message, #trade_capture_report_request_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trade_request_id, 6},{trade_request_type, 7},{subscription_request_type, 8},{tot_num_trade_reports, 9},{trade_request_result, 10},{trade_request_status, 11},{multi_leg_reporting_type, 12},{response_transport_type, 13},{response_destination, 14},{text, 15},{encoded_text, 16},{signature, 17}], 18),
  Message1.

decode_message_trade_capture_report_ack(Message, #trade_capture_report_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{trade_report_id, 6},{trade_report_trans_type, 7},{trade_report_type, 8},{trd_type, 9},{trd_sub_type, 10},{secondary_trd_type, 11},{transfer_reason, 12},{exec_type, 13},{trade_report_ref_id, 14},{secondary_trade_report_ref_id, 15},{trd_rpt_status, 16},{trade_report_reject_reason, 17},{secondary_trade_report_id, 18},{subscription_request_type, 19},{trade_link_id, 20},{trd_match_id, 21},{exec_id, 22},{secondary_exec_id, 23},{transact_time, 24},{response_transport_type, 25},{response_destination, 26},{text, 27},{encoded_text, 28},{leg_qty, 29},{leg_swap_type, 30},{leg_position_effect, 31},{leg_covered_or_uncovered, 32},{leg_ref_id, 33},{leg_price, 34},{leg_settl_type, 35},{leg_settl_date, 36},{leg_last_px, 37},{clearing_fee_indicator, 38},{order_capacity, 39},{order_restrictions, 40},{cust_order_capacity, 41},{account, 42},{acct_id_source, 43},{account_type, 44},{position_effect, 45},{prealloc_method, 46},{alloc_account, 47},{alloc_acct_id_source, 48},{alloc_settl_currency, 49},{individual_alloc_id, 50},{alloc_qty, 51},{signature, 52}], 53),
  Message1.

decode_message_allocation_report(Message, #allocation_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{alloc_report_id, 6},{alloc_id, 7},{alloc_trans_type, 8},{alloc_report_ref_id, 9},{alloc_canc_replace_reason, 10},{secondary_alloc_id, 11},{alloc_report_type, 12},{alloc_status, 13},{alloc_rej_code, 14},{ref_alloc_id, 15},{alloc_intermed_req_type, 16},{alloc_link_id, 17},{alloc_link_type, 18},{booking_ref_id, 19},{alloc_no_orders_type, 20},{cl_ord_id, 21},{order_id, 22},{secondary_order_id, 23},{secondary_cl_ord_id, 24},{list_id, 25},{order_qty, 26},{order_avg_px, 27},{order_booking_qty, 28},{last_qty, 29},{exec_id, 30},{secondary_exec_id, 31},{last_px, 32},{last_par_px, 33},{last_capacity, 34},{previously_reported, 35},{reversal_indicator, 36},{match_type, 37},{side, 38},{quantity, 39},{qty_type, 40},{last_mkt, 41},{trade_origination_date, 42},{trading_session_id, 43},{trading_session_sub_id, 44},{price_type, 45},{avg_px, 46},{avg_par_px, 47},{currency, 48},{avg_px_precision, 49},{trade_date, 50},{transact_time, 51},{settl_type, 52},{settl_date, 53},{booking_type, 54},{gross_trade_amt, 55},{concession, 56},{total_takedown, 57},{net_money, 58},{position_effect, 59},{auto_accept_indicator, 60},{text, 61},{encoded_text, 62},{num_days_interest, 63},{accrued_interest_rate, 64},{accrued_interest_amt, 65},{total_accrued_interest_amt, 66},{interest_at_maturity, 67},{end_accrued_interest_amt, 68},{start_cash, 69},{end_cash, 70},{legal_confirm, 71},{tot_no_allocs, 72},{last_fragment, 73},{alloc_account, 74},{alloc_acct_id_source, 75},{match_status, 76},{alloc_price, 77},{alloc_qty, 78},{individual_alloc_id, 79},{process_code, 80},{notify_broker_of_credit, 81},{alloc_handl_inst, 82},{alloc_text, 83},{encoded_alloc_text, 84},{alloc_avg_px, 85},{alloc_net_money, 86},{settl_curr_amt, 87},{alloc_settl_curr_amt, 88},{settl_currency, 89},{alloc_settl_currency, 90},{settl_curr_fx_rate, 91},{settl_curr_fx_rate_calc, 92},{alloc_accrued_interest_amt, 93},{alloc_interest_at_maturity, 94},{misc_fee_amt, 95},{misc_fee_curr, 96},{misc_fee_type, 97},{misc_fee_basis, 98},{clearing_instruction, 99},{clearing_fee_indicator, 100},{alloc_settl_inst_type, 101},{signature, 102}], 103),
  Message1.

decode_message_allocation_report_ack(Message, #allocation_report_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{alloc_report_id, 6},{alloc_id, 7},{secondary_alloc_id, 8},{trade_date, 9},{transact_time, 10},{alloc_status, 11},{alloc_rej_code, 12},{alloc_report_type, 13},{alloc_intermed_req_type, 14},{match_status, 15},{product, 16},{security_type, 17},{text, 18},{encoded_text, 19},{alloc_account, 20},{alloc_acct_id_source, 21},{alloc_price, 22},{individual_alloc_id, 23},{individual_alloc_rej_code, 24},{alloc_text, 25},{encoded_alloc_text, 26},{signature, 27}], 28),
  Message1.

decode_message_confirmation_ack(Message, #confirmation_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{confirm_id, 6},{trade_date, 7},{transact_time, 8},{affirm_status, 9},{confirm_rej_reason, 10},{match_status, 11},{text, 12},{encoded_text, 13},{signature, 14}], 15),
  Message1.

decode_message_settlement_instruction_request(Message, #settlement_instruction_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{settl_inst_req_id, 6},{transact_time, 7},{alloc_account, 8},{alloc_acct_id_source, 9},{side, 10},{product, 11},{security_type, 12},{cfi_code, 13},{effective_time, 14},{expire_time, 15},{last_update_time, 16},{stand_inst_db_type, 17},{stand_inst_db_name, 18},{stand_inst_db_id, 19},{signature, 20}], 21),
  Message1.

decode_message_assignment_report(Message, #assignment_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{asgn_rpt_id, 6},{tot_num_assignment_reports, 7},{last_rpt_requested, 8},{account, 9},{account_type, 10},{currency, 11},{threshold_amount, 12},{settl_price, 13},{settl_price_type, 14},{underlying_settl_price, 15},{expire_date, 16},{assignment_method, 17},{assignment_unit, 18},{open_interest, 19},{exercise_method, 20},{settl_sess_id, 21},{settl_sess_sub_id, 22},{clearing_business_date, 23},{text, 24},{encoded_text, 25},{signature, 26}], 27),
  Message1.

decode_message_collateral_request(Message, #collateral_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_req_id, 6},{coll_asgn_reason, 7},{transact_time, 8},{expire_time, 9},{account, 10},{account_type, 11},{cl_ord_id, 12},{order_id, 13},{secondary_order_id, 14},{secondary_cl_ord_id, 15},{exec_id, 16},{trade_report_id, 17},{secondary_trade_report_id, 18},{settl_date, 19},{quantity, 20},{qty_type, 21},{currency, 22},{coll_action, 23},{margin_excess, 24},{total_net_value, 25},{cash_outstanding, 26},{side, 27},{misc_fee_amt, 28},{misc_fee_curr, 29},{misc_fee_type, 30},{misc_fee_basis, 31},{price, 32},{price_type, 33},{accrued_interest_amt, 34},{end_accrued_interest_amt, 35},{start_cash, 36},{end_cash, 37},{trading_session_id, 38},{trading_session_sub_id, 39},{settl_sess_id, 40},{settl_sess_sub_id, 41},{clearing_business_date, 42},{text, 43},{encoded_text, 44},{signature, 45}], 46),
  Message1.

decode_message_collateral_assignment(Message, #collateral_assignment{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_asgn_id, 6},{coll_req_id, 7},{coll_asgn_reason, 8},{coll_asgn_trans_type, 9},{coll_asgn_ref_id, 10},{transact_time, 11},{expire_time, 12},{account, 13},{account_type, 14},{cl_ord_id, 15},{order_id, 16},{secondary_order_id, 17},{secondary_cl_ord_id, 18},{exec_id, 19},{trade_report_id, 20},{secondary_trade_report_id, 21},{settl_date, 22},{quantity, 23},{qty_type, 24},{currency, 25},{coll_action, 26},{margin_excess, 27},{total_net_value, 28},{cash_outstanding, 29},{side, 30},{misc_fee_amt, 31},{misc_fee_curr, 32},{misc_fee_type, 33},{misc_fee_basis, 34},{price, 35},{price_type, 36},{accrued_interest_amt, 37},{end_accrued_interest_amt, 38},{start_cash, 39},{end_cash, 40},{trading_session_id, 41},{trading_session_sub_id, 42},{settl_sess_id, 43},{settl_sess_sub_id, 44},{clearing_business_date, 45},{text, 46},{encoded_text, 47},{signature, 48}], 49),
  Message1.

decode_message_collateral_response(Message, #collateral_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_resp_id, 6},{coll_asgn_id, 7},{coll_req_id, 8},{coll_asgn_reason, 9},{coll_asgn_trans_type, 10},{coll_asgn_resp_type, 11},{coll_asgn_reject_reason, 12},{transact_time, 13},{account, 14},{account_type, 15},{cl_ord_id, 16},{order_id, 17},{secondary_order_id, 18},{secondary_cl_ord_id, 19},{exec_id, 20},{trade_report_id, 21},{secondary_trade_report_id, 22},{settl_date, 23},{quantity, 24},{qty_type, 25},{currency, 26},{coll_action, 27},{margin_excess, 28},{total_net_value, 29},{cash_outstanding, 30},{side, 31},{misc_fee_amt, 32},{misc_fee_curr, 33},{misc_fee_type, 34},{misc_fee_basis, 35},{price, 36},{price_type, 37},{accrued_interest_amt, 38},{end_accrued_interest_amt, 39},{start_cash, 40},{end_cash, 41},{text, 42},{encoded_text, 43},{signature, 44}], 45),
  Message1.

decode_message_collateral_report(Message, #collateral_report{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_rpt_id, 6},{coll_inquiry_id, 7},{coll_status, 8},{tot_num_reports, 9},{last_rpt_requested, 10},{account, 11},{account_type, 12},{cl_ord_id, 13},{order_id, 14},{secondary_order_id, 15},{secondary_cl_ord_id, 16},{exec_id, 17},{trade_report_id, 18},{secondary_trade_report_id, 19},{settl_date, 20},{quantity, 21},{qty_type, 22},{currency, 23},{margin_excess, 24},{total_net_value, 25},{cash_outstanding, 26},{side, 27},{misc_fee_amt, 28},{misc_fee_curr, 29},{misc_fee_type, 30},{misc_fee_basis, 31},{price, 32},{price_type, 33},{accrued_interest_amt, 34},{end_accrued_interest_amt, 35},{start_cash, 36},{end_cash, 37},{trading_session_id, 38},{trading_session_sub_id, 39},{settl_sess_id, 40},{settl_sess_sub_id, 41},{clearing_business_date, 42},{text, 43},{encoded_text, 44},{signature, 45}], 46),
  Message1.

decode_message_collateral_inquiry(Message, #collateral_inquiry{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_inquiry_id, 6},{coll_inquiry_qualifier, 7},{subscription_request_type, 8},{response_transport_type, 9},{response_destination, 10},{account, 11},{account_type, 12},{cl_ord_id, 13},{order_id, 14},{secondary_order_id, 15},{secondary_cl_ord_id, 16},{exec_id, 17},{trade_report_id, 18},{secondary_trade_report_id, 19},{settl_date, 20},{quantity, 21},{qty_type, 22},{currency, 23},{margin_excess, 24},{total_net_value, 25},{cash_outstanding, 26},{side, 27},{price, 28},{price_type, 29},{accrued_interest_amt, 30},{end_accrued_interest_amt, 31},{start_cash, 32},{end_cash, 33},{trading_session_id, 34},{trading_session_sub_id, 35},{settl_sess_id, 36},{settl_sess_sub_id, 37},{clearing_business_date, 38},{text, 39},{encoded_text, 40},{signature, 41}], 42),
  Message1.

decode_message_network_counterparty_system_status_request(Message, #network_counterparty_system_status_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{network_request_type, 6},{network_request_id, 7},{ref_comp_id, 8},{ref_sub_id, 9},{location_id, 10},{desk_id, 11},{signature, 12}], 13),
  Message1.

decode_message_network_counterparty_system_status_response(Message, #network_counterparty_system_status_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{network_status_response_type, 6},{network_request_id, 7},{network_response_id, 8},{last_network_response_id, 9},{ref_comp_id, 10},{ref_sub_id, 11},{location_id, 12},{desk_id, 13},{status_value, 14},{status_text, 15},{signature, 16}], 17),
  Message1.

decode_message_user_request(Message, #user_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{user_request_id, 6},{user_request_type, 7},{username, 8},{password, 9},{new_password, 10},{raw_data, 11},{signature, 12}], 13),
  Message1.

decode_message_user_response(Message, #user_response{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{user_request_id, 6},{username, 7},{user_status, 8},{user_status_text, 9},{signature, 10}], 11),
  Message1.

decode_message_collateral_inquiry_ack(Message, #collateral_inquiry_ack{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{coll_inquiry_id, 6},{coll_inquiry_status, 7},{coll_inquiry_result, 8},{coll_inquiry_qualifier, 9},{tot_num_reports, 10},{account, 11},{account_type, 12},{cl_ord_id, 13},{order_id, 14},{secondary_order_id, 15},{secondary_cl_ord_id, 16},{exec_id, 17},{trade_report_id, 18},{secondary_trade_report_id, 19},{settl_date, 20},{quantity, 21},{qty_type, 22},{currency, 23},{trading_session_id, 24},{trading_session_sub_id, 25},{settl_sess_id, 26},{settl_sess_sub_id, 27},{clearing_business_date, 28},{response_transport_type, 29},{response_destination, 30},{text, 31},{encoded_text, 32},{signature, 33}], 34),
  Message1.

decode_message_confirmation_request(Message, #confirmation_request{} = Record) -> 
  Message1 = decode_fields(Message, Record, [{sender_comp_id, 2},{target_comp_id, 3},{msg_seq_num, 4},{sending_time, 5},{confirm_req_id, 6},{confirm_type, 7},{cl_ord_id, 8},{order_id, 9},{secondary_order_id, 10},{secondary_cl_ord_id, 11},{list_id, 12},{order_qty, 13},{order_avg_px, 14},{order_booking_qty, 15},{alloc_id, 16},{secondary_alloc_id, 17},{individual_alloc_id, 18},{transact_time, 19},{alloc_account, 20},{alloc_acct_id_source, 21},{alloc_account_type, 22},{text, 23},{encoded_text, 24},{signature, 25}], 26),
  Message1.

decode_fields(<<"1=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(account, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"2=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(adv_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"3=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(adv_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"4=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(adv_side, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"5=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(adv_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"6=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(avg_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"7=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(begin_seq_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"8=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(begin_string, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"10=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(check_sum, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"11=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cl_ord_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"12=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(commission, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"13=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(comm_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"14=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cum_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"15=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"16=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(end_seq_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"17=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"18=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"19=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"20=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"21=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(handl_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"22=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"23=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"24=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_oth_svc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"25=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_qlty_ind, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"26=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"27=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"28=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"29=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_capacity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"30=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_mkt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"31=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"32=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"33=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_lines_of_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"34=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(msg_seq_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"35=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(msg_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"36=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(new_seq_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"37=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(order_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"38=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(order_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"39=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ord_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"40=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ord_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"41=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_cl_ord_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"42=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"43=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(poss_dup_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"44=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"45=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(ref_seq_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"46=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(relatd_sym, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"47=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(rule80a, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"48=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"49=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(sender_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"50=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(sender_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"51=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(sending_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"52=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(sending_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"53=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quantity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"54=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(side, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"55=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(symbol, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"56=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(target_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"57=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(target_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"58=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"59=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(time_in_force, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"60=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(transact_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"61=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(urgency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"62=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(valid_until_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"63=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"64=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"65=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(symbol_sfx, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"66=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(list_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"67=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(list_seq_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"68=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_orders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"69=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(list_exec_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"70=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"71=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"72=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ref_alloc_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"73=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_orders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"74=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(avg_px_precision, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"75=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"76=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_broker, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"77=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(position_effect, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"78=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_allocs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"79=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_account, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"80=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"81=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(process_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"82=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_rpts, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"83=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(rpt_seq, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"84=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cxl_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"85=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_dlvy_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"86=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(dlvy_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"87=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"88=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_rej_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"92=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(broker_of_credit, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"94=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(email_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"97=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(poss_resend, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"98=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(encrypt_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"99=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(stop_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"100=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ex_destination, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"102=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cxl_rej_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"103=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(ord_rej_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"104=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ioi_qualifier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"105=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(wave_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"106=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"107=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"108=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(heart_bt_int, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"109=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(client_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"110=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(min_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"111=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(max_floor, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"112=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(test_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"113=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(report_to_exch, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"114=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(locate_reqd, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"115=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(on_behalf_of_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"116=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(on_behalf_of_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"117=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"118=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(net_money, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"119=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_curr_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"120=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"121=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(forex_req, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"122=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_sending_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"123=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(gap_fill_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"124=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_execs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"125=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cxl_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"126=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(expire_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"127=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(dk_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"128=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(deliver_to_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"129=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(deliver_to_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"130=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(ioi_natural_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"131=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"132=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(bid_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"133=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(offer_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"134=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(bid_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"135=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(offer_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"136=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_misc_fees, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"137=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(misc_fee_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"138=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(misc_fee_curr, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"139=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(misc_fee_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"140=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(prev_close_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"141=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(reset_seq_num_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"142=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(sender_location_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"143=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(target_location_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"144=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(on_behalf_of_location_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"145=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(deliver_to_location_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"146=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_related_sym, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"147=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(subject, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"148=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(headline, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"149=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(url_link, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"150=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"151=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leaves_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"152=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cash_order_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"153=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_avg_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"154=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_net_money, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"155=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_curr_fx_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"156=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_curr_fx_rate_calc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"157=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(num_days_interest, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"158=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(accrued_interest_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"159=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(accrued_interest_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"160=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_mode, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"161=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"162=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"163=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"164=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(email_thread_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"165=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"166=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_location, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"167=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"168=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(effective_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"169=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(stand_inst_db_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"170=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(stand_inst_db_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"171=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(stand_inst_db_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"172=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_delivery_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"173=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_depository_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"174=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_brkr_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"175=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"176=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"177=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"178=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_acct_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"179=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_acct_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"180=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_contact_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"181=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_settl_agent_contact_phone, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"182=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"183=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"184=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_acct_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"185=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_acct_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"186=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_contact_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"187=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_settl_agent_contact_phone, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"188=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(bid_spot_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"189=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_forward_points, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"190=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(offer_spot_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"191=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(offer_forward_points, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"192=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(order_qty2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"193=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_date2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"194=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_spot_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"195=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_forward_points, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"196=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_link_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"197=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_link_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"198=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_order_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"199=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_ioi_qualifiers, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"200=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(maturity_month_year, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"201=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(put_or_call, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"202=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(strike_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"203=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(covered_or_uncovered, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"204=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(customer_or_firm, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"205=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(maturity_day, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"206=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(opt_attribute, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"207=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_exchange, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"208=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(notify_broker_of_credit, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"209=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_handl_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"210=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(max_show, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"211=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(peg_offset_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"214=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"215=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_routing_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"216=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(routing_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"217=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(routing_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"218=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(spread, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"219=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"220=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark_curve_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"221=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark_curve_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"222=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark_curve_point, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"223=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coupon_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"224=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coupon_payment_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"225=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(issue_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"226=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(repurchase_term, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"227=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(repurchase_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"228=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(factor, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"229=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_origination_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"230=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ex_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"231=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contract_multiplier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"232=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_stipulations, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"233=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(stipulation_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"234=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(stipulation_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"235=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(yield_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"236=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(yield, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"237=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(total_takedown, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"238=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(concession, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"239=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(repo_collateral_security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"240=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(redemption_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"241=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_coupon_payment_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"242=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_issue_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"243=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_repo_collateral_security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"244=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_repurchase_term, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"245=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_repurchase_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"246=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_factor, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"247=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_redemption_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"248=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_coupon_payment_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"249=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_issue_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"250=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_repo_collateral_security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"251=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_repurchase_term, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"252=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_repurchase_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"253=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_factor, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"254=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_redemption_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"255=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(credit_rating, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"256=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_credit_rating, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"257=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_credit_rating, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"258=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(traded_flat_switch, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"259=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(basis_feature_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"260=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(basis_feature_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"262=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"263=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(subscription_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"264=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(market_depth, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"265=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(md_update_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"266=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(aggregated_book, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"267=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_md_entry_types, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"268=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_md_entries, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"269=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"270=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(md_entry_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"271=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(md_entry_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"272=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"273=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"274=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(tick_direction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"275=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_mkt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"276=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_condition, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"277=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_condition, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"278=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"279=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_update_action, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"280=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"281=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_req_rej_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"282=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_originator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"283=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(location_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"284=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(desk_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"285=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(delete_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"286=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(open_close_settl_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"287=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(seller_days, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"288=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_buyer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"289=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(md_entry_seller, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"290=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(md_entry_position_no, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"291=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(financial_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"292=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(corporate_action, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"293=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(def_bid_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"294=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(def_offer_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"295=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_quote_entries, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"296=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_quote_sets, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"297=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"298=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_cancel_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"299=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_entry_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"300=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"301=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_response_level, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"302=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_set_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"303=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"304=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_quote_entries, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"305=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"306=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"307=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"308=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_exchange, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"309=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"310=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"311=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_symbol, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"312=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_symbol_sfx, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"313=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_maturity_month_year, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"314=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_maturity_day, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"315=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_put_or_call, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"316=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_strike_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"317=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_opt_attribute, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"318=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"319=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(ratio_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"320=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"321=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(security_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"322=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_response_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"323=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(security_response_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"324=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_status_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"325=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(unsolicited_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"326=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(security_trading_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"327=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(halt_reason_char, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"328=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(in_view_of_common, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"329=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(due_to_related, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"330=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(buy_volume, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"331=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(sell_volume, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"332=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(high_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"333=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(low_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"334=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(adjustment, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"335=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"336=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trading_session_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"337=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contra_trader, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"338=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trad_ses_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"339=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trad_ses_mode, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"340=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trad_ses_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"341=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_start_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"342=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_open_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"343=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_pre_close_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"344=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_close_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"345=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trad_ses_end_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"346=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(number_of_orders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"347=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(message_encoding, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"366=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"367=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_set_valid_until_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"368=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_entry_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"369=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_msg_seq_num_processed, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"370=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(on_behalf_of_sending_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"371=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(ref_tag_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"372=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ref_msg_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"373=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(session_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"374=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_request_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"375=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contra_broker, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"376=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(compliance_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"377=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(solicited_flag, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"378=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(exec_restatement_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"379=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(business_reject_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"380=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(business_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"381=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(gross_trade_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"382=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_contra_brokers, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"384=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_msg_types, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"385=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(msg_direction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"386=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_trading_sessions, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"387=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(total_volume_traded, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"388=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(discretion_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"389=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(discretion_offset_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"390=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"391=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(client_bid_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"392=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(list_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"393=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_related_sym, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"394=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(bid_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"395=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(num_tickets, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"396=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(side_value1, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"397=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(side_value2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"398=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_bid_descriptors, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"399=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(bid_descriptor_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"400=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_descriptor, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"401=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(side_value_ind, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"402=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(liquidity_pct_low, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"403=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(liquidity_pct_high, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"404=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(liquidity_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"405=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(efp_tracking_error, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"406=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(fair_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"407=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(outside_index_pct, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"408=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(value_of_futures, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"409=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(liquidity_ind_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"410=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(wt_average_liquidity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"411=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(exchange_for_physical, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"412=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(out_main_cntry_u_index, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"413=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cross_percent, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"414=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(prog_rpt_reqs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"415=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(prog_period_interval, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"416=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(inc_tax_ind, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"417=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(num_bidders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"418=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_trade_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"419=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(basis_px_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"420=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_bid_components, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"421=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(country, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"422=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_strikes, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"423=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"424=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(day_order_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"425=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(day_cum_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"426=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(day_avg_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"427=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(gt_booking_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"428=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_strikes, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"429=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(list_status_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"430=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(net_gross_ind, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"431=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(list_order_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"432=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(expire_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"433=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(list_exec_inst_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"434=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cxl_rej_response_to, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"435=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_coupon_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"436=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_contract_multiplier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"437=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(contra_trade_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"438=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contra_trade_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"439=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(clearing_firm, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"440=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(clearing_account, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"441=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(liquidity_num_securities, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"442=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(multi_leg_reporting_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"443=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(strike_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"444=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(list_status_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"447=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(party_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"448=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(party_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"449=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(total_volume_traded_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"450=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(total_volume_traded_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"451=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(net_chg_prev_day, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"452=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(party_role, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"453=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_party_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"454=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"455=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"456=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_alt_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"457=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_underlying_security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"458=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"459=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_alt_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"460=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(product, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"461=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cfi_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"462=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_product, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"463=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_cfi_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"464=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(test_message_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"465=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quantity_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"466=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(booking_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"467=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(individual_alloc_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"468=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(rounding_direction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"469=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(rounding_modulus, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"470=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(country_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"471=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(state_or_province_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"472=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(locale_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"473=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_regist_dtls, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"474=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mailing_dtls, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"475=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(investor_country_of_residence, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"476=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(payment_ref, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"477=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(distrib_payment_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"478=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_curr, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"479=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(comm_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"480=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cancellation_rights, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"481=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(money_laundering_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"482=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mailing_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"483=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trans_bkd_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"484=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"485=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_price_adjustment, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"486=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(date_of_birth, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"487=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_report_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"488=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(card_holder_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"489=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(card_number, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"490=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(card_exp_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"491=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(card_iss_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"492=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(payment_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"493=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_acct_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"494=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(designation, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"495=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tax_advantage_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"496=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_rej_reason_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"497=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(fund_renew_waiv, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"498=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_agent_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"499=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_agent_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"500=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_agent_acct_number, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"501=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_pay_ref, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"502=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_distrib_agent_acct_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"503=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(card_start_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"504=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(payment_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"505=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(payment_remitter_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"506=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"507=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(regist_rej_reason_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"508=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"509=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_dtls, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"510=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_distrib_insts, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"511=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_email, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"512=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(distrib_percentage, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"513=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"514=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(regist_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"515=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exec_valuation_point, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"516=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(order_percent, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"517=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ownership_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"518=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_cont_amts, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"519=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cont_amt_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"520=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cont_amt_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"521=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cont_amt_curr, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"522=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(owner_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"523=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(party_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"524=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested_party_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"525=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested_party_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"526=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_cl_ord_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"527=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_exec_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"528=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(order_capacity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"529=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(order_restrictions, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"530=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mass_cancel_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"531=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mass_cancel_response, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"532=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mass_cancel_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"533=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(total_affected_orders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"534=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_affected_orders, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"535=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(affected_order_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"536=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(affected_secondary_order_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"537=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"538=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested_party_role, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"539=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested_party_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"540=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(total_accrued_interest_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"541=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(maturity_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"542=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_maturity_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"543=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(instr_registry, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"544=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_margin, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"545=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested_party_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"546=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(scope, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"547=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(md_implicit_delete, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"548=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cross_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"549=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cross_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"550=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cross_prioritization, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"551=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_cross_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"552=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_sides, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"553=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(username, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"554=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(password, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"555=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_legs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"556=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"557=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_security_types, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"558=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_security_types, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"559=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(security_list_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"560=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(security_request_result, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"561=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(round_lot, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"562=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(min_trade_vol, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"563=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(multi_leg_rpt_type_req, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"564=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_position_effect, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"565=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_covered_or_uncovered, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"566=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"567=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trad_ses_status_rej_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"568=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_request_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"569=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"570=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(previously_reported, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"571=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_report_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"572=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_report_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"573=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(match_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"574=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(match_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"575=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(odd_lot, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"576=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_clearing_instructions, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"577=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(clearing_instruction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"578=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_input_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"579=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_input_device, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"580=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_dates, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"581=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(account_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"582=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cust_order_capacity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"583=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cl_ord_link_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"584=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mass_status_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"585=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(mass_status_req_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"586=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_ord_mod_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"587=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_settl_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"588=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_settl_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"589=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(day_booking_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"590=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(booking_unit, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"591=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(prealloc_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"592=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_country_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"593=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_state_or_province_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"594=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_locale_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"595=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_instr_registry, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"596=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_country_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"597=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_state_or_province_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"598=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_locale_of_issue, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"599=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_instr_registry, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"600=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_symbol, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"601=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_symbol_sfx, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"602=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"603=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"604=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(no_leg_security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"605=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_alt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"606=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_alt_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"607=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_product, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"608=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_cfi_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"609=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"610=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_maturity_month_year, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"611=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_maturity_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"612=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_strike_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"613=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_opt_attribute, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"614=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_contract_multiplier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"615=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_coupon_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"616=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_exchange, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"617=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"620=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"623=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_ratio_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"624=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_side, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"625=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trading_session_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"626=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"627=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_hops, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"628=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(hop_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"629=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(hop_sending_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"630=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(hop_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"631=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(mid_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"632=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_yield, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"633=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(mid_yield, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"634=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(offer_yield, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"635=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(clearing_fee_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"636=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(working_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"637=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_last_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"638=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(priority_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"639=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(price_improvement, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"640=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(price2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"641=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_forward_points2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"642=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(bid_forward_points2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"643=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(offer_forward_points2, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"644=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(rfq_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"645=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(mkt_bid_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"646=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(mkt_offer_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"647=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(min_bid_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"648=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(min_offer_size, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"649=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_status_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"650=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(legal_confirm, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"651=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_last_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"652=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_last_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"653=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(sec_def_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"654=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"655=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contra_leg_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"656=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_curr_bid_fx_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"657=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_curr_offer_fx_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"658=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_request_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"659=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(side_compliance_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"660=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(acct_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"661=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_acct_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"662=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(benchmark_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"663=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(benchmark_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"664=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(confirm_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"665=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(confirm_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"666=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(confirm_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"667=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(contract_settl_month, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"668=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(delivery_form, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"669=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_par_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"670=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_leg_allocs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"671=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_alloc_account, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"672=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_individual_alloc_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"673=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_alloc_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"674=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_alloc_acct_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"675=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_settl_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"676=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_benchmark_curve_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"677=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_benchmark_curve_name, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"678=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_benchmark_curve_point, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"679=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_benchmark_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"680=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_benchmark_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"681=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_bid_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"682=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_ioi_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"683=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_leg_stipulations, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"684=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_offer_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"685=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_order_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"686=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"687=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"688=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_stipulation_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"689=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_stipulation_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"690=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(leg_swap_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"691=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pool, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"692=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"693=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_resp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"694=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(quote_resp_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"695=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(quote_qualifier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"696=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(yield_redemption_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"697=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(yield_redemption_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"698=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(yield_redemption_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"699=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark_security_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"700=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(reversal_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"701=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(yield_calc_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"702=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_positions, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"703=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"704=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(long_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"705=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(short_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"706=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_qty_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"707=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_amt_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"708=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"709=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"710=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"711=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_underlyings, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"712=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_maint_action, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"713=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(orig_pos_req_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"714=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_maint_rpt_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"715=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(clearing_business_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"716=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_sess_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"717=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_sess_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"718=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(adjustment_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"719=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(contrary_instruction_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"720=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(prior_spread_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"721=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pos_maint_rpt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"722=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_maint_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"723=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_maint_result, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"724=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_req_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"725=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(response_transport_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"726=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(response_destination, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"727=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(total_num_pos_reports, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"728=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_req_result, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"729=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pos_req_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"730=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"731=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"732=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_settl_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"733=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_settl_price_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"734=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(prior_settl_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"735=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_quote_qualifiers, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"736=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_settl_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"737=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_settl_curr_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"738=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(interest_at_maturity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"739=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_dated_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"740=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_pool, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"741=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_interest_at_maturity, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"742=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_accrued_interest_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"743=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(delivery_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"744=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(assignment_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"745=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(assignment_unit, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"746=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(open_interest, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"747=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exercise_method, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"748=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_num_trade_reports, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"749=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_request_result, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"750=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_request_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"751=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_report_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"752=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(side_multi_leg_reporting_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"753=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_pos_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"754=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(auto_accept_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"755=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_report_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"756=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested2_party_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"757=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested2_party_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"758=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested2_party_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"759=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested2_party_role, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"760=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested2_party_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"761=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(benchmark_security_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"762=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(security_sub_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"763=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_security_sub_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"764=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_security_sub_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"765=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(allowable_one_sidedness_pct, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"766=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(allowable_one_sidedness_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"767=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(allowable_one_sidedness_curr, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"768=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_trd_reg_timestamps, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"769=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trd_reg_timestamp, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"770=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trd_reg_timestamp_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"771=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trd_reg_timestamp_origin, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"772=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(confirm_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"773=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(confirm_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"774=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(confirm_rej_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"775=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(booking_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"776=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(individual_alloc_rej_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"777=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_msg_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"778=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_settl_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"779=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_update_time, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"780=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_settl_inst_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"781=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_settl_party_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"782=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_party_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"783=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_party_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"784=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_party_role, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"785=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_party_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"786=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_party_sub_id_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"787=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(dlvy_inst_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"788=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(termination_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"789=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(next_expected_msg_seq_num, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"790=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ord_status_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"791=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(settl_inst_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"792=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(settl_inst_req_rej_code, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"793=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_alloc_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"794=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_report_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"795=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alloc_report_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"796=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_canc_replace_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"797=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(copy_msg_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"798=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_account_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"799=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(order_avg_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"800=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(order_booking_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"801=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_settl_party_sub_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"802=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_party_sub_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"803=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(party_sub_id_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"804=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested_party_sub_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"805=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested_party_sub_id_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"806=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested2_party_sub_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"807=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested2_party_sub_id_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"808=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_intermed_req_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"810=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"811=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(price_delta, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"812=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(appl_queue_max, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"813=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(appl_queue_depth, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"814=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(appl_queue_resolution, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"815=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(appl_queue_action, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"816=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_alt_md_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"817=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(alt_md_source_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"818=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_trade_report_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"819=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(avg_px_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"820=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_link_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"821=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(order_input_device, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"822=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_trading_session_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"823=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_trading_session_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"824=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trade_leg_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"825=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(exchange_rule, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"826=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_alloc_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"827=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(expiration_cycle, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"828=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trd_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"829=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trd_sub_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"830=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(transfer_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"831=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(asgn_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"832=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_num_assignment_reports, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"833=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(asgn_rpt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"834=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(threshold_amount, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"835=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(peg_move_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"836=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(peg_offset_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"837=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(peg_limit_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"838=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(peg_round_direction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"839=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(pegged_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"840=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(peg_scope, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"841=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_move_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"842=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_offset_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"843=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_limit_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"844=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_round_direction, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"845=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"846=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(discretion_scope, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"847=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(target_strategy, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"848=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(target_strategy_parameters, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"849=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(participation_rate, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"850=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(target_strategy_performance, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"851=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(last_liquidity_ind, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"852=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(publish_trd_indicator, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"853=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(short_sale_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"854=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(qty_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"855=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(secondary_trd_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"856=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trade_report_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"857=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(alloc_no_orders_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"858=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(shared_commission, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"859=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(confirm_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"860=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(avg_par_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"861=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(reported_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"862=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_capacities, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"863=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(order_capacity_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"864=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_events, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"865=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(event_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"866=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(event_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"867=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(event_px, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"868=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(event_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"869=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(pct_at_risk, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"870=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_instr_attrib, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"871=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(instr_attrib_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"872=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(instr_attrib_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"873=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(dated_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"874=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(interest_accrual_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"875=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(cp_program, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"876=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cp_reg_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"877=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_cp_program, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"878=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_cp_reg_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"879=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_qty, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"880=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(trd_match_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"881=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(secondary_trade_report_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"882=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_dirty_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"883=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(underlying_end_price, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"884=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_start_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"885=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_current_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"886=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_end_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"887=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_underlying_stips, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"888=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_stip_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"889=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_stip_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"890=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(maturity_net_money, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"891=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(misc_fee_basis, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"892=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_no_allocs, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"893=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(last_fragment, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"894=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_req_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"895=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_asgn_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"896=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_inquiry_qualifier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"897=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_trades, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"898=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(margin_ratio, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"899=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(margin_excess, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"900=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(total_net_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"901=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(cash_outstanding, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"902=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_asgn_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"903=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_asgn_trans_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"904=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_resp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"905=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_asgn_resp_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"906=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_asgn_reject_reason, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"907=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_asgn_ref_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"908=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_rpt_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"909=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(coll_inquiry_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"910=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"911=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(tot_num_reports, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"912=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue == <<"Y">>,
  Record1 = case proplists:get_value(last_rpt_requested, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"913=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(agreement_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"914=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(agreement_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"915=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(agreement_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"916=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(start_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"917=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(end_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"918=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(agreement_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"919=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(delivery_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"920=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(end_accrued_interest_amt, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"921=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(start_cash, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"922=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(end_cash, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"923=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(user_request_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"924=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(user_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"925=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(new_password, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"926=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(user_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"927=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(user_status_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"928=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(status_value, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"929=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(status_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"930=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ref_comp_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"931=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(ref_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"932=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(network_response_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"933=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(network_request_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"934=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(last_network_response_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"935=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(network_request_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"936=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_comp_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"937=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(network_status_response_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"938=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_coll_inquiry_qualifier, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"939=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(trd_rpt_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"940=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(affirm_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"941=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(underlying_strike_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"942=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_strike_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"943=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(time_bracket, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"944=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_action, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"945=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_inquiry_status, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"946=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(coll_inquiry_result, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"947=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(strike_currency, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"948=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested3_party_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"949=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested3_party_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"950=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested3_party_id_source, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"951=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested3_party_role, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"952=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(no_nested3_party_sub_ids, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"953=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(nested3_party_sub_id, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"954=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = fix:parse_num(RawValue),
  Record1 = case proplists:get_value(nested3_party_sub_id_type, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"955=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_contract_settl_month, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"956=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  Value = RawValue,
  Record1 = case proplists:get_value(leg_interest_accrual_date, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_fields(<<"9=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"90=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"93=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"95=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"212=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"348=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"350=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"352=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"354=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"356=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"358=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"360=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"362=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"364=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"383=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"445=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"618=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<"621=", Message/binary>>, Record, Indexes, Default) ->
  [RawValue, Rest] = binary:split(Message, <<1>>),
  DataLength = fix:parse_num(RawValue),
  decode_data_field(Rest, DataLength, Record, Indexes, Default);

decode_fields(<<>>, Record, _Indexes, Default) ->
  erlang:setelement(Default, Record, lists:reverse(erlang:element(Default,Record))).

decode_data_field(<<"89=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(signature, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"91=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(secure_data, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"96=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(raw_data, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"213=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(xml_data, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"349=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"351=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"353=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_list_exec_inst, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"355=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"357=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_subject, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"359=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_headline, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"361=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_alloc_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"363=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_underlying_issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"365=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_underlying_security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"446=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_list_status_text, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"619=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_leg_issuer, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default);

decode_data_field(<<"622=", Message/binary>>, DataLength, Record, Indexes, Default) ->
  <<Value:DataLength/binary, 1, Rest/binary>> = Message,
  Record1 = case proplists:get_value(encoded_leg_security_desc, Indexes) of
    undefined -> erlang:setelement(Default, Record, [Value|erlang:element(Default,Record)]);
    Index -> erlang:setelement(Index, Record, Value)
  end,  decode_fields(Rest, Record1, Indexes, Default).

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

