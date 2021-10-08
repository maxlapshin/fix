-record(execution_report, {
  sending_time,
  cl_ord_id,
  orig_cl_ord_id,
  exec_type,
  ord_status,
  side,
  transact_time,
  leaves_qty,
  cum_qty,
  last_px,
  last_qty,
  order_qty,
  ord_type,
  price,
  ord_rej_reason,
  text,
  security_exchange,
  symbol,
  avg_px,
  contra_brokers = [],
  underlyings = [],
  cont_amts = [],
  legs = [],
  misc_fees = [],
  fields = []
}).

-record(order_cancel_reject, {
  sending_time,
  order_id,
  secondary_order_id,
  secondary_cl_ord_id,
  cl_ord_id,
  cl_ord_link_id,
  orig_cl_ord_id,
  ord_status,
  working_indicator,
  orig_ord_mod_time,
  list_id,
  account,
  acct_id_source,
  account_type,
  trade_origination_date,
  trade_date,
  transact_time,
  cxl_rej_response_to,
  cxl_rej_reason,
  text,
  encoded_text,
  fields = []
}).

-record(new_order_single, {
  sending_time,
  cl_ord_id,
  side,
  transact_time,
  order_qty,
  ord_type,
  price,
  stop_px,
  time_in_force,
  allocs = [],
  trading_sessions = [],
  underlyings = [],
  fields = []
}).

-record(order_cancel_request, {
  sending_time,
  cl_ord_id,
  orig_cl_ord_id,
  side,
  transact_time,
  order_qty,
  underlyings = [],
  fields = []
}).

-record(order_status_request, {
  sending_time,
  cl_ord_id,
  side,
  underlyings = [],
  fields = []
}).

-record(market_data_request, {
  sending_time,
  md_req_id,
  subscription_request_type,
  market_depth,
  md_update_type,
  aggregated_book,
  open_close_settl_flag,
  scope,
  md_implicit_delete,
  appl_queue_action,
  appl_queue_max,
  md_entry_types = [],
  related_sym = [],
  legs = [],
  trading_sessions = [],
  fields = []
}).

-record(market_data_snapshot_full_refresh, {
  sending_time,
  exchange,
  symbol,
  md_req_id,
  underlyings = [],
  legs = [],
  md_entries = [],
  fields = []
}).

-record(market_data_incremental_refresh, {
  sending_time,
  md_req_id,
  financial_status,
  corporate_action,
  md_entry_px,
  currency,
  md_entry_size,
  md_entry_date,
  md_entry_time,
  tick_direction,
  md_mkt,
  trading_session_id,
  trading_session_sub_id,
  quote_condition,
  trade_condition,
  md_entry_originator,
  location_id,
  desk_id,
  open_close_settl_flag,
  time_in_force,
  expire_date,
  expire_time,
  min_qty,
  exec_inst,
  seller_days,
  order_id,
  quote_entry_id,
  md_entry_buyer,
  md_entry_seller,
  number_of_orders,
  md_entry_position_no,
  scope,
  price_delta,
  net_chg_prev_day,
  text,
  encoded_text,
  appl_queue_depth,
  appl_queue_resolution,
  md_entries = [],
  legs = [],
  fields = []
}).

-record(market_data_request_reject, {
  sending_time,
  md_req_id,
  md_req_rej_reason,
  text,
  encoded_text,
  alt_md_source = [],
  fields = []
}).

-record(business_message_reject, {
  sending_time,
  ref_seq_num,
  ref_msg_type,
  business_reject_ref_id,
  business_reject_reason,
  text,
  encoded_text,
  fields = []
}).

-record(order_mass_status_request, {
  mass_status_req_id,
  mass_status_req_type,
  fields = []
}).

-record(request_for_positions, {
  pos_req_id,
  pos_req_type,
  legs = [],
  underlyings = [],
  trading_sessions = [],
  fields = []
}).

-record(position_report, {
  pos_req_id,
  legs = [],
  underlyings = [],
  fields = []
}).

-record(quote, {
  quote_id,
  quote_req_id,
  bid_px,
  offer_px,
  bid_size,
  offer_size,
  quote_type,
  quote_msg_id,
  symbol,
  fields = []
}).
