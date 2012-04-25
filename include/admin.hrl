-record(heartbeat, {
  test_req_id,
  fields = []
}).

-record(test_request, {
  test_req_id,
  fields = []
}).

-record(resend_request, {
  begin_seq_no,
  end_seq_no,
  fields = []
}).

-record(reject, {
  ref_seq_num,
  ref_tag_id,
  ref_msg_type,
  session_reject_reason,
  text,
  encoded_text_len,
  encoded_text,
  fields = []
}).

-record(sequence_reset, {
  gap_fill_flag,
  new_seq_no,
  fields = []
}).

-record(logout, {
  text,
  encoded_text_len,
  encoded_text,
  fields = []
}).

-record(logon, {
  encrypt_method,
  heart_bt_int,
  raw_data_length,
  raw_data,
  reset_seq_num_flag,
  next_expected_msg_seq_num,
  max_message_size,
  ref_msg_type,
  msg_direction,
  test_message_indicator,
  username,
  password,
  fields = []
}).

