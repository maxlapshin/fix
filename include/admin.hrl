-record(heartbeat, {
  sending_time,
  test_req_id,
  fields = []
}).

-record(test_request, {
  sending_time,
  test_req_id,
  fields = []
}).

-record(resend_request, {
  sending_time,
  begin_seq_no,
  end_seq_no,
  fields = []
}).

-record(reject, {
  sending_time,
  ref_seq_num,
  ref_tag_id,
  ref_msg_type,
  session_reject_reason,
  text,
  encoded_text,
  fields = []
}).

-record(sequence_reset, {
  sending_time,
  gap_fill_flag,
  new_seq_no,
  fields = []
}).

-record(logout, {
  sending_time,
  text,
  encoded_text,
  fields = []
}).

-record(logon, {
  sending_time,
  encrypt_method,
  heart_bt_int,
  raw_data,
  reset_seq_num_flag,
  next_expected_msg_seq_num,
  test_message_indicator,
  username,
  password,
  msg_types = [],
  fields = []
}).

