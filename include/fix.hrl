
-record(fix, {
  pid :: pid(),
  message,
  bin :: binary()
}).

-type fix() :: #fix{}.
