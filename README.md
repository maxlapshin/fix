FIX protocol
============


This code is implementation of http://fixprotocol.org/ 
It is a trading protocol that allows to make orders, receive quotes and do many other things, described in protocol.

This repo includes specification of different FIX versions taken from http://quickfix.org/ project.

`fix_template` is a parser generator that takes XML description of protocol and generates `fix_parser.erl` from it.

Also `include/admin.hrl` and `include/business.hrl` headers are generated from XML.

Mention that FIX names such as `RefSeqNum` are translated to more convenient `ref_seq_num`


Usage
=====

Edit fix.conf.sample and put it into root of your project.
Mention that `fix_read` should be your broker and `fix_proxy` is for proxying your market requests via your server.
But let us start from easy usage.

Don't forget to `application:start(fix).`


Now let's subscribe to market data:

```
fix_reader:subscribe(fix_read, 'NASDAQ.AAPL')
```

What is going now? Launch `appmon:start()` and see that there are new spawned processes in supervisor tree of fix application.
You have connected to broker and now your process will receive market data.

Include header:

```
-include_lib("fix/include/business.hrl").
```

and use structure
```
#market_data_snapshot_full_refresh{}
```

from it.
