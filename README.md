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

Don't forget to `application:start(fix).`

Now take a look at `fix_session:open/0`. This call creates session, login to broker and subscribe to market data.

