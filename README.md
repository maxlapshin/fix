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

Empty here yet.
