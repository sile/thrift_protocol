thrift_protocol
================

[![hex.pm version](https://img.shields.io/hexpm/v/thrift_protocol.svg)](https://hex.pm/packages/thrift_protocol)
[![Build Status](https://travis-ci.org/sile/thrift_protocol.svg?branch=master)](https://travis-ci.org/sile/thrift_protocol)
[![Code Coverage](https://codecov.io/gh/sile/thrift_protocol/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/thrift_protocol/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

An Erlang implementation of [Thrift](https://thrift.apache.org/) protocol.

[Documentation](https://hexdocs.pm/thrift_protocol/)

Examples
---------

```erlang
Body =
    #thrift_protocol_struct{
        fields = #{1 => true, 2 => {i8, -1}}
    },
Message =
    #thrift_protocol_message{
        method_name = <<"foo">>,
        message_type = call,
        sequence_id = 0,
        body = Body
    },

Encoded = list_to_binary(thrift_protocol:encode_message(Message, compact)),
<<130,33,0,3,102,111,111,17,19,255,0>> = Encoded,

{Decoded, <<>>} = thrift_protocol:decode_message(Encoded, compact),
Message = Decoded.
```

References
-----------

- [Thrift Protocol Structure][protocol-structure]
- [Thrift Binary protocol encoding][binary-encoding]
- [Thrift Compact protocol encoding][compact-encoding]

[protocol-structure]: https://github.com/apache/thrift/blob/master/doc/specs/thrift-protocol-spec.md
[binary-encoding]: https://github.com/apache/thrift/blob/master/doc/specs/thrift-binary-protocol.md
[compact-encoding]: https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md
