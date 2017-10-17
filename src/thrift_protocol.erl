-module(thrift_protocol).

-include("thrift_protocol.hrl").

-export([encode_message/2, decode_message/2]).
-export([data_type/1]).

-export_type([message/0, message_type/0, struct/0]).
-export_type([field_id/0, data/0, data_type/0]).
-export_type([thrift_map/0, thrift_list/0, set/0]).
-export_type([format/0, i8/0, i16/0, i32/0, i64/0]).

-type format() :: binary | compact.

-type message() :: #thrift_protocol_message{}.

-type message_type() :: call | reply | exception | oneway.

-type struct() :: #thrift_protocol_struct{}.
-type thrift_map() :: #thrift_protocol_map{}.
-type thrift_list() :: #thrift_protocol_list{}.
-type set() :: #thrift_protocol_set{}.

-type field_id() :: integer().
-type i8() :: {i8, integer()}.
-type i16() :: {i16, integer()}.
-type i32() :: {i32, integer()}.
-type i64() :: {i64, integer()}.

-type data_type() :: boolean
                   | i8
                   | i16
                   | i32
                   | i64
                   | float
                   | binary
                   | struct
                   | map
                   | set
                   | list.

-type data() :: boolean()
              | i8()
              | i16()
              | i32()
              | i64()
              | float()
              | binary()
              | struct()
              | thrift_map()
              | set()
              | thrift_list().

-spec encode_message(message(), format()) -> iodata().
encode_message(Message, binary) ->
    thrift_protocol_binary:encode_message(Message);
encode_message(Message, compact) ->
    thrift_protocol_compact:encode_message(Message).

-spec decode_message(binary(), format()) -> {message(), binary()}.
decode_message(Bin, binary) ->
    thrift_protocol_binary:decode_message(Bin);
decode_message(Bin, compact) ->
    thrift_protocol_compact:decode_message(Bin).

-spec data_type(data()) -> data_type().
data_type(X) when is_boolean(X) ->
    boolean;
data_type({i8, _}) ->
    i8;
data_type({i16, _}) ->
    i16;
data_type({i32, _}) ->
    i32;
data_type({i64, _}) ->
    i64;
data_type(X) when is_float(X) ->
    float;
data_type(X) when is_binary(X) ->
    binary;
data_type(#thrift_protocol_struct{}) ->
    struct;
data_type(#thrift_protocol_map{}) ->
    map;
data_type(#thrift_protocol_set{}) ->
    set;
data_type(#thrift_protocol_list{}) ->
    list.
