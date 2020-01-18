%% @doc An Erlang implementation of Thrift protocol.
%%
%% == Examples ==
%%
%% ```
%% Body =
%%     #thrift_protocol_struct{
%%         fields = #{1 => true, 2 => {i8, -1}}
%%     },
%% Message =
%%     #thrift_protocol_message{
%%         method_name = <<"foo">>,
%%         message_type = call,
%%         sequence_id = 0,
%%         body = Body
%%     },
%%
%% Encoded = list_to_binary(thrift_protocol:encode_message(Message, compact)),
%% <<130,33,0,3,102,111,111,17,19,255,0>> = Encoded,
%%
%% {Decoded, <<>>} = thrift_protocol:decode_message(Encoded, compact),
%% Message = Decoded.
%% '''
%%
%% == References ==
%%
%% <ul>
%%   <li><a href="https://github.com/apache/thrift/blob/master/doc/specs/thrift-protocol-spec.md">Thrift Protocol Structure</a></li>
%%   <li><a href="https://github.com/apache/thrift/blob/master/doc/specs/thrift-binary-protocol.md">Thrift Binary protocol encoding</a></li>
%%   <li><a href="https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md">Thrift Compact protocol encoding</a></li>
%% </ul>
-module(thrift_protocol).

-include("thrift_protocol.hrl").

-export([encode_message/2, decode_message/2, encode_struct/2, decode_struct/2]).
-export([data_type/1]).

-export_type([message/0, message_type/0]).
-export_type([data/0, data_type/0]).
-export_type([struct/0, field_id/0]).
-export_type([thrift_map/0]).
-export_type([thrift_list/0]).
-export_type([set/0]).
-export_type([i8/0, i16/0, i32/0, i64/0]).
-export_type([format/0]).

-type format() :: binary | compact.
%% Protocol encoding format.

-type message() :: #thrift_protocol_message{}.
%% Message.

-type message_type() :: call | reply | exception | oneway.
%% Message type.

-type struct() :: #thrift_protocol_struct{}.
%% Struct.

-type field_id() :: integer().
%% The identifier of a struct field (16-bit signed integer).

-type thrift_map() :: #thrift_protocol_map{}.
%% Map.

-type thrift_list() :: #thrift_protocol_list{}.
%% List.

-type set() :: #thrift_protocol_set{}.
%% Set.

-type i8() :: {i8, integer()}.
%% 8-bit signed integer.

-type i16() :: {i16, integer()}.
%% 16-bit signed integer.

-type i32() :: {i32, integer()}.
%% 32-bit signed integer.

-type i64() :: {i64, integer()}.
%% 64-bit signed integer.

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
%% Data type.

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
%% Data.

%% @doc Encodes `Message'.
-spec encode_message(message(), Format :: format()) -> iodata().
encode_message(Message, binary) ->
    thrift_protocol_binary:encode_message(Message);
encode_message(Message, compact) ->
    thrift_protocol_compact:encode_message(Message).

%% @doc Encodes `Struct'.
-spec encode_struct(struct(), Format :: format()) -> iodata().
encode_struct(Struct, binary) ->
    thrift_protocol_binary:encode_struct(Struct);
encode_struct(Struct, compact) ->
    thrift_protocol_compact:encode_struct(Struct).

%% @doc Decodes a message from the given binary.
-spec decode_message(binary(), Format :: format()) -> {message(), binary()}.
decode_message(Bin, binary) ->
    thrift_protocol_binary:decode_message(Bin);
decode_message(Bin, compact) ->
    thrift_protocol_compact:decode_message(Bin).

%% @doc Decodes a struct from the given binary.
-spec decode_struct(binary(), Format :: format()) -> {struct(), binary()}.
decode_struct(Bin, binary) ->
    thrift_protocol_binary:decode_struct(Bin, #{});
decode_struct(Bin, compact) ->
    thrift_protocol_compact:decode_struct(Bin, 0, #{}).

%% @doc Returns the type of `Data'.
-spec data_type(Data :: data()) -> data_type().
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
example_test() ->
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
    ?assertEqual(<<130,33,0,3,102,111,111,17,19,255,0>>, Encoded),

    {Decoded, <<>>} = thrift_protocol:decode_message(Encoded, compact),
    ?assertEqual(Message, Decoded).
-endif.
