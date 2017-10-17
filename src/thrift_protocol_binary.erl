-module(thrift_protocol_binary).

-include("thrift_protocol.hrl").
-include("constants.hrl").

-export([encode_message/1]).

-define(VERSION, 1).

-spec encode_message(thrift_protocol:message()) -> iodata().
encode_message(Message) ->
    #thrift_protocol_message{method_name = Name, sequence_id = SeqId, body = Body} = Message,
    Type =
        case Message#thrift_protocol_message.message_type of
            call      -> ?MESSAGE_TYPE_CALL;
            reply     -> ?MESSAGE_TYPE_REPLY;
            exception -> ?MESSAGE_TYPE_EXCEPTION;
            oneway    -> ?MESSAGE_TYPE_ONEWAY
        end,
    [<<1:1, ?VERSION:15, 0:8, 0:5, Type:3, (byte_size(Name)):32, Name/binary, SeqId:32>> | encode_data(Body)].

-spec encode_data(thrift_protocol:data()) -> iodata().
encode_data(true) ->
    <<1:8>>;
encode_data(false) ->
    <<0:8>>;
encode_data({i8, N}) ->
    <<N:8>>;
encode_data({i16, N}) ->
    <<N:16>>;
encode_data({i32, N}) ->
    <<N:32>>;
encode_data({i64, N}) ->
    <<N:64>>;
encode_data(N) when is_float(N)->
    <<N/float>>;
encode_data(B) when is_binary(B) ->
    [<<(byte_size(B)):32>> | B];
encode_data(#thrift_protocol_struct{fields = Fields}) ->
    maps:fold(
      fun (Id, Data, Acc) ->
              Type = data_type_to_byte(thrift_protocol:data_type(Data)),
              [<<Type:8, Id:32>>, encode_data(Data) | Acc]
      end,
      [<<0:32>>],
      Fields);
encode_data(#thrift_protocol_map{key_type = KeyType, value_type = ValueType, elements = Elements}) ->
    IoData =
        maps:fold(
          fun (K, V, Acc) ->
                  KeyType = thrift_protocol:data_type(K),
                  ValueType = thrift_protocol:data_type(V),
                  [encode_data(K), encode_data(V) | Acc]
          end,
          [],
          Elements),
    [<<(data_type_to_byte(KeyType)):8, (data_type_to_byte(ValueType)):8, (maps:size(Elements)):32>> | IoData];
encode_data(#thrift_protocol_set{element_type = Type, elements = Elements}) ->
    Size = length(Elements),
    [<<(data_type_to_byte(Type)):8, Size:32>> |
     [begin
          Type = thrift_protocol:data_type(E),
          encode_data(E)
      end || E <- Elements]];
encode_data(#thrift_protocol_list{element_type = Type, elements = Elements}) ->
    Size = length(Elements),
    [<<(data_type_to_byte(Type)):8, Size:32>> |
     [begin
          Type = thrift_protocol:data_type(E),
          encode_data(E)
      end || E <- Elements]].

-spec data_type_to_byte(thrift_protocol:data_type()) -> byte().
data_type_to_byte(boolean) -> ?DATA_TYPE_BOOLEAN;
data_type_to_byte(i8)      -> ?DATA_TYPE_I8;
data_type_to_byte(i16)     -> ?DATA_TYPE_I16;
data_type_to_byte(i32)     -> ?DATA_TYPE_I32;
data_type_to_byte(i64)     -> ?DATA_TYPE_I64;
data_type_to_byte(float)   -> ?DATA_TYPE_FLOAT;
data_type_to_byte(binary)  -> ?DATA_TYPE_BINARY;
data_type_to_byte(struct)  -> ?DATA_TYPE_STRUCT;
data_type_to_byte(map)     -> ?DATA_TYPE_MAP;
data_type_to_byte(set)     -> ?DATA_TYPE_SET;
data_type_to_byte(list)    -> ?DATA_TYPE_LIST.
