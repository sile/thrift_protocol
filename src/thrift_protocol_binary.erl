%% @private
-module(thrift_protocol_binary).

-include("thrift_protocol.hrl").

-export([encode_message/1, decode_message/1, encode_struct/1, decode_struct/2]).

-define(VERSION, 1).

-spec decode_message(binary()) -> {thrift_protocol:message(), binary()}.
decode_message(<<1:1, ?VERSION:15, _:8, 0:5, TypeByte:3, NameLen:32, Name:NameLen/binary, SeqId:32/signed, Rest0/binary>>) ->
    Type = thrift_protocol_byte:to_message_type(TypeByte),
    {Body, Rest1} = decode_struct(Rest0, #{}),
    Message =
        #thrift_protocol_message{
           method_name = Name,
           message_type = Type,
           sequence_id = SeqId,
           body = Body
          },
    {Message, Rest1}.

-spec decode_struct(binary(), Fields) -> {thrift_protocol:struct(), binary()} when
      Fields :: #{thrift_protocol:field_id() => thrift_protocol:data()}.
decode_struct(<<0:8, Rest/binary>>, Fields) ->
    {#thrift_protocol_struct{fields = Fields}, Rest};
decode_struct(<<TypeByte:8, Id:16/signed, Rest0/binary>>, Fields0) ->
    Type = thrift_protocol_byte:to_data_type(TypeByte),
    {Data, Rest1} = decode_data(Type, Rest0),
    Fields1 = maps:put(Id, Data, Fields0),
    decode_struct(Rest1, Fields1).

-spec decode_elements(binary(), thrift_protocol:data_type(), non_neg_integer(), [thrift_protocol:data()]) ->
                             {[thrift_protocol:data()], binary()}.
decode_elements(Bin, _Type, 0, Elements) ->
    {lists:reverse(Elements), Bin};
decode_elements(Bin, Type, Size, Elements) ->
    {Element, Rest} = decode_data(Type, Bin),
    decode_elements(Rest, Type, Size - 1, [Element | Elements]).

-spec decode_pairs(binary(), KeyType, ValueType, Size, Acc) -> {Acc, binary()} when
      KeyType :: thrift_protocol:data_type(),
      ValueType :: thrift_protocol:data_type(),
      Size :: non_neg_integer(),
      Acc :: #{thrift_protocol:data() => thrift_protocol:data()}.
decode_pairs(Bin, _, _, 0, Pairs) ->
    {Pairs, Bin};
decode_pairs(Bin, KeyType, ValueType, Size, Pairs0) ->
    {Key, Rest0} = decode_data(KeyType, Bin),
    {Value, Rest1} = decode_data(ValueType, Rest0),
    Pairs1 = maps:put(Key, Value, Pairs0),
    decode_pairs(Rest1, KeyType, ValueType, Size - 1, Pairs1).

-spec decode_data(thrift_protocol:data_type(), binary()) -> {thrift_protocol:data(), binary()}.
decode_data(boolean, <<0, Rest/binary>>) -> {false, Rest};
decode_data(boolean, <<1, Rest/binary>>) -> {true, Rest};
decode_data(i8, <<N:8/signed, Rest/binary>>) -> {{i8, N}, Rest};
decode_data(i16, <<N:16/signed, Rest/binary>>) -> {{i16, N}, Rest};
decode_data(i32, <<N:32/signed, Rest/binary>>) -> {{i32, N}, Rest};
decode_data(i64, <<N:64/signed, Rest/binary>>) -> {{i64, N}, Rest};
decode_data(float, <<N/float, Rest/binary>>) -> {N, Rest};
decode_data(binary, <<Len:32, Bin:Len/binary, Rest/binary>>) -> {Bin, Rest};
decode_data(struct, Bin) -> decode_struct(Bin, #{});
decode_data(map, <<K:8, V:8, Size:32, Bin/binary>>) ->
    KeyType = thrift_protocol_byte:to_data_type(K),
    ValueType = thrift_protocol_byte:to_data_type(V),
    {Pairs, Rest} = decode_pairs(Bin, KeyType, ValueType, Size, #{}),
    {#thrift_protocol_map{key_type = KeyType, value_type = ValueType, elements = Pairs}, Rest};
decode_data(set, <<TypeByte:8, Size:32, Bin/binary>>) ->
    Type = thrift_protocol_byte:to_data_type(TypeByte),
    {Elements, Rest} = decode_elements(Bin, Type, Size, []),
    {#thrift_protocol_set{element_type = Type, elements = Elements}, Rest};
decode_data(list, <<TypeByte:8, Size:32, Bin/binary>>) ->
    Type = thrift_protocol_byte:to_data_type(TypeByte),
    {Elements, Rest} = decode_elements(Bin, Type, Size, []),
    {#thrift_protocol_list{element_type = Type, elements = Elements}, Rest}.

-spec encode_message(thrift_protocol:message()) -> iodata().
encode_message(Message) ->
    #thrift_protocol_message{method_name = Name, sequence_id = SeqId, body = Body} = Message,
    Type = thrift_protocol_byte:from_message_type(Message#thrift_protocol_message.message_type),
    [<<1:1, ?VERSION:15, 0:8, 0:5, Type:3, (byte_size(Name)):32, Name/binary, SeqId:32>> | encode_struct(Body)].

-spec encode_struct(thrift_protocol:struct()) -> iodata().
encode_struct(#thrift_protocol_struct{fields = Fields}) ->
    maps:fold(
      fun (Id, Data, Acc) ->
              Type = thrift_protocol_byte:from_data_type(thrift_protocol:data_type(Data)),
              [<<Type:8, Id:16>>, encode_data(Data) | Acc]
      end,
      [<<0:8>>],
      Fields).

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
encode_data(X = #thrift_protocol_struct{}) ->
    encode_struct(X);
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
    [<<(thrift_protocol_byte:from_data_type(KeyType)):8,
       (thrift_protocol_byte:from_data_type(ValueType)):8,
       (maps:size(Elements)):32>> | IoData];
encode_data(#thrift_protocol_set{element_type = Type, elements = Elements}) ->
    Size = length(Elements),
    [<<(thrift_protocol_byte:from_data_type(Type)):8, Size:32>> |
     [begin
          Type = thrift_protocol:data_type(E),
          encode_data(E)
      end || E <- Elements]];
encode_data(#thrift_protocol_list{element_type = Type, elements = Elements}) ->
    Size = length(Elements),
    [<<(thrift_protocol_byte:from_data_type(Type)):8, Size:32>> |
     [begin
          Type = thrift_protocol:data_type(E),
          encode_data(E)
      end || E <- Elements]].
