%% @private
-module(thrift_protocol_compact).

-include("thrift_protocol.hrl").

-export([decode_message/1, encode_message/1]).

-define(PROTOCOL_ID, 16#82).
-define(VERSION, 1).

-define(FIELD_TRUE, 1).
-define(FIELD_FALSE, 2).
-define(FIELD_I8, 3).
-define(FIELD_I16, 4).
-define(FIELD_I32, 5).
-define(FIELD_I64, 6).
-define(FIELD_FLOAT, 7).
-define(FIELD_BINARY, 8).
-define(FIELD_LIST, 9).
-define(FIELD_SET, 10).
-define(FIELD_MAP, 11).
-define(FIELD_STRUCT, 12).

-spec encode_message(thrift_protocol:message()) -> iodata().
encode_message(Message) ->
    Type =
        thrift_protocol_byte:from_message_type(Message#thrift_protocol_message.message_type),
    [<<?PROTOCOL_ID, Type:3, ?VERSION:5>>,
     encode_varint32(Message#thrift_protocol_message.sequence_id),
     encode_binary(Message#thrift_protocol_message.method_name) |
     encode_struct(Message#thrift_protocol_message.body)].

-spec encode_binary(binary()) -> iodata().
encode_binary(Bin) ->
    [encode_varint32(byte_size(Bin)) | Bin].

-spec encode_struct(thrift_protocol:struct()) -> iodata().
encode_struct(#thrift_protocol_struct{fields = Fields0}) ->
    encode_struct_fields(lists:sort(maps:to_list(Fields0)), 0).

-spec encode_struct_fields(Fields, PrevFieldId) -> iodata() when
      Fields :: [{thrift_protocol:field_id(), thrift_protocol:data()}],
      PrevFieldId :: thrift_protocol:field_id().
encode_struct_fields([], _PrevFieldId) ->
    <<0>>;
encode_struct_fields([{Id, Data} | Fields], PrevId) ->
    Type =
        case Data of
            true                      -> ?FIELD_TRUE;
            false                     -> ?FIELD_FALSE;
            {i8, _}                   -> ?FIELD_I8;
            {i16, _}                  -> ?FIELD_I16;
            {i32, _}                  -> ?FIELD_I32;
            {i64, _}                  -> ?FIELD_I64;
            N when is_float(N)        -> ?FIELD_FLOAT;
            N when is_binary(N)       -> ?FIELD_BINARY;
            #thrift_protocol_list{}   -> ?FIELD_LIST;
            #thrift_protocol_set{}    -> ?FIELD_SET;
            #thrift_protocol_map{}    -> ?FIELD_MAP;
            #thrift_protocol_struct{} -> ?FIELD_STRUCT
        end,
    Header =
        case Id - PrevId of
            Delta when 0 < Delta, Delta < 16 ->
                <<Delta:4, Type:4>>;
            _ ->
                [<<0:4, Type:4>> | encode_data({i16, Id})]
        end,
    DataBytes =
        case is_boolean(Data) of
            true  -> [];
            false -> encode_data(Data)
        end,
    [Header, DataBytes | encode_struct_fields(Fields, Id)].

-spec encode_data(thrift_protocol:data()) -> iodata().
encode_data(false) -> <<0>>;
encode_data(true) -> <<1>>;
encode_data({i8, N}) -> <<N:8>>;
encode_data({i16, N}) -> encode_data({i32, N});
encode_data({i32, N}) -> encode_varint32(int32_to_zigzag(N));
encode_data({i64, N}) -> encode_varint64(int64_to_zigzag(N));
encode_data(N) when is_float(N) -> <<N/float>>;
encode_data(B) when is_binary(B) -> encode_binary(B);
encode_data(X = #thrift_protocol_struct{}) -> encode_struct(X);
encode_data(#thrift_protocol_map{elements = M}) when M =:= #{} ->
    <<0>>;
encode_data(X = #thrift_protocol_map{}) ->
    #thrift_protocol_map{key_type = KeyType, value_type = ValueType} = X,
    [encode_varint32(maps:size(X#thrift_protocol_map.elements)),
     <<(thrift_protocol_byte:from_data_type(KeyType)):4,
       (thrift_protocol_byte:from_data_type(ValueType)):4>>,
     encode_pairs(maps:to_list(X#thrift_protocol_map.elements), KeyType, ValueType)];
encode_data(#thrift_protocol_set{element_type = Type, elements = Elements}) ->
    encode_elements(Elements, length(Elements), Type);
encode_data(#thrift_protocol_list{element_type = Type, elements = Elements}) ->
    encode_elements(Elements, length(Elements), Type).

-spec encode_pairs(Pairs, KeyType, ValueType) -> iodata() when
      Pairs :: [{thrift_protocol:data(), thrift_protocol:data()}],
      KeyType :: thrift_protocol:data_type(),
      ValueType :: thrift_protocol:data_type().
encode_pairs([], _, _) -> [];
encode_pairs([{K, V} | Pairs], KeyType, ValueType) ->
    KeyType = thrift_protocol:data_type(K),
    ValueType = thrift_protocol:data_type(V),
    [encode_data(K), encode_data(V) | encode_pairs(Pairs, KeyType, ValueType)].

-spec encode_elements(Elements, non_neg_integer(), thrift_protocol:data_type()) ->
                             iodata() when
      Elements :: [thrift_protocol:data()].
encode_elements(Elements, Size, Type) when Size < 15 ->
    TypeByte = thrift_protocol_byte:from_data_type(Type),
    [<<Size:4, TypeByte:4>> | encode_elements(Elements, Type)];
encode_elements(Elements, Size, Type) ->
    TypeByte = thrift_protocol_byte:from_data_type(Type),
    [<<1111:4, TypeByte:4>>, encode_varint32(Size) | encode_elements(Elements, Type)].

-spec encode_elements([thrift_protocol:data()], thrift_protocol:data_type()) -> iodata().
encode_elements([], _Type) -> [];
encode_elements([E | Elements], Type) ->
    Type = thrift_protocol:data_type(E),
    [encode_data(E) | encode_elements(Elements, Type)].

-spec decode_message(binary()) -> {thrift_protocol:message(), binary()}.
decode_message(<<?PROTOCOL_ID, Type:3, ?VERSION:5, Rest0/binary>>) ->
    MessageType = thrift_protocol_byte:to_message_type(Type),
    {SeqId, Rest1} = decode_varint32(Rest0),
    {NameLen, Rest2} = decode_varint32(Rest1),
    <<Name:NameLen/binary, Rest3/binary>> = Rest2,
    {Body, Rest4} = decode_struct(Rest3, 0, #{}),
    Message =
        #thrift_protocol_message{
           method_name = Name,
           message_type = MessageType,
           sequence_id = SeqId,
           body = Body
          },
    {Message, Rest4}.

-spec decode_struct(binary(), PrevId, Fields) -> {thrift_protocol:struct(), binary()} when
      PrevId :: thrift_protocol:field_id(),
      Fields :: #{thrift_protocol:field_id() => thrift_protocol:data()}.
decode_struct(<<0:8, Rest/binary>>, _PrevId, Fields) ->
    {#thrift_protocol_struct{fields = Fields}, Rest};
decode_struct(<<Delta:4, Type:4, Rest0/binary>>, PrevId, Fields0) ->
    {Id, Rest1} = decode_field_id(Rest0, Delta, PrevId),
    {Data, Rest2} = decode_field_data(Rest1, Type),
    Fields1 = maps:put(Id, Data, Fields0),
    decode_struct(Rest2, Id, Fields1).

-spec decode_field_id(binary(), 0..15, thrift_protocol:field_id()) ->
                             {thrift_protocol:field_id(), binary()}.
decode_field_id(Bin, 0, _PrevId) ->
    {{i16, Id}, Rest} = decode_data(Bin, i16),
    {Id, Rest};
decode_field_id(Bin, Delta, PrevId) ->
    {PrevId + Delta, Bin}.

-spec decode_field_data(binary(), byte()) -> {thrift_protocol:data(), binary()}.
decode_field_data(Bin, ?FIELD_TRUE)   -> {true, Bin};
decode_field_data(Bin, ?FIELD_FALSE)  -> {false, Bin};
decode_field_data(Bin, ?FIELD_I8)     -> decode_data(Bin, i8);
decode_field_data(Bin, ?FIELD_I16)    -> decode_data(Bin, i16);
decode_field_data(Bin, ?FIELD_I32)    -> decode_data(Bin, i32);
decode_field_data(Bin, ?FIELD_I64)    -> decode_data(Bin, i64);
decode_field_data(Bin, ?FIELD_FLOAT)  -> decode_data(Bin, float);
decode_field_data(Bin, ?FIELD_BINARY) -> decode_data(Bin, binary);
decode_field_data(Bin, ?FIELD_LIST)   -> decode_data(Bin, list);
decode_field_data(Bin, ?FIELD_SET)    -> decode_data(Bin, set);
decode_field_data(Bin, ?FIELD_MAP)    -> decode_data(Bin, map);
decode_field_data(Bin, ?FIELD_STRUCT) -> decode_data(Bin, struct).

-spec decode_data(binary(), thrift_protocol:data_type()) ->
                         {thrift_protocol:data(), binary()}.
decode_data(<<0, Bin/binary>>, boolean) -> {false, Bin};
decode_data(<<1, Bin/binary>>, boolean) -> {true, Bin};
decode_data(<<N:8/signed, Bin/binary>>, i8) -> {{i8, N}, Bin};
decode_data(Bin0, i16) ->
    {{i32, N}, Bin1} = decode_data(Bin0, i32),
    <<M:16/signed>> = <<N:16>>,
    {{i16, M}, Bin1};
decode_data(Bin0, i32) ->
    {N, Bin1} = decode_varint32(Bin0),
    {{i32, zigzag_to_int(N)}, Bin1};
decode_data(Bin0, i64) ->
    {N, Bin1} = decode_varint64(Bin0),
    {{i64, zigzag_to_int(N)}, Bin1};
decode_data(<<N/float, Bin/binary>>, float) ->
    {N, Bin};
decode_data(Bin0, binary) ->
    {Size, Bin1} = decode_varint32(Bin0),
    <<Bytes:Size/binary, Bin2/binary>> = Bin1,
    {Bytes, Bin2};
decode_data(Bin, struct) ->
    decode_struct(Bin, 0, #{});
decode_data(Bin0, set) ->
    {Type, Elements, Bin1} = decode_elements(Bin0),
    {#thrift_protocol_set{element_type = Type, elements = Elements}, Bin1};
decode_data(Bin0, list) ->
    {Type, Elements, Bin1} = decode_elements(Bin0),
    {#thrift_protocol_list{element_type = Type, elements = Elements}, Bin1};
decode_data(<<0, Bin/binary>>, map) ->
    Map = #thrift_protocol_map{key_type = undefined, value_type = undefined, elements = #{}},
    {Map, Bin};
decode_data(Bin0, map) ->
    {Size, Bin1} = decode_varint32(Bin0),
    <<KeyTypeByte:4, ValueTypeByte:4, Bin2/binary>> = Bin1,
    KeyType = thrift_protocol_byte:to_data_type(KeyTypeByte),
    ValueType = thrift_protocol_byte:to_data_type(ValueTypeByte),
    {Pairs, Bin3} = decode_pairs(Bin2, KeyType, ValueType, Size, #{}),
    Map = #thrift_protocol_map{key_type = KeyType, value_type = ValueType, elements = Pairs},
    {Map, Bin3}.

-spec decode_pairs(binary(), KeyType, ValueType, non_neg_integer(), Pairs) ->
                          {Pairs, binary()} when
      KeyType :: thrift_protocol:data_type(),
      ValueType :: thrift_protocol:data_type(),
      Pairs :: #{thrift_protocol:data() => thrift_protocol:data()}.
decode_pairs(Bin, _KeyType, _ValueType, 0, Pairs) ->
    {Pairs, Bin};
decode_pairs(Bin0, KeyType, ValueType, Size, Pairs) ->
    {Key, Bin1} = decode_data(Bin0, KeyType),
    {Value, Bin2} = decode_data(Bin1, ValueType),
    decode_pairs(Bin2, KeyType, ValueType, Size - 1, maps:put(Key, Value, Pairs)).

-spec decode_elements(binary()) -> {ElementType, Elements, binary()} when
      ElementType :: thrift_protocol:data_type(),
      Elements :: [thrift_protocol:data()].
decode_elements(<<2#1111:4, TypeByte:4, Bin0/binary>>) ->
    Type = thrift_protocol_byte:to_data_type(TypeByte),
    {Size, Bin1} = decode_varint32(Bin0),
    decode_elements(Bin1, Type, Size, []);
decode_elements(<<Size:4, TypeByte:4, Bin/binary>>) ->
    Type = thrift_protocol_byte:to_data_type(TypeByte),
    decode_elements(Bin, Type, Size, []).

-spec decode_elements(binary(), ElementType, non_neg_integer(), Elements) ->
                             {ElementType, Elements, binary()} when
      ElementType :: thrift_protocol:data_type(),
      Elements :: [thrift_protocol:data()].
decode_elements(Bin, Type, 0, Acc) ->
    {Type, lists:reverse(Acc), Bin};
decode_elements(Bin0, Type, Size, Acc) ->
    {Data, Bin1} = decode_data(Bin0, Type),
    decode_elements(Bin1, Type, Size - 1, [Data | Acc]).

-spec zigzag_to_int(integer()) -> integer().
zigzag_to_int(N) ->
    (N bsr 1) bxor -(N band 1).

-spec int32_to_zigzag(integer()) -> integer().
int32_to_zigzag(N) ->
    (N bsl 1) bxor (N bsr 31).

-spec int64_to_zigzag(integer()) -> integer().
int64_to_zigzag(N) ->
    (N bsl 1) bxor (N bsr 63).

-spec encode_varint32(integer()) -> binary().
encode_varint32(N) ->
    encode_varint(N band 16#FFFFFFFF, <<>>).

-spec encode_varint64(integer()) -> binary().
encode_varint64(N) ->
    encode_varint(N band 16#FFFFFFFFFFFFFFFF, <<>>).

-spec encode_varint(non_neg_integer(), binary()) -> binary().
encode_varint(N, Acc) ->
    A = N band 2#01111111,
    B = N bsr 7,
    case B =:= 0 of
        true  -> <<Acc/binary, 0:1, A:7>>;
        false -> encode_varint(B, <<Acc/binary, 1:1, A:7>>)
    end.

-spec decode_varint32(binary()) -> {integer(), binary()}.
decode_varint32(Bin0) ->
    {N, Bin1} = decode_varint(Bin0, 0, 5, 0),
    <<M:32/signed>> = <<N:32>>,
    {M, Bin1}.

-spec decode_varint64(binary()) -> {integer(), binary()}.
decode_varint64(Bin0) ->
    {N, Bin1} = decode_varint(Bin0, 0, 10, 0),
    <<M:64/signed>> = <<N:64>>,
    {M, Bin1}.

-spec decode_varint(binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
                           {non_neg_integer(), binary()}.
decode_varint(<<0:1, N:7, Bin/binary>>, I, _, M) ->
    {(N bsl (I * 7)) + M, Bin};
decode_varint(<<1:1, N:7, Bin/binary>>, I, Max, M) when I =< Max ->
    decode_varint(Bin, I + 1, Max, (N bsl (I * 7)) + M).
