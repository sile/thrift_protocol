-module(thrift_protocol_compact_tests).

-include_lib("eunit/include/eunit.hrl").
-include("thrift_protocol.hrl").

decode_test() ->
    Input = bytes(),
    {Message, <<>>} = thrift_protocol_compact:decode_message(Input),
    ?assertEqual(message(), Message).

encode_test() ->
    Bytes = thrift_protocol_compact:encode_message(message()),
    {Message, <<>>} = thrift_protocol_compact:decode_message(list_to_binary(Bytes)),
    ?assertEqual(message(), Message),
    ?assertEqual(lists:sort(binary_to_list(bytes())),
                 lists:sort(binary_to_list(list_to_binary(Bytes)))).

-spec bytes() -> binary().
bytes() ->
    <<130,129,1,9,101,109,105,116,66,97,116,99,104,28,28,24,11,102,
      111,111,95,115,101,114,118,105,99,101,25,60,24,14,106,97,101,
      103,101,114,46,118,101,114,115,105,111,110,21,0,24,8,71,111,
      45,50,46,57,46,48,0,24,8,104,111,115,116,110,97,109,101,21,0,
      24,3,117,98,117,0,24,2,105,112,21,0,24,9,49,48,46,48,46,50,
      46,49,53,0,0,25,28,22,128,203,251,150,247,243,173,5,22,0,22,
      170,12,22,0,24,4,109,97,105,110,37,2,22,128,203,251,150,247,
      243,173,5,22,128,137,15,0,0,0>>.

-spec message() -> thrift_protocol:message().
message() ->
    Process =
        #thrift_protocol_struct{
           fields =
               #{1 => <<"foo_service">>,
                 2 => #thrift_protocol_list{
                         element_type = struct,
                         elements =
                             [
                              #thrift_protocol_struct{
                                 fields = #{1 => <<"jaeger.version">>,
                                            2 => {i32,0},
                                            3 => <<"Go-2.9.0">>}},
                              #thrift_protocol_struct{
                                 fields = #{1 => <<"hostname">>,
                                            2 => {i32,0},
                                            3 => <<"ubu">>}},
                              #thrift_protocol_struct{
                                 fields = #{1 => <<"ip">>,
                                            2 => {i32,0},
                                            3 => <<"10.0.2.15">>}}
                             ]
                        }
                }
          },
    Spans =
        #thrift_protocol_list{
           element_type = struct,
           elements =
               [#thrift_protocol_struct{
                   fields =
                       #{1 => {i64,1508322611000000},
                         2 => {i64,0},
                         3 => {i64,789},
                         4 => {i64,0},
                         5 => <<"main">>,
                         7 => {i32,1},
                         8 => {i64,1508322611000000},
                         9 => {i64,123456}}}]
          },
    Batch =
        #thrift_protocol_struct{fields = #{1 => Process, 2 => Spans}},
    #thrift_protocol_message{
       method_name = <<"emitBatch">>,
       message_type = oneway,
       sequence_id = 1,
       body = #thrift_protocol_struct{fields = #{1 => Batch}}
      }.
