-record(thrift_protocol_message,
        {
          method_name  :: binary(),
          message_type :: thrift_protocol:message_type(),
          sequence_id  :: integer(),
          body         :: thrift_protocol:struct()
        }).

-record(thrift_protocol_struct,
        {
          fields :: #{thrift_protocol:field_id() => thrift_protocol:data()}
        }).

-record(thrift_protocol_map,
        {
          key_type   :: thrift_protocol:data_type() | undefined,
          value_type :: thrift_protocol:data_type() | undefined,
          elements   :: #{thrift_protocol:data() => thrift_protocol:data()}
        }).

-record(thrift_protocol_set,
        {
          element_type :: thrift_protocol:data_type(),
          elements     :: [thrift_protocol:data()]
        }).

-record(thrift_protocol_list,
        {
          element_type :: thrift_protocol:data_type(),
          elements     :: [thrift_protocol:data()]
        }).
