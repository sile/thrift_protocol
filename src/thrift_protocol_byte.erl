-module(thrift_protocol_byte).

-include("constants.hrl").

-export([from_message_type/1, from_data_type/1]).
-export([to_message_type/1, to_data_type/1]).

-spec to_message_type(byte()) -> thrift_protocol:message_type().
to_message_type(?MESSAGE_TYPE_CALL)      -> call;
to_message_type(?MESSAGE_TYPE_REPLY)     -> reply;
to_message_type(?MESSAGE_TYPE_EXCEPTION) -> exception;
to_message_type(?MESSAGE_TYPE_ONEWAY)    -> oneway.

-spec to_data_type(byte()) -> thrift_protocol:data_type().
to_data_type(?DATA_TYPE_BOOLEAN) -> boolean;
to_data_type(?DATA_TYPE_I8)      -> i8;
to_data_type(?DATA_TYPE_I16)     -> i16;
to_data_type(?DATA_TYPE_I32)     -> i32;
to_data_type(?DATA_TYPE_I64)     -> i64;
to_data_type(?DATA_TYPE_FLOAT)   -> float;
to_data_type(?DATA_TYPE_BINARY)  -> binary;
to_data_type(?DATA_TYPE_STRUCT)  -> struct;
to_data_type(?DATA_TYPE_MAP)     -> map;
to_data_type(?DATA_TYPE_SET)     -> set;
to_data_type(?DATA_TYPE_LIST)    -> list.

-spec from_message_type(thrift_protocol:message_type()) -> byte().
from_message_type(call)      -> ?MESSAGE_TYPE_CALL;
from_message_type(reply)     -> ?MESSAGE_TYPE_REPLY;
from_message_type(exception) -> ?MESSAGE_TYPE_EXCEPTION;
from_message_type(oneway)    -> ?MESSAGE_TYPE_ONEWAY.

-spec from_data_type(thrift_protocol:data_type()) -> byte().
from_data_type(boolean) -> ?DATA_TYPE_BOOLEAN;
from_data_type(i8)      -> ?DATA_TYPE_I8;
from_data_type(i16)     -> ?DATA_TYPE_I16;
from_data_type(i32)     -> ?DATA_TYPE_I32;
from_data_type(i64)     -> ?DATA_TYPE_I64;
from_data_type(float)   -> ?DATA_TYPE_FLOAT;
from_data_type(binary)  -> ?DATA_TYPE_BINARY;
from_data_type(struct)  -> ?DATA_TYPE_STRUCT;
from_data_type(map)     -> ?DATA_TYPE_MAP;
from_data_type(set)     -> ?DATA_TYPE_SET;
from_data_type(list)    -> ?DATA_TYPE_LIST.
