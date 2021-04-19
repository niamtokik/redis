%-type encode_error() :: {error, bitstring() | binary()}.
-type encode_integer() :: integer().
-type encode_string() :: bitstring() | binary().
-type encode_bulk_string() :: {bulk_string, bitstring() | null} | list() | {error, binary()}.
-type encode_array() :: [encode_integer() |
                         encode_string() |
                         encode_bulk_string()].
-type encode_types() :: encode_integer() | 
                        encode_string() |
                        encode_bulk_string() |
                        encode_array().
-type encode_return_ok() :: bitstring() | binary().
-type encode_return_error() :: {error, atom()}.
