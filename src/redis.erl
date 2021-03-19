-module(redis).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-type resp_type() :: simple_string | error | integer | bulk_string | array.
-type resp_simple_string() :: {simple_string, bitstring()}.
-type resp_error() :: {error, bitstring()}.
-type resp_integer() :: {integer, integer()}.
-type resp_bulk_string() :: {bulk_string, bitstring()}.
-type resp_types() :: resp_simple_string() |
                      resp_integer() |
                      resp_bulk_string().
                      
-type resp_array() :: {array, [resp_types()]}.


-spec resp(Rest) -> Return when
      Rest :: resp(),
      Return :: bitstring().
resp(simple_string) -> <<"+">>;
resp(error) -> <<"-">>;
resp(integer) -> <<":">>;
resp(bulk_string) -> <<"$">>;
resp(array) -> <<"*">>.



ok() ->
    <<"+OK\r\n">>.

error(Message) ->
    <<"-ERR ", Message, "\r\n">>.

wrongtype(Message) ->
    <<"-WRONGTYPE ", Message, "\r\n">>.

string(String) ->
    Length = erlang:size(String),
    LengthStr = erlang:integer_to_binary(Length),
    case Length of
        0 -> <<"$0\r\n\r\n">>;
        _ -> <<"$", LengthStr, "\r\n", String, "\r\n">>
    end.

     
