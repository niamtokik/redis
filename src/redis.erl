%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(redis).
-export([encode/1, decode/1]).
-include_lib("eunit/include/eunit.hrl").

-type encode_error() :: {error, bitstring() | binary()}.
-type encode_integer() :: integer().
-type encode_string() :: bitstring() | binary().
-type encode_bulk_string() :: {bulk_string, bitstring()}.
-type encode_array() :: [encode_integer() |
                         encode_string() |
                         encode_bulk_string()].
-type encode_types() :: encode_integer() | 
                        encode_string() |
                        encode_bulk_string() |
                        encode_array().
-type encode_return_ok() :: bitstring() | binary().
-type encode_return_error() :: {error, atom()}.

%%-------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------
-spec encode(Data) -> Return when
      Data :: encode_types(),
      Return :: encode_return_ok() | encode_return_error().
encode(Integer)
  when is_integer(Integer) ->
    Value = erlang:integer_to_binary(Integer),
    <<":", Value/bitstring, "\r\n">>;
encode(Bitstring) 
  when is_bitstring(Bitstring) ->
    case Return = encode_valid_char(Bitstring) of
        {ok, Valid} -> <<"+", Valid/bitstring, "\r\n">>;
        Return -> Return
    end;
encode({bulk_string, null}) ->
    <<"$-1\r\n">>;
encode({bulk_string, Bitstring})
  when is_bitstring(Bitstring) ->
    Length = erlang:integer_to_binary(erlang:size(Bitstring)),
    <<"$", Length/bitstring, "\r\n", Bitstring/bitstring, "\r\n">>;
encode([]) ->
    <<"*0\r\n">>;
encode(Array) 
  when is_list(Array) ->
    {ok, Elements, Counter} = encode_array(Array, <<>>, 0),
    C = erlang:integer_to_binary(Counter),
    <<"*", C/bitstring, "\r\n", Elements/bitstring>>;
encode({error, Bitstring})
  when is_bitstring(Bitstring) ->
    case Return = encode_valid_char(Bitstring) of
        {ok, Valid} -> <<"-", Valid/bitstring, "\r\n">>;
         Return -> Return
    end.

encode_test() ->
    [{"encode bitstring", [?assertEqual(<<"+OK\r\n">>
                                       ,encode(<<"OK">>))
                          ,?assertEqual({error, badchar}
                                       ,encode(<<"OK\r">>))
                          ,?assertEqual({error, badchar}
                                       ,encode(<<"OK\n">>))
                          ,?assertEqual({error, badchar}
                                       ,encode(<<"OK\r\n">>))
                          ]}
    ,{"encode integer", [?assertEqual(<<":0\r\n">>
                                     ,encode(0))
                        ,?assertEqual(<<":10000\r\n">>
                                     ,encode(10000))
                        ]}
    ,{"encode bulk string", [?assertEqual(<<"$-1\r\n">>
                                         ,encode({bulk_string, null}))
                            ,?assertEqual(<<"$0\r\n\r\n">>
                                         ,encode({bulk_string, <<>>}))
                            ,?assertEqual(<<"$6\r\nfoobar\r\n">>
                                         ,encode({bulk_string, <<"foobar">>}))
                            ]}
    ,{"encode array", [?assertEqual(<<"*0\r\n">>, encode([]))
                      ,?assertEqual(<<"*3\r\n:1\r\n:2\r\n:3\r\n">>
                                   ,encode([1,2,3]))
                      ,?assertEqual(<<"*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n">>
                                   ,encode([{bulk_string, <<"foo">>}
                                           ,{bulk_string, <<"bar">>}]))
                      ,?assertEqual(<<"*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$6\r\nfoobar\r\n">>
                                   ,encode([1,2,3,4,{bulk_string, <<"foobar">>}]))
                      ,?assertEqual(<<"*1\r\n*0\r\n">>
                                   ,encode([[]]))
                      ,?assertEqual(<<"*1\r\n*3\r\n:1\r\n:2\r\n:3\r\n">>
                                   ,encode([[1,2,3]]))
                      ]}
    ,{"encode error", [?assertEqual(<<"-Error message\r\n">>
                                   ,encode({error, <<"Error message">>}))
                      ,?assertEqual(<<"-ERR unknown command 'foobar'\r\n">>
                                   ,encode({error, <<"ERR unknown command 'foobar'">>}))
                      ,?assertEqual(<<"-WRONGTYPE Operation against a key holding the wrong kind of value\r\n">>
                                   ,encode({error, <<"WRONGTYPE Operation against a key holding the wrong kind of value">>}))
                      ]}
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_valid_char(Bitstring) -> Return when
      Bitstring :: bitstring() | binary(),
      Return :: {ok, bitstring() | binary()} |
                {error, badchar}.
encode_valid_char(Bitstring)
  when is_bitstring(Bitstring) ->
    try
        nomatch = binary:match(Bitstring, <<"\r">>),
        nomatch = binary:match(Bitstring, <<"\n">>)
    of
        _ -> {ok, Bitstring}
    catch
        error:_ -> {error, badchar}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_array(List, Buffer, Counter) -> Return when
      List :: list(),
      Buffer :: bitstring() | binary(),
      Counter :: integer(),
      Return :: bitstring() | binary().
encode_array([], Buffer, Counter) ->
    {ok, Buffer, Counter};
encode_array([H|T], Buffer, Counter) ->
    Data = encode(H),
    Return = <<Buffer/bitstring, Data/bitstring>>,
    encode_array(T, Return, Counter+1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode(<<"+", Rest/bitstring>>) ->
    decode_string(Rest);
decode(<<":", Rest/bitstring>>) ->
    decode_integer(Rest);
decode(<<"$", Rest/bitstring>>) ->
    decode_bulk_string(Rest);
decode(<<"*", Rest/bitstring>>) ->
    decode_array(Rest);
decode(<<"-", Rest/bitstring>>) ->
    decode_error(Rest).

decode_test() ->
    [{"decode integer", [?assertEqual({ok, 0}
                                     ,decode(<<":0\r\n">>))
                        ,?assertEqual({ok, 1}
                                     ,decode(<<":1\r\n">>))
                        ,?assertEqual({ok, 999999}
                                     ,decode(<<":999999\r\n">>))
                        ]}
     ,{"decode simple string", [?assertEqual({ok, <<>>}
                                            ,decode(<<"+\r\n">>))
                               ,?assertEqual({ok, <<"test">>}
                                            ,decode(<<"+test\r\n">>))
                               ]}
    ,{"decode bulk string", [?assertEqual({ok, <<"test">>}
                                         ,decode(<<"$4\r\ntest\r\n">>))
                            ]}
    ,{"decode array", [?assertEqual({ok, []}
                                    ,decode(<<"*0\r\n">>))
                      ,?assertEqual({ok, [1,2,3]}
                                   ,decode(<<"*3\r\n:1\r\n:2\r\n:3\r\n">>))
                      ]}
    ,{"decode error", [?assertEqual({ok, {error, <<"ERR">>}}
                                   ,decode(<<"-ERR\r\n">>))
                      ]}
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_string(Bitstring) ->
    decode_string(Bitstring, <<>>).

decode_string(<<"\r\n">>, Buffer) ->
    {ok, Buffer};
decode_string(<<"\r\n", Rest/bitstring>>, Buffer) ->
    {ok, Buffer, Rest};
decode_string(<<"\r", _/bitstring>>, _) ->
    {error, badchar};
decode_string(<<"\n", _/bitstring>>, _) ->
    {error, badchar};
decode_string(<<Char, Rest/bitstring>>, Buffer) ->
    decode_string(Rest, <<Buffer/bitstring, Char>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_integer(Integer) ->
    decode_integer(Integer, <<>>).

decode_integer(<<"\r\n">>, Buffer) ->
    {ok, erlang:binary_to_integer(Buffer)};
decode_integer(<<"\r\n", Rest/bitstring>>, Buffer) ->
    {ok, erlang:binary_to_integer(Buffer), Rest};
decode_integer(<<Char, Rest/bitstring>>, Buffer) 
  when Char >= $0 andalso Char =< $9 ->
    decode_integer(Rest, <<Buffer/bitstring, Char>>).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_separator(Bitstring) ->
    case binary:split(Bitstring, <<"\r\n">>) of
        [<<>>] -> {ok, <<>>};
        [<<>>,<<>>] -> {ok, <<>>};
        [Value,Rest] -> {ok, Value, Rest}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_bulk_string(<<"-1\r\n">>) ->
    {ok, nil};
decode_bulk_string(<<"-1\r\n", Rest/bitstring>>) ->
    {ok, nil, Rest};
decode_bulk_string(Bitstring) ->
    {ok, Size, Rest} = decode_integer(Bitstring),
    Length = Size*8,
    <<String:Length/bitstring, "\r\n", R/bitstring>> = Rest,
    case R of
        <<>> -> {ok, String};
        _ -> {ok, String, R}
    end.
             
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_array(<<"0\r\n">>) ->
    {ok, []};
decode_array(<<"0\r\n", Rest/bitstring>>) ->
    {ok, [], Rest};
decode_array(Bitstring) ->
    {ok, Size, Rest} = decode_integer(Bitstring),
    decode_array(Rest, Size, []).

decode_array(<<>>, 0, Buffer) ->
    {ok, lists:reverse(Buffer)};
decode_array(Rest, 0, Buffer) ->
    {ok, lists:reverse(Buffer), Rest};
decode_array(Bitstring, Size, Buffer) ->
    case decode(Bitstring) of
        {ok, Result} ->
            decode_array(<<>>, Size-1, [Result|Buffer]);
        {ok, Result, Rest} -> 
            decode_array(Rest, Size-1, [Result|Buffer])
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
decode_error(Bitstring) ->
    decode_error(Bitstring, <<>>).

decode_error(<<"\r\n">>, Buffer) ->
    {ok, {error, Buffer}};
decode_error(<<"\r", Rest/bitstring>>, _) ->
    {error, badchar};
decode_error(<<"\n", Rest/bitstring>>, _) ->
    {error, badchar};
decode_error(<<Char, Rest/bitstring>>, Buffer) ->
    decode_error(Rest, <<Buffer/bitstring, Char>>).


