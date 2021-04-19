%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(redis_command).
-compile(export_all).
-compile({no_auto_import,[get/0]}).
-include("redis.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec copy() -> Return when
      Return :: encode_bulk_string().
copy() -> 
    {bulk_string, <<"COPY">>}.

-spec copy(Source, Destination) -> Return when
      Source :: encode_bulk_string(),
      Destination :: encode_bulk_string(),
      Return :: any().

copy(Source, Destination) ->
    copy(Source, Destination, []).

-spec copy(Source, Destination, Args) -> Return when
      Source :: encode_bulk_string(),
      Destination :: encode_bulk_string(),
      Args :: list(),
      Return :: any().
copy(Source, Destination, _Args) ->
    Command = [copy(), Source, Destination],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec del() -> Return when
      Return :: {bulk_string, bitstring()}.
del() -> {bulk_string, <<"DEL">>}.

-spec del(Keys) -> Return when
      Keys :: encode_bulk_string(),
      Return :: any().
del(Keys) ->
    Command = [del()|Keys],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set() -> Return when
      Return :: encode_bulk_string().
set() ->
    {bulk_string, <<"SET">>}.

-spec set(Key, Value) -> Return when
      Key :: encode_bulk_string(),
      Value :: encode_types(),
      Return :: any().
set(Key, Value) ->
    set(Key, Value, []).

-spec set(Key, Value, Args) -> Return when
      Key :: encode_bulk_string(),
      Value :: encode_types(),
      Args :: list(),
      Return :: any().
set(Key, Value, _Args) ->
    Command = [set(), Key, Value],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get() ->
    {bulk_string, <<"GET">>}.

get(Key) ->
    Command = [get(), Key],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
