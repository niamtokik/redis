%%%-------------------------------------------------------------------
%%% @doc https://redis.io/commands#
%%%
%%% @todo copy
%%% @todo del
%%% @todo set
%%% @todo get
%%% @todo getdel
%%% @todo strlen
%%% @todo append
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(redis_command).
-compile({no_auto_import,[get/0]}).
-include("redis.hrl").
-export([copy/0, copy/2, copy/3]).
-export([del/0, del/1]).
-export([set/0, set/2, set/3]).
-export([get/0, get/1]).

%%--------------------------------------------------------------------
%% @doc copy/0.
%% @end
%%--------------------------------------------------------------------
-spec copy() -> Return when
      Return :: encode_bulk_string().

copy() -> 
    {bulk_string, <<"COPY">>}.

%%--------------------------------------------------------------------
%% @doc copy/2.
%% @end
%%--------------------------------------------------------------------
-spec copy(Source, Destination) -> Return when
      Source :: encode_bulk_string(),
      Destination :: encode_bulk_string(),
      Return :: any().

copy(Source, Destination) ->
    copy(Source, Destination, []).

%%--------------------------------------------------------------------
%% @doc copy/3.
%% @end
%%--------------------------------------------------------------------
-spec copy(Source, Destination, Args) -> Return when
      Source :: encode_bulk_string(),
      Destination :: encode_bulk_string(),
      Args :: list(),
      Return :: any().

copy(Source, Destination, _Args) ->
    Command = [copy(), Source, Destination],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc del/0.
%% @end
%%--------------------------------------------------------------------
-spec del() -> Return when
      Return :: encode_bulk_string().

del() -> {bulk_string, <<"DEL">>}.

%%--------------------------------------------------------------------
%% @doc del/1.
%% @end
%%--------------------------------------------------------------------
-spec del(Keys) -> Return when
      Keys :: encode_bulk_string(),
      Return :: any().

del(Keys) ->
    Command = [del()|Keys],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc set/0.
%% @end
%%--------------------------------------------------------------------
-spec set() -> Return when
      Return :: encode_bulk_string().

set() ->
    {bulk_string, <<"SET">>}.

%%--------------------------------------------------------------------
%% @doc set/0.
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Value) -> Return when
      Key :: encode_bulk_string(),
      Value :: encode_types(),
      Return :: any().

set(Key, Value) ->
    set(Key, Value, []).

%%--------------------------------------------------------------------
%% @doc set/0.
%% @end
%%--------------------------------------------------------------------
-spec set(Key, Value, Args) -> Return when
      Key :: encode_bulk_string(),
      Value :: encode_types(),
      Args :: list(),
      Return :: any().

set(Key, Value, _Args) ->
    Command = [set(), Key, Value],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc get/0.
%% @end
%%--------------------------------------------------------------------
-spec get() -> Return when
      Return :: encode_bulk_string().

get() ->
    {bulk_string, <<"GET">>}.

%%--------------------------------------------------------------------
%% @doc get/0.
%% @end
%%--------------------------------------------------------------------
-spec get(Key) -> Return when
      Key :: encode_bulk_string(),
      Return :: any().

get(Key) ->
    Command = [get(), Key],
    redis:encode(Command).

%%--------------------------------------------------------------------
%% @doc getdel/0.
%% @end
%%--------------------------------------------------------------------
