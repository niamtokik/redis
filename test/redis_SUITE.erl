%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact [at] steepath [dot] eu>
%%% @copyright 2021 (c) Mathieu Kerjouan
%%%
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(redis_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec suite() -> Return when
      Return :: [tuple()].
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_per_suite(Config) -> Return when
      Config :: [tuple()],
      Reason :: term(),
      Return :: Config | {skip,Reason} | {skip_and_save,Reason,Config}.
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
-spec end_per_suite(Config) -> Return when
      Config ::  [tuple()],
      Return :: term() | {save_config,Config}.
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_per_group(GroupName, Config) -> Return when
      GroupName :: atom(),
      Config :: [tuple()],
      Reason :: term(),
      Return :: Config | {skip,Reason} | {skip_and_save,Reason,Config}.
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec end_per_group(GroupName, Config) -> Return when
      GroupName :: atom(),
      Config :: [tuple()],
      Return :: term() | {save_config,Config}.
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init_per_testcase(TestCase, Config) -> Return when
      TestCase :: atom(),
      Config :: [tuple()],
      Reason :: term(),
      Return :: Config | {skip,Reason} | {skip_and_save,Reason,Config}.
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec end_per_testcase(TestCase, Config) -> Return when
      TestCase :: atom(),
      Config ::  [tuple()],
      Reason :: term(),
      Return :: term() | {save_config,Config} | {fail,Reason}.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec groups() -> Return when       
      Group :: {GroupName,Properties,GroupsAndTestCases},
      GroupName :: atom(),
      Properties :: [parallel | sequence | Shuffle | {RepeatType,N}],
      GroupsAndTestCases :: [Group | {group,GroupName} | TestCase],
      TestCase :: atom(),
      Shuffle :: shuffle | {shuffle,{integer(),integer(),integer()}},
      RepeatType :: repeat | repeat_until_all_ok | repeat_until_all_fail |
      repeat_until_any_ok | repeat_until_any_fail,
      N :: integer() | forever,
      Return :: [Group].
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec all() -> Return when 
      GroupsAndTestCases :: [{group,GroupName} | TestCase],
      GroupName :: atom(),
      TestCase :: atom(),
      Reason :: term(),
      Return :: GroupsAndTestCases | {skip,Reason}.
all() -> 
    [my_test_case].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec my_test_case() -> Return when
      Return :: [tuple()].
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec my_test_case(Config) -> Return when
      Config :: [tuple()],
      Reason :: term(),
      Comment :: term(),
      Return :: ok | erlang:exit() | {skip,Reason} | {comment,Comment} |
      {save_config,Config} | {skip_and_save,Reason,Config}.
my_test_case(_Config) -> 
    ok.

