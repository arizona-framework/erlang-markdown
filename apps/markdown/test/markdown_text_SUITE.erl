%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  08 Apr 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_text_SUITE).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(ct_suite).

-include_lib("stdlib/include/assert.hrl").

%% ct_suite callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    test_text_case_1/1,
    test_text_case_2/1,
    test_text_case_3/1
]).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> [TestDef :: ct_suite:ct_test_def()] | {skip, Reason :: term()}.
all() ->
    [
        {group, static}
    ].

-spec groups() -> [GroupDef :: ct_suite:ct_group_def()].
groups() ->
    [
        {static, [parallel], [
            test_text_case_1,
            test_text_case_2,
            test_text_case_3
        ]}
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) ->
    NewConfig ::
        ct_suite:ct_config()
        | {skip, Reason :: term()}
        | {skip_and_save, Reason :: term(), SaveConfig :: ct_suite:ct_config()}.
init_per_suite(Config) ->
    Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) ->
    term()
    | {save_config, SaveConfig :: ct_suite:ct_config()}.
end_per_suite(_Config) ->
    ok.

-spec init_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    NewConfig ::
        ct_suite:ct_config()
        | {skip, Reason :: term()}.
init_per_group(_Group, Config) ->
    Config.

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    term()
    | {return_group_result, Status :: ct_suite:ct_status()}.
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    NewConfig ::
        ct_suite:ct_config()
        | {fail, Reason :: term()}
        | {skip, Reason :: term()}.
init_per_testcase(_TestCase, Config) ->
    Config.

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    term()
    | {fail, Reason :: term()}
    | {save_config, SaveConfig :: ct_suite:ct_config()}.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

test_text_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>hello $.;'there</p>"/utf8>>},
        markdown:to_html(<<"hello $.;'there"/utf8>>),
        "should support ascii text"
    ),
    ok.

test_text_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>Foo χρῆν</p>"/utf8>>}, markdown:to_html(<<"Foo χρῆν"/utf8>>), "should support unicode text"
    ),
    ok.

test_text_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>Multiple     spaces</p>"/utf8>>},
        markdown:to_html(<<"Multiple     spaces"/utf8>>),
        "should preserve internal spaces verbatim"
    ),
    ok.
