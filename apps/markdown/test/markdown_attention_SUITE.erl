%%%-----------------------------------------------------------------------------
%%% %CopyrightBegin%
%%%
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_attention_SUITE).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
% -compile(warn_missing_spec_all).
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
    %% Rule 1.
    test_attention_rule_1_case_1/1,
    test_attention_rule_1_case_2/1,
    test_attention_rule_1_case_3/1,
    test_attention_rule_1_case_4/1,
    test_attention_rule_1_case_5/1,
    test_attention_rule_1_case_6/1,
    %% Rule 2.
    test_attention_rule_2_case_1/1,
    test_attention_rule_2_case_2/1,
    test_attention_rule_2_case_3/1,
    test_attention_rule_2_case_4/1,
    test_attention_rule_2_case_5/1,
    test_attention_rule_2_case_6/1,
    test_attention_rule_2_case_7/1,
    test_attention_rule_2_case_8/1,
    %% Rule 3.
    test_attention_rule_3_case_1/1,
    test_attention_rule_3_case_2/1,
    test_attention_rule_3_case_3/1,
    test_attention_rule_3_case_4/1,
    test_attention_rule_3_case_5/1,
    %% Rule 4.
    test_attention_rule_4_case_1/1,
    test_attention_rule_4_case_2/1,
    test_attention_rule_4_case_3/1,
    test_attention_rule_4_case_4/1,
    test_attention_rule_4_case_5/1,
    test_attention_rule_4_case_6/1,
    test_attention_rule_4_case_7/1,
    %% Rule 5.
    test_attention_rule_5_case_1/1,
    test_attention_rule_5_case_2/1,
    test_attention_rule_5_case_3/1,
    test_attention_rule_5_case_4/1,
    %% Rule 6.
    test_attention_rule_6_case_1/1,
    test_attention_rule_6_case_2/1,
    test_attention_rule_6_case_3/1,
    test_attention_rule_6_case_4/1,
    test_attention_rule_6_case_5/1,
    test_attention_rule_6_case_6/1,
    test_attention_rule_6_case_7/1,
    test_attention_rule_6_case_8/1,
    test_attention_rule_6_case_9/1,
    %% Rule 7.
    test_attention_rule_7_case_1/1,
    test_attention_rule_7_case_2/1,
    test_attention_rule_7_case_3/1,
    test_attention_rule_7_case_4/1,
    test_attention_rule_7_case_5/1,
    test_attention_rule_7_case_6/1,
    %% Rule 8.
    test_attention_rule_8_case_1/1,
    test_attention_rule_8_case_2/1,
    test_attention_rule_8_case_3/1,
    test_attention_rule_8_case_4/1,
    test_attention_rule_8_case_5/1,
    test_attention_rule_8_case_6/1,
    test_attention_rule_8_case_7/1,
    %% Rule 9.
    test_attention_rule_9_case_1/1,
    test_attention_rule_9_case_2/1,
    test_attention_rule_9_case_3/1,
    test_attention_rule_9_case_4/1,
    test_attention_rule_9_case_5/1,
    test_attention_rule_9_case_6/1,
    test_attention_rule_9_case_7/1,
    test_attention_rule_9_case_8/1,
    test_attention_rule_9_case_9/1,
    test_attention_rule_9_case_10/1,
    test_attention_rule_9_case_11/1,
    test_attention_rule_9_case_12/1,
    test_attention_rule_9_case_13/1,
    test_attention_rule_9_case_14/1,
    test_attention_rule_9_case_15/1,
    test_attention_rule_9_case_16/1,
    test_attention_rule_9_case_17/1,
    test_attention_rule_9_case_18/1,
    %% Rule 10.
    test_attention_rule_10_case_1/1,
    test_attention_rule_10_case_2/1,
    test_attention_rule_10_case_3/1,
    test_attention_rule_10_case_4/1,
    test_attention_rule_10_case_5/1,
    test_attention_rule_10_case_6/1,
    test_attention_rule_10_case_7/1,
    test_attention_rule_10_case_8/1,
    test_attention_rule_10_case_9/1,
    test_attention_rule_10_case_10/1,
    test_attention_rule_10_case_11/1,
    test_attention_rule_10_case_12/1,
    test_attention_rule_10_case_13/1,
    test_attention_rule_10_case_14/1,
    %% Rule 11.
    test_attention_rule_11_case_1/1,
    test_attention_rule_11_case_2/1,
    test_attention_rule_11_case_3/1,
    test_attention_rule_11_case_4/1,
    test_attention_rule_11_case_5/1,
    test_attention_rule_11_case_6/1,
    test_attention_rule_11_case_7/1,
    test_attention_rule_11_case_8/1,
    test_attention_rule_11_case_9/1,
    test_attention_rule_11_case_10/1,
    test_attention_rule_11_case_11/1,
    test_attention_rule_11_case_12/1,
    %% Rule 12.
    test_attention_rule_12_case_1/1,
    test_attention_rule_12_case_2/1,
    test_attention_rule_12_case_3/1,
    test_attention_rule_12_case_4/1,
    test_attention_rule_12_case_5/1,
    test_attention_rule_12_case_6/1,
    test_attention_rule_12_case_7/1,
    test_attention_rule_12_case_8/1,
    test_attention_rule_12_case_9/1,
    test_attention_rule_12_case_10/1,
    test_attention_rule_12_case_11/1,
    test_attention_rule_12_case_12/1,
    %% Rule 13.
    test_attention_rule_13_case_1/1,
    test_attention_rule_13_case_2/1,
    test_attention_rule_13_case_3/1,
    test_attention_rule_13_case_4/1,
    test_attention_rule_13_case_5/1,
    test_attention_rule_13_case_6/1,
    test_attention_rule_13_case_7/1,
    %% Rule 14.
    test_attention_rule_14_case_1/1,
    test_attention_rule_14_case_2/1,
    %% Rule 15.
    test_attention_rule_15_case_1/1,
    test_attention_rule_15_case_2/1,
    %% Rule 16.
    test_attention_rule_16_case_1/1,
    test_attention_rule_16_case_2/1,
    %% Rule 17.
    test_attention_rule_17_case_1/1,
    test_attention_rule_17_case_2/1,
    test_attention_rule_17_case_3/1,
    test_attention_rule_17_case_4/1,
    test_attention_rule_17_case_5/1,
    test_attention_rule_17_case_6/1,
    test_attention_rule_17_case_7/1,
    test_attention_rule_17_case_8/1,
    test_attention_rule_17_case_9/1,
    test_attention_rule_17_case_10/1,
    test_attention_rule_17_case_11/1
]).

%% Macros
-define(DANGER(),
    markdown_options:new(#{compile => #{allow_dangerous_html => true, allow_dangerous_protocol => true}})
).

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
            %% Rule 1.
            test_attention_rule_1_case_1,
            test_attention_rule_1_case_2,
            test_attention_rule_1_case_3,
            test_attention_rule_1_case_4,
            test_attention_rule_1_case_5,
            test_attention_rule_1_case_6,
            %% Rule 2.
            test_attention_rule_2_case_1,
            test_attention_rule_2_case_2,
            test_attention_rule_2_case_3,
            test_attention_rule_2_case_4,
            test_attention_rule_2_case_5,
            test_attention_rule_2_case_6,
            test_attention_rule_2_case_7,
            test_attention_rule_2_case_8,
            %% Rule 3.
            test_attention_rule_3_case_1,
            test_attention_rule_3_case_2,
            test_attention_rule_3_case_3,
            test_attention_rule_3_case_4,
            test_attention_rule_3_case_5,
            %% Rule 4.
            test_attention_rule_4_case_1,
            test_attention_rule_4_case_2,
            test_attention_rule_4_case_3,
            test_attention_rule_4_case_4,
            test_attention_rule_4_case_5,
            test_attention_rule_4_case_6,
            test_attention_rule_4_case_7,
            %% Rule 5.
            test_attention_rule_5_case_1,
            test_attention_rule_5_case_2,
            test_attention_rule_5_case_3,
            test_attention_rule_5_case_4,
            %% Rule 6.
            test_attention_rule_6_case_1,
            test_attention_rule_6_case_2,
            test_attention_rule_6_case_3,
            test_attention_rule_6_case_4,
            test_attention_rule_6_case_5,
            test_attention_rule_6_case_6,
            test_attention_rule_6_case_7,
            test_attention_rule_6_case_8,
            test_attention_rule_6_case_9,
            %% Rule 7.
            test_attention_rule_7_case_1,
            test_attention_rule_7_case_2,
            test_attention_rule_7_case_3,
            test_attention_rule_7_case_4,
            test_attention_rule_7_case_5,
            test_attention_rule_7_case_6,
            %% Rule 8.
            test_attention_rule_8_case_1,
            test_attention_rule_8_case_2,
            test_attention_rule_8_case_3,
            test_attention_rule_8_case_4,
            test_attention_rule_8_case_5,
            test_attention_rule_8_case_6,
            test_attention_rule_8_case_7,
            %% Rule 9.
            test_attention_rule_9_case_1,
            test_attention_rule_9_case_2,
            test_attention_rule_9_case_3,
            test_attention_rule_9_case_4,
            test_attention_rule_9_case_5,
            test_attention_rule_9_case_6,
            test_attention_rule_9_case_7,
            test_attention_rule_9_case_8,
            test_attention_rule_9_case_9,
            test_attention_rule_9_case_10,
            test_attention_rule_9_case_11,
            test_attention_rule_9_case_12,
            test_attention_rule_9_case_13,
            test_attention_rule_9_case_14,
            test_attention_rule_9_case_15,
            test_attention_rule_9_case_16,
            test_attention_rule_9_case_17,
            test_attention_rule_9_case_18,
            %% Rule 10.
            test_attention_rule_10_case_1,
            test_attention_rule_10_case_2,
            test_attention_rule_10_case_3,
            test_attention_rule_10_case_4,
            test_attention_rule_10_case_5,
            test_attention_rule_10_case_6,
            test_attention_rule_10_case_7,
            test_attention_rule_10_case_8,
            test_attention_rule_10_case_9,
            test_attention_rule_10_case_10,
            test_attention_rule_10_case_11,
            test_attention_rule_10_case_12,
            test_attention_rule_10_case_13,
            test_attention_rule_10_case_14,
            %% Rule 11.
            test_attention_rule_11_case_1,
            test_attention_rule_11_case_2,
            test_attention_rule_11_case_3,
            test_attention_rule_11_case_4,
            test_attention_rule_11_case_5,
            test_attention_rule_11_case_6,
            test_attention_rule_11_case_7,
            test_attention_rule_11_case_8,
            test_attention_rule_11_case_9,
            test_attention_rule_11_case_10,
            test_attention_rule_11_case_11,
            test_attention_rule_11_case_12,
            %% Rule 12.
            test_attention_rule_12_case_1,
            test_attention_rule_12_case_2,
            test_attention_rule_12_case_3,
            test_attention_rule_12_case_4,
            test_attention_rule_12_case_5,
            test_attention_rule_12_case_6,
            test_attention_rule_12_case_7,
            test_attention_rule_12_case_8,
            test_attention_rule_12_case_9,
            test_attention_rule_12_case_10,
            test_attention_rule_12_case_11,
            test_attention_rule_12_case_12,
            %% Rule 13.
            test_attention_rule_13_case_1,
            test_attention_rule_13_case_2,
            test_attention_rule_13_case_3,
            test_attention_rule_13_case_4,
            test_attention_rule_13_case_5,
            test_attention_rule_13_case_6,
            test_attention_rule_13_case_7,
            %% Rule 14.
            test_attention_rule_14_case_1,
            test_attention_rule_14_case_2,
            %% Rule 15.
            test_attention_rule_15_case_1,
            test_attention_rule_15_case_2,
            %% Rule 16.
            test_attention_rule_16_case_1,
            test_attention_rule_16_case_2,
            %% Rule 17.
            test_attention_rule_17_case_1,
            test_attention_rule_17_case_2,
            test_attention_rule_17_case_3,
            test_attention_rule_17_case_4,
            test_attention_rule_17_case_5,
            test_attention_rule_17_case_6,
            test_attention_rule_17_case_7,
            test_attention_rule_17_case_8,
            test_attention_rule_17_case_9,
            test_attention_rule_17_case_10,
            test_attention_rule_17_case_11
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

%% Rule 1.
test_attention_rule_1_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo bar</em></p>"/utf8>>},
        markdown:to_html(<<"*foo bar*"/utf8>>),
        "should support emphasis w/ `*`"
    ),
    ok.

test_attention_rule_1_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a * foo bar*</p>"/utf8>>},
        markdown:to_html(<<"a * foo bar*"/utf8>>),
        "should not support emphasis if the opening is not left flanking (1)"
    ),
    ok.

test_attention_rule_1_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a*&quot;foo&quot;*</p>"/utf8>>},
        markdown:to_html(<<"a*\"foo\"*"/utf8>>),
        "should not support emphasis if the opening is not left flanking (2b)"
    ),
    ok.

test_attention_rule_1_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>* a *</p>"/utf8>>},
        markdown:to_html(<<"* a *"/utf8>>),
        "should not support emphasis unicode whitespace either"
    ),
    ok.

test_attention_rule_1_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo<em>bar</em></p>"/utf8>>},
        markdown:to_html(<<"foo*bar*"/utf8>>),
        "should support intraword emphasis w/ `*` (1)"
    ),
    ok.

test_attention_rule_1_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>5<em>6</em>78</p>"/utf8>>},
        markdown:to_html(<<"5*6*78"/utf8>>),
        "should support intraword emphasis w/ `*` (2)"
    ),
    ok.

%% Rule 2.
test_attention_rule_2_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo bar</em></p>"/utf8>>},
        markdown:to_html(<<"_foo bar_"/utf8>>),
        "should support emphasis w/ `_`"
    ),
    ok.

test_attention_rule_2_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_ foo bar_</p>"/utf8>>},
        markdown:to_html(<<"_ foo bar_"/utf8>>),
        "should not support emphasis if the opening is followed by whitespace"
    ),
    ok.

test_attention_rule_2_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a_&quot;foo&quot;_</p>"/utf8>>},
        markdown:to_html(<<"a_\"foo\"_"/utf8>>),
        "should not support emphasis if the opening is preceded by something else and followed by punctuation"
    ),
    ok.

test_attention_rule_2_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo_bar_</p>"/utf8>>},
        markdown:to_html(<<"foo_bar_"/utf8>>),
        "should not support intraword emphasis (1)"
    ),
    ok.

test_attention_rule_2_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>5_6_78</p>"/utf8>>}, markdown:to_html(<<"5_6_78"/utf8>>), "should not support intraword emphasis (2)"
    ),
    ok.

test_attention_rule_2_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>пристаням_стремятся_</p>"/utf8>>},
        markdown:to_html(<<"пристаням_стремятся_"/utf8>>),
        "should not support intraword emphasis (3)"
    ),
    ok.

test_attention_rule_2_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p>aa_&quot;bb&quot;_cc</p>"/utf8>>},
        markdown:to_html(<<"aa_\"bb\"_cc"/utf8>>),
        "should not support emphasis if the opening is right flanking and the closing is left flanking"
    ),
    ok.

test_attention_rule_2_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo-<em>(bar)</em></p>"/utf8>>},
        markdown:to_html(<<"foo-_(bar)_"/utf8>>),
        "should support emphasis if the opening is both left and right flanking, if it’s preceded by punctuation"
    ),
    ok.

%% Rule 3.
test_attention_rule_3_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_foo*</p>"/utf8>>},
        markdown:to_html(<<"_foo*"/utf8>>),
        "should not support emphasis if opening and closing markers don’t match"
    ),
    ok.

test_attention_rule_3_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*foo bar *</p>"/utf8>>},
        markdown:to_html(<<"*foo bar *"/utf8>>),
        "should not support emphasis w/ `*` if the closing markers are preceded by whitespace"
    ),
    ok.

test_attention_rule_3_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*foo bar\n*</p>"/utf8>>},
        markdown:to_html(<<"*foo bar\n*"/utf8>>),
        "should not support emphasis w/ `*` if the closing markers are preceded by a line break (also whitespace)"
    ),
    ok.

test_attention_rule_3_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*(*foo)</p>"/utf8>>},
        markdown:to_html(<<"*(*foo)"/utf8>>),
        "should not support emphasis w/ `*` if the closing markers are not right flanking"
    ),
    ok.

test_attention_rule_3_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>(<em>foo</em>)</em></p>"/utf8>>},
        markdown:to_html(<<"*(*foo*)*"/utf8>>),
        "should support nested emphasis"
    ),
    ok.

%% Rule 4.
test_attention_rule_4_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_foo bar _</p>"/utf8>>},
        markdown:to_html(<<"_foo bar _"/utf8>>),
        "should not support emphasis if the closing `_` is preceded by whitespace"
    ),
    ok.

test_attention_rule_4_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_(_foo)</p>"/utf8>>},
        markdown:to_html(<<"_(_foo)"/utf8>>),
        "should not support emphasis w/ `_` if the closing markers are not right flanking"
    ),
    ok.

test_attention_rule_4_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>(<em>foo</em>)</em></p>"/utf8>>},
        markdown:to_html(<<"_(_foo_)_"/utf8>>),
        "should support nested emphasis w/ `_`"
    ),
    ok.

test_attention_rule_4_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_foo_bar</p>"/utf8>>},
        markdown:to_html(<<"_foo_bar"/utf8>>),
        "should not support intraword emphasis w/ `_` (1)"
    ),
    ok.

test_attention_rule_4_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_пристаням_стремятся</p>"/utf8>>},
        markdown:to_html(<<"_пристаням_стремятся"/utf8>>),
        "should not support intraword emphasis w/ `_` (2)"
    ),
    ok.

test_attention_rule_4_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo_bar_baz</em></p>"/utf8>>},
        markdown:to_html(<<"_foo_bar_baz_"/utf8>>),
        "should not support intraword emphasis w/ `_` (3)"
    ),
    ok.

test_attention_rule_4_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>(bar)</em>.</p>"/utf8>>},
        markdown:to_html(<<"_(bar)_."/utf8>>),
        "should support emphasis if the opening is both left and right flanking, if it’s followed by punctuation"
    ),
    ok.

%% Rule 5.
test_attention_rule_5_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo bar</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo bar**"/utf8>>),
        "should support strong emphasis"
    ),
    ok.

test_attention_rule_5_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>** foo bar**</p>"/utf8>>},
        markdown:to_html(<<"** foo bar**"/utf8>>),
        "should not support strong emphasis if the opening is followed by whitespace"
    ),
    ok.

test_attention_rule_5_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a**&quot;foo&quot;**</p>"/utf8>>},
        markdown:to_html(<<"a**\"foo\"**"/utf8>>),
        "should not support strong emphasis if the opening is preceded by something else and followed by punctuation"
    ),
    ok.

test_attention_rule_5_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo<strong>bar</strong></p>"/utf8>>},
        markdown:to_html(<<"foo**bar**"/utf8>>),
        "should support strong intraword emphasis"
    ),
    ok.

%% Rule 6.
test_attention_rule_6_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo bar</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo bar__"/utf8>>),
        "should support strong emphasis w/ `_`"
    ),
    ok.

test_attention_rule_6_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__ foo bar__</p>"/utf8>>},
        markdown:to_html(<<"__ foo bar__"/utf8>>),
        "should not support strong emphasis if the opening is followed by whitespace"
    ),
    ok.

test_attention_rule_6_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__\nfoo bar__</p>"/utf8>>},
        markdown:to_html(<<"__\nfoo bar__"/utf8>>),
        "should not support strong emphasis if the opening is followed by a line ending (also whitespace)"
    ),
    ok.

test_attention_rule_6_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a__&quot;foo&quot;__</p>"/utf8>>},
        markdown:to_html(<<"a__\"foo\"__"/utf8>>),
        "should not support strong emphasis if the opening is preceded by something else and followed by punctuation"
    ),
    ok.

test_attention_rule_6_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo__bar__</p>"/utf8>>},
        markdown:to_html(<<"foo__bar__"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (1)"
    ),
    ok.

test_attention_rule_6_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>5__6__78</p>"/utf8>>},
        markdown:to_html(<<"5__6__78"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (2)"
    ),
    ok.

test_attention_rule_6_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p>пристаням__стремятся__</p>"/utf8>>},
        markdown:to_html(<<"пристаням__стремятся__"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (3)"
    ),
    ok.

test_attention_rule_6_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo, <strong>bar</strong>, baz</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo, __bar__, baz__"/utf8>>),
        "should support nested strong emphasis"
    ),
    ok.

test_attention_rule_6_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo-<strong>(bar)</strong></p>"/utf8>>},
        markdown:to_html(<<"foo-__(bar)__"/utf8>>),
        "should support strong emphasis if the opening is both left and right flanking, if it’s preceded by punctuation"
    ),
    ok.

%% Rule 7.
test_attention_rule_7_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**foo bar **</p>"/utf8>>},
        markdown:to_html(<<"**foo bar **"/utf8>>),
        "should not support strong emphasis w/ `*` if the closing is preceded by whitespace"
    ),
    ok.

test_attention_rule_7_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**(**foo)</p>"/utf8>>},
        markdown:to_html(<<"**(**foo)"/utf8>>),
        "should not support strong emphasis w/ `*` if the closing is preceded by punctuation and followed by something else"
    ),
    ok.

test_attention_rule_7_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>(<strong>foo</strong>)</em></p>"/utf8>>},
        markdown:to_html(<<"*(**foo**)*"/utf8>>),
        "should support strong emphasis in emphasis"
    ),
    ok.

test_attention_rule_7_case_4(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>"/utf8>>},
        markdown:to_html(<<"**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**"/utf8>>),
        "should support emphasis in strong emphasis (1)"
    ),
    ok.

test_attention_rule_7_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo \"*bar*\" foo**"/utf8>>),
        "should support emphasis in strong emphasis (2)"
    ),
    ok.

test_attention_rule_7_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo</strong>bar</p>"/utf8>>},
        markdown:to_html(<<"**foo**bar"/utf8>>),
        "should support strong intraword emphasis"
    ),
    ok.

%% Rule 8.
test_attention_rule_8_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__foo bar __</p>"/utf8>>},
        markdown:to_html(<<"__foo bar __"/utf8>>),
        "should not support strong emphasis w/ `_` if the closing is preceded by whitespace"
    ),
    ok.

test_attention_rule_8_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__(__foo)</p>"/utf8>>},
        markdown:to_html(<<"__(__foo)"/utf8>>),
        "should not support strong emphasis w/ `_` if the closing is preceded by punctuation and followed by something else"
    ),
    ok.

test_attention_rule_8_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>(<strong>foo</strong>)</em></p>"/utf8>>},
        markdown:to_html(<<"_(__foo__)_"/utf8>>),
        "should support strong emphasis w/ `_` in emphasis"
    ),
    ok.

test_attention_rule_8_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__foo__bar</p>"/utf8>>},
        markdown:to_html(<<"__foo__bar"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (1)"
    ),
    ok.

test_attention_rule_8_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__пристаням__стремятся</p>"/utf8>>},
        markdown:to_html(<<"__пристаням__стремятся"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (2)"
    ),
    ok.

test_attention_rule_8_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo__bar__baz</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo__bar__baz__"/utf8>>),
        "should not support strong intraword emphasis w/ `_` (3)"
    ),
    ok.

test_attention_rule_8_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>(bar)</strong>.</p>"/utf8>>},
        markdown:to_html(<<"__(bar)__."/utf8>>),
        "should support strong emphasis if the opening is both left and right flanking, if it’s followed by punctuation"
    ),
    ok.

%% Rule 9.
test_attention_rule_9_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <a href=\"/url\">bar</a></em></p>"/utf8>>},
        markdown:to_html(<<"*foo [bar](/url)*"/utf8>>),
        "should support content in emphasis"
    ),
    ok.

test_attention_rule_9_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo\nbar</em></p>"/utf8>>},
        markdown:to_html(<<"*foo\nbar*"/utf8>>),
        "should support line endings in emphasis"
    ),
    ok.

test_attention_rule_9_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <strong>bar</strong> baz</em></p>"/utf8>>},
        markdown:to_html(<<"_foo __bar__ baz_"/utf8>>),
        "should support nesting emphasis and strong (1)"
    ),
    ok.

test_attention_rule_9_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <em>bar</em> baz</em></p>"/utf8>>},
        markdown:to_html(<<"_foo _bar_ baz_"/utf8>>),
        "should support nesting emphasis and strong (2)"
    ),
    ok.

test_attention_rule_9_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><em>foo</em> bar</em></p>"/utf8>>},
        markdown:to_html(<<"__foo_ bar_"/utf8>>),
        "should support nesting emphasis and strong (3)"
    ),
    ok.

test_attention_rule_9_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <em>bar</em></em></p>"/utf8>>},
        markdown:to_html(<<"*foo *bar**"/utf8>>),
        "should support nesting emphasis and strong (4)"
    ),
    ok.

test_attention_rule_9_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <strong>bar</strong> baz</em></p>"/utf8>>},
        markdown:to_html(<<"*foo **bar** baz*"/utf8>>),
        "should support nesting emphasis and strong (5)"
    ),
    ok.

test_attention_rule_9_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo<strong>bar</strong>baz</em></p>"/utf8>>},
        markdown:to_html(<<"*foo**bar**baz*"/utf8>>),
        "should support nesting emphasis and strong (6)"
    ),
    ok.

test_attention_rule_9_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo**bar</em></p>"/utf8>>},
        markdown:to_html(<<"*foo**bar*"/utf8>>),
        "should not support adjacent emphasis in certain cases"
    ),
    ok.

test_attention_rule_9_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><strong>foo</strong> bar</em></p>"/utf8>>},
        markdown:to_html(<<"***foo** bar*"/utf8>>),
        "complex (1)"
    ),
    ok.

test_attention_rule_9_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <strong>bar</strong></em></p>"/utf8>>},
        markdown:to_html(<<"*foo **bar***"/utf8>>),
        "complex (2)"
    ),
    ok.

test_attention_rule_9_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo<strong>bar</strong></em></p>"/utf8>>},
        markdown:to_html(<<"*foo**bar***"/utf8>>),
        "complex (3)"
    ),
    ok.

test_attention_rule_9_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo<em><strong>bar</strong></em>baz</p>"/utf8>>},
        markdown:to_html(<<"foo***bar***baz"/utf8>>),
        "complex (a)"
    ),
    ok.

test_attention_rule_9_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>"/utf8>>},
        markdown:to_html(<<"foo******bar*********baz"/utf8>>),
        "complex (b)"
    ),
    ok.

test_attention_rule_9_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>"/utf8>>},
        markdown:to_html(<<"*foo **bar *baz* bim** bop*"/utf8>>),
        "should support indefinite nesting of emphasis (1)"
    ),
    ok.

test_attention_rule_9_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>"/utf8>>},
        markdown:to_html(<<"*foo [*bar*](/url)*"/utf8>>),
        "should support indefinite nesting of emphasis (2)"
    ),
    ok.

test_attention_rule_9_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p>** is not an empty emphasis</p>"/utf8>>},
        markdown:to_html(<<"** is not an empty emphasis"/utf8>>),
        "should not support empty emphasis"
    ),
    ok.

test_attention_rule_9_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**** is not an empty emphasis</p>"/utf8>>},
        markdown:to_html(<<"**** is not an empty emphasis"/utf8>>),
        "should not support empty strong emphasis"
    ),
    ok.

%% Rule 10.
test_attention_rule_10_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <a href=\"/url\">bar</a></strong></p>"/utf8>>},
        markdown:to_html(<<"**foo [bar](/url)**"/utf8>>),
        "should support content in strong emphasis"
    ),
    ok.

test_attention_rule_10_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo\nbar</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo\nbar**"/utf8>>),
        "should support line endings in emphasis"
    ),
    ok.

test_attention_rule_10_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <em>bar</em> baz</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo _bar_ baz__"/utf8>>),
        "should support nesting emphasis and strong (1)"
    ),
    ok.

test_attention_rule_10_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <strong>bar</strong> baz</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo __bar__ baz__"/utf8>>),
        "should support nesting emphasis and strong (2)"
    ),
    ok.

test_attention_rule_10_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong><strong>foo</strong> bar</strong></p>"/utf8>>},
        markdown:to_html(<<"____foo__ bar__"/utf8>>),
        "should support nesting emphasis and strong (3)"
    ),
    ok.

test_attention_rule_10_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <strong>bar</strong></strong></p>"/utf8>>},
        markdown:to_html(<<"**foo **bar****"/utf8>>),
        "should support nesting emphasis and strong (4)"
    ),
    ok.

test_attention_rule_10_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <em>bar</em> baz</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo *bar* baz**"/utf8>>),
        "should support nesting emphasis and strong (5)"
    ),
    ok.

test_attention_rule_10_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo<em>bar</em>baz</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo*bar*baz**"/utf8>>),
        "should support nesting emphasis and strong (6)"
    ),
    ok.

test_attention_rule_10_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong><em>foo</em> bar</strong></p>"/utf8>>},
        markdown:to_html(<<"***foo* bar**"/utf8>>),
        "should support nesting emphasis and strong (7)"
    ),
    ok.

test_attention_rule_10_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <em>bar</em></strong></p>"/utf8>>},
        markdown:to_html(<<"**foo *bar***"/utf8>>),
        "should support nesting emphasis and strong (8)"
    ),
    ok.

test_attention_rule_10_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo *bar **baz**\nbim* bop**"/utf8>>),
        "should support indefinite nesting of emphasis (1)"
    ),
    ok.

test_attention_rule_10_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>"/utf8>>},
        markdown:to_html(<<"**foo [*bar*](/url)**"/utf8>>),
        "should support indefinite nesting of emphasis (2)"
    ),
    ok.

test_attention_rule_10_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__ is not an empty emphasis</p>"/utf8>>},
        markdown:to_html(<<"__ is not an empty emphasis"/utf8>>),
        "should not support empty emphasis"
    ),
    ok.

test_attention_rule_10_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p>____ is not an empty emphasis</p>"/utf8>>},
        markdown:to_html(<<"____ is not an empty emphasis"/utf8>>),
        "should not support empty strong emphasis"
    ),
    ok.

%% Rule 11.
test_attention_rule_11_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo ***</p>"/utf8>>},
        markdown:to_html(<<"foo ***"/utf8>>),
        "should not support emphasis around the same marker"
    ),
    ok.

test_attention_rule_11_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <em>*</em></p>"/utf8>>},
        markdown:to_html(<<"foo *\\**"/utf8>>),
        "should support emphasis around an escaped marker"
    ),
    ok.

test_attention_rule_11_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <em>_</em></p>"/utf8>>},
        markdown:to_html(<<"foo *_*"/utf8>>),
        "should support emphasis around the other marker"
    ),
    ok.

test_attention_rule_11_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo *****</p>"/utf8>>},
        markdown:to_html(<<"foo *****"/utf8>>),
        "should not support strong emphasis around the same marker"
    ),
    ok.

test_attention_rule_11_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <strong>*</strong></p>"/utf8>>},
        markdown:to_html(<<"foo **\\***"/utf8>>),
        "should support strong emphasis around an escaped marker"
    ),
    ok.

test_attention_rule_11_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <strong>_</strong></p>"/utf8>>},
        markdown:to_html(<<"foo **_**"/utf8>>),
        "should support strong emphasis around the other marker"
    ),
    ok.

test_attention_rule_11_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*<em>foo</em></p>"/utf8>>},
        markdown:to_html(<<"**foo*"/utf8>>),
        "should support a superfluous marker at the start of emphasis"
    ),
    ok.

test_attention_rule_11_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo</em>*</p>"/utf8>>},
        markdown:to_html(<<"*foo**"/utf8>>),
        "should support a superfluous marker at the end of emphasis"
    ),
    ok.

test_attention_rule_11_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*<strong>foo</strong></p>"/utf8>>},
        markdown:to_html(<<"***foo**"/utf8>>),
        "should support a superfluous marker at the start of strong"
    ),
    ok.

test_attention_rule_11_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p>***<em>foo</em></p>"/utf8>>},
        markdown:to_html(<<"****foo*"/utf8>>),
        "should support multiple superfluous markers at the start of strong"
    ),
    ok.

test_attention_rule_11_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo</strong>*</p>"/utf8>>},
        markdown:to_html(<<"**foo***"/utf8>>),
        "should support a superfluous marker at the end of strong"
    ),
    ok.

test_attention_rule_11_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo</em>***</p>"/utf8>>},
        markdown:to_html(<<"*foo****"/utf8>>),
        "should support multiple superfluous markers at the end of strong"
    ),
    ok.

%% Rule 12.
test_attention_rule_12_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo ___</p>"/utf8>>},
        markdown:to_html(<<"foo ___"/utf8>>),
        "should not support emphasis around the same marker"
    ),
    ok.

test_attention_rule_12_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <em>_</em></p>"/utf8>>},
        markdown:to_html(<<"foo _\\__"/utf8>>),
        "should support emphasis around an escaped marker"
    ),
    ok.

test_attention_rule_12_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <em>X</em></p>"/utf8>>},
        markdown:to_html(<<"foo _X_"/utf8>>),
        "should support emphasis around the other marker"
    ),
    ok.

test_attention_rule_12_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo _____</p>"/utf8>>},
        markdown:to_html(<<"foo _____"/utf8>>),
        "should not support strong emphasis around the same marker"
    ),
    ok.

test_attention_rule_12_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <strong>_</strong></p>"/utf8>>},
        markdown:to_html(<<"foo __\\___"/utf8>>),
        "should support strong emphasis around an escaped marker"
    ),
    ok.

test_attention_rule_12_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo <strong>X</strong></p>"/utf8>>},
        markdown:to_html(<<"foo __X__"/utf8>>),
        "should support strong emphasis around the other marker"
    ),
    ok.

test_attention_rule_12_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_<em>foo</em></p>"/utf8>>},
        markdown:to_html(<<"__foo_"/utf8>>),
        "should support a superfluous marker at the start of emphasis"
    ),
    ok.

test_attention_rule_12_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo</em>_</p>"/utf8>>},
        markdown:to_html(<<"_foo__"/utf8>>),
        "should support a superfluous marker at the end of emphasis"
    ),
    ok.

test_attention_rule_12_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_<strong>foo</strong></p>"/utf8>>},
        markdown:to_html(<<"___foo__"/utf8>>),
        "should support a superfluous marker at the start of strong"
    ),
    ok.

test_attention_rule_12_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p>___<em>foo</em></p>"/utf8>>},
        markdown:to_html(<<"____foo_"/utf8>>),
        "should support multiple superfluous markers at the start of strong"
    ),
    ok.

test_attention_rule_12_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo</strong>_</p>"/utf8>>},
        markdown:to_html(<<"__foo___"/utf8>>),
        "should support a superfluous marker at the end of strong"
    ),
    ok.

test_attention_rule_12_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo</em>___</p>"/utf8>>},
        markdown:to_html(<<"_foo____"/utf8>>),
        "should support multiple superfluous markers at the end of strong"
    ),
    ok.

%% Rule 13.
test_attention_rule_13_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo**"/utf8>>),
        "should support strong w/ `*`"
    ),
    ok.

test_attention_rule_13_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><em>foo</em></em></p>"/utf8>>},
        markdown:to_html(<<"*_foo_*"/utf8>>),
        "should support emphasis directly in emphasis w/ `_` in `*`"
    ),
    ok.

test_attention_rule_13_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong>foo</strong></p>"/utf8>>},
        markdown:to_html(<<"__foo__"/utf8>>),
        "should support strong w/ `_`"
    ),
    ok.

test_attention_rule_13_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><em>foo</em></em></p>"/utf8>>},
        markdown:to_html(<<"_*foo*_"/utf8>>),
        "should support emphasis directly in emphasis w/ `*` in `_`"
    ),
    ok.

test_attention_rule_13_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong><strong>foo</strong></strong></p>"/utf8>>},
        markdown:to_html(<<"****foo****"/utf8>>),
        "should support strong emphasis directly in strong emphasis w/ `*`"
    ),
    ok.

test_attention_rule_13_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong><strong>foo</strong></strong></p>"/utf8>>},
        markdown:to_html(<<"____foo____"/utf8>>),
        "should support strong emphasis directly in strong emphasis w/ `_`"
    ),
    ok.

test_attention_rule_13_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><strong><strong><strong>foo</strong></strong></strong></p>"/utf8>>},
        markdown:to_html(<<"******foo******"/utf8>>),
        "should support indefinite strong emphasis"
    ),
    ok.

%% Rule 14.
test_attention_rule_14_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><strong>foo</strong></em></p>"/utf8>>},
        markdown:to_html(<<"***foo***"/utf8>>),
        "should support strong directly in emphasis w/ `*`"
    ),
    ok.

test_attention_rule_14_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em><strong>foo</strong></em></p>"/utf8>>},
        markdown:to_html(<<"___foo___"/utf8>>),
        "should support strong directly in emphasis w/ `_`"
    ),
    ok.

%% Rule 15.
test_attention_rule_15_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo _bar</em> baz_</p>"/utf8>>},
        markdown:to_html(<<"*foo _bar* baz_"/utf8>>),
        "should not support mismatched emphasis"
    ),
    ok.

test_attention_rule_15_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>foo <strong>bar *baz bim</strong> bam</em></p>"/utf8>>},
        markdown:to_html(<<"*foo __bar *baz bim__ bam*"/utf8>>),
        "should not support mismatched strong emphasis"
    ),
    ok.

%% Rule 16.
test_attention_rule_16_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**foo <strong>bar baz</strong></p>"/utf8>>},
        markdown:to_html(<<"**foo **bar baz**"/utf8>>),
        "should not shortest strong possible"
    ),
    ok.

test_attention_rule_16_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*foo <em>bar baz</em></p>"/utf8>>},
        markdown:to_html(<<"*foo *bar baz*"/utf8>>),
        "should not shortest emphasis possible"
    ),
    ok.

%% Rule 17.
test_attention_rule_17_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*<a href=\"/url\">bar*</a></p>"/utf8>>},
        markdown:to_html(<<"*[bar*](/url)"/utf8>>),
        "should not mismatch inside links (1)"
    ),
    ok.

test_attention_rule_17_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_<a href=\"/url\">bar_</a></p>"/utf8>>},
        markdown:to_html(<<"_[bar_](/url)"/utf8>>),
        "should not mismatch inside links (1)"
    ),
    ok.

test_attention_rule_17_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*<img src=\"foo\" title=\"*\"/></p>"/utf8>>},
        markdown:to_html_with_options(<<"*<img src=\"foo\" title=\"*\"/>"/utf8>>, ?DANGER()),
        "should not end inside HTML"
    ),
    ok.

test_attention_rule_17_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*<img src=\"foo\" title=\"*\"/></p>"/utf8>>},
        markdown:to_html_with_options(<<"*<img src=\"foo\" title=\"*\"/>"/utf8>>, ?DANGER()),
        "should not end emphasis inside HTML"
    ),
    ok.

test_attention_rule_17_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**<a href=\"**\"></p>"/utf8>>},
        markdown:to_html_with_options(<<"**<a href=\"**\">"/utf8>>, ?DANGER()),
        "should not end strong inside HTML (1)"
    ),
    ok.

test_attention_rule_17_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__<a href=\"__\"></p>"/utf8>>},
        markdown:to_html_with_options(<<"__<a href=\"__\">"/utf8>>, ?DANGER()),
        "should not end strong inside HTML (2)"
    ),
    ok.

test_attention_rule_17_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>a <code>*</code></em></p>"/utf8>>},
        markdown:to_html(<<"*a `*`*"/utf8>>),
        "should not end emphasis inside code (1)"
    ),
    ok.

test_attention_rule_17_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>a <code>_</code></em></p>"/utf8>>},
        markdown:to_html(<<"_a `_`_"/utf8>>),
        "should not end emphasis inside code (2)"
    ),
    ok.

test_attention_rule_17_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**a<a href=\"http://foo.bar/?q=**\">http://foo.bar/?q=**</a></p>"/utf8>>},
        markdown:to_html(<<"**a<http://foo.bar/?q=**>"/utf8>>),
        "should not end strong emphasis inside autolinks (1)"
    ),
    ok.

test_attention_rule_17_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__a<a href=\"http://foo.bar/?q=__\">http://foo.bar/?q=__</a></p>"/utf8>>},
        markdown:to_html(<<"__a<http://foo.bar/?q=__>"/utf8>>),
        "should not end strong emphasis inside autolinks (2)"
    ),
    ok.

test_attention_rule_17_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*a*</p>"/utf8>>},
        markdown:to_html_with_options(<<"*a*"/utf8>>, #{parse => #{constructs => #{attention => false}}}),
        "should support turning off attention"
    ),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
