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
-module(markdown_character_escape_SUITE).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-04-08", modified => "2025-04-08"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
% -compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(ct_suite).

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_util.hrl").
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
    test_character_escape_case_1/1,
    test_character_escape_case_2/1,
    test_character_escape_case_3/1,
    test_character_escape_case_4/1,
    test_character_escape_case_5/1,
    test_character_escape_case_6/1,
    test_character_escape_case_7/1,
    test_character_escape_case_8/1,
    test_character_escape_case_9/1,
    test_character_escape_case_10/1,
    test_character_escape_case_11/1,
    test_character_escape_case_12/1,
    test_character_escape_case_13/1
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
            test_character_escape_case_1,
            test_character_escape_case_2,
            test_character_escape_case_3,
            test_character_escape_case_4,
            test_character_escape_case_5,
            test_character_escape_case_6,
            test_character_escape_case_7,
            test_character_escape_case_8,
            test_character_escape_case_9,
            test_character_escape_case_10,
            test_character_escape_case_11,
            test_character_escape_case_12,
            test_character_escape_case_13
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

test_character_escape_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>"/utf8>>},
        markdown:to_html(
            <<"\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~"/utf8>>
        ),
        "should support escaped ascii punctuation"
    ),
    ok.

test_character_escape_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>\\→\\A\\a\\ \\3\\φ\\«</p>"/utf8>>},
        markdown:to_html(<<"\\→\\A\\a\\ \\3\\φ\\«"/utf8>>),
        "should not support other characters after a backslash"
    ),
    ok.

test_character_escape_case_3(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;\n&amp;ouml; not a character entity</p>"/utf8>>},
        markdown:to_html(
            <<"\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n\\&ouml; not a character entity"/utf8>>
        ),
        "should escape other constructs"
    ),
    ok.

test_character_escape_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo<br />\nbar</p>"/utf8>>}, markdown:to_html(<<"foo\\\nbar"/utf8>>), "should escape a line break"
    ),
    ok.

test_character_escape_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>\\[\\`</code></p>"/utf8>>},
        markdown:to_html(<<"`` \\[\\` ``"/utf8>>),
        "should not escape in text code"
    ),
    ok.

test_character_escape_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>\\[\\]\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    \\[\\]"/utf8>>),
        "should not escape in indented code"
    ),
    ok.

test_character_escape_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"http://example.com?find=%5C*\">http://example.com?find=\\*</a></p>"/utf8>>},
        markdown:to_html(<<"<http://example.com?find=\\*>"/utf8>>),
        "should not escape in autolink"
    ),
    ok.

test_character_escape_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<a href=\"/bar\\/)\">"/utf8>>},
        markdown:to_html_with_options(<<"<a href=\"/bar\\/)\">"/utf8>>, ?DANGER()),
        "should not escape in flow html"
    ),
    ok.

test_character_escape_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>"/utf8>>},
        markdown:to_html(<<"[foo](/bar\\* \"ti\\*tle\")"/utf8>>),
        "should escape in resource and title"
    ),
    ok.

test_character_escape_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>"/utf8>>},
        markdown:to_html(<<"[foo]: /bar\\* \"ti\\*tle\"\n\n[foo]"/utf8>>),
        "should escape in definition resource and title"
    ),
    ok.

test_character_escape_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-foo+bar\">foo\n</code></pre>"/utf8>>},
        markdown:to_html(<<"``` foo\\+bar\nfoo\n```"/utf8>>),
        "should escape in fenced code info"
    ),
    ok.

test_character_escape_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p>\\&gt; a</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"\\> a"/utf8>>, markdown_options:new(#{parse => #{constructs => #{character_escape => false}}})
        ),
        "should support turning off character escapes"
    ),
    ok.

test_character_escape_case_13(_Config) ->
    ?assertEqual(
        {ok,
            markdown_mdast_node:root(#markdown_mdast_root{
                children = ?'vec!'([
                    markdown_mdast_node:paragraph(#markdown_mdast_paragraph{
                        children = ?'vec!'([
                            markdown_mdast_node:text(#markdown_mdast_text{
                                value = <<"a * b">>,
                                position = {some, markdown_unist_position:new(1, 1, 0, 1, 7, 6)}
                            })
                        ]),
                        position = {some, markdown_unist_position:new(1, 1, 0, 1, 7, 6)}
                    })
                ]),
                position = {some, markdown_unist_position:new(1, 1, 0, 1, 7, 6)}
            })},
        markdown:to_mdast(<<"a \\* b">>, markdown_parse_options:default()),
        "should support character escapes as `Text`s in mdast"
    ),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
