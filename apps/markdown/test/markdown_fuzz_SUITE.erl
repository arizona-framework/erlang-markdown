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
-module(markdown_fuzz_SUITE).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-04-08", modified => "2025-04-08"}.
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
    test_fuzz_case_1/1,
    test_fuzz_case_2/1,
    test_fuzz_case_3/1,
    test_fuzz_case_4/1,
    test_fuzz_case_5/1,
    test_fuzz_case_6/1,
    test_fuzz_case_7/1,
    test_fuzz_case_8/1,
    test_fuzz_case_9/1,
    test_fuzz_case_10/1,
    test_fuzz_case_11/1,
    test_fuzz_case_12/1,
    test_fuzz_case_13/1,
    test_fuzz_case_14/1,
    test_fuzz_case_15/1,
    test_fuzz_case_16/1,
    test_fuzz_case_17/1,
    test_fuzz_case_18/1,
    test_fuzz_case_19/1
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
            test_fuzz_case_1,
            test_fuzz_case_2,
            test_fuzz_case_3,
            test_fuzz_case_4,
            test_fuzz_case_5,
            test_fuzz_case_6,
            test_fuzz_case_7,
            test_fuzz_case_8,
            test_fuzz_case_9,
            test_fuzz_case_10,
            test_fuzz_case_11,
            test_fuzz_case_12,
            test_fuzz_case_13,
            test_fuzz_case_14,
            test_fuzz_case_15,
            test_fuzz_case_16,
            test_fuzz_case_17,
            test_fuzz_case_18,
            test_fuzz_case_19
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

test_fuzz_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<h2>[\n~\na</h2>\n"/utf8>>},
        markdown:to_html(<<"[\n~\na\n-\n\n"/utf8>>),
        "1: label, blank lines, and code"
    ),
    ok.

test_fuzz_case_2(_Config) ->
    %% The first link is stopped by the `+` (so it’s `a@b.c`), but the next
    %% link overlaps it (`b.c+d@e.f`).
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:a@b.c\">a@b.c</a><a href=\"mailto:+d@e.f\">+d@e.f</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"a@b.c+d@e.f"/utf8>>, markdown_options:gfm()),
        "2: gfm: email autolink literals running into each other"
    ),
    ok.

test_fuzz_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>x\n</code></pre>\n<ul>\n<li></li>\n</ul>"/utf8>>},
        markdown:to_html(<<"    x\n*    "/utf8>>),
        "3-a: containers should not pierce into indented code"
    ),
    ok.

test_fuzz_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>a\n</code></pre>\n<ul>\n<li>\n<pre><code>b\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"    a\n*     b"/utf8>>),
        "3-b: containers should not pierce into indented code"
    ),
    ok.

test_fuzz_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a *</p>"/utf8>>}, markdown:to_html(<<"a * "/utf8>>), "4-a: trailing whitespace and broken data"
    ),
    ok.

test_fuzz_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_</p>"/utf8>>}, markdown:to_html(<<"_  "/utf8>>), "4-b: trailing whitespace and broken data (GH-13)"
    ),
    ok.

test_fuzz_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a ~</p>"/utf8>>},
        markdown:to_html_with_options(<<"a ~ "/utf8>>, markdown_options:gfm()),
        "4-c: trailing whitespace and broken data (GH-14)"
    ),
    ok.

test_fuzz_case_8(_Config) ->
    % assert!(
    %     matches!(
    %         to_mdast("123456789. ok", &Default::default()),
    %         Ok(mdast::Node::Root(_))
    %     ),
    %     "5: lists should support high start numbers (GH-17)"
    % );
    ok.

test_fuzz_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code>\n</code></pre>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> ```\n"/utf8>>),
        "6-a: container close after unclosed fenced code, with eol (block quote, GH-16)"
    ),
    ok.

test_fuzz_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n"/utf8>>),
        "6-b: container close after unclosed fenced code, with eol (list, GH-16)"
    ),
    ok.

test_fuzz_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>x</p>\n</blockquote>\n<p>``</p>"/utf8>>},
        markdown:to_html_with_options(<<"> x\n``"/utf8>>, markdown_options:gfm()),
        "7: lazy container lines almost starting fenced code (GH-19)"
    ),
    ok.

test_fuzz_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a\t<a href=\"mailto:b@c.d\">b@c.d</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"a\tb@c.d"/utf8>>, markdown_options:gfm()),
        "8-a: autolink literals after tabs (GH-18)"
    ),
    ok.

test_fuzz_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p>aa\t<a href=\"mailto:b@c.d\">b@c.d</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"aa\tb@c.d"/utf8>>, markdown_options:gfm()),
        "8-b: autolink literals after tabs (GH-18)"
    ),
    ok.

test_fuzz_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p>aaa\t<a href=\"mailto:b@c.d\">b@c.d</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"aaa\tb@c.d"/utf8>>, markdown_options:gfm()),
        "8-c: autolink literals after tabs (GH-18)"
    ),
    ok.

test_fuzz_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<p>aaaa\t<a href=\"mailto:b@c.d\">b@c.d</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"aaaa\tb@c.d"/utf8>>, markdown_options:gfm()),
        "8-d: autolink literals after tabs (GH-18)"
    ),
    ok.

test_fuzz_case_16(_Config) ->
    ?assertMatch(
        {ok,
            <<"<table>\n<thead>\n<tr>\n<th>a</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td><a href=\"http://www.a\">www.a</a></td>\n</tr>\n</tbody>\n</table>"/utf8>>},
        markdown:to_html_with_options(<<"| a |\n| - |\n| www.a|"/utf8>>, markdown_options:gfm()),
        "9: autolink literals that end in table cell delimiter (GH-20)"
    ),
    ok.

test_fuzz_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"\">*</a> <a href=\"\">*</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"[*]() [*]()"/utf8>>, markdown_options:gfm()),
        "10: attention in different links (GH-21)"
    ),
    ok.

test_fuzz_case_18(_Config) ->
    % assert!(
    %     matches!(
    %         to_mdast("* [ ]\na", &Default::default()),
    %         Ok(mdast::Node::Root(_))
    %     ),
    %     "11: gfm task list items followed by eols (GH-24)"
    % );
    ok.

test_fuzz_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;</p>"/utf8>>},
        markdown:to_html_with_options(<<"<"/utf8>>, #{parse => markdown_parse_options:mdx()}),
        "12: mdx: handle invalid mdx without panic (GH-26)"
    ),
    ok.
