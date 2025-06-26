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
-module(markdown_code_text_SUITE).
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
    test_code_text_case_1/1,
    test_code_text_case_2/1,
    test_code_text_case_3/1,
    test_code_text_case_4/1,
    test_code_text_case_5/1,
    test_code_text_case_6/1,
    test_code_text_case_7/1,
    test_code_text_case_8/1,
    test_code_text_case_9/1,
    test_code_text_case_10/1,
    test_code_text_case_11/1,
    test_code_text_case_12/1,
    test_code_text_case_13/1,
    test_code_text_case_14/1,
    test_code_text_case_15/1,
    test_code_text_case_16/1,
    test_code_text_case_17/1,
    test_code_text_case_18/1,
    test_code_text_case_19/1,
    test_code_text_case_20/1,
    test_code_text_case_21/1,
    test_code_text_case_22/1,
    test_code_text_case_23/1,
    test_code_text_case_24/1,
    test_code_text_case_25/1,
    test_code_text_case_26/1,
    test_code_text_case_27/1,
    test_code_text_case_28/1
]).

%% Macros
-define(DANGER(),
    markdown_options:new(#{compile => #{allow_dangerous_html => true, allow_dangerous_protocol => true}})
).
-define(OFF(),
    markdown_options:new(#{parse => #{constructs => #{code_text => false}}})
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
            test_code_text_case_1,
            test_code_text_case_2,
            test_code_text_case_3,
            test_code_text_case_4,
            test_code_text_case_5,
            test_code_text_case_6,
            test_code_text_case_7,
            test_code_text_case_8,
            test_code_text_case_9,
            test_code_text_case_10,
            test_code_text_case_11,
            test_code_text_case_12,
            test_code_text_case_13,
            test_code_text_case_14,
            test_code_text_case_15,
            test_code_text_case_16,
            test_code_text_case_17,
            test_code_text_case_18,
            test_code_text_case_19,
            test_code_text_case_20,
            test_code_text_case_21,
            test_code_text_case_22,
            test_code_text_case_23,
            test_code_text_case_24,
            test_code_text_case_25,
            test_code_text_case_26,
            test_code_text_case_27,
            test_code_text_case_28
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

test_code_text_case_1(_Config) ->
    ?assertMatch({ok, <<"<p><code>foo</code></p>"/utf8>>}, markdown:to_html(<<"`foo`"/utf8>>), "should support code"),
    ok.

test_code_text_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo ` bar</code></p>"/utf8>>},
        markdown:to_html(<<"`` foo ` bar ``"/utf8>>),
        "should support code w/ more accents"
    ),
    ok.

test_code_text_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>``</code></p>"/utf8>>},
        markdown:to_html(<<"` `` `"/utf8>>),
        "should support code w/ fences inside, and padding"
    ),
    ok.

test_code_text_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code> `` </code></p>"/utf8>>},
        markdown:to_html(<<"`  ``  `"/utf8>>),
        "should support code w/ extra padding"
    ),
    ok.

test_code_text_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code> a</code></p>"/utf8>>},
        markdown:to_html(<<"` a`"/utf8>>),
        "should support code w/ unbalanced padding"
    ),
    ok.

test_code_text_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>\u{a0}b\u{a0}</code></p>"/utf8>>},
        markdown:to_html(<<"`\u{a0}b\u{a0}`"/utf8>>),
        "should support code w/ non-padding whitespace"
    ),
    ok.

test_code_text_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code> </code>\n<code>  </code></p>"/utf8>>},
        markdown:to_html(<<"` `\n`  `"/utf8>>),
        "should support code w/o data"
    ),
    ok.

test_code_text_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo bar   baz</code></p>"/utf8>>},
        markdown:to_html(<<"``\nfoo\nbar  \nbaz\n``"/utf8>>),
        "should support code w/o line endings (1)"
    ),
    ok.

test_code_text_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo </code></p>"/utf8>>},
        markdown:to_html(<<"``\nfoo \n``"/utf8>>),
        "should support code w/o line endings (2)"
    ),
    ok.

test_code_text_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo   bar  baz</code></p>"/utf8>>},
        markdown:to_html(<<"`foo   bar \nbaz`"/utf8>>),
        "should not support whitespace collapsing"
    ),
    ok.

test_code_text_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo\\</code>bar`</p>"/utf8>>},
        markdown:to_html(<<"`foo\\`bar`"/utf8>>),
        "should not support character escapes"
    ),
    ok.

test_code_text_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo`bar</code></p>"/utf8>>},
        markdown:to_html(<<"``foo`bar``"/utf8>>),
        "should support more accents"
    ),
    ok.

test_code_text_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo `` bar</code></p>"/utf8>>},
        markdown:to_html(<<"` foo `` bar `"/utf8>>),
        "should support less accents"
    ),
    ok.

test_code_text_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*foo<code>*</code></p>"/utf8>>}, markdown:to_html(<<"*foo`*`"/utf8>>), "should precede over emphasis"
    ),
    ok.

test_code_text_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<p>[not a <code>link](/foo</code>)</p>"/utf8>>},
        markdown:to_html(<<"[not a `link](/foo`)"/utf8>>),
        "should precede over links"
    ),
    ok.

test_code_text_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>"/utf8>>},
        markdown:to_html(<<"`<a href=\"`\">`"/utf8>>),
        "should have same precedence as HTML (1)"
    ),
    ok.

test_code_text_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"`\">`</p>"/utf8>>},
        markdown:to_html_with_options(<<"<a href=\"`\">`"/utf8>>, ?DANGER()),
        "should have same precedence as HTML (2)"
    ),
    ok.

test_code_text_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>"/utf8>>},
        markdown:to_html(<<"`<http://foo.bar.`baz>`"/utf8>>),
        "should have same precedence as autolinks (1)"
    ),
    ok.

test_code_text_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"http://foo.bar.%60baz\">http://foo.bar.`baz</a>`</p>"/utf8>>},
        markdown:to_html(<<"<http://foo.bar.`baz>`"/utf8>>),
        "should have same precedence as autolinks (2)"
    ),
    ok.

test_code_text_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<p>```foo``</p>"/utf8>>},
        markdown:to_html(<<"```foo``"/utf8>>),
        "should not support more accents before a fence"
    ),
    ok.

test_code_text_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<p>`foo</p>"/utf8>>}, markdown:to_html(<<"`foo"/utf8>>), "should not support no closing fence (1)"
    ),
    ok.

test_code_text_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<p>`foo<code>bar</code></p>"/utf8>>},
        markdown:to_html(<<"`foo``bar``"/utf8>>),
        "should not support no closing fence (2)"
    ),
    ok.

test_code_text_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo\t\tbar</code></p>"/utf8>>},
        markdown:to_html(<<"`foo\t\tbar`"/utf8>>),
        "should support tabs in code"
    ),
    ok.

test_code_text_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<p>`<code>x</code></p>"/utf8>>},
        markdown:to_html(<<"\\``x`"/utf8>>),
        "should support an escaped initial grave accent"
    ),
    ok.

test_code_text_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<p>`a`</p>"/utf8>>},
        markdown:to_html_with_options(<<"`a`"/utf8>>, ?OFF()),
        "should support turning off code (text)"
    ),
    ok.

test_code_text_case_26(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("a `alpha` b."/utf8>>), Node::Root(Root {
    %         children: vec![Node::Paragraph(Paragraph {
    %             children: vec![
    %                 Node::Text(Text {
    %                     value: "a ".into(),
    ok.

test_code_text_case_27(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("`  alpha `"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Paragraph(Paragraph {
    %             children: vec![Node::InlineCode(InlineCode {
    %                 value: " alpha".into(),
    ok.

test_code_text_case_28(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("`   `"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Paragraph(Paragraph {
    %             children: vec![Node::InlineCode(InlineCode {
    %                 value: "   ".into(),
    ok.
