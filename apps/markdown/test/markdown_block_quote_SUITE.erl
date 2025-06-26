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
-module(markdown_block_quote_SUITE).
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
    test_block_quote_case_1/1,
    test_block_quote_case_2/1,
    test_block_quote_case_3/1,
    test_block_quote_case_4/1,
    test_block_quote_case_5/1,
    test_block_quote_case_6/1,
    test_block_quote_case_7/1,
    test_block_quote_case_8/1,
    test_block_quote_case_9/1,
    test_block_quote_case_10/1,
    test_block_quote_case_11/1,
    test_block_quote_case_12/1,
    test_block_quote_case_13/1,
    test_block_quote_case_14/1,
    test_block_quote_case_15/1,
    test_block_quote_case_16/1,
    test_block_quote_case_17/1,
    test_block_quote_case_18/1,
    test_block_quote_case_19/1,
    test_block_quote_case_20/1,
    test_block_quote_case_21/1,
    test_block_quote_case_22/1,
    test_block_quote_case_23/1,
    test_block_quote_case_24/1,
    test_block_quote_case_25/1,
    test_block_quote_case_26/1,
    test_block_quote_case_27/1,
    test_block_quote_case_28/1,
    test_block_quote_case_29/1,
    test_block_quote_case_30/1,
    test_block_quote_case_31/1,
    test_block_quote_case_32/1,
    test_block_quote_case_33/1,
    test_block_quote_case_34/1
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
            test_block_quote_case_1,
            test_block_quote_case_2,
            test_block_quote_case_3,
            test_block_quote_case_4,
            test_block_quote_case_5,
            test_block_quote_case_6,
            test_block_quote_case_7,
            test_block_quote_case_8,
            test_block_quote_case_9,
            test_block_quote_case_10,
            test_block_quote_case_11,
            test_block_quote_case_12,
            test_block_quote_case_13,
            test_block_quote_case_14,
            test_block_quote_case_15,
            test_block_quote_case_16,
            test_block_quote_case_17,
            test_block_quote_case_18,
            test_block_quote_case_19,
            test_block_quote_case_20,
            test_block_quote_case_21,
            test_block_quote_case_22,
            test_block_quote_case_23,
            test_block_quote_case_24,
            test_block_quote_case_25,
            test_block_quote_case_26,
            test_block_quote_case_27,
            test_block_quote_case_28,
            test_block_quote_case_29,
            test_block_quote_case_30,
            test_block_quote_case_31,
            test_block_quote_case_32,
            test_block_quote_case_33,
            test_block_quote_case_34
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

test_block_quote_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<h1>a</h1>\n<p>b\nc</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> # a\n> b\n> c"/utf8>>),
        "should support block quotes"
    ),
    ok.

test_block_quote_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<h1>a</h1>\n<p>b\nc</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"># a\n>b\n> c"/utf8>>),
        "should support block quotes w/o space"
    ),
    ok.

test_block_quote_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<h1>a</h1>\n<p>b\nc</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"   > # a\n   > b\n > c"/utf8>>),
        "should support prefixing block quotes w/ spaces"
    ),
    ok.

test_block_quote_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>&gt; # a\n&gt; b\n&gt; c\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    > # a\n    > b\n    > c"/utf8>>),
        "should not support block quotes w/ 4 spaces"
    ),
    ok.

test_block_quote_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<h1>a</h1>\n<p>b\nc</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> # a\n> b\nc"/utf8>>),
        "should support lazy content lines"
    ),
    ok.

test_block_quote_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a\nb\nc</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\nb\n> c"/utf8>>),
        "should support lazy content lines inside block quotes"
    ),
    ok.

test_block_quote_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<h2>a</h2>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n> ---"/utf8>>),
        "should support setext headings underlines in block quotes"
    ),
    ok.

test_block_quote_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<hr />"/utf8>>},
        markdown:to_html(<<"> a\n---"/utf8>>),
        "should not support lazy setext headings underlines in block quotes"
    ),
    ok.

test_block_quote_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<ul>\n<li>a</li>\n<li>b</li>\n</ul>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> - a\n> - b"/utf8>>),
        "should support lists in block quotes"
    ),
    ok.

test_block_quote_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<ul>\n<li>a</li>\n</ul>\n</blockquote>\n<ul>\n<li>b</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"> - a\n- b"/utf8>>),
        "should not support lazy lists in block quotes"
    ),
    ok.

test_block_quote_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code>a\n</code></pre>\n</blockquote>\n<pre><code>b\n</code></pre>"/utf8>>},
        markdown:to_html(<<">     a\n    b"/utf8>>),
        "should not support lazy indented code in block quotes"
    ),
    ok.

test_block_quote_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>a</p>\n<pre><code></code></pre>\n"/utf8>>},
        markdown:to_html(<<"> ```\na\n```"/utf8>>),
        "should not support lazy fenced code in block quotes (1)"
    ),
    ok.

test_block_quote_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<pre><code>b\n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"> a\n```\nb"/utf8>>),
        "should not support lazy fenced code in block quotes (2)"
    ),
    ok.

test_block_quote_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a\n- b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n    - b"/utf8>>),
        "should not support lazy indented code (or lazy list) in block quotes"
    ),
    ok.

test_block_quote_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>[\na</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> [\na"/utf8>>),
        "should support lazy, definition-like lines"
    ),
    ok.

test_block_quote_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>c</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> [a]: b\nc"/utf8>>),
        "should support a definition, followed by a lazy paragraph"
    ),
    ok.

test_block_quote_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">"/utf8>>),
        "should support empty block quotes (1)"
    ),
    ok.

test_block_quote_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">\n>  \n> "/utf8>>),
        "should support empty block quotes (2)"
    ),
    ok.

test_block_quote_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">\n> a\n>  "/utf8>>),
        "should support initial or final lazy empty block quote lines"
    ),
    ok.

test_block_quote_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<blockquote>\n<p>b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n\n> b"/utf8>>),
        "should support adjacent block quotes"
    ),
    ok.

test_block_quote_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a\nb</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n> b"/utf8>>),
        "should support a paragraph in a block quote"
    ),
    ok.

test_block_quote_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n<p>b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n>\n> b"/utf8>>),
        "should support adjacent paragraphs in block quotes"
    ),
    ok.

test_block_quote_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"b\">a</a></p>\n<blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"[a]\n\n> [a]: b"/utf8>>),
        "should support a definition in a block quote (1)"
    ),
    ok.

test_block_quote_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n</blockquote>\n<p><a href=\"b\">a</a></p>"/utf8>>},
        markdown:to_html(<<"> [a]: b\n\n[a]"/utf8>>),
        "should support a definition in a block quote (2)"
    ),
    ok.

test_block_quote_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a</p>\n<blockquote>\n<p>b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"a\n> b"/utf8>>),
        "should support interrupting paragraphs w/ block quotes"
    ),
    ok.

test_block_quote_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\n***\n> b"/utf8>>),
        "should support interrupting block quotes w/ thematic breaks"
    ),
    ok.

test_block_quote_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a\nb</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> a\nb"/utf8>>),
        "should not support interrupting block quotes w/ paragraphs"
    ),
    ok.

test_block_quote_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<p>b</p>"/utf8>>},
        markdown:to_html(<<"> a\n\nb"/utf8>>),
        "should support interrupting block quotes w/ blank lines"
    ),
    ok.

test_block_quote_case_29(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<p>b</p>"/utf8>>},
        markdown:to_html(<<"> a\n>\nb"/utf8>>),
        "should not support interrupting a blank line in a block quotes w/ paragraphs"
    ),
    ok.

test_block_quote_case_30(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<blockquote>\n<blockquote>\n<p>a\nb</p>\n</blockquote>\n</blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> > > a\nb"/utf8>>),
        "should not support interrupting many block quotes w/ paragraphs (1)"
    ),
    ok.

test_block_quote_case_31(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<blockquote>\n<blockquote>\n<p>a\nb\nc</p>\n</blockquote>\n</blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">>> a\n> b\n>>c"/utf8>>),
        "should not support interrupting many block quotes w/ paragraphs (2)"
    ),
    ok.

test_block_quote_case_32(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<pre><code>a\n</code></pre>\n</blockquote>\n<blockquote>\n<p>b</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">     a\n\n>    b"/utf8>>),
        "should support 5 spaces for indented code, not 4"
    ),
    ok.

test_block_quote_case_33(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&gt; # a\n&gt; b\n&gt; c</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"> # a\n> b\n> c"/utf8>>, markdown_options:new(#{parse => #{constructs => #{block_quote => false}}})
        ),
        "should support turning off block quotes"
    ),
    ok.

test_block_quote_case_34(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("> a"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Blockquote(Blockquote {
    %             children: vec![Node::Paragraph(Paragraph {
    %                 children: vec![Node::Text(Text {
    %                     value: "a".into(),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
