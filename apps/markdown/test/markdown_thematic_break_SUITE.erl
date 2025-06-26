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
-module(markdown_thematic_break_SUITE).
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
    test_thematic_break_case_1/1,
    test_thematic_break_case_2/1,
    test_thematic_break_case_3/1,
    test_thematic_break_case_4/1,
    test_thematic_break_case_5/1,
    test_thematic_break_case_6/1,
    test_thematic_break_case_7/1,
    test_thematic_break_case_8/1,
    test_thematic_break_case_9/1,
    test_thematic_break_case_10/1,
    test_thematic_break_case_11/1,
    test_thematic_break_case_12/1,
    test_thematic_break_case_13/1,
    test_thematic_break_case_14/1,
    test_thematic_break_case_15/1,
    test_thematic_break_case_16/1,
    test_thematic_break_case_17/1,
    test_thematic_break_case_18/1,
    test_thematic_break_case_19/1,
    test_thematic_break_case_20/1,
    test_thematic_break_case_21/1,
    test_thematic_break_case_22/1,
    test_thematic_break_case_23/1,
    test_thematic_break_case_24/1,
    test_thematic_break_case_25/1,
    test_thematic_break_case_26/1,
    test_thematic_break_case_27/1,
    test_thematic_break_case_28/1,
    test_thematic_break_case_29/1
]).

%% Macros
-define(OFF(),
    markdown_options:new(#{parse => #{constructs => #{thematic_break => false}}})
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
            test_thematic_break_case_1,
            test_thematic_break_case_2,
            test_thematic_break_case_3,
            test_thematic_break_case_4,
            test_thematic_break_case_5,
            test_thematic_break_case_6,
            test_thematic_break_case_7,
            test_thematic_break_case_8,
            test_thematic_break_case_9,
            test_thematic_break_case_10,
            test_thematic_break_case_11,
            test_thematic_break_case_12,
            test_thematic_break_case_13,
            test_thematic_break_case_14,
            test_thematic_break_case_15,
            test_thematic_break_case_16,
            test_thematic_break_case_17,
            test_thematic_break_case_18,
            test_thematic_break_case_19,
            test_thematic_break_case_20,
            test_thematic_break_case_21,
            test_thematic_break_case_22,
            test_thematic_break_case_23,
            test_thematic_break_case_24,
            test_thematic_break_case_25,
            test_thematic_break_case_26,
            test_thematic_break_case_27,
            test_thematic_break_case_28,
            test_thematic_break_case_29
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

test_thematic_break_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />\n<hr />\n<hr />"/utf8>>},
        markdown:to_html(<<"***\n---\n___"/utf8>>),
        "should support thematic breaks w/ asterisks, dashes, and underscores"
    ),
    ok.

test_thematic_break_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p>+++</p>"/utf8>>}, markdown:to_html(<<"+++"/utf8>>), "should not support thematic breaks w/ plusses"
    ),
    ok.

test_thematic_break_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>===</p>"/utf8>>}, markdown:to_html(<<"==="/utf8>>), "should not support thematic breaks w/ equals"
    ),
    ok.

test_thematic_break_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p>--</p>"/utf8>>}, markdown:to_html(<<"--"/utf8>>), "should not support thematic breaks w/ two dashes"
    ),
    ok.

test_thematic_break_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>**</p>"/utf8>>},
        markdown:to_html(<<"**"/utf8>>),
        "should not support thematic breaks w/ two asterisks"
    ),
    ok.

test_thematic_break_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>__</p>"/utf8>>},
        markdown:to_html(<<"__"/utf8>>),
        "should not support thematic breaks w/ two underscores"
    ),
    ok.

test_thematic_break_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>}, markdown:to_html(<<" ***"/utf8>>), "should support thematic breaks w/ 1 space"
    ),
    ok.

test_thematic_break_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>}, markdown:to_html(<<"  ***"/utf8>>), "should support thematic breaks w/ 2 spaces"
    ),
    ok.

test_thematic_break_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>}, markdown:to_html(<<"   ***"/utf8>>), "should support thematic breaks w/ 3 spaces"
    ),
    ok.

test_thematic_break_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>***\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    ***"/utf8>>),
        "should not support thematic breaks w/ 4 spaces"
    ),
    ok.

test_thematic_break_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p>Foo\n***</p>"/utf8>>},
        markdown:to_html(<<"Foo\n    ***"/utf8>>),
        "should not support thematic breaks w/ 4 spaces as paragraph continuation"
    ),
    ok.

test_thematic_break_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>},
        markdown:to_html(<<"_____________________________________"/utf8>>),
        "should support thematic breaks w/ many markers"
    ),
    ok.

test_thematic_break_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>}, markdown:to_html(<<" - - -"/utf8>>), "should support thematic breaks w/ spaces (1)"
    ),
    ok.

test_thematic_break_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>},
        markdown:to_html(<<" **  * ** * ** * **"/utf8>>),
        "should support thematic breaks w/ spaces (2)"
    ),
    ok.

test_thematic_break_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>},
        markdown:to_html(<<"-     -      -      -"/utf8>>),
        "should support thematic breaks w/ spaces (3)"
    ),
    ok.

test_thematic_break_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<hr />"/utf8>>},
        markdown:to_html(<<"- - - -    "/utf8>>),
        "should support thematic breaks w/ trailing spaces"
    ),
    ok.

test_thematic_break_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p>_ _ _ _ a</p>"/utf8>>},
        markdown:to_html(<<"_ _ _ _ a"/utf8>>),
        "should not support thematic breaks w/ other characters (1)"
    ),
    ok.

test_thematic_break_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a------</p>"/utf8>>},
        markdown:to_html(<<"a------"/utf8>>),
        "should not support thematic breaks w/ other characters (2)"
    ),
    ok.

test_thematic_break_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<p>---a---</p>"/utf8>>},
        markdown:to_html(<<"---a---"/utf8>>),
        "should not support thematic breaks w/ other characters (3)"
    ),
    ok.

test_thematic_break_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<p><em>-</em></p>"/utf8>>},
        markdown:to_html(<<" *-*"/utf8>>),
        "should not support thematic breaks w/ mixed markers"
    ),
    ok.

test_thematic_break_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n***\n- bar"/utf8>>),
        "should support thematic breaks mixed w/ lists (1)"
    ),
    ok.

test_thematic_break_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* Foo\n* * *\n* Bar"/utf8>>),
        "should support thematic breaks mixed w/ lists (2)"
    ),
    ok.

test_thematic_break_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<p>Foo</p>\n<hr />\n<p>bar</p>"/utf8>>},
        markdown:to_html(<<"Foo\n***\nbar"/utf8>>),
        "should support thematic breaks interrupting paragraphs"
    ),
    ok.

test_thematic_break_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<h2>Foo</h2>\n<p>bar</p>"/utf8>>},
        markdown:to_html(<<"Foo\n---\nbar"/utf8>>),
        "should not support thematic breaks w/ dashes interrupting paragraphs (setext heading)"
    ),
    ok.

test_thematic_break_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- Foo\n- * * *"/utf8>>),
        "should support thematic breaks in lists"
    ),
    ok.

test_thematic_break_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<hr />\n</blockquote>\n<p>a</p>"/utf8>>},
        markdown:to_html(<<"> ---\na"/utf8>>),
        "should not support lazyness (1)"
    ),
    ok.

test_thematic_break_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<hr />"/utf8>>},
        markdown:to_html(<<"> a\n---"/utf8>>),
        "should not support lazyness (2)"
    ),
    ok.

test_thematic_break_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<p>***</p>"/utf8>>},
        markdown:to_html_with_options(<<"***"/utf8>>, ?OFF()),
        "should support turning off thematic breaks"
    ),
    ok.

test_thematic_break_case_29(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("***"/utf8>>), Node::Root(Root {
    %         children: vec![Node::ThematicBreak(ThematicBreak {
    %             position: Some(Position::new(1, 1, 0, 1, 4, 3),
    ok.
