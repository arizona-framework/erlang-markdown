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
-module(markdown_code_fenced_SUITE).
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
    test_code_fenced_case_1/1,
    test_code_fenced_case_2/1,
    test_code_fenced_case_3/1,
    test_code_fenced_case_4/1,
    test_code_fenced_case_5/1,
    test_code_fenced_case_6/1,
    test_code_fenced_case_7/1,
    test_code_fenced_case_8/1,
    test_code_fenced_case_9/1,
    test_code_fenced_case_10/1,
    test_code_fenced_case_11/1,
    test_code_fenced_case_12/1,
    test_code_fenced_case_13/1,
    test_code_fenced_case_14/1,
    test_code_fenced_case_15/1,
    test_code_fenced_case_16/1,
    test_code_fenced_case_17/1,
    test_code_fenced_case_18/1,
    test_code_fenced_case_19/1,
    test_code_fenced_case_20/1,
    test_code_fenced_case_21/1,
    test_code_fenced_case_22/1,
    test_code_fenced_case_23/1,
    test_code_fenced_case_24/1,
    test_code_fenced_case_25/1,
    test_code_fenced_case_26/1,
    test_code_fenced_case_27/1,
    test_code_fenced_case_28/1,
    test_code_fenced_case_29/1,
    test_code_fenced_case_30/1,
    test_code_fenced_case_31/1,
    test_code_fenced_case_32/1,
    test_code_fenced_case_33/1,
    test_code_fenced_case_34/1,
    test_code_fenced_case_35/1,
    test_code_fenced_case_36/1,
    test_code_fenced_case_37/1,
    test_code_fenced_case_38/1,
    test_code_fenced_case_39/1,
    test_code_fenced_case_40/1,
    test_code_fenced_case_41/1,
    test_code_fenced_case_42/1,
    test_code_fenced_case_43/1,
    test_code_fenced_case_44/1,
    test_code_fenced_case_45/1,
    test_code_fenced_case_46/1,
    test_code_fenced_case_47/1
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
            test_code_fenced_case_1,
            test_code_fenced_case_2,
            test_code_fenced_case_3,
            test_code_fenced_case_4,
            test_code_fenced_case_5,
            test_code_fenced_case_6,
            test_code_fenced_case_7,
            test_code_fenced_case_8,
            test_code_fenced_case_9,
            test_code_fenced_case_10,
            test_code_fenced_case_11,
            test_code_fenced_case_12,
            test_code_fenced_case_13,
            test_code_fenced_case_14,
            test_code_fenced_case_15,
            test_code_fenced_case_16,
            test_code_fenced_case_17,
            test_code_fenced_case_18,
            test_code_fenced_case_19,
            test_code_fenced_case_20,
            test_code_fenced_case_21,
            test_code_fenced_case_22,
            test_code_fenced_case_23,
            test_code_fenced_case_24,
            test_code_fenced_case_25,
            test_code_fenced_case_26,
            test_code_fenced_case_27,
            test_code_fenced_case_28,
            test_code_fenced_case_29,
            test_code_fenced_case_30,
            test_code_fenced_case_31,
            test_code_fenced_case_32,
            test_code_fenced_case_33,
            test_code_fenced_case_34,
            test_code_fenced_case_35,
            test_code_fenced_case_36,
            test_code_fenced_case_37,
            test_code_fenced_case_38,
            test_code_fenced_case_39,
            test_code_fenced_case_40,
            test_code_fenced_case_41,
            test_code_fenced_case_42,
            test_code_fenced_case_43,
            test_code_fenced_case_44,
            test_code_fenced_case_45,
            test_code_fenced_case_46,
            test_code_fenced_case_47
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

test_code_fenced_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>&lt;\n &gt;\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```\n<\n >\n```"/utf8>>),
        "should support fenced code w/ grave accents"
    ),
    ok.

test_code_fenced_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>&lt;\n &gt;\n</code></pre>"/utf8>>},
        markdown:to_html(<<"~~~\n<\n >\n~~~"/utf8>>),
        "should support fenced code w/ tildes"
    ),
    ok.

test_code_fenced_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>foo</code></p>"/utf8>>},
        markdown:to_html(<<"``\nfoo\n``"/utf8>>),
        "should not support fenced code w/ less than three markers"
    ),
    ok.

test_code_fenced_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n~~~\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```\naaa\n~~~\n```"/utf8>>),
        "should not support a tilde closing sequence for a grave accent opening sequence"
    ),
    ok.

test_code_fenced_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n```\n</code></pre>"/utf8>>},
        markdown:to_html(<<"~~~\naaa\n```\n~~~"/utf8>>),
        "should not support a grave accent closing sequence for a tilde opening sequence"
    ),
    ok.

test_code_fenced_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n```\n</code></pre>"/utf8>>},
        markdown:to_html(<<"````\naaa\n```\n``````"/utf8>>),
        "should support a closing sequence longer, but not shorter than, the opening"
    ),
    ok.

test_code_fenced_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n~~~\n</code></pre>"/utf8>>},
        markdown:to_html(<<"~~~~\naaa\n~~~\n~~~~"/utf8>>),
        "should support a closing sequence equal to, but not shorter than, the opening"
    ),
    ok.

test_code_fenced_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code></code></pre>\n"/utf8>>},
        markdown:to_html(<<"```"/utf8>>),
        "should support an eof right after an opening sequence"
    ),
    ok.

test_code_fenced_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>\n```\naaa\n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"`````\n\n```\naaa\n"/utf8>>),
        "should support an eof somewhere in content"
    ),
    ok.

test_code_fenced_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>"/utf8>>},
        markdown:to_html(<<"> ```\n> aaa\n\nbbb"/utf8>>),
        "should support no closing sequence in a block quote"
    ),
    ok.

test_code_fenced_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>\n  \n</code></pre>"/utf8>>},
        markdown:to_html(<<"```\n\n  \n```"/utf8>>),
        "should support blank lines in fenced code"
    ),
    ok.

test_code_fenced_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code></code></pre>"/utf8>>},
        markdown:to_html(<<"```\n```"/utf8>>),
        "should support empty fenced code"
    ),
    ok.

test_code_fenced_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\naaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<" ```\n aaa\naaa\n```"/utf8>>),
        "should remove up to one space from the content if the opening sequence is indented w/ 1 space"
    ),
    ok.

test_code_fenced_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\naaa\naaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<"  ```\naaa\n  aaa\naaa\n  ```"/utf8>>),
        "should remove up to two space from the content if the opening sequence is indented w/ 2 spaces"
    ),
    ok.

test_code_fenced_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n aaa\naaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<"   ```\n   aaa\n    aaa\n  aaa\n   ```"/utf8>>),
        "should remove up to three space from the content if the opening sequence is indented w/ 3 spaces"
    ),
    ok.

test_code_fenced_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>```\naaa\n```\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    ```\n    aaa\n    ```"/utf8>>),
        "should not support indenteding the opening sequence w/ 4 spaces"
    ),
    ok.

test_code_fenced_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```\naaa\n  ```"/utf8>>),
        "should support an indented closing sequence"
    ),
    ok.

test_code_fenced_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<"   ```\naaa\n  ```"/utf8>>),
        "should support a differently indented closing sequence than the opening sequence"
    ),
    ok.

test_code_fenced_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n    ```\n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"```\naaa\n    ```\n"/utf8>>),
        "should not support an indented closing sequence w/ 4 spaces"
    ),
    ok.

test_code_fenced_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code> </code>\naaa</p>"/utf8>>},
        markdown:to_html(<<"``` ```\naaa"/utf8>>),
        "should not support grave accents in the opening fence after the opening sequence"
    ),
    ok.

test_code_fenced_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n~~~ ~~\n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"~~~~~~\naaa\n~~~ ~~\n"/utf8>>),
        "should not support spaces in the closing sequence"
    ),
    ok.

test_code_fenced_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>"/utf8>>},
        markdown:to_html(<<"foo\n```\nbar\n```\nbaz"/utf8>>),
        "should support interrupting paragraphs"
    ),
    ok.

test_code_fenced_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>"/utf8>>},
        markdown:to_html(<<"foo\n---\n~~~\nbar\n~~~\n# baz"/utf8>>),
        "should support interrupting other content"
    ),
    ok.

test_code_fenced_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```ruby\ndef foo(x)\n  return 3\nend\n```"/utf8>>),
        "should support the info string as a `language-` class (1)"
    ),
    ok.

test_code_fenced_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-;\"></code></pre>"/utf8>>},
        markdown:to_html(<<"````;\n````"/utf8>>),
        "should support the info string as a `language-` class (2)"
    ),
    ok.

test_code_fenced_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>"/utf8>>},
        markdown:to_html(<<"~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~"/utf8>>),
        "should support the info string as a `language-` class, but not the meta string"
    ),
    ok.

test_code_fenced_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>aa</code>\nfoo</p>"/utf8>>},
        markdown:to_html(<<"``` aa ```\nfoo"/utf8>>),
        "should not support grave accents in the meta string"
    ),
    ok.

test_code_fenced_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-aa\">foo\n</code></pre>"/utf8>>},
        markdown:to_html(<<"~~~ aa ``` ~~~\nfoo\n~~~"/utf8>>),
        "should support grave accents and tildes in the meta string of tilde fenced code"
    ),
    ok.

test_code_fenced_case_29(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>``` aaa\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```\n``` aaa\n```"/utf8>>),
        "should not support info string on closing sequences"
    ),
    ok.

test_code_fenced_case_30(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code></code></pre>\n"/utf8>>},
        markdown:to_html(<<"```  "/utf8>>),
        "should support an eof after whitespace, after the start fence sequence"
    ),
    ok.

test_code_fenced_case_31(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-js\">alert(1)\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```  js\nalert(1)\n```"/utf8>>),
        "should support whitespace between the sequence and the info string"
    ),
    ok.

test_code_fenced_case_32(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-js\"></code></pre>\n"/utf8>>},
        markdown:to_html(<<"```js"/utf8>>),
        "should support an eof after the info string"
    ),
    ok.

test_code_fenced_case_33(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-js\">alert(1)\n</code></pre>"/utf8>>},
        markdown:to_html(<<"```  js \nalert(1)\n```"/utf8>>),
        "should support whitespace after the info string"
    ),
    ok.

test_code_fenced_case_34(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>  \n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"```\n  "/utf8>>),
        "should support an eof after whitespace in content"
    ),
    ok.

test_code_fenced_case_35(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code></code></pre>\n"/utf8>>},
        markdown:to_html(<<"  ```\n "/utf8>>),
        "should support an eof in the prefix, in content"
    ),
    ok.

test_code_fenced_case_36(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-j+s©\"></code></pre>\n"/utf8>>},
        markdown:to_html(<<"```j\\+s&copy;"/utf8>>),
        "should support character escapes and character references in info strings"
    ),
    ok.

test_code_fenced_case_37(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-a&amp;b�c\"></code></pre>\n"/utf8>>},
        markdown:to_html(<<"```a\\&b\0c"/utf8>>),
        "should encode dangerous characters in languages"
    ),
    ok.

test_code_fenced_case_38(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>aaa\n ```\n</code></pre>\n"/utf8>>},
        markdown:to_html(<<"   ```\naaa\n    ```"/utf8>>),
        "should not support a closing sequence w/ too much indent, regardless of opening sequence (1)"
    ),
    ok.

test_code_fenced_case_39(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code>\n\n\n</code></pre>\n</blockquote>\n<p>a</p>"/utf8>>},
        markdown:to_html(<<"> ```\n>\n>\n>\n\na"/utf8>>),
        "should not support a closing sequence w/ too much indent, regardless of opening sequence (2)"
    ),
    ok.

test_code_fenced_case_40(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<pre><code class=\"language-a\"></code></pre>\n</blockquote>\n<p>b</p>"/utf8>>},
        markdown:to_html(<<"> ```a\nb"/utf8>>),
        "should not support lazyness (1)"
    ),
    ok.

test_code_fenced_case_41(_Config) ->
    ?assertMatch(
        {ok, <<"<blockquote>\n<p>a</p>\n</blockquote>\n<pre><code class=\"language-b\"></code></pre>\n"/utf8>>},
        markdown:to_html(<<"> a\n```b"/utf8>>),
        "should not support lazyness (2)"
    ),
    ok.

test_code_fenced_case_42(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<pre><code class=\"language-a\"></code></pre>\n</blockquote>\n<pre><code></code></pre>\n"/utf8>>},
        markdown:to_html(<<"> ```a\n```"/utf8>>),
        "should not support lazyness (3)"
    ),
    ok.

test_code_fenced_case_43(_Config) ->
    ?assertMatch(
        {ok, <<"<p>```</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"```"/utf8>>, markdown_options:new(#{parse => #{constructs => #{code_fenced => false}}})
        ),
        "should support turning off code (fenced)"
    ),
    ok.

test_code_fenced_case_44(_Config) ->
    % ?assertMatch({ok, <<&Default::default()
    %     )?/utf8>>}, markdown:to_html(<<to_mdast(
    %         "```js extra\nconsole.log(1)\nconsole.log(2)\n```"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Code(Code {
    %             lang: Some("js".into(),
    ok.

test_code_fenced_case_45(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("```\nasd"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Code(Code {
    %             lang: None,
    %             meta: None,
    %             value: "asd".into(),
    ok.

test_code_fenced_case_46(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("```\rasd\r```"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Code(Code {
    %             lang: None,
    %             meta: None,
    %             value: "asd".into(),
    ok.

test_code_fenced_case_47(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("```\r\nasd\r\n```"/utf8>>), Node::Root(Root {
    %         children: vec![Node::Code(Code {
    %             lang: None,
    %             meta: None,
    %             value: "asd".into(),
    ok.
