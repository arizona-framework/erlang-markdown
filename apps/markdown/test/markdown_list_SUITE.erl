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
-module(markdown_list_SUITE).
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
    test_list_case_1/1,
    test_list_case_2/1,
    test_list_case_3/1,
    test_list_case_4/1,
    test_list_case_5/1,
    test_list_case_6/1,
    test_list_case_7/1,
    test_list_case_8/1,
    test_list_case_9/1,
    test_list_case_10/1,
    test_list_case_11/1,
    test_list_case_12/1,
    test_list_case_13/1,
    test_list_case_14/1,
    test_list_case_15/1,
    test_list_case_16/1,
    test_list_case_17/1,
    test_list_case_18/1,
    test_list_case_19/1,
    test_list_case_20/1,
    test_list_case_21/1,
    test_list_case_22/1,
    test_list_case_23/1,
    test_list_case_24/1,
    test_list_case_25/1,
    test_list_case_26/1,
    test_list_case_27/1,
    test_list_case_28/1,
    test_list_case_29/1,
    test_list_case_30/1,
    test_list_case_31/1,
    test_list_case_32/1,
    test_list_case_33/1,
    test_list_case_34/1,
    test_list_case_35/1,
    test_list_case_36/1,
    test_list_case_37/1,
    test_list_case_38/1,
    test_list_case_39/1,
    test_list_case_40/1,
    test_list_case_41/1,
    test_list_case_42/1,
    test_list_case_43/1,
    test_list_case_44/1,
    test_list_case_45/1,
    test_list_case_46/1,
    test_list_case_47/1,
    test_list_case_48/1,
    test_list_case_49/1,
    test_list_case_50/1,
    test_list_case_51/1,
    test_list_case_52/1,
    test_list_case_53/1,
    test_list_case_54/1,
    test_list_case_55/1,
    test_list_case_56/1,
    test_list_case_57/1,
    test_list_case_58/1,
    test_list_case_59/1,
    test_list_case_60/1,
    test_list_case_61/1,
    test_list_case_62/1,
    test_list_case_63/1,
    test_list_case_64/1,
    test_list_case_65/1,
    test_list_case_66/1,
    test_list_case_67/1,
    test_list_case_68/1,
    test_list_case_69/1,
    test_list_case_70/1,
    test_list_case_71/1,
    test_list_case_72/1,
    test_list_case_73/1,
    test_list_case_74/1,
    test_list_case_75/1,
    test_list_case_76/1,
    test_list_case_77/1,
    test_list_case_78/1,
    test_list_case_79/1,
    test_list_case_80/1,
    test_list_case_81/1,
    test_list_case_82/1,
    test_list_case_83/1,
    test_list_case_84/1,
    test_list_case_85/1,
    test_list_case_86/1,
    test_list_case_87/1,
    test_list_case_88/1,
    test_list_case_89/1,
    test_list_case_90/1,
    test_list_case_91/1,
    test_list_case_92/1,
    test_list_case_93/1,
    test_list_case_94/1,
    test_list_case_95/1
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
            test_list_case_1,
            test_list_case_2,
            test_list_case_3,
            test_list_case_4,
            test_list_case_5,
            test_list_case_6,
            test_list_case_7,
            test_list_case_8,
            test_list_case_9,
            test_list_case_10,
            test_list_case_11,
            test_list_case_12,
            test_list_case_13,
            test_list_case_14,
            test_list_case_15,
            test_list_case_16,
            test_list_case_17,
            test_list_case_18,
            test_list_case_19,
            test_list_case_20,
            test_list_case_21,
            test_list_case_22,
            test_list_case_23,
            test_list_case_24,
            test_list_case_25,
            test_list_case_26,
            test_list_case_27,
            test_list_case_28,
            test_list_case_29,
            test_list_case_30,
            test_list_case_31,
            test_list_case_32,
            test_list_case_33,
            test_list_case_34,
            test_list_case_35,
            test_list_case_36,
            test_list_case_37,
            test_list_case_38,
            test_list_case_39,
            test_list_case_40,
            test_list_case_41,
            test_list_case_42,
            test_list_case_43,
            test_list_case_44,
            test_list_case_45,
            test_list_case_46,
            test_list_case_47,
            test_list_case_48,
            test_list_case_49,
            test_list_case_50,
            test_list_case_51,
            test_list_case_52,
            test_list_case_53,
            test_list_case_54,
            test_list_case_55,
            test_list_case_56,
            test_list_case_57,
            test_list_case_58,
            test_list_case_59,
            test_list_case_60,
            test_list_case_61,
            test_list_case_62,
            test_list_case_63,
            test_list_case_64,
            test_list_case_65,
            test_list_case_66,
            test_list_case_67,
            test_list_case_68,
            test_list_case_69,
            test_list_case_70,
            test_list_case_71,
            test_list_case_72,
            test_list_case_73,
            test_list_case_74,
            test_list_case_75,
            test_list_case_76,
            test_list_case_77,
            test_list_case_78,
            test_list_case_79,
            test_list_case_80,
            test_list_case_81,
            test_list_case_82,
            test_list_case_83,
            test_list_case_84,
            test_list_case_85,
            test_list_case_86,
            test_list_case_87,
            test_list_case_88,
            test_list_case_89,
            test_list_case_90,
            test_list_case_91,
            test_list_case_92,
            test_list_case_93,
            test_list_case_94,
            test_list_case_95
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

test_list_case_1(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote."/utf8>>),
        "should support documents"
    ),
    ok.

test_list_case_2(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>a\nb.</p>\n<pre><code>c\n</code></pre>\n<blockquote>\n<p>d.</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1.  a\n    b.\n\n        c\n\n    > d."/utf8>>),
        "should support documents in list items"
    ),
    ok.

test_list_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>one</li>\n</ul>\n<p>two</p>"/utf8>>},
        markdown:to_html(<<"- one\n\n two"/utf8>>),
        "should not support 1 space for a two-character list prefix"
    ),
    ok.

test_list_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n<p>b</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n\n  b"/utf8>>),
        "should support blank lines in list items"
    ),
    ok.

test_list_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>one</li>\n</ul>\n<pre><code> two\n</code></pre>"/utf8>>},
        markdown:to_html(<<" -    one\n\n     two"/utf8>>),
        "should support indented code after lists"
    ),
    ok.

test_list_case_6(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"   > > 1.  one\n>>\n>>     two"/utf8>>),
        "should support proper indent mixed w/ block quotes (1)"
    ),
    ok.

test_list_case_7(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<blockquote>\n<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n</blockquote>\n</blockquote>"/utf8>>},
        markdown:to_html(<<">>- one\n>>\n  >  > two"/utf8>>),
        "should support proper indent mixed w/ block quotes (2)"
    ),
    ok.

test_list_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p>-one</p>\n<p>2.two</p>"/utf8>>},
        markdown:to_html(<<"-one\n\n2.two"/utf8>>),
        "should not support a missing space after marker"
    ),
    ok.

test_list_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n\n\n  bar"/utf8>>),
        "should support multiple blank lines between items"
    ),
    ok.

test_list_case_10(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam"/utf8>>),
        "should support flow in items"
    ),
    ok.

test_list_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- Foo\n\n      bar\n\n\n      baz"/utf8>>),
        "should support blank lines in indented code in items"
    ),
    ok.

test_list_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"123456789\">\n<li>ok</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"123456789. ok"/utf8>>),
        "should support start on the first list item"
    ),
    ok.

test_list_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p>1234567890. not ok</p>"/utf8>>},
        markdown:to_html(<<"1234567890. not ok"/utf8>>),
        "should not support ordered item values over 10 digits"
    ),
    ok.

test_list_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"0\">\n<li>ok</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"0. ok"/utf8>>),
        "should support ordered item values of `0`"
    ),
    ok.

test_list_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"3\">\n<li>ok</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"003. ok"/utf8>>),
        "should support ordered item values starting w/ `0`s"
    ),
    ok.

test_list_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<p>-1. not ok</p>"/utf8>>},
        markdown:to_html(<<"-1. not ok"/utf8>>),
        "should not support “negative” ordered item values"
    ),
    ok.

test_list_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n\n      bar"/utf8>>),
        "should support indented code in list items (1)"
    ),
    ok.

test_list_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"  10.  foo\n\n           bar"/utf8>>),
        "should support indented code in list items (2)"
    ),
    ok.

test_list_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    indented code\n\nparagraph\n\n    more code"/utf8>>),
        "should support indented code in list items (3)"
    ),
    ok.

test_list_case_20(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1.     indented code\n\n   paragraph\n\n       more code"/utf8>>),
        "should support indented code in list items (4)"
    ),
    ok.

test_list_case_21(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1.      indented code\n\n   paragraph\n\n       more code"/utf8>>),
        "should support indented code in list items (5)"
    ),
    ok.

test_list_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo</p>\n<p>bar</p>"/utf8>>},
        markdown:to_html(<<"   foo\n\nbar"/utf8>>),
        "should support indented code in list items (6)"
    ),
    ok.

test_list_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n</ul>\n<p>bar</p>"/utf8>>},
        markdown:to_html(<<"-    foo\n\n  bar"/utf8>>),
        "should support indented code in list items (7)"
    ),
    ok.

test_list_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"-  foo\n\n   bar"/utf8>>),
        "should support indented code in list items (8)"
    ),
    ok.

test_list_case_25(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>foo</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz"/utf8>>),
        "should support blank first lines (1)"
    ),
    ok.

test_list_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"-   \n  foo"/utf8>>),
        "should support blank first lines (2)"
    ),
    ok.

test_list_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li></li>\n</ul>\n<p>foo</p>"/utf8>>},
        markdown:to_html(<<"-\n\n  foo"/utf8>>),
        "should support empty only items"
    ),
    ok.

test_list_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n-\n- bar"/utf8>>),
        "should support empty continued items"
    ),
    ok.

test_list_case_29(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n-   \n- bar"/utf8>>),
        "should support blank continued items"
    ),
    ok.

test_list_case_30(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1. foo\n2.\n3. bar"/utf8>>),
        "should support empty continued items (ordered)"
    ),
    ok.

test_list_case_31(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li></li>\n</ul>"/utf8>>}, markdown:to_html(<<"*"/utf8>>), "should support a single empty item"
    ),
    ok.

test_list_case_32(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo\n*</p>\n<p>foo\n1.</p>"/utf8>>},
        markdown:to_html(<<"foo\n*\n\nfoo\n1."/utf8>>),
        "should not support empty items to interrupt paragraphs"
    ),
    ok.

test_list_case_33(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(
            <<" 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote."/utf8>>
        ),
        "should support indenting w/ 1 space"
    ),
    ok.

test_list_case_34(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(
            <<"  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote."/utf8>>
        ),
        "should support indenting w/ 2 spaces"
    ),
    ok.

test_list_case_35(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(
            <<"   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote."/utf8>>
        ),
        "should support indenting w/ 3 spaces"
    ),
    ok.

test_list_case_36(_Config) ->
    ?assertMatch(
        {ok,
            <<"<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>"/utf8>>},
        markdown:to_html(
            <<"    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote."/utf8>>
        ),
        "should not support indenting w/ 4 spaces"
    ),
    ok.

test_list_case_37(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(
            <<"  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote."/utf8>>
        ),
        "should support lazy lines"
    ),
    ok.

test_list_case_38(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>A paragraph\nwith two lines.</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"  1.  A paragraph\n    with two lines."/utf8>>),
        "should support partially lazy lines"
    ),
    ok.

test_list_case_39(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> 1. > Blockquote\ncontinued here."/utf8>>),
        "should support lazy lines combined w/ other containers"
    ),
    ok.

test_list_case_40(_Config) ->
    ?assertMatch(
        {ok,
            <<"<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>"/utf8>>},
        markdown:to_html(<<"> 1. > Blockquote\n> continued here."/utf8>>),
        "should support partially continued, partially lazy lines combined w/ other containers"
    ),
    ok.

test_list_case_41(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>[\na</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- [\na"/utf8>>),
        "should support lazy, definition-like lines"
    ),
    ok.

test_list_case_42(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>c</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- [a]: b\nc"/utf8>>),
        "should support a definition, followed by a lazy paragraph"
    ),
    ok.

test_list_case_43(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz\n<ul>\n<li>boo</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n  - bar\n    - baz\n      - boo"/utf8>>),
        "should support sublists w/ enough spaces (1)"
    ),
    ok.

test_list_case_44(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n<li>boo</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n - bar\n  - baz\n   - boo"/utf8>>),
        "should not support sublists w/ too few spaces"
    ),
    ok.

test_list_case_45(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"10\">\n<li>foo\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"10) foo\n    - bar"/utf8>>),
        "should support sublists w/ enough spaces (2)"
    ),
    ok.

test_list_case_46(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"10\">\n<li>foo</li>\n</ol>\n<ul>\n<li>bar</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"10) foo\n   - bar"/utf8>>),
        "should not support sublists w/ too few spaces (2)"
    ),
    ok.

test_list_case_47(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<ul>\n<li>foo</li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- - foo"/utf8>>),
        "should support sublists (1)"
    ),
    ok.

test_list_case_48(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>foo</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1. - 2. foo"/utf8>>),
        "should support sublists (2)"
    ),
    ok.

test_list_case_49(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<h1>Foo</h1>\n</li>\n<li>\n<h2>Bar</h2>\nbaz</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- # Foo\n- Bar\n  ---\n  baz"/utf8>>),
        "should support headings in list items"
    ),
    ok.

test_list_case_50(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<ul>\n<li>baz</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n- bar\n+ baz"/utf8>>),
        "should support a new list by changing the marker (unordered)"
    ),
    ok.

test_list_case_51(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>foo</li>\n<li>bar</li>\n</ol>\n<ol start=\"3\">\n<li>baz</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1. foo\n2. bar\n3) baz"/utf8>>),
        "should support a new list by changing the marker (ordered)"
    ),
    ok.

test_list_case_52(_Config) ->
    ?assertMatch(
        {ok, <<"<p>Foo</p>\n<ul>\n<li>bar</li>\n<li>baz</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"Foo\n- bar\n- baz"/utf8>>),
        "should support interrupting a paragraph"
    ),
    ok.

test_list_case_53(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a\n2. b</p>"/utf8>>},
        markdown:to_html(<<"a\n2. b"/utf8>>),
        "should not support interrupting a paragraph with a non-1 numbered item"
    ),
    ok.

test_list_case_54(_Config) ->
    ?assertMatch(
        {ok, <<"<ol start=\"2\">\n<li>a</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"\n2. a"/utf8>>),
        "should “interrupt” a blank line (1)"
    ),
    ok.

test_list_case_55(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a</p>\n<ol start=\"2\">\n<li>b</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"a\n\n2. b"/utf8>>),
        "should “interrupt” a blank line (2)"
    ),
    ok.

test_list_case_56(_Config) ->
    ?assertMatch(
        {ok, <<"<p>a</p>\n<ol>\n<li>b</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"a\n1. b"/utf8>>),
        "should support interrupting a paragraph with a 1 numbered item"
    ),
    ok.

test_list_case_57(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n\n- bar\n\n\n- baz"/utf8>>),
        "should support blank lines between items (1)"
    ),
    ok.

test_list_case_58(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- foo\n  - bar\n    - baz\n\n\n      bim"/utf8>>),
        "should support blank lines between items (2)"
    ),
    ok.

test_list_case_59(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<!-- -->\n<ul>\n<li>baz</li>\n<li>bim</li>\n</ul>"/utf8>>},
        markdown:to_html_with_options(<<"- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim"/utf8>>, ?DANGER()),
        "should support HTML comments between lists"
    ),
    ok.

test_list_case_60(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<!-- -->\n<pre><code>code\n</code></pre>"/utf8>>},
        markdown:to_html_with_options(<<"-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code"/utf8>>, ?DANGER()),
        "should support HTML comments between lists and indented code"
    ),
    ok.

test_list_case_61(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n<li>e</li>\n<li>f</li>\n<li>g</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n - b\n  - c\n   - d\n  - e\n - f\n- g"/utf8>>),
        "should not support lists in lists w/ too few spaces (1)"
    ),
    ok.

test_list_case_62(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1. a\n\n  2. b\n\n   3. c"/utf8>>),
        "should not support lists in lists w/ too few spaces (2)"
    ),
    ok.

test_list_case_63(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d\n- e</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n - b\n  - c\n   - d\n    - e"/utf8>>),
        "should not support lists in lists w/ too few spaces (3)"
    ),
    ok.

test_list_case_64(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ol>\n<pre><code>3. c\n</code></pre>"/utf8>>},
        markdown:to_html(<<"1. a\n\n  2. b\n\n    3. c"/utf8>>),
        "should not support lists in lists w/ too few spaces (3)"
    ),
    ok.

test_list_case_65(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n- b\n\n- c"/utf8>>),
        "should support loose lists w/ a blank line between (1)"
    ),
    ok.

test_list_case_66(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>c</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* a\n*\n\n* c"/utf8>>),
        "should support loose lists w/ a blank line between (2)"
    ),
    ok.

test_list_case_67(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n- b\n\n  c\n- d"/utf8>>),
        "should support loose lists w/ a blank line in an item (1)"
    ),
    ok.

test_list_case_68(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n- b\n\n  [ref]: /url\n- d"/utf8>>),
        "should support loose lists w/ a blank line in an item (2)"
    ),
    ok.

test_list_case_69(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n- ```\n  b\n\n\n  ```\n- c"/utf8>>),
        "should support tight lists w/ a blank line in fenced code"
    ),
    ok.

test_list_case_70(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n  - b\n\n    c\n- d"/utf8>>),
        "should support tight lists w/ a blank line in a sublist"
    ),
    ok.

test_list_case_71(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>c</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* a\n  > b\n  >\n* c"/utf8>>),
        "should support tight lists w/ a blank line in a block quote"
    ),
    ok.

test_list_case_72(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>d</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n  > b\n  ```\n  c\n  ```\n- d"/utf8>>),
        "should support tight lists w/ flow w/o blank line"
    ),
    ok.

test_list_case_73(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a"/utf8>>),
        "should support tight lists w/ a single content"
    ),
    ok.

test_list_case_74(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n  - b"/utf8>>),
        "should support tight lists w/ a sublist"
    ),
    ok.

test_list_case_75(_Config) ->
    ?assertMatch(
        {ok, <<"<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>"/utf8>>},
        markdown:to_html(<<"1. ```\n   foo\n   ```\n\n   bar"/utf8>>),
        "should support loose lists w/ a blank line in an item"
    ),
    ok.

test_list_case_76(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n<p>baz</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* foo\n  * bar\n\n  baz"/utf8>>),
        "should support loose lists w/ tight sublists (1)"
    ),
    ok.

test_list_case_77(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>\n<p>a</p>\n<ul>\n<li>b</li>\n<li>c</li>\n</ul>\n</li>\n<li>\n<p>d</p>\n<ul>\n<li>e</li>\n<li>f</li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- a\n  - b\n  - c\n\n- d\n  - e\n  - f"/utf8>>),
        "should support loose lists w/ tight sublists (2)"
    ),
    ok.

test_list_case_78(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>b</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* a\n*\n\n  \n\t\n* b"/utf8>>),
        "should support continued list items after an empty list item w/ many blank lines"
    ),
    ok.

test_list_case_79(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code class=\"language-p\">\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"*\n  ~~~p\n\n  ~~~"/utf8>>),
        "should support blank lines in code after an initial blank line"
    ),
    ok.

test_list_case_80(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>a tight item that ends with an html element: <code>x</code></li>\n</ul>\n<p>Paragraph</p>"/utf8>>},
        markdown:to_html(<<"* a tight item that ends with an html element: `x`\n\nParagraph"/utf8>>),
        "should ignore line endings after tight items ending in tags"
    ),
    ok.

test_list_case_81(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<p>foo</p>\n</li>\n<li></li>\n<li>\n<p>bar</p>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"*   foo\n\n*\n\n*   bar"/utf8>>),
        "should support empty items in a spread list"
    ),
    ok.

test_list_case_82(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n\n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (0 space)"
    ),
    ok.

test_list_case_83(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n \n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (1 space)"
    ),
    ok.

test_list_case_84(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>\n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n  \n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (2 spaces)"
    ),
    ok.

test_list_case_85(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code> \n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n   \n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (3 spaces)"
    ),
    ok.

test_list_case_86(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>  \n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n    \n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (4 spaces)"
    ),
    ok.

test_list_case_87(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<pre><code>  \n</code></pre>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- ```\n\t\n  ```"/utf8>>),
        "should remove indent of code (fenced) in list (1 tab)"
    ),
    ok.

test_list_case_88(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<ul>\n<li></li>\n</ul>\n</li>\n<li></li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- +\n-"/utf8>>),
        "should support complex nested and empty lists (1)"
    ),
    ok.

test_list_case_89(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>\n<ol>\n<li></li>\n</ol>\n</li>\n<li></li>\n</ul>"/utf8>>},
        markdown:to_html(<<"- 1.\n-"/utf8>>),
        "should support complex nested and empty lists (2)"
    ),
    ok.

test_list_case_90(_Config) ->
    ?assertMatch(
        {ok,
            <<"<ul>\n<li>\n<ul>\n<li>\n<ul>\n<li></li>\n</ul>\n</li>\n</ul>\n</li>\n<li>\n<ul>\n<li></li>\n</ul>\n</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"* - +\n* -"/utf8>>),
        "should support complex nested and empty lists (3)"
    ),
    ok.

test_list_case_91(_Config) ->
    ?assertMatch(
        {ok, <<"<ul>\n<li>a</li>\n</ul>\n<!---->\n<ul>\n<li>b</li>\n</ul>"/utf8>>},
        markdown:to_html_with_options(<<"* a\n\n<!---->\n\n* b"/utf8>>, ?DANGER()),
        "should support the common list breaking comment method"
    ),
    ok.

test_list_case_92(_Config) ->
    ?assertMatch(
        {ok, <<"<p>- one</p>\n<p>two</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"- one\n\n two"/utf8>>, markdown_options:new(#{parse => #{constructs => #{list_item => false}}})
        ),
        "should support turning off lists"
    ),
    ok.

test_list_case_93(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("* a"/utf8>>), Node::Root(Root {
    %         children: vec![Node::List(List {
    %             ordered: false,
    %             spread: false,
    %             start: None,
    %             children: vec![Node::ListItem(ListItem {
    %                 checked: None,
    %                 spread: false,
    %                 children: vec![Node::Paragraph(Paragraph {
    %                     children: vec![Node::Text(Text {
    %                         value: "a".into(),
    ok.

test_list_case_94(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("3. a\n4. b"/utf8>>), Node::Root(Root {
    %         children: vec![Node::List(List {
    %             ordered: true,
    %             spread: false,
    %             start: Some(3),
    ok.

test_list_case_95(_Config) ->
    % ?assertMatch({ok, <<&Default::default())?/utf8>>}, markdown:to_html(<<to_mdast("* a\n\n  b\n* c"/utf8>>), Node::Root(Root {
    %         children: vec![Node::List(List {
    %             ordered: false,
    %             spread: false,
    %             start: None,
    %             children: vec![
    %                 Node::ListItem(ListItem {
    %                     checked: None,
    %                     spread: true,
    %                     children: vec![
    %                         Node::Paragraph(Paragraph {
    %                             children: vec![Node::Text(Text {
    %                                 value: "a".into(),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
