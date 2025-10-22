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
-module(markdown_character_reference_SUITE).
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
    test_character_reference_case_1/1,
    test_character_reference_case_2/1,
    test_character_reference_case_3/1,
    test_character_reference_case_4/1,
    test_character_reference_case_5/1,
    test_character_reference_case_6/1,
    test_character_reference_case_7/1,
    test_character_reference_case_8/1,
    test_character_reference_case_9/1,
    test_character_reference_case_10/1,
    test_character_reference_case_11/1,
    test_character_reference_case_12/1,
    test_character_reference_case_13/1,
    test_character_reference_case_14/1,
    test_character_reference_case_15/1,
    test_character_reference_case_16/1,
    test_character_reference_case_17/1,
    test_character_reference_case_18/1,
    test_character_reference_case_19/1,
    test_character_reference_case_20/1,
    test_character_reference_case_21/1,
    test_character_reference_case_22/1,
    test_character_reference_case_23/1,
    test_character_reference_case_24/1,
    test_character_reference_case_25/1,
    test_character_reference_case_26/1,
    test_character_reference_case_27/1,
    test_character_reference_case_28/1,
    test_character_reference_case_29/1,
    test_character_reference_case_30/1,
    test_character_reference_case_31/1
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
            test_character_reference_case_1,
            test_character_reference_case_2,
            test_character_reference_case_3,
            test_character_reference_case_4,
            test_character_reference_case_5,
            test_character_reference_case_6,
            test_character_reference_case_7,
            test_character_reference_case_8,
            test_character_reference_case_9,
            test_character_reference_case_10,
            test_character_reference_case_11,
            test_character_reference_case_12,
            test_character_reference_case_13,
            test_character_reference_case_14,
            test_character_reference_case_15,
            test_character_reference_case_16,
            test_character_reference_case_17,
            test_character_reference_case_18,
            test_character_reference_case_19,
            test_character_reference_case_20,
            test_character_reference_case_21,
            test_character_reference_case_22,
            test_character_reference_case_23,
            test_character_reference_case_24,
            test_character_reference_case_25,
            test_character_reference_case_26,
            test_character_reference_case_27,
            test_character_reference_case_28,
            test_character_reference_case_29,
            test_character_reference_case_30,
            test_character_reference_case_31
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

test_character_reference_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p>\x{a0} &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>"/utf8>>},
        markdown:to_html(
            <<"&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;"/utf8>>
        ),
        "should support named character references"
    ),
    ok.

test_character_reference_case_2(_Config) ->
    ?assertMatch(
        {ok, <<"<p># Ӓ Ϡ �</p>"/utf8>>},
        markdown:to_html(<<"&#35; &#1234; &#992; &#0;"/utf8>>),
        "should support decimal character references"
    ),
    ok.

test_character_reference_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&quot; ആ ಫ</p>"/utf8>>},
        markdown:to_html(<<"&#X22; &#XD06; &#xcab;"/utf8>>),
        "should support hexadecimal character references"
    ),
    ok.

test_character_reference_case_4(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;\n&amp;#987654321;\n&amp;#abcdef0;\n&amp;ThisIsNotDefined; &amp;hi?;</p>"/utf8>>},
        markdown:to_html(<<"&nbsp &x; &#; &#x;\n&#987654321;\n&#abcdef0;\n&ThisIsNotDefined; &hi?;"/utf8>>),
        "should not support other things that look like character references"
    ),
    ok.

test_character_reference_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;copy</p>"/utf8>>},
        markdown:to_html(<<"&copy"/utf8>>),
        "should not support character references w/o semicolon"
    ),
    ok.

test_character_reference_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;MadeUpEntity;</p>"/utf8>>},
        markdown:to_html(<<"&MadeUpEntity;"/utf8>>),
        "should not support unknown named character references"
    ),
    ok.

test_character_reference_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<a href=\"&ouml;&ouml;.html\">"/utf8>>},
        markdown:to_html_with_options(<<"<a href=\"&ouml;&ouml;.html\">"/utf8>>, ?DANGER()),
        "should not care about character references in html"
    ),
    ok.

test_character_reference_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>"/utf8>>},
        markdown:to_html(<<"[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")"/utf8>>),
        "should support character references in resource URLs and titles"
    ),
    ok.

test_character_reference_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>"/utf8>>},
        markdown:to_html(<<"[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n\n[foo]"/utf8>>),
        "should support character references in definition URLs and titles"
    ),
    ok.

test_character_reference_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code class=\"language-föö\">foo\n</code></pre>"/utf8>>},
        markdown:to_html(<<"``` f&ouml;&ouml;\nfoo\n```"/utf8>>),
        "should support character references in code language"
    ),
    ok.

test_character_reference_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p><code>f&amp;ouml;&amp;ouml;</code></p>"/utf8>>},
        markdown:to_html(<<"`f&ouml;&ouml;`"/utf8>>),
        "should not support character references in text code"
    ),
    ok.

test_character_reference_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>"/utf8>>},
        markdown:to_html(<<"    f&ouml;f&ouml;"/utf8>>),
        "should not support character references in indented code"
    ),
    ok.

test_character_reference_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p>*foo*\n<em>foo</em></p>"/utf8>>},
        markdown:to_html(<<"&#42;foo&#42;\n*foo*"/utf8>>),
        "should not support character references as construct markers (1)"
    ),
    ok.

test_character_reference_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p>* foo</p>\n<ul>\n<li>foo</li>\n</ul>"/utf8>>},
        markdown:to_html(<<"&#42; foo\n\n* foo"/utf8>>),
        "should not support character references as construct markers (2)"
    ),
    ok.

test_character_reference_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<p>[a](url &quot;tit&quot;)</p>"/utf8>>},
        markdown:to_html(<<"[a](url &quot;tit&quot;)"/utf8>>),
        "should not support character references as construct markers (3)"
    ),
    ok.

test_character_reference_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo\n\nbar</p>"/utf8>>},
        markdown:to_html(<<"foo&#10;&#10;bar"/utf8>>),
        "should not support character references as whitespace (1)"
    ),
    ok.

test_character_reference_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p>\tfoo</p>"/utf8>>},
        markdown:to_html(<<"&#9;foo"/utf8>>),
        "should not support character references as whitespace (2)"
    ),
    ok.

test_character_reference_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<p>∳</p>"/utf8>>},
        markdown:to_html(<<"&CounterClockwiseContourIntegral;"/utf8>>),
        "should support the longest possible named character reference"
    ),
    ok.

test_character_reference_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<p>�</p>"/utf8>>},
        markdown:to_html(<<"&#xff9999;"/utf8>>),
        "should “support” a longest possible hexadecimal character reference"
    ),
    ok.

test_character_reference_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<p>�</p>"/utf8>>},
        markdown:to_html(<<"&#9999999;"/utf8>>),
        "should “support” a longest possible decimal character reference"
    ),
    ok.

test_character_reference_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;CounterClockwiseContourIntegrali;</p>"/utf8>>},
        markdown:to_html(<<"&CounterClockwiseContourIntegrali;"/utf8>>),
        "should not support the longest possible named character reference"
    ),
    ok.

test_character_reference_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#xff99999;</p>"/utf8>>},
        markdown:to_html(<<"&#xff99999;"/utf8>>),
        "should not support a longest possible hexadecimal character reference"
    ),
    ok.

test_character_reference_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#99999999;</p>"/utf8>>},
        markdown:to_html(<<"&#99999999;"/utf8>>),
        "should not support a longest possible decimal character reference"
    ),
    ok.

test_character_reference_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;-;</p>"/utf8>>},
        markdown:to_html(<<"&-;"/utf8>>),
        "should not support the other characters after `&`"
    ),
    ok.

test_character_reference_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#-;</p>"/utf8>>},
        markdown:to_html(<<"&#-;"/utf8>>),
        "should not support the other characters after `#`"
    ),
    ok.

test_character_reference_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#x-;</p>"/utf8>>},
        markdown:to_html(<<"&#x-;"/utf8>>),
        "should not support the other characters after `#x`"
    ),
    ok.

test_character_reference_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;lt-;</p>"/utf8>>},
        markdown:to_html(<<"&lt-;"/utf8>>),
        "should not support the other characters inside a name"
    ),
    ok.

test_character_reference_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#9-;</p>"/utf8>>},
        markdown:to_html(<<"&#9-;"/utf8>>),
        "should not support the other characters inside a demical"
    ),
    ok.

test_character_reference_case_29(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;#x9-;</p>"/utf8>>},
        markdown:to_html(<<"&#x9-;"/utf8>>),
        "should not support the other characters inside a hexademical"
    ),
    ok.

test_character_reference_case_30(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&amp;amp;</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"&amp;"/utf8>>, markdown_options:new(#{parse => #{constructs => #{character_reference => false}}})
        ),
        "should support turning off character references"
    ),
    ok.

test_character_reference_case_31(_Config) ->
    ?assertEqual(
        {ok,
            markdown_mdast_node:root(#markdown_mdast_root{
                children = ?'vec!'([
                    markdown_mdast_node:paragraph(#markdown_mdast_paragraph{
                        children = ?'vec!'([
                            markdown_mdast_node:text(#markdown_mdast_text{
                                value = <<"\x{a0} & © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸\n# Ӓ Ϡ �\n\" ആ ಫ"/utf8>>,
                                position = {some, markdown_unist_position:new(1, 1, 0, 5, 23, 158)}
                            })
                        ]),
                        position = {some, markdown_unist_position:new(1, 1, 0, 5, 23, 158)}
                    })
                ]),
                position = {some, markdown_unist_position:new(1, 1, 0, 5, 23, 158)}
            })},
        markdown:to_mdast(
            <<"&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n&#35; &#1234; &#992; &#0;\n&#X22; &#XD06; &#xcab;"/utf8>>,
            markdown_parse_options:default()
        ),
        "should support character references as `Text`s in mdast"
    ),
    ok.
