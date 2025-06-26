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
-module(markdown_autolink_SUITE).
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
    test_autolink_case_1/1,
    test_autolink_case_2/1,
    test_autolink_case_3/1,
    test_autolink_case_4/1,
    test_autolink_case_5/1,
    test_autolink_case_6/1,
    test_autolink_case_7/1,
    test_autolink_case_8/1,
    test_autolink_case_9/1,
    test_autolink_case_10/1,
    test_autolink_case_11/1,
    test_autolink_case_12/1,
    test_autolink_case_13/1,
    test_autolink_case_14/1,
    test_autolink_case_15/1,
    test_autolink_case_16/1,
    test_autolink_case_17/1,
    test_autolink_case_18/1,
    test_autolink_case_19/1,
    test_autolink_case_20/1,
    test_autolink_case_21/1,
    test_autolink_case_22/1,
    test_autolink_case_23/1,
    test_autolink_case_24/1,
    test_autolink_case_25/1,
    test_autolink_case_26/1,
    test_autolink_case_27/1,
    test_autolink_case_28/1,
    test_autolink_case_29/1,
    test_autolink_case_30/1,
    test_autolink_case_31/1,
    test_autolink_case_32/1,
    test_autolink_case_33/1,
    test_autolink_case_34/1,
    test_autolink_case_35/1,
    test_autolink_case_36/1,
    test_autolink_case_37/1,
    test_autolink_case_38/1,
    test_autolink_case_39/1,
    test_autolink_case_40/1,
    test_autolink_case_41/1,
    test_autolink_case_42/1
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
            test_autolink_case_1,
            test_autolink_case_2,
            test_autolink_case_3,
            test_autolink_case_4,
            test_autolink_case_5,
            test_autolink_case_6,
            test_autolink_case_7,
            test_autolink_case_8,
            test_autolink_case_9,
            test_autolink_case_10,
            test_autolink_case_11,
            test_autolink_case_12,
            test_autolink_case_13,
            test_autolink_case_14,
            test_autolink_case_15,
            test_autolink_case_16,
            test_autolink_case_17,
            test_autolink_case_18,
            test_autolink_case_19,
            test_autolink_case_20,
            test_autolink_case_21,
            test_autolink_case_22,
            test_autolink_case_23,
            test_autolink_case_24,
            test_autolink_case_25,
            test_autolink_case_26,
            test_autolink_case_27,
            test_autolink_case_28,
            test_autolink_case_29,
            test_autolink_case_30,
            test_autolink_case_31,
            test_autolink_case_32,
            test_autolink_case_33,
            test_autolink_case_34,
            test_autolink_case_35,
            test_autolink_case_36,
            test_autolink_case_37,
            test_autolink_case_38,
            test_autolink_case_39,
            test_autolink_case_40,
            test_autolink_case_41,
            test_autolink_case_42
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

test_autolink_case_1(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>"/utf8>>},
        markdown:to_html(<<"<http://foo.bar.baz>"/utf8>>),
        "should support protocol autolinks (1)"
    ),
    ok.

test_autolink_case_2(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p><a href=\"http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>"/utf8>>},
        markdown:to_html(<<"<http://foo.bar.baz/test?q=hello&id=22&boolean>"/utf8>>),
        "should support protocol autolinks (2)"
    ),
    ok.

test_autolink_case_3(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>"/utf8>>},
        markdown:to_html(<<"<irc://foo.bar:2233/baz>"/utf8>>),
        "should support protocol autolinks w/ non-HTTP schemes"
    ),
    ok.

test_autolink_case_4(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>"/utf8>>},
        markdown:to_html(<<"<MAILTO:FOO@BAR.BAZ>"/utf8>>),
        "should support protocol autolinks in uppercase"
    ),
    ok.

test_autolink_case_5(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"\">a+b+c:d</a></p>"/utf8>>},
        markdown:to_html(<<"<a+b+c:d>"/utf8>>),
        "should support protocol autolinks w/ incorrect URIs (1, default)"
    ),
    ok.

test_autolink_case_6(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"a+b+c:d\">a+b+c:d</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"<a+b+c:d>"/utf8>>, ?DANGER()),
        "should support protocol autolinks w/ incorrect URIs (1, danger)"
    ),
    ok.

test_autolink_case_7(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"\">made-up-scheme://foo,bar</a></p>"/utf8>>},
        markdown:to_html(<<"<made-up-scheme://foo,bar>"/utf8>>),
        "should support protocol autolinks w/ incorrect URIs (2, default)"
    ),
    ok.

test_autolink_case_8(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"made-up-scheme://foo,bar\">made-up-scheme://foo,bar</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"<made-up-scheme://foo,bar>"/utf8>>, ?DANGER()),
        "should support protocol autolinks w/ incorrect URIs (2, danger)"
    ),
    ok.

test_autolink_case_9(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"http://../\">http://../</a></p>"/utf8>>},
        markdown:to_html(<<"<http://../>"/utf8>>),
        "should support protocol autolinks w/ incorrect URIs (3)"
    ),
    ok.

test_autolink_case_10(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>"/utf8>>},
        markdown:to_html_with_options(<<"<localhost:5001/foo>"/utf8>>, ?DANGER()),
        "should support protocol autolinks w/ incorrect URIs (4)"
    ),
    ok.

test_autolink_case_11(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;http://foo.bar/baz bim&gt;</p>"/utf8>>},
        markdown:to_html(<<"<http://foo.bar/baz bim>"/utf8>>),
        "should not support protocol autolinks w/ spaces"
    ),
    ok.

test_autolink_case_12(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"http://example.com/%5C%5B%5C\">http://example.com/\\[\\</a></p>"/utf8>>},
        markdown:to_html(<<"<http://example.com/\\[\\>"/utf8>>),
        "should not support character escapes in protocol autolinks"
    ),
    ok.

test_autolink_case_13(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>"/utf8>>},
        markdown:to_html(<<"<foo@bar.example.com>"/utf8>>),
        "should support email autolinks (1)"
    ),
    ok.

test_autolink_case_14(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>"/utf8>>},
        markdown:to_html(<<"<foo+special@Bar.baz-bar0.com>"/utf8>>),
        "should support email autolinks (2)"
    ),
    ok.

test_autolink_case_15(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:a@b.c\">a@b.c</a></p>"/utf8>>},
        markdown:to_html(<<"<a@b.c>"/utf8>>),
        "should support email autolinks (3)"
    ),
    ok.

test_autolink_case_16(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;foo+@bar.example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<foo\\+@bar.example.com>"/utf8>>),
        "should not support character escapes in email autolinks"
    ),
    ok.

test_autolink_case_17(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;&gt;</p>"/utf8>>}, markdown:to_html(<<"<>"/utf8>>), "should not support empty autolinks"
    ),
    ok.

test_autolink_case_18(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt; http://foo.bar &gt;</p>"/utf8>>},
        markdown:to_html(<<"< http://foo.bar >"/utf8>>),
        "should not support autolinks w/ space"
    ),
    ok.

test_autolink_case_19(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;m:abc&gt;</p>"/utf8>>},
        markdown:to_html(<<"<m:abc>"/utf8>>),
        "should not support autolinks w/ a single character for a scheme"
    ),
    ok.

test_autolink_case_20(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;foo.bar.baz&gt;</p>"/utf8>>},
        markdown:to_html(<<"<foo.bar.baz>"/utf8>>),
        "should not support autolinks w/o a colon or at sign"
    ),
    ok.

test_autolink_case_21(_Config) ->
    ?assertMatch(
        {ok, <<"<p>http://example.com</p>"/utf8>>},
        markdown:to_html(<<"http://example.com"/utf8>>),
        "should not support protocol autolinks w/o angle brackets"
    ),
    ok.

test_autolink_case_22(_Config) ->
    ?assertMatch(
        {ok, <<"<p>foo@bar.example.com</p>"/utf8>>},
        markdown:to_html(<<"foo@bar.example.com"/utf8>>),
        "should not support email autolinks w/o angle brackets"
    ),
    ok.

test_autolink_case_23(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:*@example.com\">*@example.com</a></p>"/utf8>>},
        markdown:to_html(<<"<*@example.com>"/utf8>>),
        "should support autolinks w/ atext (1)"
    ),
    ok.

test_autolink_case_24(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:a*@example.com\">a*@example.com</a></p>"/utf8>>},
        markdown:to_html(<<"<a*@example.com>"/utf8>>),
        "should support autolinks w/ atext (2)"
    ),
    ok.

test_autolink_case_25(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:aa*@example.com\">aa*@example.com</a></p>"/utf8>>},
        markdown:to_html(<<"<aa*@example.com>"/utf8>>),
        "should support autolinks w/ atext (3)"
    ),
    ok.

test_autolink_case_26(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;aaa©@example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<aaa©@example.com>"/utf8>>),
        "should support non-atext in email autolinks local part (1)"
    ),
    ok.

test_autolink_case_27(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;a*a©@example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<a*a©@example.com>"/utf8>>),
        "should support non-atext in email autolinks local part (2)"
    ),
    ok.

test_autolink_case_28(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@.example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@.example.com>"/utf8>>),
        "should not support a dot after an at sign in email autolinks"
    ),
    ok.

test_autolink_case_29(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@e..xample.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@e..xample.com>"/utf8>>),
        "should not support a dot after another dot in email autolinks"
    ),
    ok.

test_autolink_case_30(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p><a href=\"mailto:asd@012345678901234567890123456789012345678901234567890123456789012\">asd@012345678901234567890123456789012345678901234567890123456789012</a></p>"/utf8>>},
        markdown:to_html(<<"<asd@012345678901234567890123456789012345678901234567890123456789012>"/utf8>>),
        "should support 63 character in email autolinks domains"
    ),
    ok.

test_autolink_case_31(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@0123456789012345678901234567890123456789012345678901234567890123&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@0123456789012345678901234567890123456789012345678901234567890123>"/utf8>>),
        "should not support 64 character in email autolinks domains"
    ),
    ok.

test_autolink_case_32(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p><a href=\"mailto:asd@012345678901234567890123456789012345678901234567890123456789012.a\">asd@012345678901234567890123456789012345678901234567890123456789012.a</a></p>"/utf8>>},
        markdown:to_html(<<"<asd@012345678901234567890123456789012345678901234567890123456789012.a>"/utf8>>),
        "should support a TLD after a 63 character domain in email autolinks"
    ),
    ok.

test_autolink_case_33(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@0123456789012345678901234567890123456789012345678901234567890123.a&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@0123456789012345678901234567890123456789012345678901234567890123.a>"/utf8>>),
        "should not support a TLD after a 64 character domain in email autolinks"
    ),
    ok.

test_autolink_case_34(_Config) ->
    ?assertMatch(
        {ok,
            <<"<p><a href=\"mailto:asd@a.012345678901234567890123456789012345678901234567890123456789012\">asd@a.012345678901234567890123456789012345678901234567890123456789012</a></p>"/utf8>>},
        markdown:to_html(<<"<asd@a.012345678901234567890123456789012345678901234567890123456789012>"/utf8>>),
        "should support a 63 character TLD in email autolinks"
    ),
    ok.

test_autolink_case_35(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@a.0123456789012345678901234567890123456789012345678901234567890123&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@a.0123456789012345678901234567890123456789012345678901234567890123>"/utf8>>),
        "should not support a 64 character TLD in email autolinks"
    ),
    ok.

test_autolink_case_36(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@-example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@-example.com>"/utf8>>),
        "should not support a dash after `@` in email autolinks"
    ),
    ok.

test_autolink_case_37(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:asd@e-xample.com\">asd@e-xample.com</a></p>"/utf8>>},
        markdown:to_html(<<"<asd@e-xample.com>"/utf8>>),
        "should support a dash after other domain characters in email autolinks"
    ),
    ok.

test_autolink_case_38(_Config) ->
    ?assertMatch(
        {ok, <<"<p><a href=\"mailto:asd@e--xample.com\">asd@e--xample.com</a></p>"/utf8>>},
        markdown:to_html(<<"<asd@e--xample.com>"/utf8>>),
        "should support a dash after another dash in email autolinks"
    ),
    ok.

test_autolink_case_39(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;asd@example-.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<asd@example-.com>"/utf8>>),
        "should not support a dash before a dot in email autolinks"
    ),
    ok.

test_autolink_case_40(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;@example.com&gt;</p>"/utf8>>},
        markdown:to_html(<<"<@example.com>"/utf8>>),
        "should not support an at sign at the start of email autolinks"
    ),
    ok.

test_autolink_case_41(_Config) ->
    ?assertMatch(
        {ok, <<"<p>&lt;a@b.co&gt;</p>"/utf8>>},
        markdown:to_html_with_options(
            <<"<a@b.co>"/utf8>>, markdown_options:new(#{parse => #{constructs => #{autolink => false}}})
        ),
        "should support turning off autolinks"
    ),
    ok.

test_autolink_case_42(_Config) ->
    % ?assertMatch({ok, <<&Default::default()
    %     )?/utf8>>}, markdown:to_html(<<to_mdast(
    %         "a <https://alpha.com> b <bravo@charlie.com> c."/utf8>>), Node::Root(Root {
    %         children: vec![Node::Paragraph(Paragraph {
    %             children: vec![
    %                 Node::Text(Text {
    %                     value: "a ".into(),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
