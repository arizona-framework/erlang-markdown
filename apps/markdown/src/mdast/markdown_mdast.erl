%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  26 Jun 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_mdast).
-moduledoc """
Turn events into a syntax tree.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile({no_auto_import, [exit/1]}).

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    compile/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Turn events and bytes into a syntax tree.
""".
-spec compile(Events, Bytes) -> {ok, Node} | {error, Message} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    Node :: markdown_mdast_node:t(),
    Message :: markdown_message:t().
compile(Events = #markdown_vec{}, Bytes) when is_binary(Bytes) ->
    CompileContext1 = markdown_mdast_compile_context:new(Events, Bytes),
    case handle_loop(0, CompileContext1) of
        {ok, CompileContext2 = #markdown_mdast_compile_context{trees = Trees2}} ->
            %% BEGIN: assertions
            ?assertEqual(1, markdown_vec:size(Trees2), "expected 1 final tree"),
            %% END: assertions
            {Trees3, {Tree, _, EventStack}} = markdown_vec:pop(Trees2),
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{trees = Trees3},
            case markdown_vec:last_option(EventStack) of
                none ->
                    {ok, Tree};
                {some, Index} ->
                    Event = markdown_vec:get(Events, Index),
                    markdown_mdast_compile_context:on_mismatch_error(CompileContext3, none, Event)
            end;
        Error = {error, _Message} ->
            Error
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Handle the event at `index`.
""".
-spec handle(CompileContext, Index) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(),
    Index :: markdown_vec:index(),
    Message :: markdown_message:t().
handle(CompileContext1 = #markdown_mdast_compile_context{events = Events}, Index) ->
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{index = Index},
    % io:format("~ts\n", [markdown_debug:rust_debug_string(markdown_vec:get(Events, Index))]),
    case markdown_vec:get(Events, Index) of
        #markdown_event{kind = 'enter'} ->
            enter(CompileContext2);
        #markdown_event{kind = 'exit'} ->
            exit(CompileContext2)
    end.

%% @private
-spec handle_loop(Index, CompileContext) ->
    {ok, CompileContext} | {error, Message}
when
    Index :: markdown_vec:index(),
    CompileContext :: markdown_mdast_compile_context:t(),
    Message :: markdown_message:t().
handle_loop(Index, CompileContext1 = #markdown_mdast_compile_context{events = Events}) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events)
->
    case handle(CompileContext1, Index) of
        {ok, CompileContext2} ->
            handle_loop(Index + 1, CompileContext2);
        Error = {error, _Message} ->
            Error
    end;
handle_loop(_Index, CompileContext = #markdown_mdast_compile_context{}) ->
    {ok, CompileContext}.

%% @private
-doc """
Format a JSX tag, ignoring its attributes.
""".
-spec serialize_abbreviated_tag(JsxTag) -> String when
    JsxTag :: markdown_mdast_jsx_tag:t(), String :: unicode:unicode_binary().
serialize_abbreviated_tag(#markdown_mdast_jsx_tag{close = Close, name = OptionName}) ->
    case {Close, OptionName} of
        {false, none} -> <<"<>">>;
        {false, {some, Name}} -> <<"<", Name/bytes, ">">>;
        {true, none} -> <<"</>">>;
        {true, {some, Name}} -> <<"</", Name/bytes, ">">>
    end.

%% @private
-doc """
Remove initial/final EOLs.
""".
-spec trim_eol(Bytes, AtStart, AtEnd) -> Bytes when Bytes :: binary(), AtStart :: boolean(), AtEnd :: boolean().
trim_eol(Bytes, AtStart, AtEnd) ->
    Start1 = 0,
    End1 = byte_size(Bytes),
    Start2 =
        case AtStart of
            false ->
                Start1;
            true ->
                case Bytes of
                    <<"\n", _/bytes>> ->
                        Start1 + 1;
                    <<"\r\n", _/bytes>> ->
                        Start1 + 2;
                    <<"\r", _/bytes>> ->
                        Start1 + 1;
                    _ ->
                        Start1
                end
        end,
    Skip1 = byte_size(Bytes) - 1,
    Skip2 = byte_size(Bytes) - 2,
    End2 =
        case AtEnd of
            false ->
                End1;
            true ->
                case Bytes of
                    <<_:Skip2/bytes, "\r\n">> ->
                        End1 - 2;
                    <<_:Skip1/bytes, "\n">> ->
                        End1 - 1;
                    <<_:Skip1/bytes, "\r">> ->
                        End1 - 1;
                    _ ->
                        End1
                end
        end,
    case Start2 > 0 orelse End2 < byte_size(Bytes) of
        false ->
            Bytes;
        true ->
            binary:part(Bytes, Start2, End2 - Start2)
    end.

%%%-----------------------------------------------------------------------------
%%% Internal enter functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Handle [`Enter`][Kind::Enter].
""".
-spec enter(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
enter(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Name = Event#markdown_event.name,
    case Name of
        %% on_enter_data
        autolink_email ->
            on_enter_data(CompileContext1);
        autolink_protocol ->
            on_enter_data(CompileContext1);
        character_escape_value ->
            on_enter_data(CompileContext1);
        character_reference ->
            on_enter_data(CompileContext1);
        code_flow_chunk ->
            on_enter_data(CompileContext1);
        code_text_data ->
            on_enter_data(CompileContext1);
        data ->
            on_enter_data(CompileContext1);
        frontmatter_chunk ->
            on_enter_data(CompileContext1);
        html_flow_data ->
            on_enter_data(CompileContext1);
        html_text_data ->
            on_enter_data(CompileContext1);
        math_flow_chunk ->
            on_enter_data(CompileContext1);
        math_text_data ->
            on_enter_data(CompileContext1);
        mdx_jsx_tag_attribute_value_literal_value ->
            on_enter_data(CompileContext1);
        %% on_enter_buffer
        code_fenced_fence_info ->
            on_enter_buffer(CompileContext1);
        code_fenced_fence_meta ->
            on_enter_buffer(CompileContext1);
        definition_destination_string ->
            on_enter_buffer(CompileContext1);
        definition_label_string ->
            on_enter_buffer(CompileContext1);
        definition_title_string ->
            on_enter_buffer(CompileContext1);
        gfm_footnote_definition_label_string ->
            on_enter_buffer(CompileContext1);
        label_text ->
            on_enter_buffer(CompileContext1);
        math_flow_fence_meta ->
            on_enter_buffer(CompileContext1);
        mdx_jsx_tag_attribute_value_literal ->
            on_enter_buffer(CompileContext1);
        reference_string ->
            on_enter_buffer(CompileContext1);
        resource_destination_string ->
            on_enter_buffer(CompileContext1);
        resource_title_string ->
            on_enter_buffer(CompileContext1);
        %% on_enter_autolink
        autolink ->
            on_enter_autolink(CompileContext1);
        %% on_enter_block_quote
        block_quote ->
            on_enter_block_quote(CompileContext1);
        %% on_enter_code_fenced
        code_fenced ->
            on_enter_code_fenced(CompileContext1);
        %% on_enter_code_indented
        code_indented ->
            on_enter_code_indented(CompileContext1);
        %% on_enter_code_text
        code_text ->
            on_enter_code_text(CompileContext1);
        %% on_enter_definition
        definition ->
            on_enter_definition(CompileContext1);
        %% on_enter_emphasis
        emphasis ->
            on_enter_emphasis(CompileContext1);
        %% on_enter_frontmatter
        frontmatter ->
            on_enter_frontmatter(CompileContext1);
        %% on_enter_gfm_autolink_literal
        gfm_autolink_literal_email ->
            on_enter_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_mailto ->
            on_enter_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_protocol ->
            on_enter_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_www ->
            on_enter_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_xmpp ->
            on_enter_gfm_autolink_literal(CompileContext1);
        %% on_enter_gfm_footnote_call
        gfm_footnote_call ->
            on_enter_gfm_footnote_call(CompileContext1);
        %% on_enter_gfm_footnote_definition
        gfm_footnote_definition ->
            on_enter_gfm_footnote_definition(CompileContext1);
        %% on_enter_gfm_strikethrough
        gfm_strikethrough ->
            on_enter_gfm_strikethrough(CompileContext1);
        %% on_enter_gfm_table
        gfm_table ->
            on_enter_gfm_table(CompileContext1);
        %% on_enter_gfm_table_row
        gfm_table_row ->
            on_enter_gfm_table_row(CompileContext1);
        %% on_enter_gfm_table_cell
        gfm_table_cell ->
            on_enter_gfm_table_cell(CompileContext1);
        %% on_enter_hard_break
        hard_break_escape ->
            on_enter_hard_break(CompileContext1);
        hard_break_trailing ->
            on_enter_hard_break(CompileContext1);
        %% on_enter_heading
        heading_atx ->
            on_enter_heading(CompileContext1);
        heading_setext ->
            on_enter_heading(CompileContext1);
        % %% on_enter_html
        % html_flow -> on_enter_html(CompileContext1);
        % html_text -> on_enter_html(CompileContext1);
        %% on_enter_image
        image ->
            on_enter_image(CompileContext1);
        %% on_enter_link
        link ->
            on_enter_link(CompileContext1);
        %% on_enter_list_item
        list_item ->
            on_enter_list_item(CompileContext1);
        %% on_enter_list
        list_ordered ->
            on_enter_list(CompileContext1);
        list_unordered ->
            on_enter_list(CompileContext1);
        %% on_enter_math_flow
        math_flow ->
            on_enter_math_flow(CompileContext1);
        %% on_enter_math_text
        math_text ->
            on_enter_math_text(CompileContext1);
        %% on_enter_mdx_esm
        mdx_esm ->
            on_enter_mdx_esm(CompileContext1);
        %% on_enter_mdx_flow_expression
        mdx_flow_expression ->
            on_enter_mdx_flow_expression(CompileContext1);
        %% on_enter_mdx_text_expression
        mdx_text_expression ->
            on_enter_mdx_text_expression(CompileContext1);
        %% on_enter_mdx_jsx_tag
        mdx_jsx_flow_tag ->
            on_enter_mdx_jsx_tag(CompileContext1);
        mdx_jsx_text_tag ->
            on_enter_mdx_jsx_tag(CompileContext1);
        %% on_enter_mdx_jsx_tag_closing_marker
        mdx_jsx_tag_closing_marker ->
            on_enter_mdx_jsx_tag_closing_marker(CompileContext1);
        %% on_enter_mdx_jsx_tag_attribute
        mdx_jsx_tag_attribute ->
            on_enter_mdx_jsx_tag_attribute(CompileContext1);
        %% on_enter_mdx_jsx_tag_attribute_expression
        mdx_jsx_tag_attribute_expression ->
            on_enter_mdx_jsx_tag_attribute_expression(CompileContext1);
        %% on_enter_mdx_jsx_tag_attribute_value_expression
        mdx_jsx_tag_attribute_value_expression ->
            on_enter_mdx_jsx_tag_attribute_value_expression(CompileContext1);
        %% on_enter_mdx_jsx_tag_self_closing_marker
        mdx_jsx_tag_self_closing_marker ->
            on_enter_mdx_jsx_tag_self_closing_marker(CompileContext1);
        %% on_enter_paragraph
        paragraph ->
            on_enter_paragraph(CompileContext1);
        %% on_enter_reference
        reference ->
            on_enter_reference(CompileContext1);
        %% on_enter_resource
        resource ->
            on_enter_resource(CompileContext1);
        %% on_enter_strong
        strong ->
            on_enter_strong(CompileContext1);
        %% on_enter_thematic_break
        thematic_break ->
            on_enter_thematic_break(CompileContext1);
        % %% otherwise
        _ ->
            io:format("~ts\n", [markdown_debug:rust_debug_string(Event)]),
            {ok, CompileContext1}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Autolink`][Name::Autolink].
""".
-spec on_enter_autolink(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_autolink(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:link(#markdown_mdast_link{
                url = <<>>,
                title = none,
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`BlockQuote`][Name::BlockQuote].
""".
-spec on_enter_block_quote(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_block_quote(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:blockquote(#markdown_mdast_blockquote{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:`*`.
""".
-spec on_enter_buffer(CompileContext) -> {ok, CompileContext} when CompileContext :: markdown_mdast_compile_context:t().
on_enter_buffer(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 = markdown_mdast_compile_context:buffer(CompileContext1),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`CodeFenced`][Name::CodeFenced].
""".
-spec on_enter_code_fenced(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_code_fenced(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:code(#markdown_mdast_code{
                lang = none,
                meta = none,
                value = <<>>,
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`CodeIndented`][Name::CodeIndented].
""".
-spec on_enter_code_indented(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_code_indented(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_enter_code_fenced(CompileContext1),
        {ok, CompileContext3} ?= on_enter_buffer(CompileContext2),
        {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`CodeText`][Name::CodeText].
""".
-spec on_enter_code_text(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_code_text(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:inline_code(#markdown_mdast_inline_code{
                value = <<>>,
                position = none
            })
        ),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Data`][Name::Data] (and many text things).
""".
-spec on_enter_data(CompileContext) -> {ok, CompileContext} when CompileContext :: markdown_mdast_compile_context:t().
on_enter_data(CompileContext1 = #markdown_mdast_compile_context{}) ->
    Node = markdown_mdast_compile_context:tail(CompileContext1),
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_mdast_node:children(Node), "expected parent"),
    %% END: assertions
    {some, Children} = markdown_mdast_node:children(Node),
    CompileContext2 =
        case markdown_vec:last_option(Children) of
            {some, #markdown_mdast_node{inner = #markdown_mdast_text{}}} ->
                markdown_mdast_compile_context:tail_push_again(CompileContext1);
            _ ->
                markdown_mdast_compile_context:tail_push(
                    CompileContext1,
                    markdown_mdast_node:text(#markdown_mdast_text{
                        value = <<>>,
                        position = none
                    })
                )
        end,
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Definition`][Name::Definition].
""".
-spec on_enter_definition(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_definition(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:definition(#markdown_mdast_definition{
                url = <<>>,
                identifier = <<>>,
                label = none,
                title = none,
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Emphasis`][Name::Emphasis].
""".
-spec on_enter_emphasis(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_emphasis(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:emphasis(#markdown_mdast_emphasis{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Frontmatter`][Name::Frontmatter].
""".
-spec on_enter_frontmatter(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_frontmatter(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index, bytes = Bytes}) ->
    Event = markdown_vec:get(Events, Index),
    ByteIndex = Event#markdown_event.point#markdown_point.offset,
    Byte = binary:at(Bytes, ByteIndex),
    Node =
        case Byte of
            $+ ->
                markdown_mdast_node:toml(#markdown_mdast_toml{
                    value = <<>>,
                    position = none
                });
            _ ->
                markdown_mdast_node:yaml(#markdown_mdast_yaml{
                    value = <<>>,
                    position = none
                })
        end,
    CompileContext2 = markdown_mdast_compile_context:tail_push(CompileContext1, Node),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`GfmAutolinkLiteralEmail`][Name::GfmAutolinkLiteralEmail],[`GfmAutolinkLiteralMailto`][Name::GfmAutolinkLiteralMailto],[`GfmAutolinkLiteralProtocol`][Name::GfmAutolinkLiteralProtocol],[`GfmAutolinkLiteralWww`][Name::GfmAutolinkLiteralWww],[`GfmAutolinkLiteralXmpp`][Name::GfmAutolinkLiteralXmpp]}.
""".
-spec on_enter_gfm_autolink_literal(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_autolink_literal(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_enter_autolink(CompileContext1),
        {ok, CompileContext3} ?= on_enter_data(CompileContext2),
        {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmFootnoteCall`][Name::GfmFootnoteCall].
""".
-spec on_enter_gfm_footnote_call(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_footnote_call(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:footnote_reference(#markdown_mdast_footnote_reference{
                identifier = <<>>,
                label = none,
                position = none
            })
        ),
    MediaReferenceStack2 = CompileContext2#markdown_mdast_compile_context.media_reference_stack,
    MediaReferenceStack3 = markdown_vec:push(MediaReferenceStack2, markdown_mdast_reference:default()),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack3},
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmFootnoteDefinition`][Name::GfmFootnoteDefinition].
""".
-spec on_enter_gfm_footnote_definition(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_footnote_definition(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:footnote_definition(#markdown_mdast_footnote_definition{
                identifier = <<>>,
                label = none,
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmStrikethrough`][Name::GfmStrikethrough].
""".
-spec on_enter_gfm_strikethrough(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_strikethrough(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:delete(#markdown_mdast_delete{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTable`][Name::GfmTable].
""".
-spec on_enter_gfm_table(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_table(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Align = markdown_util_infer:gfm_table_align(Events, Index),
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:table(#markdown_mdast_table{
                align = Align,
                children = markdown_vec:new(),
                position = none
            })
        ),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{gfm_table_inside = true},
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableRow`][Name::GfmTableRow].
""".
-spec on_enter_gfm_table_row(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_table_row(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:table_row(#markdown_mdast_table_row{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableCell`][Name::GfmTableCell].
""".
-spec on_enter_gfm_table_cell(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_gfm_table_cell(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:table_cell(#markdown_mdast_table_cell{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`HardBreakEscape`][Name::HardBreakEscape].
""".
-spec on_enter_hard_break(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_hard_break(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:break(#markdown_mdast_break{
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`HeadingAtx`][Name::HeadingAtx].
""".
-spec on_enter_heading(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_heading(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:heading(#markdown_mdast_heading{
                % Will be set later.
                depth = 0,
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Image`][Name::Image].
""".
-spec on_enter_image(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_image(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:image(#markdown_mdast_image{
                url = <<>>,
                title = none,
                alt = <<>>,
                position = none
            })
        ),
    MediaReferenceStack2 = CompileContext2#markdown_mdast_compile_context.media_reference_stack,
    MediaReferenceStack3 = markdown_vec:push(MediaReferenceStack2, markdown_mdast_reference:default()),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack3},
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Link`][Name::Link].
""".
-spec on_enter_link(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_link(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:link(#markdown_mdast_link{
                url = <<>>,
                title = none,
                children = markdown_vec:new(),
                position = none
            })
        ),
    MediaReferenceStack2 = CompileContext2#markdown_mdast_compile_context.media_reference_stack,
    MediaReferenceStack3 = markdown_vec:push(MediaReferenceStack2, markdown_mdast_reference:default()),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack3},
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`ListOrdered`][Name::ListOrdered],[`ListUnordered`][Name::ListUnordered]}.
""".
-spec on_enter_list(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_list(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Ordered = Event#markdown_event.name =:= list_ordered,
    Spread = markdown_util_infer:list_loose(Events, Index, false),
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:list(#markdown_mdast_list{
                ordered = Ordered,
                spread = Spread,
                start = none,
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`ListItem`][Name::ListItem].
""".
-spec on_enter_list_item(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_list_item(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Spread = markdown_util_infer:list_item_loose(Events, Index),
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:list_item(#markdown_mdast_list_item{
                spread = Spread,
                checked = none,
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MathFlow`][Name::MathFlow].
""".
-spec on_enter_math_flow(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_math_flow(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:math(#markdown_mdast_math{
                meta = none,
                value = <<>>,
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MathText`][Name::MathText].
""".
-spec on_enter_math_text(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_math_text(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:inline_math(#markdown_mdast_inline_math{
                value = <<>>,
                position = none
            })
        ),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxEsm`][Name::MdxEsm].
""".
-spec on_enter_mdx_esm(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_mdx_esm(CompileContext1 = #markdown_mdast_compile_context{events = Events, bytes = Bytes, index = Index}) ->
    Result = markdown_mdx_collect:collect(
        Events,
        Bytes,
        Index,
        [mdx_esm_data, line_ending],
        [mdx_esm]
    ),
    StopMapFun = fun(_Index, #markdown_stop{relative = Relative, absolute = Absolute}) ->
        markdown_mdast_stop:new(Relative, Absolute)
    end,
    Stops = markdown_vec:map(Result#markdown_mdx_collect_result.stops, StopMapFun),
    CompileContext2 = markdown_mdast_compile_context:tail_push(
        CompileContext1,
        markdown_mdast_node:mdxjs_esm(#markdown_mdast_mdxjs_esm{
            value = Result#markdown_mdx_collect_result.value,
            position = none,
            stops = Stops
        })
    ),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxFlowExpression`][Name::MdxFlowExpression].
""".
-spec on_enter_mdx_flow_expression(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_mdx_flow_expression(
    CompileContext1 = #markdown_mdast_compile_context{events = Events, bytes = Bytes, index = Index}
) ->
    Result = markdown_mdx_collect:collect(
        Events,
        Bytes,
        Index,
        [mdx_expression_data, line_ending],
        [mdx_flow_expression]
    ),
    StopMapFun = fun(_Index, #markdown_stop{relative = Relative, absolute = Absolute}) ->
        markdown_mdast_stop:new(Relative, Absolute)
    end,
    Stops = markdown_vec:map(Result#markdown_mdx_collect_result.stops, StopMapFun),
    CompileContext2 = markdown_mdast_compile_context:tail_push(
        CompileContext1,
        markdown_mdast_node:mdx_flow_expression(#markdown_mdast_mdx_flow_expression{
            value = Result#markdown_mdx_collect_result.value,
            position = none,
            stops = Stops
        })
    ),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`MdxJsxFlowTag`][Name::MdxJsxFlowTag],[`MdxJsxTextTag`][Name::MdxJsxTextTag]}.
""".
-spec on_enter_mdx_jsx_tag(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_mdx_jsx_tag(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Point = markdown_point:to_unist(Event#markdown_event.point),
    JsxTag = markdown_mdast_jsx_tag:new(none, markdown_vec:new(), false, false, Point, Point),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag}},
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`MdxJsxTagAttribute`][Name::MdxJsxTagAttribute],[`MdxJsxTagAttributeExpression`][Name::MdxJsxTagAttributeExpression]}.
""".
-spec on_enter_mdx_jsx_tag_any_attribute(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_enter_mdx_jsx_tag_any_attribute(
    CompileContext1 = #markdown_mdast_compile_context{jsx_tag = {some, JsxTag}, events = Events, index = Index}
) ->
    case JsxTag#markdown_mdast_jsx_tag.close of
        true ->
            Event = markdown_vec:get(Events, Index),
            Point = markdown_point:to_unist(Event#markdown_event.point),
            Place = markdown_place:point(Point),
            Message = markdown_message:new(
                {some, Place},
                <<"Unexpected attribute in closing tag, expected the end of the tag"/utf8>>,
                <<"unexpected-attribute"/utf8>>,
                <<"erlang-markdown"/utf8>>
            ),
            {error, Message};
        false ->
            {ok, CompileContext1}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxJsxTagAttribute`][Name::MdxJsxTagAttribute].
""".
-spec on_enter_mdx_jsx_tag_attribute(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_enter_mdx_jsx_tag_attribute(CompileContext1 = #markdown_mdast_compile_context{}) ->
    case on_enter_mdx_jsx_tag_any_attribute(CompileContext1) of
        {error, Message} ->
            {error, Message};
        {ok, CompileContext2} ->
            {some, JsxTag1} = CompileContext2#markdown_mdast_compile_context.jsx_tag,
            Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
            Attribute = markdown_mdast_mdx_jsx_attribute:new(<<>>, none),
            AttributeContent = markdown_mdast_attribute_content:property(Attribute),
            Attributes2 = markdown_vec:push(Attributes1, AttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxJsxTagAttributeExpression`][Name::MdxJsxTagAttributeExpression].
""".
-spec on_enter_mdx_jsx_tag_attribute_expression(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_enter_mdx_jsx_tag_attribute_expression(
    CompileContext1 = #markdown_mdast_compile_context{events = Events, bytes = Bytes, index = Index}
) ->
    case on_enter_mdx_jsx_tag_any_attribute(CompileContext1) of
        {error, Message} ->
            {error, Message};
        {ok, CompileContext2} ->
            Result = markdown_mdx_collect:collect(
                Events,
                Bytes,
                Index,
                [mdx_expression_data, line_ending],
                [mdx_jsx_tag_attribute_expression]
            ),
            StopMapFun = fun(_Index, #markdown_stop{relative = Relative, absolute = Absolute}) ->
                markdown_mdast_stop:new(Relative, Absolute)
            end,
            Stops = markdown_vec:map(Result#markdown_mdx_collect_result.stops, StopMapFun),
            {some, JsxTag1} = CompileContext2#markdown_mdast_compile_context.jsx_tag,
            Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
            ExpressionAttribute = markdown_mdast_mdx_jsx_expression_attribute:new(
                Result#markdown_mdx_collect_result.value, Stops
            ),
            ExpressionAttributeContent = markdown_mdast_attribute_content:expression(ExpressionAttribute),
            Attributes2 = markdown_vec:push(Attributes1, ExpressionAttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            CompileContext4 = markdown_mdast_compile_context:buffer(CompileContext3),
            {ok, CompileContext4}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxJsxTagAttributeValueExpression`][Name::MdxJsxTagAttributeValueExpression].
""".
-spec on_enter_mdx_jsx_tag_attribute_value_expression(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_mdx_jsx_tag_attribute_value_expression(
    CompileContext1 = #markdown_mdast_compile_context{events = Events, bytes = Bytes, index = Index}
) ->
    Result = markdown_mdx_collect:collect(
        Events,
        Bytes,
        Index,
        [mdx_expression_data, line_ending],
        [mdx_jsx_tag_attribute_value_expression]
    ),
    StopMapFun = fun(_Index, #markdown_stop{relative = Relative, absolute = Absolute}) ->
        markdown_mdast_stop:new(Relative, Absolute)
    end,
    Stops = markdown_vec:map(Result#markdown_mdx_collect_result.stops, StopMapFun),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
    {some, LastAttribute} = markdown_vec:last_option(Attributes1),
    case LastAttribute of
        #markdown_mdast_attribute_content{inner = {property, Attribute1 = #markdown_mdast_mdx_jsx_attribute{}}} ->
            ValueExpression = markdown_mdast_attribute_value_expression:new(
                Result#markdown_mdx_collect_result.value, Stops
            ),
            AttributeValue = markdown_mdast_attribute_value:expression(ValueExpression),
            Attribute2 = Attribute1#markdown_mdast_mdx_jsx_attribute{value = {some, AttributeValue}},
            AttributeContent = markdown_mdast_attribute_content:property(Attribute2),
            Attributes2 = markdown_vec:set_last(Attributes1, AttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
            {ok, CompileContext3};
        _ ->
            ?'unreachable!'("expected property", [])
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxJsxTagClosingMarker`][Name::MdxJsxTagClosingMarker].
""".
-spec on_enter_mdx_jsx_tag_closing_marker(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_enter_mdx_jsx_tag_closing_marker(
    CompileContext1 = #markdown_mdast_compile_context{jsx_tag_stack = JsxTagStack, events = Events, index = Index}
) ->
    case markdown_vec:is_empty(JsxTagStack) of
        true ->
            Event = markdown_vec:get(Events, Index),
            Point = markdown_point:to_unist(Event#markdown_event.point),
            Place = markdown_place:point(Point),
            Message = markdown_message:new(
                {some, Place},
                <<"Unexpected closing slash `/` in tag, expected an open tag first"/utf8>>,
                <<"unexpected-closing-slash"/utf8>>,
                <<"erlang-markdown"/utf8>>
            ),
            {error, Message};
        false ->
            {ok, CompileContext1}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxJsxTagSelfClosingMarker`][Name::MdxJsxTagSelfClosingMarker].
""".
-spec on_enter_mdx_jsx_tag_self_closing_marker(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_enter_mdx_jsx_tag_self_closing_marker(
    CompileContext1 = #markdown_mdast_compile_context{jsx_tag = {some, JsxTag}, events = Events, index = Index}
) ->
    case JsxTag#markdown_mdast_jsx_tag.close of
        true ->
            Event = markdown_vec:get(Events, Index),
            Point = markdown_point:to_unist(Event#markdown_event.point),
            Place = markdown_place:point(Point),
            Message = markdown_message:new(
                {some, Place},
                <<"Unexpected self-closing slash `/` in closing tag, expected the end of the tag"/utf8>>,
                <<"unexpected-self-closing-slash"/utf8>>,
                <<"erlang-markdown"/utf8>>
            ),
            {error, Message};
        false ->
            {ok, CompileContext1}
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`MdxTextExpression`][Name::MdxTextExpression].
""".
-spec on_enter_mdx_text_expression(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_mdx_text_expression(
    CompileContext1 = #markdown_mdast_compile_context{events = Events, bytes = Bytes, index = Index}
) ->
    Result = markdown_mdx_collect:collect(
        Events,
        Bytes,
        Index,
        [mdx_expression_data, line_ending],
        [mdx_text_expression]
    ),
    StopMapFun = fun(_Index, #markdown_stop{relative = Relative, absolute = Absolute}) ->
        markdown_mdast_stop:new(Relative, Absolute)
    end,
    Stops = markdown_vec:map(Result#markdown_mdx_collect_result.stops, StopMapFun),
    CompileContext2 = markdown_mdast_compile_context:tail_push(
        CompileContext1,
        markdown_mdast_node:mdx_text_expression(#markdown_mdast_mdx_text_expression{
            value = Result#markdown_mdx_collect_result.value,
            position = none,
            stops = Stops
        })
    ),
    CompileContext3 = markdown_mdast_compile_context:buffer(CompileContext2),
    {ok, CompileContext3}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Paragraph`][Name::Paragraph].
""".
-spec on_enter_paragraph(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_paragraph(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:paragraph(#markdown_mdast_paragraph{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Reference`][Name::Reference].
""".
-spec on_enter_reference(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_reference(CompileContext1 = #markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack1}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(MediaReferenceStack1), "expected reference on media stack"),
    %% END: assertions
    Reference1 = markdown_vec:last(MediaReferenceStack1),
    %% Assume collapsed.
    %% If there’s a string after it, we set `Full`.
    Reference2 = Reference1#markdown_mdast_reference{kind = {some, collapsed}},
    MediaReferenceStack2 = markdown_vec:set_last(MediaReferenceStack1, Reference2),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack2},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Resource`][Name::Resource].
""".
-spec on_enter_resource(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_resource(CompileContext1 = #markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack1}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(MediaReferenceStack1), "expected reference on media stack"),
    %% END: assertions
    Reference1 = markdown_vec:last(MediaReferenceStack1),
    %% It's not a reference.
    Reference2 = Reference1#markdown_mdast_reference{kind = none},
    MediaReferenceStack2 = markdown_vec:set_last(MediaReferenceStack1, Reference2),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack2},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Strong`][Name::Strong].
""".
-spec on_enter_strong(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_strong(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:strong(#markdown_mdast_strong{
                children = markdown_vec:new(),
                position = none
            })
        ),
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`ThematicBreak`][Name::ThematicBreak].
""".
-spec on_enter_thematic_break(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_enter_thematic_break(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 =
        markdown_mdast_compile_context:tail_push(
            CompileContext1,
            markdown_mdast_node:thematic_break(#markdown_mdast_thematic_break{
                position = none
            })
        ),
    {ok, CompileContext2}.

%%%-----------------------------------------------------------------------------
%%% Internal exit functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Handle [`Exit`][Kind::Exit].
""".
-spec exit(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
exit(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Name = Event#markdown_event.name,
    case Name of
        %% on_exit
        autolink ->
            on_exit(CompileContext1);
        block_quote ->
            on_exit(CompileContext1);
        character_reference ->
            on_exit(CompileContext1);
        definition ->
            on_exit(CompileContext1);
        emphasis ->
            on_exit(CompileContext1);
        gfm_footnote_definition ->
            on_exit(CompileContext1);
        gfm_strikethrough ->
            on_exit(CompileContext1);
        gfm_table_row ->
            on_exit(CompileContext1);
        gfm_table_cell ->
            on_exit(CompileContext1);
        heading_atx ->
            on_exit(CompileContext1);
        list_ordered ->
            on_exit(CompileContext1);
        list_unordered ->
            on_exit(CompileContext1);
        paragraph ->
            on_exit(CompileContext1);
        strong ->
            on_exit(CompileContext1);
        thematic_break ->
            on_exit(CompileContext1);
        %% on_exit_data
        character_escape_value ->
            on_exit_data(CompileContext1);
        code_flow_chunk ->
            on_exit_data(CompileContext1);
        code_text_data ->
            on_exit_data(CompileContext1);
        data ->
            on_exit_data(CompileContext1);
        frontmatter_chunk ->
            on_exit_data(CompileContext1);
        html_flow_data ->
            on_exit_data(CompileContext1);
        html_text_data ->
            on_exit_data(CompileContext1);
        math_flow_chunk ->
            on_exit_data(CompileContext1);
        math_text_data ->
            on_exit_data(CompileContext1);
        mdx_jsx_tag_attribute_value_literal_value ->
            on_exit_data(CompileContext1);
        % %% on_exit_drop
        % mdx_jsx_tag_attribute_expression -> on_exit_drop(CompileContext1);
        % mdx_jsx_tag_attribute_value_expression -> on_exit_drop(CompileContext1);
        %% on_exit_autolink_protocol
        autolink_protocol ->
            on_exit_autolink_protocol(CompileContext1);
        %% on_exit_autolink_email
        autolink_email ->
            on_exit_autolink_email(CompileContext1);
        %% on_exit_character_reference_marker
        character_reference_marker ->
            on_exit_character_reference_marker(CompileContext1);
        %% on_exit_character_reference_marker_numeric
        character_reference_marker_numeric ->
            on_exit_character_reference_marker_numeric(CompileContext1);
        %% on_exit_character_reference_marker_hexadecimal
        character_reference_marker_hexadecimal ->
            on_exit_character_reference_marker_hexadecimal(CompileContext1);
        %% on_exit_character_reference_value
        character_reference_value ->
            on_exit_character_reference_value(CompileContext1);
        %% on_exit_code_fenced_fence_info
        code_fenced_fence_info ->
            on_exit_code_fenced_fence_info(CompileContext1);
        %% on_exit_raw_flow_fence_meta
        code_fenced_fence_meta ->
            on_exit_raw_flow_fence_meta(CompileContext1);
        math_flow_fence_meta ->
            on_exit_raw_flow_fence_meta(CompileContext1);
        %% on_exit_raw_flow_fence
        code_fenced_fence ->
            on_exit_raw_flow_fence(CompileContext1);
        math_flow_fence ->
            on_exit_raw_flow_fence(CompileContext1);
        %% on_exit_raw_flow
        code_fenced ->
            on_exit_raw_flow(CompileContext1);
        math_flow ->
            on_exit_raw_flow(CompileContext1);
        %% on_exit_code_indented
        code_indented ->
            on_exit_code_indented(CompileContext1);
        %% on_exit_raw_text
        code_text ->
            on_exit_raw_text(CompileContext1);
        math_text ->
            on_exit_raw_text(CompileContext1);
        %% on_exit_definition_destination_string
        definition_destination_string ->
            on_exit_definition_destination_string(CompileContext1);
        %% on_exit_definition_id
        definition_label_string ->
            on_exit_definition_id(CompileContext1);
        gfm_footnote_definition_label_string ->
            on_exit_definition_id(CompileContext1);
        %% on_exit_definition_title_string
        definition_title_string ->
            on_exit_definition_title_string(CompileContext1);
        %% on_exit_frontmatter
        frontmatter ->
            on_exit_frontmatter(CompileContext1);
        %% on_exit_gfm_autolink_literal
        gfm_autolink_literal_email ->
            on_exit_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_mailto ->
            on_exit_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_protocol ->
            on_exit_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_www ->
            on_exit_gfm_autolink_literal(CompileContext1);
        gfm_autolink_literal_xmpp ->
            on_exit_gfm_autolink_literal(CompileContext1);
        %% on_exit_media
        gfm_footnote_call ->
            on_exit_media(CompileContext1);
        image ->
            on_exit_media(CompileContext1);
        link ->
            on_exit_media(CompileContext1);
        %% on_exit_gfm_table
        gfm_table ->
            on_exit_gfm_table(CompileContext1);
        % %% on_exit_gfm_task_list_item_value
        % gfm_task_list_item_value_unchecked -> on_exit_gfm_task_list_item_value(CompileContext1);
        % gfm_task_list_item_value_checked -> on_exit_gfm_task_list_item_value(CompileContext1);
        %% on_exit_hard_break
        hard_break_escape ->
            on_exit_hard_break(CompileContext1);
        hard_break_trailing ->
            on_exit_hard_break(CompileContext1);
        %% on_exit_heading_atx_sequence
        heading_atx_sequence ->
            on_exit_heading_atx_sequence(CompileContext1);
        %% on_exit_heading_setext
        heading_setext ->
            on_exit_heading_setext(CompileContext1);
        %% on_exit_heading_setext_underline_sequence
        heading_setext_underline_sequence ->
            on_exit_heading_setext_underline_sequence(CompileContext1);
        %% on_exit_heading_setext_text
        heading_setext_text ->
            on_exit_heading_setext_text(CompileContext1);
        % %% on_exit_html
        % html_flow -> on_exit_html(CompileContext1);
        % html_text -> on_exit_html(CompileContext1);
        %% on_exit_label_text
        label_text ->
            on_exit_label_text(CompileContext1);
        %% on_exit_line_ending
        line_ending ->
            on_exit_line_ending(CompileContext1);
        %% on_exit_list_item
        list_item ->
            on_exit_list_item(CompileContext1);
        %% on_exit_list_item_value
        list_item_value ->
            on_exit_list_item_value(CompileContext1);
        %% on_exit_mdx_esm_or_expression
        mdx_esm ->
            on_exit_mdx_esm_or_expression(CompileContext1);
        mdx_flow_expression ->
            on_exit_mdx_esm_or_expression(CompileContext1);
        mdx_text_expression ->
            on_exit_mdx_esm_or_expression(CompileContext1);
        %% on_exit_mdx_jsx_tag
        mdx_jsx_flow_tag ->
            on_exit_mdx_jsx_tag(CompileContext1);
        mdx_jsx_text_tag ->
            on_exit_mdx_jsx_tag(CompileContext1);
        %% on_exit_mdx_jsx_tag_closing_marker
        mdx_jsx_tag_closing_marker ->
            on_exit_mdx_jsx_tag_closing_marker(CompileContext1);
        %% on_exit_mdx_jsx_tag_name_primary
        mdx_jsx_tag_name_primary ->
            on_exit_mdx_jsx_tag_name_primary(CompileContext1);
        %% on_exit_mdx_jsx_tag_name_member
        mdx_jsx_tag_name_member ->
            on_exit_mdx_jsx_tag_name_member(CompileContext1);
        %% on_exit_mdx_jsx_tag_name_local
        mdx_jsx_tag_name_local ->
            on_exit_mdx_jsx_tag_name_local(CompileContext1);
        %% on_exit_mdx_jsx_tag_attribute_primary_name
        mdx_jsx_tag_attribute_primary_name ->
            on_exit_mdx_jsx_tag_attribute_primary_name(CompileContext1);
        %% on_exit_mdx_jsx_tag_attribute_name_local
        mdx_jsx_tag_attribute_name_local ->
            on_exit_mdx_jsx_tag_attribute_name_local(CompileContext1);
        %% on_exit_mdx_jsx_tag_attribute_value_literal
        mdx_jsx_tag_attribute_value_literal ->
            on_exit_mdx_jsx_tag_attribute_value_literal(CompileContext1);
        %% on_exit_mdx_jsx_tag_self_closing_marker
        mdx_jsx_tag_self_closing_marker ->
            on_exit_mdx_jsx_tag_self_closing_marker(CompileContext1);
        %% on_exit_reference_string
        reference_string ->
            on_exit_reference_string(CompileContext1);
        %% on_exit_resource_destination_string
        resource_destination_string ->
            on_exit_resource_destination_string(CompileContext1);
        %% on_exit_resource_title_string
        resource_title_string ->
            on_exit_resource_title_string(CompileContext1);
        %% otherwise
        _ ->
            io:format("~ts\n", [markdown_debug:rust_debug_string(Event)]),
            {ok, CompileContext1}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:`*`.
""".
-spec on_exit(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit(CompileContext1 = #markdown_mdast_compile_context{}) ->
    markdown_mdast_compile_context:tail_pop(CompileContext1).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`AutolinkEmail`][Name::AutolinkEmail].
""".
-spec on_exit_autolink_email(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_autolink_email(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_exit_data(CompileContext1),
        Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
        Events = CompileContext2#markdown_mdast_compile_context.events,
        Index = CompileContext2#markdown_mdast_compile_context.index,
        Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
        Value = markdown_slice:as_binary(Slice),
        NodeMutFunc = fun on_exit_autolink_email__node_mut_func/2,
        {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
        {ok, CompileContext3}
    end.

%% @private
-spec on_exit_autolink_email__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_autolink_email__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{url = Url1}}, {some, Value}
) ->
    Url2 = <<Url1/bytes, "mailto:", Value/bytes>>,
    Link2 = Link1#markdown_mdast_link{url = Url2},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_autolink_email__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected link on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`AutolinkProtocol`][Name::AutolinkProtocol].
""".
-spec on_exit_autolink_protocol(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_autolink_protocol(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_exit_data(CompileContext1),
        Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
        Events = CompileContext2#markdown_mdast_compile_context.events,
        Index = CompileContext2#markdown_mdast_compile_context.index,
        Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
        Value = markdown_slice:as_binary(Slice),
        NodeMutFunc = fun on_exit_autolink_protocol__node_mut_func/2,
        {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
        {ok, CompileContext3}
    end.

%% @private
-spec on_exit_autolink_protocol__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_autolink_protocol__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{url = Url1}}, {some, Value}
) ->
    Url2 = <<Url1/bytes, Value/bytes>>,
    Link2 = Link1#markdown_mdast_link{url = Url2},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_autolink_protocol__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected link on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarker`][Name::CharacterReferenceMarker].
""".
-spec on_exit_character_reference_marker(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_character_reference_marker(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{character_reference_marker = $&},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarkerHexadecimal`][Name::CharacterReferenceMarkerHexadecimal].
""".
-spec on_exit_character_reference_marker_hexadecimal(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_character_reference_marker_hexadecimal(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{character_reference_marker = $x},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarkerNumeric`][Name::CharacterReferenceMarkerNumeric].
""".
-spec on_exit_character_reference_marker_numeric(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_character_reference_marker_numeric(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{character_reference_marker = $#},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceValue`][Name::CharacterReferenceValue].
""".
-spec on_exit_character_reference_value(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_character_reference_value(
    CompileContext1 = #markdown_mdast_compile_context{
        bytes = Bytes, character_reference_marker = CharacterReferenceMarker, events = Events, index = Index
    }
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    OptionValue = markdown_util_character_reference:decode(
        markdown_slice:as_binary(Slice), CharacterReferenceMarker, true
    ),
    %% BEGIN: assertions
    ?assertMatch({some, _}, OptionValue, "expected to parse only valid named references"),
    %% END: assertions
    {some, Value} = OptionValue,
    NodeMutFunc = fun on_exit_character_reference_value__node_mut_func/2,
    {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(CompileContext1, {some, Value}, NodeMutFunc),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{character_reference_marker = 0},
    {ok, CompileContext3}.

%% @private
-spec on_exit_character_reference_value__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_character_reference_value__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Text1 = #markdown_mdast_text{value = TextValue}}, {some, Value}
) ->
    Text2 = Text1#markdown_mdast_text{value = <<TextValue/bytes, Value/bytes>>},
    Node2 = Node1#markdown_mdast_node{inner = Text2},
    {Node2, none};
on_exit_character_reference_value__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected text on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CodeFencedFenceInfo`][Name::CodeFencedFenceInfo].
""".
-spec on_exit_code_fenced_fence_info(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_code_fenced_fence_info(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_code_fenced_fence_info__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_code_fenced_fence_info__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_code_fenced_fence_info__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Code1 = #markdown_mdast_code{}}, {some, Value}
) ->
    Code2 = Code1#markdown_mdast_code{lang = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Code2},
    {Node2, none};
on_exit_code_fenced_fence_info__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected code on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CodeIndented`][Name::CodeIndented].
""".
-spec on_exit_code_indented(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_code_indented(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value1 = markdown_mdast_node:to_string(Node),
    Value2 = trim_eol(Value1, false, true),
    NodeMutFunc = fun on_exit_code_indented__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value2}, NodeMutFunc),
    maybe
        {ok, CompileContext4} ?= on_exit(CompileContext3),
        CompileContext5 = CompileContext4#markdown_mdast_compile_context{raw_flow_fence_seen = false},
        {ok, CompileContext5}
    end.

%% @private
-spec on_exit_code_indented__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_code_indented__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Code1 = #markdown_mdast_code{}}, {some, Value}
) ->
    Code2 = Code1#markdown_mdast_code{value = Value},
    Node2 = Node1#markdown_mdast_node{inner = Code2},
    {Node2, none};
on_exit_code_indented__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected code on stack for value", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Data`][Name::Data] (and many text things).
""".
-spec on_exit_data(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_data(CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    NodeMutFunc = fun on_exit_data__node_mut_func/2,
    {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(CompileContext1, {some, Value}, NodeMutFunc),
    on_exit(CompileContext2).

%% @private
-spec on_exit_data__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_data__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Text1 = #markdown_mdast_text{value = Value1}}, {some, Value}
) ->
    Value2 = <<Value1/bytes, Value/bytes>>,
    Text2 = Text1#markdown_mdast_text{value = Value2},
    Node2 = Node1#markdown_mdast_node{inner = Text2},
    {Node2, none};
on_exit_data__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected text on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`DefinitionDestinationString`][Name::DefinitionDestinationString].
""".
-spec on_exit_definition_destination_string(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_definition_destination_string(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_definition_destination_string__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_definition_destination_string__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_definition_destination_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Definition1 = #markdown_mdast_definition{}}, {some, Value}
) ->
    Definition2 = Definition1#markdown_mdast_definition{url = Value},
    Node2 = Node1#markdown_mdast_node{inner = Definition2},
    {Node2, none};
on_exit_definition_destination_string__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected definition on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`DefinitionLabelString`][Name::DefinitionLabelString],[`GfmFootnoteDefinitionLabelString`][Name::GfmFootnoteDefinitionLabelString]}.
""".
-spec on_exit_definition_id(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_definition_id(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Label = markdown_mdast_node:to_string(Node),
    Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
    Events = CompileContext2#markdown_mdast_compile_context.events,
    Index = CompileContext2#markdown_mdast_compile_context.index,
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    Identifier = markdown_types:unicode_binary(
        string:casefold(markdown_util_normalize_identifier:normalize_identifier(SliceBytes))
    ),
    NodeMutFunc = fun on_exit_definition_id__node_mut_func/2,
    Value = {Identifier, Label},
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_definition_id__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(),
    OptionValue :: markdown_option:t(Value),
    Value :: {Identifier, Label},
    Identifier :: unicode:unicode_binary(),
    Label :: unicode:unicode_binary().
on_exit_definition_id__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Definition1 = #markdown_mdast_definition{}}, {some, {Identifier, Label}}
) ->
    Definition2 = Definition1#markdown_mdast_definition{identifier = Identifier, label = {some, Label}},
    Node2 = Node1#markdown_mdast_node{inner = Definition2},
    {Node2, none};
on_exit_definition_id__node_mut_func(
    Node1 = #markdown_mdast_node{inner = FootnoteDefinition1 = #markdown_mdast_footnote_definition{}},
    {some, {Identifier, Label}}
) ->
    FootnoteDefinition2 = FootnoteDefinition1#markdown_mdast_footnote_definition{
        identifier = Identifier, label = {some, Label}
    },
    Node2 = Node1#markdown_mdast_node{inner = FootnoteDefinition2},
    {Node2, none};
on_exit_definition_id__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected definition or footnote definition on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`DefinitionTitleString`][Name::DefinitionTitleString].
""".
-spec on_exit_definition_title_string(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_definition_title_string(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_definition_title_string__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_definition_title_string__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_definition_title_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Definition1 = #markdown_mdast_definition{}}, {some, Value}
) ->
    Definition2 = Definition1#markdown_mdast_definition{title = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Definition2},
    {Node2, none};
on_exit_definition_title_string__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected definition on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Frontmatter`][Name::Frontmatter].
""".
-spec on_exit_frontmatter(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_frontmatter(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value1 = markdown_mdast_node:to_string(Node),
    Value2 = trim_eol(Value1, true, true),
    NodeMutFunc = fun on_exit_frontmatter__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value2}, NodeMutFunc),
    on_exit(CompileContext3).

%% @private
-spec on_exit_frontmatter__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_frontmatter__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Yaml1 = #markdown_mdast_yaml{}}, {some, Value}
) ->
    Yaml2 = Yaml1#markdown_mdast_yaml{value = Value},
    Node2 = Node1#markdown_mdast_node{inner = Yaml2},
    {Node2, none};
on_exit_frontmatter__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Toml1 = #markdown_mdast_toml{}}, {some, Value}
) ->
    Toml2 = Toml1#markdown_mdast_toml{value = Value},
    Node2 = Node1#markdown_mdast_node{inner = Toml2},
    {Node2, none};
on_exit_frontmatter__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected yaml/toml on stack for value", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`GfmAutolinkLiteralEmail`][Name::GfmAutolinkLiteralEmail],[`GfmAutolinkLiteralMailto`][Name::GfmAutolinkLiteralMailto],[`GfmAutolinkLiteralProtocol`][Name::GfmAutolinkLiteralProtocol],[`GfmAutolinkLiteralWww`][Name::GfmAutolinkLiteralWww],[`GfmAutolinkLiteralXmpp`][Name::GfmAutolinkLiteralXmpp]}.
""".
-spec on_exit_gfm_autolink_literal(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_gfm_autolink_literal(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_exit_data(CompileContext1),
        Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
        Events = CompileContext2#markdown_mdast_compile_context.events,
        Index = CompileContext2#markdown_mdast_compile_context.index,
        Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
        Value = markdown_slice:as_binary(Slice),
        Event = markdown_vec:get(Events, Index),
        Name = Event#markdown_event.name,
        Prefix =
            case Name of
                gfm_autolink_literal_email -> {some, <<"mailto:">>};
                gfm_autolink_literal_www -> {some, <<"http://">>};
                _ -> none
            end,
        NodeMutFunc = fun(Node, OptionValue) ->
            on_exit_gfm_autolink_literal__node_mut_func(Node, OptionValue, Prefix)
        end,
        {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
        {ok, CompileContext4} ?= on_exit(CompileContext3),
        {ok, CompileContext4}
    end.

%% @private
-spec on_exit_gfm_autolink_literal__node_mut_func(Node, OptionValue, Prefix) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(),
    OptionValue :: markdown_option:t(Value),
    Value :: unicode:unicode_binary(),
    Prefix :: markdown_option:t(unicode:unicode_binary()).
on_exit_gfm_autolink_literal__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{url = Url1}}, {some, Value}, Prefix
) ->
    Url2 =
        case Prefix of
            {some, PrefixValue} ->
                <<PrefixValue/bytes, Value/bytes>>;
            none ->
                <<Url1/bytes, Value/bytes>>
        end,
    Link2 = Link1#markdown_mdast_link{url = Url2},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_gfm_autolink_literal__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}, _Prefix) ->
    ?'unreachable!'("expected link on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTable`][Name::GfmTable].
""".
-spec on_exit_gfm_table(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_gfm_table(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_exit(CompileContext1),
        CompileContext3 = CompileContext2#markdown_mdast_compile_context{gfm_table_inside = false},
        {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`HardBreakEscape`][Name::HardBreakEscape],[`HardBreakTrailing`][Name::HardBreakTrailing]}.
""".
-spec on_exit_hard_break(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_hard_break(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        {ok, CompileContext2} ?= on_exit(CompileContext1),
        CompileContext3 = CompileContext2#markdown_mdast_compile_context{hard_break_after = true},
        {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingAtxSequence`][Name::HeadingAtxSequence].
""".
-spec on_exit_heading_atx_sequence(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_heading_atx_sequence(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    NodeMutFunc = fun on_exit_heading_atx_sequence__node_mut_func/2,
    Depth = byte_size(SliceBytes),
    {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(CompileContext1, {some, Depth}, NodeMutFunc),
    {ok, CompileContext2}.

%% @private
-spec on_exit_heading_atx_sequence__node_mut_func(Node, OptionDepth) -> {Node, OptionDepth} when
    Node :: markdown_mdast_node:t(), OptionDepth :: markdown_option:t(Depth), Depth :: pos_integer().
on_exit_heading_atx_sequence__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Heading1 = #markdown_mdast_heading{depth = 0}}, {some, Depth}
) ->
    Heading2 = Heading1#markdown_mdast_heading{depth = Depth},
    Node2 = Node1#markdown_mdast_node{inner = Heading2},
    {Node2, none};
on_exit_heading_atx_sequence__node_mut_func(
    Node1 = #markdown_mdast_node{inner = #markdown_mdast_heading{}}, {some, _Depth}
) ->
    %% Depth already set, don't override
    {Node1, none};
on_exit_heading_atx_sequence__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Depth}) ->
    ?'unreachable!'("expected heading on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingSetext`][Name::HeadingSetext].
""".
-spec on_exit_heading_setext(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_heading_setext(CompileContext1 = #markdown_mdast_compile_context{}) ->
    maybe
        CompileContext2 = CompileContext1#markdown_mdast_compile_context{heading_setext_text_after = false},
        {ok, CompileContext3} ?= on_exit(CompileContext2),
        {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingSetextText`][Name::HeadingSetextText].
""".
-spec on_exit_heading_setext_text(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_heading_setext_text(CompileContext1 = #markdown_mdast_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{heading_setext_text_after = true},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingSetextUnderlineSequence`][Name::HeadingSetextUnderlineSequence].
""".
-spec on_exit_heading_setext_underline_sequence(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_heading_setext_underline_sequence(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    Head = binary:first(SliceBytes),
    Depth =
        case Head of
            $- -> 2;
            _ -> 1
        end,
    NodeMutFunc = fun on_exit_heading_setext_underline_sequence__node_mut_func/2,
    {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(CompileContext1, {some, Depth}, NodeMutFunc),
    {ok, CompileContext2}.

%% @private
-spec on_exit_heading_setext_underline_sequence__node_mut_func(Node, OptionDepth) -> {Node, OptionDepth} when
    Node :: markdown_mdast_node:t(), OptionDepth :: markdown_option:t(Depth), Depth :: pos_integer().
on_exit_heading_setext_underline_sequence__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Heading1 = #markdown_mdast_heading{}}, {some, Depth}
) ->
    Heading2 = Heading1#markdown_mdast_heading{depth = Depth},
    Node2 = Node1#markdown_mdast_node{inner = Heading2},
    {Node2, none};
on_exit_heading_setext_underline_sequence__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Depth}) ->
    ?'unreachable!'("expected heading on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`LabelText`][Name::LabelText].
""".
-spec on_exit_label_text(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_label_text(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Fragment1} = markdown_mdast_compile_context:resume(CompileContext1),
    Label = markdown_mdast_node:to_string(Fragment1),
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_mdast_node:children(Fragment1), "expected parent"),
    %% END: assertions
    ChildrenMutFunc = fun on_exit_label_text__children_mut_func/2,
    {_Fragment2, {some, FragmentChildren}} = markdown_mdast_node:children_mut(Fragment1, none, ChildrenMutFunc),
    Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
    Events = CompileContext2#markdown_mdast_compile_context.events,
    Index = CompileContext2#markdown_mdast_compile_context.index,
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    Identifier = markdown_types:unicode_binary(
        string:casefold(markdown_util_normalize_identifier:normalize_identifier(SliceBytes))
    ),
    MediaReferenceStack2 = CompileContext2#markdown_mdast_compile_context.media_reference_stack,
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(MediaReferenceStack2), "expected reference on media stack"),
    %% END: assertions
    Reference1 = markdown_vec:last(MediaReferenceStack2),
    Reference2 = Reference1#markdown_mdast_reference{
        label = Label,
        identifier = Identifier
    },
    MediaReferenceStack3 = markdown_vec:set_last(MediaReferenceStack2, Reference2),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{
        media_reference_stack = MediaReferenceStack3
    },
    NodeMutFunc = fun on_exit_label_text__node_mut_func/2,
    {CompileContext4, none} = markdown_mdast_compile_context:tail_mut(
        CompileContext3, {some, {Label, FragmentChildren}}, NodeMutFunc
    ),
    {ok, CompileContext4}.

%% @private
-spec on_exit_label_text__children_mut_func(Children, OptionFragmentChildren) -> {Children, OptionFragmentChildren} when
    Children :: markdown_mdast_node:children(),
    OptionFragmentChildren :: markdown_option:t(FragmentChildren),
    FragmentChildren :: markdown_mdast_node:children().
on_exit_label_text__children_mut_func(Children1, none) ->
    {Children2, FragmentChildren} = markdown_vec:split_off(Children1, 0),
    {Children2, {some, FragmentChildren}}.

%% @private
-spec on_exit_label_text__node_mut_func(Node, OptionContext) -> {Node, OptionContext} when
    Node :: markdown_mdast_node:t(),
    OptionContext :: markdown_option:t(Context),
    Context :: {Label, FragmentChildren},
    Label :: unicode:unicode_binary(),
    FragmentChildren :: markdown_mdast_node:children().
on_exit_label_text__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{}}, {some, {_Label, FragmentChildren}}
) ->
    Link2 = Link1#markdown_mdast_link{children = FragmentChildren},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_label_text__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Image1 = #markdown_mdast_image{}}, {some, {Label, _FragmentChildren}}
) ->
    Image2 = Image1#markdown_mdast_image{alt = Label},
    Node2 = Node1#markdown_mdast_node{inner = Image2},
    {Node2, none};
on_exit_label_text__node_mut_func(
    Node1 = #markdown_mdast_node{inner = #markdown_mdast_footnote_reference{}}, {some, {_Label, _FragmentChildren}}
) ->
    {Node1, none};
on_exit_label_text__node_mut_func(_Node = #markdown_mdast_node{}, {some, {_Label, _FragmentChildren}}) ->
    ?'unreachable!'("expected footnote reference, image, or link on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`LineEnding`][Name::LineEnding].
""".
-spec on_exit_line_ending(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_line_ending(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    case CompileContext1 of
        #markdown_mdast_compile_context{heading_setext_text_after = true} ->
            %% Ignore.
            {ok, CompileContext1};
        #markdown_mdast_compile_context{hard_break_after = true} ->
            %% Line ending position after hard break is part of it.
            End = markdown_point:to_unist((markdown_vec:get(Events, Index))#markdown_event.point),
            NodeMutFunc = fun on_exit_line_ending__hard_break_after_node_mut_func/2,
            {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(
                CompileContext1, {some, End}, NodeMutFunc
            ),
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{hard_break_after = false},
            {ok, CompileContext3};
        _ ->
            %% Line ending is a part of nodes that accept phrasing.
            Node = markdown_mdast_compile_context:tail(CompileContext1),
            IsMatch =
                case Node#markdown_mdast_node.inner of
                    #markdown_mdast_emphasis{} -> true;
                    #markdown_mdast_heading{} -> true;
                    #markdown_mdast_paragraph{} -> true;
                    #markdown_mdast_strong{} -> true;
                    #markdown_mdast_delete{} -> true;
                    _ -> false
                end,
            case IsMatch of
                false ->
                    {ok, CompileContext1};
                true ->
                    maybe
                        Index1 = CompileContext1#markdown_mdast_compile_context.index,
                        Index2 = Index1 - 1,
                        CompileContext2 = CompileContext1#markdown_mdast_compile_context{index = Index2},
                        {ok, CompileContext3} ?= on_enter_data(CompileContext2),
                        Index3 = CompileContext3#markdown_mdast_compile_context.index,
                        Index4 = Index3 + 1,
                        CompileContext4 = CompileContext3#markdown_mdast_compile_context{index = Index4},
                        {ok, CompileContext5} ?= on_exit_data(CompileContext4),
                        {ok, CompileContext5}
                    end
            end
    end.

%% @private
-spec on_exit_line_ending__hard_break_after_children_mut_func(Children, OptionEnd) -> {Children, OptionEnd} when
    Children :: markdown_mdast_node:children(), OptionEnd :: markdown_option:t(End), End :: markdown_unist_point:t().
on_exit_line_ending__hard_break_after_children_mut_func(Children1, {some, End}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Children1), "expected tail (break)"),
    %% END: assertions
    Tail1 = markdown_vec:last(Children1),
    {some, Position1} = markdown_mdast_node:position(Tail1),
    Position2 = Position1#markdown_unist_position{'end' = End},
    Tail2 = markdown_mdast_node:position_set(Tail1, {some, Position2}),
    Children2 = markdown_vec:set_last(Children1, Tail2),
    {Children2, none}.

%% @private
-spec on_exit_line_ending__hard_break_after_node_mut_func(Node, OptionEnd) -> {Node, OptionEnd} when
    Node :: markdown_mdast_node:t(), OptionEnd :: markdown_option:t(End), End :: markdown_unist_point:t().
on_exit_line_ending__hard_break_after_node_mut_func(Node1, OptionEnd = {some, _End}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_mdast_node:children(Node1), "expected parent"),
    %% END: assertions
    ChildrenMutFunc = fun on_exit_line_ending__hard_break_after_children_mut_func/2,
    {Node2, none} = markdown_mdast_node:children_mut(Node1, OptionEnd, ChildrenMutFunc),
    {Node2, none}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ListItem`][Name::ListItem].
""".
-spec on_exit_list_item(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_list_item(CompileContext1 = #markdown_mdast_compile_context{}) ->
    NodeMutFunc = fun on_exit_list_item__node_mut_func/2,
    {CompileContext2, none} = markdown_mdast_compile_context:tail_mut(CompileContext1, none, NodeMutFunc),
    on_exit(CompileContext2).

%% @private
-spec on_exit_list_item__move_past_eol(Bytes, Point, Start) -> {Point, Start} when
    Bytes :: binary(), Point :: markdown_unist_point:t(), Start :: non_neg_integer().
on_exit_list_item__move_past_eol(<<Byte1:8, _/bytes>>, Point1 = #markdown_unist_point{}, Start1) when
    Byte1 =:= $\t orelse Byte1 =:= $\s
->
    Point2 = Point1#markdown_unist_point{
        offset = Point1#markdown_unist_point.offset + 1,
        column = Point1#markdown_unist_point.column + 1
    },
    {Point2, Start1 + 1};
on_exit_list_item__move_past_eol(Bytes = <<Byte1:8, _/bytes>>, Point1 = #markdown_unist_point{}, Start1) when
    Byte1 =:= $\r orelse Byte1 =:= $\n
->
    Point2 = Point1#markdown_unist_point{
        offset = Point1#markdown_unist_point.offset + 1,
        column = Point1#markdown_unist_point.column + 1,
        line = Point1#markdown_unist_point.line + 1
    },
    Start2 = Start1 + 1,
    %% Move past the LF of CRLF.
    case Bytes of
        <<$\r, $\n, _/bytes>> ->
            Point3 = Point2#markdown_unist_point{
                offset = Point2#markdown_unist_point.offset + 1
            },
            Start3 = Start2 + 1,
            {Point3, Start3};
        _ ->
            {Point2, Start2}
    end;
on_exit_list_item__move_past_eol(_Bytes, Point1 = #markdown_unist_point{}, Start1) ->
    {Point1, Start1}.

%% @private
-spec on_exit_list_item__node_mut_func(Node, none) -> {Node, none} when
    Node :: markdown_mdast_node:t().
on_exit_list_item__node_mut_func(
    ListItemNode1 = #markdown_mdast_node{inner = ListItem1 = #markdown_mdast_list_item{checked = {some, _}}}, none
) ->
    ListItemChildren1 = ListItem1#markdown_mdast_list_item.children,
    case markdown_vec:first_option(ListItemChildren1) of
        {some,
            ParagraphNode1 = #markdown_mdast_node{
                inner = Paragraph1 = #markdown_mdast_paragraph{position = {some, ParagraphPosition1}}
            }} ->
            ParagraphChildren1 = ParagraphNode1#markdown_mdast_paragraph.children,
            case markdown_vec:first_option(ParagraphChildren1) of
                {some,
                    TextNode1 = #markdown_mdast_node{
                        inner =
                            Text1 = #markdown_mdast_text{
                                position = {some, #markdown_unist_position{start = Point1}}, value = Bytes
                            }
                    }} ->
                    Start1 = 0,
                    %% Move past eol.
                    {Point2, Start2} = on_exit_list_item__move_past_eol(Bytes, Point1, Start1),
                    Paragraph2 =
                        case byte_size(Bytes) of
                            Start2 ->
                                %% The whole text is whitespace: update the text.
                                ParagraphChildren2 = markdown_vec:remove(ParagraphChildren1, 0),
                                ParagraphPosition2 = ParagraphPosition1#markdown_unist_position{start = Point2},
                                Paragraph1#markdown_mdast_paragraph{
                                    children = ParagraphChildren2, position = {some, ParagraphPosition2}
                                };
                            _ ->
                                <<TextValue1:Start2/bytes, _/bytes>> = Bytes,
                                TextValue2 = markdown_types:unicode_binary(TextValue1),
                                {some, TextPosition1} = Text1#markdown_mdast_text.position,
                                TextPosition2 = TextPosition1#markdown_unist_position{start = Point2},
                                Text2 = Text1#markdown_mdast_text{
                                    value = TextValue2,
                                    position = {some, TextPosition2}
                                },
                                TextNode2 = TextNode1#markdown_mdast_node{inner = Text2},
                                ParagraphChildren2 = markdown_vec:set(ParagraphPosition1, 0, TextNode2),
                                ParagraphPosition2 = ParagraphPosition1#markdown_unist_position{start = Point2},
                                Paragraph1#markdown_mdast_paragraph{
                                    children = ParagraphChildren2, position = {some, ParagraphPosition2}
                                }
                        end,
                    ParagraphNode2 = ParagraphNode1#markdown_mdast_node{inner = Paragraph2},
                    ListItemChildren2 = markdown_vec:set(ListItemChildren1, 0, ParagraphNode2),
                    ListItem2 = ListItem1#markdown_mdast_list_item{children = ListItemChildren2},
                    ListItemNode2 = ListItemNode1#markdown_mdast_node{inner = ListItem2},
                    {ListItemNode2, none};
                _ ->
                    {ListItemNode1, none}
            end;
        _ ->
            {ListItemNode1, none}
    end;
on_exit_list_item__node_mut_func(Node = #markdown_mdast_node{}, none) ->
    {Node, none}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ListItemValue`][Name::ListItemValue].
""".
-spec on_exit_list_item_value(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_list_item_value(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Start = erlang:binary_to_integer(markdown_slice:as_binary(Slice)),
    NodeMutFunc = fun on_exit_list_item_value__node_mut_func/2,
    {CompileContext2, none} = markdown_mdast_compile_context:tail_penultimate_mut(
        CompileContext1, {some, Start}, NodeMutFunc
    ),
    {ok, CompileContext2}.

%% @private
-spec on_exit_list_item_value__node_mut_func(Node, OptionStartValue) -> {Node, OptionStartValue} when
    Node :: markdown_mdast_node:t(), OptionStartValue :: markdown_option:t(StartValue), StartValue :: pos_integer().
on_exit_list_item_value__node_mut_func(
    Node1 = #markdown_mdast_node{inner = List1 = #markdown_mdast_list{start = OptionStart}}, {some, Start}
) ->
    %% BEGIN: assertions
    ?assert(List1#markdown_mdast_list.ordered =:= true, "expected list to be ordered"),
    %% END: assertions
    Node2 =
        case OptionStart of
            none ->
                List2 = List1#markdown_mdast_list{start = {some, Start}},
                Node1#markdown_mdast_node{inner = List2};
            {some, _} ->
                Node1
        end,
    {Node2, none};
on_exit_list_item_value__node_mut_func(_Node, {some, _Start}) ->
    ?'unreachable!'("expected list on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`GfmFootnoteCall`][Name::GfmFootnoteCall],[`Image`][Name::Image],[`Link`][Name::Link]}.
""".
-spec on_exit_media(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_media(CompileContext1 = #markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack1}) ->
    {MediaReferenceStack2, Reference} = markdown_vec:pop(MediaReferenceStack1),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack2},
    maybe
        {ok, CompileContext3} ?= on_exit(CompileContext2),
        case Reference#markdown_mdast_reference.kind of
            none ->
                {ok, CompileContext3};
            {some, _Kind} ->
                %% It's a reference.
                NodeMutFunc = fun on_exit_media__node_mut_func/2,
                {CompileContext4, none} = markdown_mdast_compile_context:tail_mut(
                    CompileContext3, {some, Reference}, NodeMutFunc
                ),
                {ok, CompileContext4}
        end
    end.

%% @private
-spec on_exit_media__children_mut_func(Children, OptionReference) -> {Children, OptionReference} when
    Children :: markdown_mdast_node:children(),
    OptionReference :: markdown_option:t(Reference),
    Reference :: markdown_mdast_reference:t().
on_exit_media__children_mut_func(Children1, {some, Reference = #markdown_mdast_reference{kind = {some, Kind}}}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Children1), "expected tail (break)"),
    %% END: assertions
    LastNode1 = markdown_vec:last(Children1),
    LastNode2 =
        case LastNode1#markdown_mdast_node.inner of
            FootnoteReference1 = #markdown_mdast_footnote_reference{} ->
                FootnoteReference2 = FootnoteReference1#markdown_mdast_footnote_reference{
                    identifier = Reference#markdown_mdast_reference.identifier,
                    label = {some, Reference#markdown_mdast_reference.label}
                },
                LastNode1#markdown_mdast_node{inner = FootnoteReference2};
            Image1 = #markdown_mdast_image{} ->
                %% Need to swap it with a reference version of the node.
                markdown_mdast_node:image_reference(#markdown_mdast_image_reference{
                    reference_kind = Kind,
                    identifier = Reference#markdown_mdast_reference.identifier,
                    label = {some, Reference#markdown_mdast_reference.label},
                    alt = Image1#markdown_mdast_image.alt,
                    position = Image1#markdown_mdast_image.position
                });
            Link1 = #markdown_mdast_link{} ->
                %% Need to swap it with a reference version of the node.
                markdown_mdast_node:link_reference(#markdown_mdast_link_reference{
                    reference_kind = Kind,
                    identifier = Reference#markdown_mdast_reference.identifier,
                    label = {some, Reference#markdown_mdast_reference.label},
                    children = Link1#markdown_mdast_link.children,
                    position = Link1#markdown_mdast_link.position
                });
            _ ->
                ?'unreachable!'("expected footnote reference, image, or link on stack", [])
        end,
    Children2 = markdown_vec:set_last(Children1, LastNode2),
    {Children2, none}.

%% @private
-spec on_exit_media__node_mut_func(Node, OptionReference) -> {Node, OptionReference} when
    Node :: markdown_mdast_node:t(),
    OptionReference :: markdown_option:t(Reference),
    Reference :: markdown_mdast_reference:t().
on_exit_media__node_mut_func(Node1, OptionReference = {some, _Reference}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_mdast_node:children(Node1), "expected parent"),
    %% END: assertions
    ChildrenMutFunc = fun on_exit_media__children_mut_func/2,
    {Node2, none} = markdown_mdast_node:children_mut(Node1, OptionReference, ChildrenMutFunc),
    {Node2, none}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`MdxEsm`][Name::MdxEsm],[`MdxFlowExpression`][Name::MdxFlowExpression],[`MdxTextExpression`][Name::MdxTextExpression]}.
""".
-spec on_exit_mdx_esm_or_expression(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_mdx_esm_or_expression(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, _Node} = markdown_mdast_compile_context:resume(CompileContext1),
    on_exit(CompileContext2).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`MdxJsxFlowTag`][Name::MdxJsxFlowTag],[`MdxJsxTextTag`][Name::MdxJsxTextTag]}.
""".
-spec on_exit_mdx_jsx_tag(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_mdx_jsx_tag(CompileContext1 = #markdown_mdast_compile_context{jsx_tag = {some, JsxTag1}}) ->
    %% End of a tag, so drop the buffer.
    {CompileContext2, _Node} = markdown_mdast_compile_context:resume(CompileContext1),
    %% Set end point.
    Event = markdown_vec:get(
        CompileContext2#markdown_mdast_compile_context.events, CompileContext2#markdown_mdast_compile_context.index
    ),
    EndPoint = markdown_point:to_unist(Event#markdown_event.point),
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{'end' = EndPoint},
    JsxTagStack1 = CompileContext2#markdown_mdast_compile_context.jsx_tag_stack,
    JsxTagStart = JsxTag2#markdown_mdast_jsx_tag.start,
    JsxTagEnd = JsxTag2#markdown_mdast_jsx_tag.'end',
    OptionTail = markdown_vec:last_option(JsxTagStack1),
    case JsxTag2#markdown_mdast_jsx_tag.close of
        true ->
            %% Unwrap: we crashed earlier if there's nothing on the stack.
            {some, Tail} = OptionTail,
            JsxTag2Name = JsxTag2#markdown_mdast_jsx_tag.name,
            case Tail#markdown_mdast_jsx_tag.name of
                JsxTag2Name ->
                    %% Remove from our custom stack.
                    %% Note that this does not exit the node.
                    {JsxTagStack2, _} = markdown_vec:pop(JsxTagStack1),
                    CompileContext3 = CompileContext2#markdown_mdast_compile_context{jsx_tag_stack = JsxTagStack2},
                    on_exit_mdx_jsx_tag__finalize(CompileContext3, JsxTag2);
                _ ->
                    Label = serialize_abbreviated_tag(JsxTag2),
                    TailStartPoint = Tail#markdown_mdast_jsx_tag.start,
                    Reason = markdown_types:unicode_binary(
                        io_lib:format(
                            "Unexpected closing tag `~ts`, expected corresponding closing tag for `~ts` (~w:~w)", [
                                Label,
                                serialize_abbreviated_tag(Tail),
                                TailStartPoint#markdown_unist_point.line,
                                TailStartPoint#markdown_unist_point.column
                            ]
                        )
                    ),
                    Place = markdown_place:position(markdown_unist_position:new(JsxTagStart, JsxTagEnd)),
                    Message = markdown_message:new(
                        {some, Place}, Reason, <<"end-tag-mismatch"/utf8>>, <<"erlang-markdown"/utf8>>
                    ),
                    {error, Message}
            end;
        false ->
            EventName = Event#markdown_event.name,
            Node =
                case EventName of
                    mdx_jsx_flow_tag ->
                        markdown_mdast_node:mdx_jsx_flow_element(#markdown_mdast_mdx_jsx_flow_element{
                            name = JsxTag2#markdown_mdast_jsx_tag.name,
                            attributes = JsxTag2#markdown_mdast_jsx_tag.attributes,
                            children = markdown_vec:new(),
                            position = {some, markdown_unist_position:new(JsxTagStart, JsxTagEnd)}
                        });
                    _ ->
                        markdown_mdast_node:mdx_jsx_text_element(#markdown_mdast_mdx_jsx_text_element{
                            name = JsxTag2#markdown_mdast_jsx_tag.name,
                            attributes = JsxTag2#markdown_mdast_jsx_tag.attributes,
                            children = markdown_vec:new(),
                            position = {some, markdown_unist_position:new(JsxTagStart, JsxTagEnd)}
                        })
                end,
            CompileContext3 = markdown_mdast_compile_context:tail_push(CompileContext2, Node),
            on_exit_mdx_jsx_tag__finalize(CompileContext3, JsxTag2)
    end.

%% @private
-spec on_exit_mdx_jsx_tag__finalize(CompileContext, JsxTag) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(),
    JsxTag :: markdown_mdast_jsx_tag:t(),
    Message :: markdown_message:t().
on_exit_mdx_jsx_tag__finalize(CompileContext1 = #markdown_mdast_compile_context{}, JsxTag = #markdown_mdast_jsx_tag{}) ->
    case JsxTag#markdown_mdast_jsx_tag.self_closing orelse JsxTag#markdown_mdast_jsx_tag.close of
        true ->
            markdown_mdast_compile_context:tail_pop(CompileContext1);
        false ->
            JsxTagStack1 = CompileContext1#markdown_mdast_compile_context.jsx_tag_stack,
            JsxTagStack2 = markdown_vec:push(JsxTagStack1, JsxTag),
            CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag_stack = JsxTagStack2},
            {ok, CompileContext2}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagAttributeNameLocal`][Name::MdxJsxTagAttributeNameLocal].
""".
-spec on_exit_mdx_jsx_tag_attribute_name_local(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_attribute_name_local(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
    {some, LastAttribute} = markdown_vec:last_option(Attributes1),
    case LastAttribute of
        #markdown_mdast_attribute_content{inner = {property, Attribute1 = #markdown_mdast_mdx_jsx_attribute{}}} ->
            Name1 = Attribute1#markdown_mdast_mdx_jsx_attribute.name,
            Name2 = <<Name1/bytes, ":", Value/bytes>>,
            Attribute2 = Attribute1#markdown_mdast_mdx_jsx_attribute{name = Name2},
            AttributeContent = markdown_mdast_attribute_content:property(Attribute2),
            Attributes2 = markdown_vec:set_last(Attributes1, AttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            {ok, CompileContext2};
        _ ->
            ?'unreachable!'("expected property", [])
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagAttributePrimaryName`][Name::MdxJsxTagAttributePrimaryName].
""".
-spec on_exit_mdx_jsx_tag_attribute_primary_name(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_attribute_primary_name(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
    {some, LastAttribute} = markdown_vec:last_option(Attributes1),
    case LastAttribute of
        #markdown_mdast_attribute_content{inner = {property, Attribute1 = #markdown_mdast_mdx_jsx_attribute{}}} ->
            Attribute2 = Attribute1#markdown_mdast_mdx_jsx_attribute{name = Value},
            AttributeContent = markdown_mdast_attribute_content:property(Attribute2),
            Attributes2 = markdown_vec:set_last(Attributes1, AttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            {ok, CompileContext2};
        _ ->
            ?'unreachable!'("expected property", [])
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagAttributeValueLiteral`][Name::MdxJsxTagAttributeValueLiteral].
""".
-spec on_exit_mdx_jsx_tag_attribute_value_literal(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_attribute_value_literal(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    {some, JsxTag1} = CompileContext2#markdown_mdast_compile_context.jsx_tag,
    Attributes1 = JsxTag1#markdown_mdast_jsx_tag.attributes,
    {some, LastAttribute} = markdown_vec:last_option(Attributes1),
    case LastAttribute of
        #markdown_mdast_attribute_content{inner = {property, Attribute1 = #markdown_mdast_mdx_jsx_attribute{}}} ->
            ParsedValue = markdown_util_character_reference:parse(Value),
            AttributeValue = markdown_mdast_attribute_value:literal(ParsedValue),
            Attribute2 = Attribute1#markdown_mdast_mdx_jsx_attribute{value = {some, AttributeValue}},
            AttributeContent = markdown_mdast_attribute_content:property(Attribute2),
            Attributes2 = markdown_vec:set_last(Attributes1, AttributeContent),
            JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{attributes = Attributes2},
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
            {ok, CompileContext3};
        _ ->
            ?'unreachable!'("expected property", [])
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagClosingMarker`][Name::MdxJsxTagClosingMarker].
""".
-spec on_exit_mdx_jsx_tag_closing_marker(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_closing_marker(CompileContext1 = #markdown_mdast_compile_context{jsx_tag = {some, JsxTag1}}) ->
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{close = true},
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagNameLocal`][Name::MdxJsxTagNameLocal].
""".
-spec on_exit_mdx_jsx_tag_name_local(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_name_local(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    {some, Name1} = JsxTag1#markdown_mdast_jsx_tag.name,
    Name2 = <<Name1/bytes, ":", Value/bytes>>,
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{name = {some, Name2}},
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagNameMember`][Name::MdxJsxTagNameMember].
""".
-spec on_exit_mdx_jsx_tag_name_member(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_name_member(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    {some, Name1} = JsxTag1#markdown_mdast_jsx_tag.name,
    Name2 = <<Name1/bytes, ".", Value/bytes>>,
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{name = {some, Name2}},
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagNamePrimary`][Name::MdxJsxTagNamePrimary].
""".
-spec on_exit_mdx_jsx_tag_name_primary(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_name_primary(
    CompileContext1 = #markdown_mdast_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    Value = markdown_slice:as_binary(Slice),
    {some, JsxTag1} = CompileContext1#markdown_mdast_compile_context.jsx_tag,
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{name = {some, Value}},
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`MdxJsxTagSelfClosingMarker`][Name::MdxJsxTagSelfClosingMarker].
""".
-spec on_exit_mdx_jsx_tag_self_closing_marker(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_mdx_jsx_tag_self_closing_marker(CompileContext1 = #markdown_mdast_compile_context{jsx_tag = {some, JsxTag1}}) ->
    JsxTag2 = JsxTag1#markdown_mdast_jsx_tag{self_closing = true},
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{jsx_tag = {some, JsxTag2}},
    {ok, CompileContext2}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFenced`][Name::CodeFenced],[`MathFlow`][Name::MathFlow]}.
""".
-spec on_exit_raw_flow(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_raw_flow(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value1 = markdown_mdast_node:to_string(Node),
    Value2 = trim_eol(Value1, true, true),
    NodeMutFunc = fun on_exit_raw_flow__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value2}, NodeMutFunc),
    maybe
        {ok, CompileContext4} ?= on_exit(CompileContext3),
        CompileContext5 = CompileContext4#markdown_mdast_compile_context{raw_flow_fence_seen = false},
        {ok, CompileContext5}
    end.

%% @private
-spec on_exit_raw_flow__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_raw_flow__node_mut_func(Node1 = #markdown_mdast_node{inner = Inner1}, {some, Value}) ->
    Inner2 =
        case Inner1 of
            Code = #markdown_mdast_code{} ->
                Code#markdown_mdast_code{value = Value};
            Math = #markdown_mdast_math{} ->
                Math#markdown_mdast_math{value = Value};
            _ ->
                ?'unreachable!'("expected code or math on stack for value", [])
        end,
    Node2 = Node1#markdown_mdast_node{inner = Inner2},
    {Node2, none}.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFencedFence`][Name::CodeFencedFence],[`MathFlowFence`][Name::MathFlowFence]}.
""".
-spec on_exit_raw_flow_fence(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_raw_flow_fence(CompileContext1 = #markdown_mdast_compile_context{}) ->
    case CompileContext1 of
        #markdown_mdast_compile_context{raw_flow_fence_seen = true} ->
            %% Second fence, ignore.
            {ok, CompileContext1};
        _ ->
            CompileContext2 = markdown_mdast_compile_context:buffer(CompileContext1),
            CompileContext3 = CompileContext2#markdown_mdast_compile_context{raw_flow_fence_seen = true},
            {ok, CompileContext3}
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFencedFenceMeta`][Name::CodeFencedFenceMeta],[`MathFlowFenceMeta`][Name::MathFlowFenceMeta]}.
""".
-spec on_exit_raw_flow_fence_meta(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_raw_flow_fence_meta(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_raw_flow_fence_meta__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_raw_flow_fence_meta__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_raw_flow_fence_meta__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Code1 = #markdown_mdast_code{}}, {some, Value}
) ->
    Code2 = Code1#markdown_mdast_code{meta = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Code2},
    {Node2, none};
on_exit_raw_flow_fence_meta__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Math1 = #markdown_mdast_math{}}, {some, Value}
) ->
    Math2 = Math1#markdown_mdast_math{meta = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Math2},
    {Node2, none};
on_exit_raw_flow_fence_meta__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected code or math on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeText`][Name::CodeText],[`MathText`][Name::MathText]}.
""".
-spec on_exit_raw_text(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_raw_text(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value1 = markdown_mdast_node:to_string(Node),
    %% To do: share with `to_html`.
    %% If we are in a GFM table, we need to decode escaped pipes.
    %% This is rather weird GFM feature.
    Value2 =
        case CompileContext2 of
            #markdown_mdast_compile_context{gfm_table_inside = true} ->
                on_exit_raw_text__gfm_table_inside(Value1, false, Value1, <<>>);
            _ ->
                Value1
        end,
    Value3 = on_exit_raw_text__maybe_trim_once(Value2),
    NodeMutFunc = fun on_exit_raw_text__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value3}, NodeMutFunc),
    on_exit(CompileContext3).

%% @private
-spec on_exit_raw_text__gfm_table_inside(Bytes, Replace, Value, Acc) -> Value when
    Bytes :: binary(), Replace :: boolean(), Value :: unicode:unicode_binary(), Acc :: binary().
on_exit_raw_text__gfm_table_inside(<<C1:8, C2:8, Bytes/bytes>>, _Replace, Value, Acc) when
    C1 =:= $\\ andalso C2 =:= $|
->
    on_exit_raw_text__gfm_table_inside(Bytes, true, Value, <<Acc/bytes, C2:8>>);
on_exit_raw_text__gfm_table_inside(<<C:8, Bytes/bytes>>, Replace, Value, Acc) ->
    on_exit_raw_text__gfm_table_inside(Bytes, Replace, Value, <<Acc/bytes, C:8>>);
on_exit_raw_text__gfm_table_inside(<<>>, Replace, Value, Acc) ->
    case Replace of
        false ->
            Value;
        true ->
            markdown_types:unicode_binary(Acc)
    end.

%% @private
-spec on_exit_raw_text__is_all_whitespace(Bytes) -> IsAllWhitespace when
    Bytes :: binary(), IsAllWhitespace :: boolean().
on_exit_raw_text__is_all_whitespace(<<" ", Bytes/bytes>>) ->
    on_exit_raw_text__is_all_whitespace(Bytes);
on_exit_raw_text__is_all_whitespace(<<_:8, _/bytes>>) ->
    false;
on_exit_raw_text__is_all_whitespace(<<>>) ->
    true.

%% @private
-spec on_exit_raw_text__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_raw_text__node_mut_func(Node1 = #markdown_mdast_node{inner = Inner1}, {some, Value}) ->
    Inner2 =
        case Inner1 of
            InlineCode = #markdown_mdast_inline_code{} ->
                InlineCode#markdown_mdast_inline_code{value = Value};
            InlineMath = #markdown_mdast_inline_math{} ->
                InlineMath#markdown_mdast_inline_math{value = Value};
            _ ->
                ?'unreachable!'("expected inline code or math on stack for value", [])
        end,
    Node2 = Node1#markdown_mdast_node{inner = Inner2},
    {Node2, none}.

-spec on_exit_raw_text__maybe_trim_once(Bytes) -> Bytes when Bytes :: binary().
on_exit_raw_text__maybe_trim_once(Bytes = <<" ", _/bytes>>) when byte_size(Bytes) >= 2 ->
    Last = binary:last(Bytes),
    case Last =:= $\s andalso not on_exit_raw_text__is_all_whitespace(Bytes) of
        false ->
            Bytes;
        true ->
            binary:part(Bytes, 1, byte_size(Bytes) - 2)
    end;
on_exit_raw_text__maybe_trim_once(Bytes) ->
    Bytes.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ReferenceString`][Name::ReferenceString].
""".
-spec on_exit_reference_string(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_reference_string(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Label = markdown_mdast_node:to_string(Node),
    Bytes = CompileContext2#markdown_mdast_compile_context.bytes,
    Events = CompileContext2#markdown_mdast_compile_context.events,
    Index = CompileContext2#markdown_mdast_compile_context.index,
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    Identifier = markdown_types:unicode_binary(
        string:casefold(markdown_util_normalize_identifier:normalize_identifier(SliceBytes))
    ),
    MediaReferenceStack1 = CompileContext2#markdown_mdast_compile_context.media_reference_stack,
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(MediaReferenceStack1), "expected reference on media stack"),
    %% END: assertions
    Reference1 = markdown_vec:last(MediaReferenceStack1),
    Reference2 = Reference1#markdown_mdast_reference{
        kind = {some, full},
        label = Label,
        identifier = Identifier
    },
    MediaReferenceStack2 = markdown_vec:set_last(MediaReferenceStack1, Reference2),
    CompileContext3 = CompileContext2#markdown_mdast_compile_context{media_reference_stack = MediaReferenceStack2},
    {ok, CompileContext3}.
%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ResourceDestinationString`][Name::ResourceDestinationString].
""".
-spec on_exit_resource_destination_string(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_resource_destination_string(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_resource_destination_string__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_resource_destination_string__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_resource_destination_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{}}, {some, Value}
) ->
    Link2 = Link1#markdown_mdast_link{url = Value},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_resource_destination_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Image1 = #markdown_mdast_image{}}, {some, Value}
) ->
    Image2 = Image1#markdown_mdast_image{url = Value},
    Node2 = Node1#markdown_mdast_node{inner = Image2},
    {Node2, none};
on_exit_resource_destination_string__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected link, image on stack", []).

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ResourceTitleString`][Name::ResourceTitleString].
""".
-spec on_exit_resource_title_string(CompileContext) -> {ok, CompileContext} when
    CompileContext :: markdown_mdast_compile_context:t().
on_exit_resource_title_string(CompileContext1 = #markdown_mdast_compile_context{}) ->
    {CompileContext2, Node} = markdown_mdast_compile_context:resume(CompileContext1),
    Value = markdown_mdast_node:to_string(Node),
    NodeMutFunc = fun on_exit_resource_title_string__node_mut_func/2,
    {CompileContext3, none} = markdown_mdast_compile_context:tail_mut(CompileContext2, {some, Value}, NodeMutFunc),
    {ok, CompileContext3}.

%% @private
-spec on_exit_resource_title_string__node_mut_func(Node, OptionValue) -> {Node, OptionValue} when
    Node :: markdown_mdast_node:t(), OptionValue :: markdown_option:t(Value), Value :: unicode:unicode_binary().
on_exit_resource_title_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Link1 = #markdown_mdast_link{}}, {some, Value}
) ->
    Link2 = Link1#markdown_mdast_link{title = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Link2},
    {Node2, none};
on_exit_resource_title_string__node_mut_func(
    Node1 = #markdown_mdast_node{inner = Image1 = #markdown_mdast_image{}}, {some, Value}
) ->
    Image2 = Image1#markdown_mdast_image{title = {some, Value}},
    Node2 = Node1#markdown_mdast_node{inner = Image2},
    {Node2, none};
on_exit_resource_title_string__node_mut_func(_Node = #markdown_mdast_node{}, {some, _Value}) ->
    ?'unreachable!'("expected link, image on stack", []).
