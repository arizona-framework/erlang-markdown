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
-include_lib("markdown/include/markdown_parser.hrl").
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
        % %% on_enter_autolink
        % autolink -> on_enter_autolink(CompileContext1);
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
        % %% on_enter_definition
        % definition -> on_enter_definition(CompileContext1);
        %% on_enter_emphasis
        emphasis ->
            on_enter_emphasis(CompileContext1);
        % %% on_enter_frontmatter
        % frontmatter -> on_enter_frontmatter(CompileContext1);
        % %% on_enter_gfm_autolink_literal
        % gfm_autolink_literal_email -> on_enter_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_mailto -> on_enter_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_protocol -> on_enter_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_www -> on_enter_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_xmpp -> on_enter_gfm_autolink_literal(CompileContext1);
        % %% on_enter_gfm_footnote_call
        % gfm_footnote_call -> on_enter_gfm_footnote_call(CompileContext1);
        % %% on_enter_gfm_footnote_definition
        % gfm_footnote_definition -> on_enter_gfm_footnote_definition(CompileContext1);
        % %% on_enter_gfm_strikethrough
        % gfm_strikethrough -> on_enter_gfm_strikethrough(CompileContext1);
        % %% on_enter_gfm_table
        % gfm_table -> on_enter_gfm_table(CompileContext1);
        % %% on_enter_gfm_table_row
        % gfm_table_row -> on_enter_gfm_table_row(CompileContext1);
        % %% on_enter_gfm_table_cell
        % gfm_table_cell -> on_enter_gfm_table_cell(CompileContext1);
        % %% on_enter_hard_break
        % hard_break_escape -> on_enter_hard_break(CompileContext1);
        % hard_break_trailing -> on_enter_hard_break(CompileContext1);
        % %% on_enter_heading
        % heading_atx -> on_enter_heading(CompileContext1);
        % heading_setext -> on_enter_heading(CompileContext1);
        % %% on_enter_html
        % html_flow -> on_enter_html(CompileContext1);
        % html_text -> on_enter_html(CompileContext1);
        % %% on_enter_image
        % image -> on_enter_image(CompileContext1);
        % %% on_enter_link
        % link -> on_enter_link(CompileContext1);
        % %% on_enter_list_item
        % list_item -> on_enter_list_item(CompileContext1);
        % %% on_enter_list
        % list_ordered -> on_enter_list(CompileContext1);
        % list_unordered -> on_enter_list(CompileContext1);
        % %% on_enter_math_flow
        % math_flow -> on_enter_math_flow(CompileContext1);
        % %% on_enter_math_text
        % math_text -> on_enter_math_text(CompileContext1);
        % %% on_enter_mdx_esm
        % mdx_esm -> on_enter_mdx_esm(CompileContext1);
        % %% on_enter_mdx_flow_expression
        % mdx_flow_expression -> on_enter_mdx_flow_expression(CompileContext1);
        % %% on_enter_mdx_text_expression
        % mdx_text_expression -> on_enter_mdx_text_expression(CompileContext1);
        % %% on_enter_mdx_jsx_tag
        % mdx_jsx_flow_tag -> on_enter_mdx_jsx_tag(CompileContext1);
        % mdx_jsx_text_tag -> on_enter_mdx_jsx_tag(CompileContext1);
        % %% on_enter_mdx_jsx_tag_closing_marker
        % mdx_jsx_tag_closing_marker -> on_enter_mdx_jsx_tag_closing_marker(CompileContext1);
        % %% on_enter_mdx_jsx_tag_attribute
        % mdx_jsx_tag_attribute -> on_enter_mdx_jsx_tag_attribute(CompileContext1);
        % %% on_enter_mdx_jsx_tag_attribute_expression
        % mdx_jsx_tag_attribute_expression -> on_enter_mdx_jsx_tag_attribute_expression(CompileContext1);
        % %% on_enter_mdx_jsx_tag_attribute_value_expression
        % mdx_jsx_tag_attribute_value_expression -> on_enter_mdx_jsx_tag_attribute_value_expression(CompileContext1);
        % %% on_enter_mdx_jsx_tag_self_closing_marker
        % mdx_jsx_tag_self_closing_marker -> on_enter_mdx_jsx_tag_self_closing_marker(CompileContext1);
        %% on_enter_paragraph
        paragraph ->
            on_enter_paragraph(CompileContext1);
        % %% on_enter_reference
        % reference -> on_enter_reference(CompileContext1);
        % %% on_enter_resource
        % resource -> on_enter_resource(CompileContext1);
        % %% on_enter_strong
        % strong -> on_enter_strong(CompileContext1);
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
        % %% on_exit_autolink_protocol
        % autolink_protocol -> on_exit_autolink_protocol(CompileContext1);
        % %% on_exit_autolink_email
        % autolink_email -> on_exit_autolink_email(CompileContext1);
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
        % %% on_exit_definition_destination_string
        % definition_destination_string -> on_exit_definition_destination_string(CompileContext1);
        % %% on_exit_definition_id
        % definition_label_string -> on_exit_definition_id(CompileContext1);
        % gfm_footnote_definition_label_string -> on_exit_definition_id(CompileContext1);
        % %% on_exit_definition_title_string
        % definition_title_string -> on_exit_definition_title_string(CompileContext1);
        % %% on_exit_frontmatter
        % frontmatter -> on_exit_frontmatter(CompileContext1);
        % %% on_exit_gfm_autolink_literal
        % gfm_autolink_literal_email -> on_exit_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_mailto -> on_exit_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_protocol -> on_exit_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_www -> on_exit_gfm_autolink_literal(CompileContext1);
        % gfm_autolink_literal_xmpp -> on_exit_gfm_autolink_literal(CompileContext1);
        % %% on_exit_media
        % gfm_footnote_call -> on_exit_media(CompileContext1);
        % image -> on_exit_media(CompileContext1);
        % link -> on_exit_media(CompileContext1);
        % %% on_exit_gfm_table
        % gfm_table -> on_exit_gfm_table(CompileContext1);
        % %% on_exit_gfm_task_list_item_value
        % gfm_task_list_item_value_unchecked -> on_exit_gfm_task_list_item_value(CompileContext1);
        % gfm_task_list_item_value_checked -> on_exit_gfm_task_list_item_value(CompileContext1);
        % %% on_exit_hard_break
        % hard_break_escape -> on_exit_hard_break(CompileContext1);
        % hard_break_trailing -> on_exit_hard_break(CompileContext1);
        % %% on_exit_heading_atx_sequence
        % heading_atx_sequence -> on_exit_heading_atx_sequence(CompileContext1);
        % %% on_exit_heading_setext
        % heading_setext -> on_exit_heading_setext(CompileContext1);
        % %% on_exit_heading_setext_underline_sequence
        % heading_setext_underline_sequence -> on_exit_heading_setext_underline_sequence(CompileContext1);
        % %% on_exit_heading_setext_text
        % heading_setext_text -> on_exit_heading_setext_text(CompileContext1);
        % %% on_exit_html
        % html_flow -> on_exit_html(CompileContext1);
        % html_text -> on_exit_html(CompileContext1);
        % %% on_exit_label_text
        % label_text -> on_exit_label_text(CompileContext1);
        %% on_exit_line_ending
        line_ending ->
            on_exit_line_ending(CompileContext1);
        % %% on_exit_list_item
        % list_item -> on_exit_list_item(CompileContext1);
        % %% on_exit_list_item_value
        % list_item_value -> on_exit_list_item_value(CompileContext1);
        % %% on_exit_mdx_esm_or_expression
        % mdx_esm -> on_exit_mdx_esm_or_expression(CompileContext1);
        % mdx_flow_expression -> on_exit_mdx_esm_or_expression(CompileContext1);
        % mdx_text_expression -> on_exit_mdx_esm_or_expression(CompileContext1);
        % %% on_exit_mdx_jsx_tag
        % mdx_jsx_flow_tag -> on_exit_mdx_jsx_tag(CompileContext1);
        % mdx_jsx_text_tag -> on_exit_mdx_jsx_tag(CompileContext1);
        % %% on_exit_mdx_jsx_tag_closing_marker
        % mdx_jsx_tag_closing_marker -> on_exit_mdx_jsx_tag_closing_marker(CompileContext1);
        % %% on_exit_mdx_jsx_tag_name_primary
        % mdx_jsx_tag_name_primary -> on_exit_mdx_jsx_tag_name_primary(CompileContext1);
        % %% on_exit_mdx_jsx_tag_name_member
        % mdx_jsx_tag_name_member -> on_exit_mdx_jsx_tag_name_member(CompileContext1);
        % %% on_exit_mdx_jsx_tag_name_local
        % mdx_jsx_tag_name_local -> on_exit_mdx_jsx_tag_name_local(CompileContext1);
        % %% on_exit_mdx_jsx_tag_attribute_primary_name
        % mdx_jsx_tag_attribute_primary_name -> on_exit_mdx_jsx_tag_attribute_primary_name(CompileContext1);
        % %% on_exit_mdx_jsx_tag_attribute_name_local
        % mdx_jsx_tag_attribute_name_local -> on_exit_mdx_jsx_tag_attribute_name_local(CompileContext1);
        % %% on_exit_mdx_jsx_tag_attribute_value_literal
        % mdx_jsx_tag_attribute_value_literal -> on_exit_mdx_jsx_tag_attribute_value_literal(CompileContext1);
        % %% on_exit_mdx_jsx_tag_self_closing_marker
        % mdx_jsx_tag_self_closing_marker -> on_exit_mdx_jsx_tag_self_closing_marker(CompileContext1);
        % %% on_exit_reference_string
        % reference_string -> on_exit_reference_string(CompileContext1);
        % %% on_exit_resource_destination_string
        % resource_destination_string -> on_exit_resource_destination_string(CompileContext1);
        % %% on_exit_resource_title_string
        % resource_title_string -> on_exit_resource_title_string(CompileContext1);
        % %% otherwise
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
Handle [`Exit`][Kind::Exit]:[`LineEnding`][Name::LineEnding].
""".
-spec on_exit_line_ending(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: markdown_mdast_compile_context:t(), Message :: markdown_message:t().
on_exit_line_ending(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index}) ->
    case CompileContext1 of
        #markdown_mdast_compile_context{heading_setext_text_after = true} ->
            %% Ignore.
            {ok, CompileContext1};
        #markdown_mdast_compile_context{heading_setext_text_after = hard_break_after} ->
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
    {some, Tail1} = markdown_vec:last_option(Children1),
    Tail2 = markdown_mdast_node:position_set(Tail1, {some, End}),
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
