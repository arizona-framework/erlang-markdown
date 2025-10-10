%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  05 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_html).
-moduledoc """
Turn events into a string of HTML.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-compile({no_auto_import, [exit/1]}).

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_html.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    compile/3
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Turn events and bytes into a string of HTML.
""".
-spec compile(Events, Bytes, CompileOptions) -> HtmlOutput when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    CompileOptions :: markdown_compile_options:t(),
    HtmlOutput :: binary().
compile(Events = #markdown_vec{}, Bytes, CompileOptions = #markdown_compile_options{}) when is_binary(Bytes) ->
    %% First, we figure out what the used line ending style is.
    %% Stop when we find a line ending.
    OptionLineEndingInferred = detect_line_ending_style(Events, Bytes, 0, none),

    %% Figure out which line ending style we'll use.
    LineEndingDefault =
        case OptionLineEndingInferred of
            {some, LineEndingInferred} ->
                LineEndingInferred;
            none ->
                CompileOptions#markdown_compile_options.default_line_ending
        end,

    CompileContext1 = markdown_html_compile_context:new(Events, Bytes, CompileOptions, LineEndingDefault),
    DefinitionIndices1 = markdown_vec:new(),
    DefinitionInside1 = false,

    %% Handle all definitions first.
    %% We must do two passes because we need to compile the events in
    %% definitions which come after references already.
    %%
    %% To speed things up, we collect the places we can jump over for the
    %% second pass.
    %%
    %% We don't need to handle GFM footnote definitions like this, because
    %% unlike normal definitions, what they produce is not used in calls.
    %% It would also get very complex, because footnote definitions can be
    %% nested.
    {CompileContext2, DefinitionIndices2, _DefinitionInside2} = handle_loop(
        0, CompileContext1, DefinitionIndices1, DefinitionInside1
    ),

    DefinitionIndex = 0,
    JumpDefault = markdown_indices:new(markdown_vec:size(Events), markdown_vec:size(Events)),
    Jump =
        case markdown_vec:get_option(DefinitionIndices2, DefinitionIndex) of
            none ->
                JumpDefault;
            {some, JumpIndices} ->
                JumpIndices
        end,
    CompileContext3 = jump_loop(0, CompileContext2, DefinitionIndex, DefinitionIndices2, JumpDefault, Jump),

    %% No section to generate.
    CompileContext4 =
        case not markdown_vec:is_empty(CompileContext3#markdown_html_compile_context.gfm_footnote_definition_calls) of
            true ->
                generate_footnote_section(CompileContext3);
            false ->
                CompileContext3
        end,

    %% BEGIN: assertions
    ?assertEqual(
        1, markdown_vec:size(CompileContext4#markdown_html_compile_context.buffers), "expected 1 final buffer"
    ),
    %% END: assertions
    markdown_vec:first(CompileContext4#markdown_html_compile_context.buffers).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec detect_line_ending_style(Events, Bytes, Index, OptionLineEndingInferred) -> OptionLineEndingInferred when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    Index :: markdown_vec:index(),
    OptionLineEndingInferred :: markdown_option:t(LineEndingInferred),
    LineEndingInferred :: markdown_line_ending:t().
detect_line_ending_style(Events, Bytes, Index, OptionLineEndingInferred1) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events)
->
    case markdown_vec:get(Events, Index) of
        #markdown_event{kind = 'exit', name = Name} when Name =:= blank_line_ending orelse Name =:= line_ending ->
            Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
            OptionLineEndingInferred2 = {some, markdown_line_ending:from_binary(markdown_slice:as_binary(Slice))},
            OptionLineEndingInferred2;
        _ ->
            detect_line_ending_style(Events, Bytes, Index + 1, OptionLineEndingInferred1)
    end;
detect_line_ending_style(_Events, _Bytes, _Index, OptionLineEndingInferred) ->
    OptionLineEndingInferred.

%% @private
-doc """
Generate an autolink (used by unicode autolinks and GFM autolink literals).
""".
-spec generate_autolink(CompileContext, OptionProtocol, Value, IsGfmLiteral) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    OptionProtocol :: markdown_option:t(Protocol),
    Protocol :: binary(),
    Value :: binary(),
    IsGfmLiteral :: boolean().
generate_autolink(
    CompileContext1 = #markdown_html_compile_context{
        encode_html = EncodeHtml,
        image_alt_inside = ImageAltInside,
        media_stack = MediaStack,
        options = #markdown_compile_options{allow_dangerous_protocol = AllowDangerousProtocol}
    },
    OptionProtocol,
    Value,
    IsGfmLiteral
) when ?is_option_binary(OptionProtocol) andalso is_binary(Value) andalso is_boolean(IsGfmLiteral) ->
    IsInLink = markdown_vec:reduce_while(MediaStack, false, fun(_Index, MediaEntry, IsInLinkAcc) ->
        case not MediaEntry#markdown_html_media.image of
            true ->
                {halt, true};
            false ->
                {cont, IsInLinkAcc}
        end
    end),
    CompileContext2 =
        case (not ImageAltInside) andalso ((not IsInLink) orelse (not IsGfmLiteral)) of
            true ->
                CompileContext1_1 = CompileContext1,
                CompileContext1_2 = markdown_html_compile_context:push(CompileContext1_1, <<"<a href=\""/utf8>>),
                Url1 =
                    case OptionProtocol of
                        {some, Protocol} ->
                            <<Protocol/bytes, Value/bytes>>;
                        none ->
                            Value
                    end,
                Url2 =
                    case AllowDangerousProtocol of
                        true ->
                            markdown_util_sanitize_uri:sanitize(Url1);
                        false ->
                            markdown_util_sanitize_uri:sanitize_with_protocols(Url1, ?SAFE_PROTOCOL_HREF)
                    end,
                CompileContext1_3 = markdown_html_compile_context:push(CompileContext1_2, Url2),
                CompileContext1_4 = markdown_html_compile_context:push(CompileContext1_3, <<"\">"/utf8>>),
                CompileContext1_4;
            false ->
                CompileContext1
        end,
    EncodedValue = markdown_util_encode:encode(Value, EncodeHtml),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, EncodedValue),
    CompileContext4 =
        case (not ImageAltInside) andalso ((not IsInLink) orelse (not IsGfmLiteral)) of
            true ->
                markdown_html_compile_context:push(CompileContext3, <<"</a>"/utf8>>);
            false ->
                CompileContext3
        end,
    CompileContext4.

%% @private
-doc """
Generate a footnote section.
""".
-spec generate_footnote_section(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
generate_footnote_section(
    CompileContext1 = #markdown_html_compile_context{
        gfm_footnote_definition_calls = GfmFootnoteDefinitionCalls,
        options = #markdown_compile_options{
            gfm_footnote_label_tag_name = OptionGfmFootnoteLabelTagName,
            gfm_footnote_label_attributes = OptionGfmFootnoteLabelAttributes,
            gfm_footnote_label = OptionGfmFootnoteLabel
        }
    }
) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(
        CompileContext2, <<"<section data-footnotes=\"\" class=\"footnotes\"><"/utf8>>
    ),
    CompileContext4 =
        case OptionGfmFootnoteLabelTagName of
            {some, Value} ->
                EncodedValue = markdown_util_encode:encode(
                    Value, CompileContext3#markdown_html_compile_context.encode_html
                ),
                markdown_html_compile_context:push(CompileContext3, EncodedValue);
            none ->
                markdown_html_compile_context:push(CompileContext3, <<"h2"/utf8>>)
        end,
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<" id=\"footnote-label\" "/utf8>>),
    CompileContext6 =
        case OptionGfmFootnoteLabelAttributes of
            {some, GfmFootnoteLabelAttributes} ->
                markdown_html_compile_context:push(CompileContext5, GfmFootnoteLabelAttributes);
            none ->
                markdown_html_compile_context:push(CompileContext5, <<"class=\"sr-only\""/utf8>>)
        end,
    CompileContext7 = markdown_html_compile_context:push(CompileContext6, <<">"/utf8>>),
    CompileContext8 =
        case OptionGfmFootnoteLabel of
            {some, GfmFootnoteLabel} ->
                EncodedGfmFootnoteLabel = markdown_util_encode:encode(
                    GfmFootnoteLabel, CompileContext7#markdown_html_compile_context.encode_html
                ),
                markdown_html_compile_context:push(CompileContext7, EncodedGfmFootnoteLabel);
            none ->
                markdown_html_compile_context:push(CompileContext7, <<"Footnotes"/utf8>>)
        end,
    CompileContext9 = markdown_html_compile_context:push(CompileContext8, <<"</"/utf8>>),
    CompileContext10 =
        case OptionGfmFootnoteLabelTagName of
            {some, GfmFootnoteLabelTagName} ->
                EncodedGfmFootnoteLabelTagName = markdown_util_encode:encode(
                    GfmFootnoteLabelTagName, CompileContext9#markdown_html_compile_context.encode_html
                ),
                markdown_html_compile_context:push(CompileContext9, EncodedGfmFootnoteLabelTagName);
            none ->
                markdown_html_compile_context:push(CompileContext9, <<"h2"/utf8>>)
        end,
    CompileContext11 = markdown_html_compile_context:push(CompileContext10, <<">"/utf8>>),
    CompileContext12 = markdown_html_compile_context:line_ending(CompileContext11),
    CompileContext13 = markdown_html_compile_context:push(CompileContext12, <<"<ol>"/utf8>>),
    CompileContext14 = generate_footnote_section_loop(
        CompileContext13, 0, markdown_vec:size(GfmFootnoteDefinitionCalls)
    ),
    CompileContext15 = markdown_html_compile_context:line_ending(CompileContext14),
    CompileContext16 = markdown_html_compile_context:push(CompileContext15, <<"</ol>"/utf8>>),
    CompileContext17 = markdown_html_compile_context:line_ending(CompileContext16),
    CompileContext18 = markdown_html_compile_context:push(CompileContext17, <<"</section>"/utf8>>),
    CompileContext19 = markdown_html_compile_context:line_ending(CompileContext18),
    CompileContext19.

%% @private
-spec generate_footnote_section_loop(CompileContext, Index, Size) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    Index :: non_neg_integer(),
    Size :: non_neg_integer().
generate_footnote_section_loop(CompileContext, Index, Size) when Index < Size ->
    CompileContext2 = generate_footnote_item(CompileContext, Index),
    generate_footnote_section_loop(CompileContext2, Index + 1, Size);
generate_footnote_section_loop(CompileContext, _Index, _Size) ->
    CompileContext.

%% @private
-doc """
Generate a footnote item from a call.
""".
-spec generate_footnote_item(CompileContext, Index) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    Index :: non_neg_integer().
generate_footnote_item(
    CompileContext1 = #markdown_html_compile_context{
        gfm_footnote_definition_calls = GfmFootnoteDefinitionCalls,
        gfm_footnote_definitions = GfmFootnoteDefinitions,
        options = #markdown_compile_options{
            gfm_footnote_clobber_prefix = OptionGfmFootnoteClobberPrefix,
            gfm_footnote_back_label = OptionGfmFootnoteBackLabel
        }
    },
    Index
) ->
    {Id, CallCount} = markdown_vec:get(GfmFootnoteDefinitionCalls, Index),
    SafeId = markdown_util_sanitize_uri:sanitize(string:lowercase(Id)),

    %% Find definition: we'll always find it.
    DefinitionIndex = generate_footnote_item__find_definition_index(GfmFootnoteDefinitions, Id, 0),

    CompileContext2 = markdown_html_compile_context:line_ending(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<li id=\""/utf8>>),
    CompileContext4 =
        case OptionGfmFootnoteClobberPrefix of
            {some, GfmFootnoteClobberPrefix} ->
                EncodedGfmFootnoteClobberPrefix = markdown_util_encode:encode(
                    GfmFootnoteClobberPrefix, CompileContext3#markdown_html_compile_context.encode_html
                ),
                markdown_html_compile_context:push(CompileContext3, EncodedGfmFootnoteClobberPrefix);
            none ->
                markdown_html_compile_context:push(CompileContext3, <<"user-content-"/utf8>>)
        end,
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"fn-"/utf8>>),
    CompileContext6 = markdown_html_compile_context:push(CompileContext5, SafeId),
    CompileContext7 = markdown_html_compile_context:push(CompileContext6, <<"\">"/utf8>>),
    CompileContext8 = markdown_html_compile_context:line_ending(CompileContext7),

    %% Create one or more backreferences.
    Backreferences = generate_footnote_item__build_backreferences(
        SafeId,
        CallCount,
        OptionGfmFootnoteClobberPrefix,
        OptionGfmFootnoteBackLabel,
        CompileContext8#markdown_html_compile_context.encode_html,
        0,
        <<>>
    ),

    {_DefinitionId, Value} = markdown_vec:get(GfmFootnoteDefinitions, DefinitionIndex),
    ValueBytes = Value,
    ByteSize = byte_size(ValueBytes),

    %% Move back past EOL.
    ByteIndex = generate_footnote_item__skip_trailing_line_endings(ValueBytes, ByteSize),

    %% Check if it ends in `</p>`.
    CompileContext9 =
        case
            ByteIndex > 3 andalso
                binary:at(ValueBytes, ByteIndex - 4) =:= $< andalso
                binary:at(ValueBytes, ByteIndex - 3) =:= $/ andalso
                binary:at(ValueBytes, ByteIndex - 2) =:= $p andalso
                binary:at(ValueBytes, ByteIndex - 1) =:= $>
        of
            true ->
                Before = binary:part(ValueBytes, 0, ByteIndex - 4),
                After = binary:part(ValueBytes, ByteIndex - 4, ByteSize - (ByteIndex - 4)),
                Result = <<Before/binary, " ", Backreferences/binary, After/binary>>,
                markdown_html_compile_context:push(CompileContext8, Result);
            false ->
                CompileContext8_1 = markdown_html_compile_context:push(CompileContext8, Value),
                CompileContext8_2 = markdown_html_compile_context:line_ending_if_needed(CompileContext8_1),
                markdown_html_compile_context:push(CompileContext8_2, Backreferences)
        end,
    CompileContext10 = markdown_html_compile_context:line_ending_if_needed(CompileContext9),
    CompileContext11 = markdown_html_compile_context:push(CompileContext10, <<"</li>"/utf8>>),
    CompileContext11.

%% @private
-spec generate_footnote_item__find_definition_index(GfmFootnoteDefinitions, Id, Index) -> Index when
    GfmFootnoteDefinitions :: markdown_vec:t({Id, Value}),
    Id :: binary(),
    Value :: binary(),
    Index :: non_neg_integer().
generate_footnote_item__find_definition_index(GfmFootnoteDefinitions, Id, Index) when
    Index < ?markdown_vec_size(GfmFootnoteDefinitions)
->
    case markdown_vec:get(GfmFootnoteDefinitions, Index) of
        {Id, _Value} ->
            Index;
        {_DefId, _Value} ->
            generate_footnote_item__find_definition_index(GfmFootnoteDefinitions, Id, Index + 1)
    end;
generate_footnote_item__find_definition_index(_GfmFootnoteDefinitions, _Id, _Index) ->
    ?'unreachable!'("expected definition", []).

%% @private
-spec generate_footnote_item__build_backreferences(
    SafeId, CallCount, OptionGfmFootnoteClobberPrefix, OptionGfmFootnoteBackLabel, EncodeHtml, ReferenceIndex, Acc
) -> Backreferences when
    SafeId :: binary(),
    CallCount :: non_neg_integer(),
    OptionGfmFootnoteClobberPrefix :: markdown_option:t(binary()),
    OptionGfmFootnoteBackLabel :: markdown_option:t(binary()),
    EncodeHtml :: boolean(),
    ReferenceIndex :: non_neg_integer(),
    Acc :: binary(),
    Backreferences :: binary().
generate_footnote_item__build_backreferences(
    SafeId, CallCount, OptionGfmFootnoteClobberPrefix, OptionGfmFootnoteBackLabel, EncodeHtml, ReferenceIndex, Acc
) when ReferenceIndex < CallCount ->
    Separator =
        case ReferenceIndex of
            0 -> <<>>;
            _ -> <<" ">>
        end,

    ClobberPrefix =
        case OptionGfmFootnoteClobberPrefix of
            {some, GfmFootnoteClobberPrefix} ->
                markdown_util_encode:encode(GfmFootnoteClobberPrefix, EncodeHtml);
            none ->
                <<"user-content-"/utf8>>
        end,

    RefSuffix =
        case ReferenceIndex of
            0 -> <<>>;
            _ -> <<"-"/utf8, (erlang:integer_to_binary(ReferenceIndex + 1))/bytes>>
        end,

    BackLabel =
        case OptionGfmFootnoteBackLabel of
            {some, GfmFootnoteBackLabel} ->
                markdown_util_encode:encode(GfmFootnoteBackLabel, EncodeHtml);
            none ->
                <<"Back to content"/utf8>>
        end,

    SuperScript =
        case ReferenceIndex of
            0 -> <<>>;
            _ -> <<"<sup>"/utf8, (erlang:integer_to_binary(ReferenceIndex + 1))/bytes, "</sup>"/utf8>>
        end,

    BackRef =
        <<Separator/bytes, "<a href=\"#"/utf8, ClobberPrefix/bytes, "fnref-"/utf8, SafeId/bytes, RefSuffix/bytes,
            "\" data-footnote-backref=\"\" aria-label=\""/utf8, BackLabel/bytes,
            "\" class=\"data-footnote-backref\">↩"/utf8, SuperScript/bytes, "</a>"/utf8>>,

    NewAcc = <<Acc/bytes, BackRef/bytes>>,
    generate_footnote_item__build_backreferences(
        SafeId,
        CallCount,
        OptionGfmFootnoteClobberPrefix,
        OptionGfmFootnoteBackLabel,
        EncodeHtml,
        ReferenceIndex + 1,
        NewAcc
    );
generate_footnote_item__build_backreferences(
    _SafeId, _CallCount, _OptionGfmFootnoteClobberPrefix, _OptionGfmFootnoteBackLabel, _EncodeHtml, _ReferenceIndex, Acc
) ->
    Acc.

%% @private
-spec generate_footnote_item__skip_trailing_line_endings(Bytes, ByteIndex) -> ByteIndex when
    Bytes :: binary(),
    ByteIndex :: non_neg_integer().
generate_footnote_item__skip_trailing_line_endings(Bytes, ByteIndex) when ByteIndex > 0 ->
    case binary:at(Bytes, ByteIndex - 1) of
        $\n -> generate_footnote_item__skip_trailing_line_endings(Bytes, ByteIndex - 1);
        $\r -> generate_footnote_item__skip_trailing_line_endings(Bytes, ByteIndex - 1);
        _ -> ByteIndex
    end;
generate_footnote_item__skip_trailing_line_endings(_Bytes, ByteIndex) ->
    ByteIndex.

%% @private
-doc """
Handle the event at `index`.
""".
-spec handle(CompileContext, Index) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(), Index :: markdown_vec:index().
handle(CompileContext1 = #markdown_html_compile_context{events = Events}, Index) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{index = Index},
    case markdown_vec:get(Events, Index) of
        #markdown_event{kind = 'enter'} ->
            enter(CompileContext2);
        #markdown_event{kind = 'exit'} ->
            exit(CompileContext2)
    end.

%% @private
-spec handle_loop(Index, CompileContext, DefinitionIndices, DefinitionInside) ->
    {CompileContext, DefinitionIndices, DefinitionInside}
when
    Index :: markdown_vec:index(),
    CompileContext :: markdown_html_compile_context:t(),
    DefinitionIndices :: markdown_vec:t(Indices),
    Indices :: markdown_indices:t(),
    DefinitionInside :: boolean().
handle_loop(
    Index, CompileContext1 = #markdown_html_compile_context{events = Events}, DefinitionIndices1, DefinitionInside1
) when Index >= 0 andalso Index < ?markdown_vec_size(Events) ->
    Event = markdown_vec:get(Events, Index),
    CompileContext2 =
        case DefinitionInside1 of
            true ->
                handle(CompileContext1, Index);
            false ->
                CompileContext1
        end,
    case Event of
        #markdown_event{kind = 'enter', name = 'definition'} ->
            %% Also handle start.
            CompileContext3 = handle(CompileContext2, Index),
            DefinitionInside2 = true,
            DefinitionIndices2 = markdown_vec:push(DefinitionIndices1, markdown_indices:new(Index, Index)),
            handle_loop(Index + 1, CompileContext3, DefinitionIndices2, DefinitionInside2);
        #markdown_event{kind = 'exit', name = 'definition'} ->
            DefinitionInside2 = false,
            DefinitionIndices2 = markdown_vec:update_last(DefinitionIndices1, fun(Indices) ->
                Indices#markdown_indices{'end' = Index}
            end),
            handle_loop(Index + 1, CompileContext2, DefinitionIndices2, DefinitionInside2);
        _ ->
            handle_loop(Index + 1, CompileContext2, DefinitionIndices1, DefinitionInside1)
    end;
handle_loop(_Index, CompileContext, DefinitionIndices, DefinitionInside) ->
    {CompileContext, DefinitionIndices, DefinitionInside}.

%% @private
-spec jump_loop(Index, CompileContext, DefinitionIndex, DefinitionIndices, JumpDefault, Jump) -> CompileContext when
    Index :: markdown_vec:index(),
    CompileContext :: markdown_html_compile_context:t(),
    DefinitionIndex :: markdown_vec:index(),
    DefinitionIndices :: markdown_vec:t(Indices),
    Indices :: markdown_indices:t(),
    JumpDefault :: Indices,
    Jump :: Indices.
jump_loop(
    Index1,
    CompileContext1 = #markdown_html_compile_context{events = Events},
    DefinitionIndex1,
    DefinitionIndices,
    JumpDefault,
    Jump1
) when Index1 >= 0 andalso Index1 < ?markdown_vec_size(Events) ->
    case Index1 =:= Jump1#markdown_indices.start of
        true ->
            Index2 = Jump1#markdown_indices.'end' + 1,
            DefinitionIndex2 = DefinitionIndex1 + 1,
            Jump2 =
                case markdown_vec:get_option(DefinitionIndices, DefinitionIndex2) of
                    none ->
                        JumpDefault;
                    {some, JumpIndices} ->
                        JumpIndices
                end,
            jump_loop(Index2, CompileContext1, DefinitionIndex2, DefinitionIndices, JumpDefault, Jump2);
        false ->
            CompileContext2 = handle(CompileContext1, Index1),
            Index2 = Index1 + 1,
            jump_loop(Index2, CompileContext2, DefinitionIndex1, DefinitionIndices, JumpDefault, Jump1)
    end;
jump_loop(_Index, CompileContext, _DefinitionIndex, _DefinitionIndices, _JumpDefault, _Jump) ->
    CompileContext.

%%%-----------------------------------------------------------------------------
%%% Internal enter functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Handle [`Enter`][Kind::Enter].
""".
-spec enter(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
enter(CompileContext1 = #markdown_html_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Name = Event#markdown_event.name,
    CompileContext2 =
        case Name of
            code_fenced_fence_info -> on_enter_buffer(CompileContext1);
            code_fenced_fence_meta -> on_enter_buffer(CompileContext1);
            math_flow_fence_meta -> on_enter_buffer(CompileContext1);
            definition_label_string -> on_enter_buffer(CompileContext1);
            definition_title_string -> on_enter_buffer(CompileContext1);
            gfm_footnote_definition_prefix -> on_enter_buffer(CompileContext1);
            heading_atx_text -> on_enter_buffer(CompileContext1);
            heading_setext_text -> on_enter_buffer(CompileContext1);
            label -> on_enter_buffer(CompileContext1);
            mdx_esm -> on_enter_buffer(CompileContext1);
            mdx_flow_expression -> on_enter_buffer(CompileContext1);
            mdx_text_expression -> on_enter_buffer(CompileContext1);
            mdx_jsx_flow_tag -> on_enter_buffer(CompileContext1);
            mdx_jsx_text_tag -> on_enter_buffer(CompileContext1);
            reference_string -> on_enter_buffer(CompileContext1);
            resource_title_string -> on_enter_buffer(CompileContext1);
            block_quote -> on_enter_block_quote(CompileContext1);
            code_indented -> on_enter_code_indented(CompileContext1);
            code_fenced -> on_enter_raw_flow(CompileContext1);
            math_flow -> on_enter_raw_flow(CompileContext1);
            code_text -> on_enter_raw_text(CompileContext1);
            math_text -> on_enter_raw_text(CompileContext1);
            definition -> on_enter_definition(CompileContext1);
            definition_destination_string -> on_enter_definition_destination_string(CompileContext1);
            emphasis -> on_enter_emphasis(CompileContext1);
            frontmatter -> on_enter_frontmatter(CompileContext1);
            gfm_footnote_definition -> on_enter_gfm_footnote_definition(CompileContext1);
            gfm_footnote_call -> on_enter_gfm_footnote_call(CompileContext1);
            gfm_strikethrough -> on_enter_gfm_strikethrough(CompileContext1);
            gfm_table -> on_enter_gfm_table(CompileContext1);
            gfm_table_body -> on_enter_gfm_table_body(CompileContext1);
            gfm_table_cell -> on_enter_gfm_table_cell(CompileContext1);
            gfm_table_head -> on_enter_gfm_table_head(CompileContext1);
            gfm_table_row -> on_enter_gfm_table_row(CompileContext1);
            gfm_task_list_item_check -> on_enter_gfm_task_list_item_check(CompileContext1);
            html_flow -> on_enter_html_flow(CompileContext1);
            html_text -> on_enter_html_text(CompileContext1);
            image -> on_enter_image(CompileContext1);
            link -> on_enter_link(CompileContext1);
            list_item_marker -> on_enter_list_item_marker(CompileContext1);
            list_ordered -> on_enter_list(CompileContext1);
            list_unordered -> on_enter_list(CompileContext1);
            paragraph -> on_enter_paragraph(CompileContext1);
            resource -> on_enter_resource(CompileContext1);
            resource_destination_string -> on_enter_resource_destination_string(CompileContext1);
            strong -> on_enter_strong(CompileContext1);
            _ -> CompileContext1
        end,
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`BlockQuote`][Name::BlockQuote].
""".
-spec on_enter_block_quote(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_block_quote(CompileContext1 = #markdown_html_compile_context{tight_stack = TightStack1}) ->
    TightStack2 = markdown_vec:push(TightStack1, false),
    CompileContext2 = CompileContext1#markdown_html_compile_context{tight_stack = TightStack2},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"<blockquote>">>),
    CompileContext4.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:`*`.

Buffers data.
""".
-spec on_enter_buffer(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_buffer(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`CodeIndented`][Name::CodeIndented].
""".
-spec on_enter_code_indented(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_code_indented(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{raw_flow_seen_data = {some, false}},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"<pre><code>">>),
    CompileContext4.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Definition`][Name::Definition].
""".
-spec on_enter_definition(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_definition(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    MediaStack2 = markdown_vec:push(MediaStack1, markdown_html_media:new(false, none, none, none, none, none)),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`DefinitionDestinationString`][Name::DefinitionDestinationString].
""".
-spec on_enter_definition_destination_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_definition_destination_string(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    CompileContext3 = CompileContext2#markdown_html_compile_context{encode_html = false},
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Emphasis`][Name::Emphasis].
""".
-spec on_enter_emphasis(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_emphasis(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<em>">>),
    CompileContext2;
on_enter_emphasis(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Frontmatter`][Name::Frontmatter].
""".
-spec on_enter_frontmatter(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_frontmatter(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmFootnoteDefinition`][Name::GfmFootnoteDefinition].
""".
-spec on_enter_gfm_footnote_definition(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_footnote_definition(CompileContext1 = #markdown_html_compile_context{tight_stack = TightStack1}) ->
    TightStack2 = markdown_vec:push(TightStack1, false),
    CompileContext2 = CompileContext1#markdown_html_compile_context{tight_stack = TightStack2},
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmFootnoteCall`][Name::GfmFootnoteCall].
""".
-spec on_enter_gfm_footnote_call(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_footnote_call(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    MediaStack2 = markdown_vec:push(MediaStack1, markdown_html_media:new(false, none, none, none, none, none)),
    CompileContext2 = CompileContext1#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmStrikethrough`][Name::GfmStrikethrough].
""".
-spec on_enter_gfm_strikethrough(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_strikethrough(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<del>"/utf8>>),
    CompileContext2;
on_enter_gfm_strikethrough(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTable`][Name::GfmTable].
""".
-spec on_enter_gfm_table(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_table(CompileContext1 = #markdown_html_compile_context{events = Events, index = Index}) ->
    Align = markdown_util_infer:gfm_table_align(Events, Index),
    CompileContext2 = CompileContext1#markdown_html_compile_context{gfm_table_align = {some, Align}},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"<table>"/utf8>>),
    CompileContext4.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableBody`][Name::GfmTableBody].
""".
-spec on_enter_gfm_table_body(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_table_body(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<tbody>"/utf8>>),
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableCell`][Name::GfmTableCell].
""".
-spec on_enter_gfm_table_cell(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_table_cell(
    CompileContext1 = #markdown_html_compile_context{
        gfm_table_column = Column,
        gfm_table_align = {some, Align},
        gfm_table_in_head = GfmTableInHead
    }
) ->
    case Column >= ?markdown_vec_size(Align) of
        true ->
            %% Capture cell to ignore it.
            CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
            CompileContext2;
        false ->
            Value = markdown_vec:get(Align, Column),
            CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
            CompileContext3 =
                case GfmTableInHead of
                    true ->
                        markdown_html_compile_context:push(CompileContext2, <<"<th"/utf8>>);
                    false ->
                        markdown_html_compile_context:push(CompileContext2, <<"<td"/utf8>>)
                end,
            CompileContext4 =
                case Value of
                    left -> markdown_html_compile_context:push(CompileContext3, <<" align=\"left\""/utf8>>);
                    right -> markdown_html_compile_context:push(CompileContext3, <<" align=\"right\""/utf8>>);
                    center -> markdown_html_compile_context:push(CompileContext3, <<" align=\"center\""/utf8>>);
                    none -> CompileContext3
                end,
            CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<">"/utf8>>),
            CompileContext5
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableHead`][Name::GfmTableHead].
""".
-spec on_enter_gfm_table_head(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_table_head(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<thead>"/utf8>>),
    CompileContext4 = CompileContext3#markdown_html_compile_context{gfm_table_in_head = true},
    CompileContext4.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTableRow`][Name::GfmTableRow].
""".
-spec on_enter_gfm_table_row(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_table_row(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<tr>"/utf8>>),
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`GfmTaskListItemCheck`][Name::GfmTaskListItemCheck].
""".
-spec on_enter_gfm_task_list_item_check(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_gfm_task_list_item_check(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<input type=\"checkbox\" "/utf8>>),
    CompileContext3 =
        case CompileContext2#markdown_html_compile_context.options of
            #markdown_compile_options{gfm_task_list_item_checkable = false} ->
                markdown_html_compile_context:push(CompileContext2, <<"disabled=\"\" "/utf8>>);
            _ ->
                CompileContext2
        end,
    CompileContext3;
on_enter_gfm_task_list_item_check(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`HtmlFlow`][Name::HtmlFlow].
""".
-spec on_enter_html_flow(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_html_flow(
    CompileContext1 = #markdown_html_compile_context{
        options = #markdown_compile_options{allow_dangerous_html = AllowDangerousHtml}
    }
) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 =
        case AllowDangerousHtml of
            true -> CompileContext2#markdown_html_compile_context{encode_html = false};
            false -> CompileContext2
        end,
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`HtmlText`][Name::HtmlText].
""".
-spec on_enter_html_text(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_html_text(
    CompileContext1 = #markdown_html_compile_context{options = #markdown_compile_options{allow_dangerous_html = true}}
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{encode_html = false},
    CompileContext2;
on_enter_html_text(CompileContext = #markdown_html_compile_context{}) ->
    CompileContext.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Image`][Name::Image].
""".
-spec on_enter_image(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_image(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    MediaStack2 = markdown_vec:push(MediaStack1, markdown_html_media:new(true, none, none, none, none, none)),
    %% Disallow tags.
    CompileContext2 = CompileContext1#markdown_html_compile_context{image_alt_inside = true, media_stack = MediaStack2},
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Link`][Name::Link].
""".
-spec on_enter_link(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_link(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    MediaStack2 = markdown_vec:push(MediaStack1, markdown_html_media:new(false, none, none, none, none, none)),
    CompileContext2 = CompileContext1#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext2.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`ListOrdered`][Name::ListOrdered],[`ListUnordered`][Name::ListUnordered]}.
""".
-spec on_enter_list(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_list(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, tight_stack = TightStack1}
) ->
    Loose = markdown_util_infer:list_loose(Events, Index, true),
    TightStack2 = markdown_vec:push(TightStack1, not Loose),
    CompileContext2 = CompileContext1#markdown_html_compile_context{tight_stack = TightStack2},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    %% Note: no `>`.
    CompileContext4 = markdown_html_compile_context:push(
        CompileContext3,
        case markdown_vec:get(Events, Index) of
            #markdown_event{name = 'list_ordered'} -> <<"<ol">>;
            _ -> <<"<ul">>
        end
    ),
    CompileContext5 = CompileContext4#markdown_html_compile_context{list_expect_first_marker = {some, true}},
    CompileContext5.

-doc """
Handle [`Enter`][Kind::Enter]:[`ListItemMarker`][Name::ListItemMarker].
""".
-spec on_enter_list_item_marker(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_list_item_marker(
    CompileContext1 = #markdown_html_compile_context{list_expect_first_marker = {some, ListExpectFirstMarker}}
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{list_expect_first_marker = none},
    CompileContext3 =
        case ListExpectFirstMarker of
            true ->
                markdown_html_compile_context:push(CompileContext2, <<">">>);
            false ->
                CompileContext2
        end,
    CompileContext4 = markdown_html_compile_context:line_ending_if_needed(CompileContext3),
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"<li>">>),
    CompileContext6 = CompileContext5#markdown_html_compile_context{list_expect_first_marker = {some, false}},
    CompileContext6.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Paragraph`][Name::Paragraph].
""".
-spec on_enter_paragraph(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_paragraph(CompileContext1 = #markdown_html_compile_context{tight_stack = TightStack}) ->
    Tight =
        case markdown_vec:last_option(TightStack) of
            none ->
                false;
            {some, TightLast} ->
                TightLast
        end,
    case not Tight of
        true ->
            CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
            CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<p>">>),
            CompileContext3;
        false ->
            CompileContext1
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`CodeFenced`][Name::CodeFenced],[`MathFlow`][Name::MathFlow]}.
""".
-spec on_enter_raw_flow(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_raw_flow(CompileContext1 = #markdown_html_compile_context{events = Events, index = Index}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{raw_flow_seen_data = {some, false}},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    %% Note that no `>` is used, which is added later (due to info)
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"<pre><code">>),
    CompileContext5 = CompileContext4#markdown_html_compile_context{raw_flow_fences_count = {some, 0}},
    case markdown_vec:get(Events, Index) of
        #markdown_event{name = math_flow} ->
            markdown_html_compile_context:push(CompileContext5, <<" class=\"language-math math-display\"">>);
        _ ->
            CompileContext5
    end.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:{[`CodeText`][Name::CodeText],[`MathText`][Name::MathText]}.
""".
-spec on_enter_raw_text(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_raw_text(
    CompileContext1 = #markdown_html_compile_context{events = Events, image_alt_inside = ImageAltInside, index = Index}
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{raw_text_inside = true},
    CompileContext3 =
        case not ImageAltInside of
            true ->
                CompileContext2_1 = CompileContext2,
                CompileContext2_2 = markdown_html_compile_context:push(CompileContext2_1, <<"<code">>),
                CompileContext2_3 =
                    case markdown_vec:get(Events, Index) of
                        #markdown_event{name = math_text} ->
                            markdown_html_compile_context:push(
                                CompileContext2_2, <<" class=\"language-math math-inline\"">>
                            );
                        _ ->
                            CompileContext2_2
                    end,
                CompileContext2_4 = markdown_html_compile_context:push(CompileContext2_3, <<">">>),
                CompileContext2_4;
            false ->
                CompileContext2
        end,
    CompileContext4 = markdown_html_compile_context:buffer(CompileContext3),
    CompileContext4.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Resource`][Name::Resource].
""".
-spec on_enter_resource(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_resource(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    %% We can have line endings in the resource, ignore them.
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{destination = {some, <<>>}}
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`ResourceDestinationString`][Name::ResourceDestinationString].
""".
-spec on_enter_resource_destination_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_enter_resource_destination_string(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:buffer(CompileContext1),
    %% Ignore encoding the result, as we'll first percent encode the url and
    %% encode manually after.
    CompileContext3 = CompileContext2#markdown_html_compile_context{encode_html = false},
    CompileContext3.

%% @private
-doc """
Handle [`Enter`][Kind::Enter]:[`Strong`][Name::Strong].
""".
-spec on_enter_strong(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_enter_strong(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<strong>">>),
    CompileContext2;
on_enter_strong(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%%%-----------------------------------------------------------------------------
%%% Internal exit functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Handle [`Exit`][Kind::Exit].
""".
-spec exit(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
exit(CompileContext1 = #markdown_html_compile_context{events = Events, index = Index}) ->
    Event = markdown_vec:get(Events, Index),
    Name = Event#markdown_event.name,
    CompileContext2 =
        case Name of
            code_fenced_fence_meta -> on_exit_drop(CompileContext1);
            math_flow_fence_meta -> on_exit_drop(CompileContext1);
            mdx_jsx_text_tag -> on_exit_drop(CompileContext1);
            mdx_text_expression -> on_exit_drop(CompileContext1);
            resource -> on_exit_drop(CompileContext1);
            mdx_esm -> on_exit_drop_slurp(CompileContext1);
            mdx_flow_expression -> on_exit_drop_slurp(CompileContext1);
            mdx_jsx_flow_tag -> on_exit_drop_slurp(CompileContext1);
            character_escape_value -> on_exit_data(CompileContext1);
            code_text_data -> on_exit_data(CompileContext1);
            data -> on_exit_data(CompileContext1);
            math_text_data -> on_exit_data(CompileContext1);
            autolink_email -> on_exit_autolink_email(CompileContext1);
            autolink_protocol -> on_exit_autolink_protocol(CompileContext1);
            blank_line_ending -> on_exit_blank_line_ending(CompileContext1);
            block_quote -> on_exit_block_quote(CompileContext1);
            character_reference_marker -> on_exit_character_reference_marker(CompileContext1);
            character_reference_marker_numeric -> on_exit_character_reference_marker_numeric(CompileContext1);
            character_reference_marker_hexadecimal -> on_exit_character_reference_marker_hexadecimal(CompileContext1);
            character_reference_value -> on_exit_character_reference_value(CompileContext1);
            code_fenced -> on_exit_raw_flow(CompileContext1);
            code_indented -> on_exit_raw_flow(CompileContext1);
            math_flow -> on_exit_raw_flow(CompileContext1);
            code_fenced_fence -> on_exit_raw_flow_fence(CompileContext1);
            math_flow_fence -> on_exit_raw_flow_fence(CompileContext1);
            code_fenced_fence_info -> on_exit_raw_flow_fence_info(CompileContext1);
            code_flow_chunk -> on_exit_raw_flow_chunk(CompileContext1);
            math_flow_chunk -> on_exit_raw_flow_chunk(CompileContext1);
            code_text -> on_exit_raw_text(CompileContext1);
            math_text -> on_exit_raw_text(CompileContext1);
            definition -> on_exit_definition(CompileContext1);
            definition_destination_string -> on_exit_definition_destination_string(CompileContext1);
            definition_label_string -> on_exit_definition_label_string(CompileContext1);
            definition_title_string -> on_exit_definition_title_string(CompileContext1);
            emphasis -> on_exit_emphasis(CompileContext1);
            frontmatter -> on_exit_frontmatter(CompileContext1);
            gfm_autolink_literal_email -> on_exit_gfm_autolink_literal_email(CompileContext1);
            gfm_autolink_literal_mailto -> on_exit_gfm_autolink_literal_mailto(CompileContext1);
            gfm_autolink_literal_protocol -> on_exit_gfm_autolink_literal_protocol(CompileContext1);
            gfm_autolink_literal_www -> on_exit_gfm_autolink_literal_www(CompileContext1);
            gfm_autolink_literal_xmpp -> on_exit_gfm_autolink_literal_xmpp(CompileContext1);
            gfm_footnote_call -> on_exit_gfm_footnote_call(CompileContext1);
            gfm_footnote_definition_label_string -> on_exit_gfm_footnote_definition_label_string(CompileContext1);
            gfm_footnote_definition_prefix -> on_exit_gfm_footnote_definition_prefix(CompileContext1);
            gfm_footnote_definition -> on_exit_gfm_footnote_definition(CompileContext1);
            gfm_strikethrough -> on_exit_gfm_strikethrough(CompileContext1);
            gfm_table -> on_exit_gfm_table(CompileContext1);
            gfm_table_body -> on_exit_gfm_table_body(CompileContext1);
            gfm_table_cell -> on_exit_gfm_table_cell(CompileContext1);
            gfm_table_head -> on_exit_gfm_table_head(CompileContext1);
            gfm_table_row -> on_exit_gfm_table_row(CompileContext1);
            gfm_task_list_item_check -> on_exit_gfm_task_list_item_check(CompileContext1);
            gfm_task_list_item_value_checked -> on_exit_gfm_task_list_item_value_checked(CompileContext1);
            hard_break_escape -> on_exit_break(CompileContext1);
            hard_break_trailing -> on_exit_break(CompileContext1);
            heading_atx -> on_exit_heading_atx(CompileContext1);
            heading_atx_sequence -> on_exit_heading_atx_sequence(CompileContext1);
            heading_atx_text -> on_exit_heading_atx_text(CompileContext1);
            heading_setext_text -> on_exit_heading_setext_text(CompileContext1);
            heading_setext_underline_sequence -> on_exit_heading_setext_underline_sequence(CompileContext1);
            html_flow -> on_exit_html(CompileContext1);
            html_text -> on_exit_html(CompileContext1);
            html_flow_data -> on_exit_html_data(CompileContext1);
            html_text_data -> on_exit_html_data(CompileContext1);
            image -> on_exit_media(CompileContext1);
            link -> on_exit_media(CompileContext1);
            label -> on_exit_label(CompileContext1);
            label_text -> on_exit_label_text(CompileContext1);
            line_ending -> on_exit_line_ending(CompileContext1);
            list_ordered -> on_exit_list(CompileContext1);
            list_unordered -> on_exit_list(CompileContext1);
            list_item -> on_exit_list_item(CompileContext1);
            list_item_value -> on_exit_list_item_value(CompileContext1);
            paragraph -> on_exit_paragraph(CompileContext1);
            reference_string -> on_exit_reference_string(CompileContext1);
            resource_destination_string -> on_exit_resource_destination_string(CompileContext1);
            resource_title_string -> on_exit_resource_title_string(CompileContext1);
            strong -> on_exit_strong(CompileContext1);
            thematic_break -> on_exit_thematic_break(CompileContext1);
            _ -> CompileContext1
        end,
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`AutolinkEmail`][Name::AutolinkEmail].
""".
-spec on_exit_autolink_email(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_autolink_email(CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, {some, <<"mailto:">>}, SliceBytes, false),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`AutolinkProtocol`][Name::AutolinkProtocol].
""".
-spec on_exit_autolink_protocol(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_autolink_protocol(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, none, SliceBytes, false),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`BlankLineEnding`][Name::BlankLineEnding].
""".
-spec on_exit_blank_line_ending(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_blank_line_ending(CompileContext1 = #markdown_html_compile_context{events = Events, index = Index}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{slurp_one_line_ending = false},
    case Index =:= (markdown_vec:size(Events) - 1) of
        true ->
            CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
            CompileContext3;
        false ->
            CompileContext2
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`BlockQuote`][Name::BlockQuote].
""".
-spec on_exit_block_quote(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_block_quote(CompileContext1 = #markdown_html_compile_context{tight_stack = TightStack1}) ->
    {TightStack2, _} = markdown_vec:pop(TightStack1),
    CompileContext2 = CompileContext1#markdown_html_compile_context{tight_stack = TightStack2},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = CompileContext3#markdown_html_compile_context{slurp_one_line_ending = false},
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"</blockquote>">>),
    CompileContext5.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`HardBreakEscape`][Name::HardBreakEscape],[`HardBreakTrailing`][Name::HardBreakTrailing]}.
""".
-spec on_exit_break(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_break(CompileContext1 = #markdown_html_compile_context{image_alt_inside = ImageAltInside}) ->
    case not ImageAltInside of
        true ->
            CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"<br />">>),
            CompileContext2;
        false ->
            CompileContext1
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarker`][Name::CharacterReferenceMarker].
""".
-spec on_exit_character_reference_marker(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_character_reference_marker(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{character_reference_marker = {some, $&}},
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarkerHexadecimal`][Name::CharacterReferenceMarkerHexadecimal].
""".
-spec on_exit_character_reference_marker_hexadecimal(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_character_reference_marker_hexadecimal(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{character_reference_marker = {some, $x}},
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceMarkerNumeric`][Name::CharacterReferenceMarkerNumeric].
""".
-spec on_exit_character_reference_marker_numeric(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_character_reference_marker_numeric(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{character_reference_marker = {some, $#}},
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CharacterReferenceValue`][Name::CharacterReferenceValue].
""".
-spec on_exit_character_reference_value(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_character_reference_value(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes,
        character_reference_marker = {some, CharacterReferenceMarker},
        encode_html = EncodeHtml,
        events = Events,
        index = Index
    }
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{character_reference_marker = none},
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    {some, Value} = markdown_util_character_reference:decode(SliceBytes, CharacterReferenceMarker, true),
    EncodedValue = markdown_util_encode:encode(Value, EncodeHtml),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, EncodedValue),
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeTextData`][Name::CodeTextData],[`Data`][Name::Data],[`CharacterEscapeValue`][Name::CharacterEscapeValue]}.
""".
-spec on_exit_data(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_data(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, encode_html = EncodeHtml, events = Events, index = Index
    }
) ->
    Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
    SliceBytes = markdown_slice:as_binary(Slice),
    Value = markdown_util_encode:encode(SliceBytes, EncodeHtml),
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, Value),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Emphasis`][Name::Emphasis].
""".
-spec on_exit_emphasis(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_emphasis(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"</em>">>),
    CompileContext2;
on_exit_emphasis(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Definition`][Name::Definition].
""".
-spec on_exit_definition(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_definition(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, definitions = Definitions1, media_stack = MediaStack1
    }
) ->
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    {MediaStack2, Media} = markdown_vec:pop(MediaStack1),
    {some, Indices} = Media#markdown_html_media.reference_id,
    Slice = markdown_slice:from_indices(Bytes, Indices),
    SliceBytes = markdown_slice:as_binary(Slice),
    Id = markdown_util_normalize_identifier:normalize_identifier(SliceBytes),
    Definitions2 = markdown_vec:push(
        Definitions1,
        markdown_html_definition:new(Id, Media#markdown_html_media.destination, Media#markdown_html_media.title)
    ),
    CompileContext3 = CompileContext2#markdown_html_compile_context{
        definitions = Definitions2, media_stack = MediaStack2
    },
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`DefinitionDestinationString`][Name::DefinitionDestinationString].
""".
-spec on_exit_definition_destination_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_definition_destination_string(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    {CompileContext2, Buffer} = markdown_html_compile_context:resume(CompileContext1),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            destination = {some, Buffer}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{encode_html = true, media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`DefinitionLabelString`][Name::DefinitionLabelString].
""".
-spec on_exit_definition_label_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_definition_label_string(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, media_stack = MediaStack1}
) ->
    %% Discard label, use the source content instead.
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    ReferenceIdPosition = markdown_position:from_exit_event(Events, Index),
    ReferenceId = markdown_position:to_indices(ReferenceIdPosition),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            reference_id = {some, ReferenceId}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`DefinitionTitleString`][Name::DefinitionTitleString].
""".
-spec on_exit_definition_title_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_definition_title_string(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    {CompileContext2, Buffer} = markdown_html_compile_context:resume(CompileContext1),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            title = {some, Buffer}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:*.

Resumes, and ignores what was resumed.
""".
-spec on_exit_drop(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_drop(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:*.

Resumes, ignores what was resumed, and slurps the following line ending.
""".
-spec on_exit_drop_slurp(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_drop_slurp(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext3 = CompileContext2#markdown_html_compile_context{slurp_one_line_ending = true},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Frontmatter`][Name::Frontmatter].
""".
-spec on_exit_frontmatter(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_frontmatter(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext3 = CompileContext2#markdown_html_compile_context{slurp_one_line_ending = true},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmAutolinkLiteralEmail`][Name::GfmAutolinkLiteralEmail].
""".
-spec on_exit_gfm_autolink_literal_email(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_autolink_literal_email(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, {some, <<"mailto:">>}, SliceBytes, true),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmAutolinkLiteralMailto`][Name::GfmAutolinkLiteralMailto].
""".
-spec on_exit_gfm_autolink_literal_mailto(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_autolink_literal_mailto(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, none, SliceBytes, true),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmAutolinkLiteralProtocol`][Name::GfmAutolinkLiteralProtocol].
""".
-spec on_exit_gfm_autolink_literal_protocol(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_autolink_literal_protocol(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, none, SliceBytes, true),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmAutolinkLiteralWww`][Name::GfmAutolinkLiteralWww].
""".
-spec on_exit_gfm_autolink_literal_www(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_autolink_literal_www(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, {some, <<"http://">>}, SliceBytes, true),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmAutolinkLiteralXmpp`][Name::GfmAutolinkLiteralXmpp].
""".
-spec on_exit_gfm_autolink_literal_xmpp(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_autolink_literal_xmpp(
    CompileContext1 = #markdown_html_compile_context{bytes = Bytes, events = Events, index = Index}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    CompileContext2 = generate_autolink(CompileContext1, none, SliceBytes, true),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmFootnoteCall`][Name::GfmFootnoteCall].
""".
-spec on_exit_gfm_footnote_call(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_footnote_call(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes,
        gfm_footnote_definition_calls = GfmFootnoteDefinitionCalls1,
        image_alt_inside = ImageAltInside,
        media_stack = MediaStack1,
        options = #markdown_compile_options{gfm_footnote_clobber_prefix = OptionGfmFootnoteClobberPrefix}
    }
) ->
    {MediaStack2, Media} = markdown_vec:pop(MediaStack1),
    {some, LabelId} = Media#markdown_html_media.label_id,
    Slice = markdown_slice:from_indices(Bytes, LabelId),
    Id = markdown_util_normalize_identifier:normalize_identifier(markdown_slice:as_binary(Slice)),
    SafeId = markdown_util_sanitize_uri:sanitize(string:lowercase(Id)),

    %% See if this has been called before.
    CallIndex = on_exit_gfm_footnote_call__find_call_index(GfmFootnoteDefinitionCalls1, Id, 0),

    %% Update or add the footnote call.
    GfmFootnoteDefinitionCalls2 =
        case CallIndex < markdown_vec:size(GfmFootnoteDefinitionCalls1) of
            true ->
                %% Increment existing call count.
                markdown_vec:update(GfmFootnoteDefinitionCalls1, CallIndex, fun({CallId, Count}) ->
                    {CallId, Count + 1}
                end);
            false ->
                %% New footnote call.
                markdown_vec:push(GfmFootnoteDefinitionCalls1, {Id, 1})
        end,

    CompileContext2 = CompileContext1#markdown_html_compile_context{
        gfm_footnote_definition_calls = GfmFootnoteDefinitionCalls2,
        media_stack = MediaStack2
    },

    %% No call is output in an image alt, though the definition and
    %% backreferences are generated as if it was the case.
    case ImageAltInside of
        true ->
            CompileContext2;
        false ->
            {_CallId, CallCount} = markdown_vec:get(GfmFootnoteDefinitionCalls2, CallIndex),
            CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<sup><a href=\"#">>),
            CompileContext4 =
                case OptionGfmFootnoteClobberPrefix of
                    {some, GfmFootnoteClobberPrefix1} ->
                        EncodedGfmFootnoteClobberPrefix1 = markdown_util_encode:encode(
                            GfmFootnoteClobberPrefix1, CompileContext3#markdown_html_compile_context.encode_html
                        ),
                        markdown_html_compile_context:push(CompileContext3, EncodedGfmFootnoteClobberPrefix1);
                    none ->
                        markdown_html_compile_context:push(CompileContext3, <<"user-content-">>)
                end,
            CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"fn-">>),
            CompileContext6 = markdown_html_compile_context:push(CompileContext5, SafeId),
            CompileContext7 = markdown_html_compile_context:push(CompileContext6, <<"\" id=\"">>),
            CompileContext8 =
                case OptionGfmFootnoteClobberPrefix of
                    {some, GfmFootnoteClobberPrefix2} ->
                        EncodedGfmFootnoteClobberPrefix2 = markdown_util_encode:encode(
                            GfmFootnoteClobberPrefix2, CompileContext7#markdown_html_compile_context.encode_html
                        ),
                        markdown_html_compile_context:push(CompileContext7, EncodedGfmFootnoteClobberPrefix2);
                    none ->
                        markdown_html_compile_context:push(CompileContext7, <<"user-content-">>)
                end,
            CompileContext9 = markdown_html_compile_context:push(CompileContext8, <<"fnref-">>),
            CompileContext10 = markdown_html_compile_context:push(CompileContext9, SafeId),
            CompileContext11 =
                case CallCount > 1 of
                    true ->
                        CompileContext10_1 = markdown_html_compile_context:push(CompileContext10, <<"-">>),
                        CompileContext10_2 = markdown_html_compile_context:push(
                            CompileContext10_1, erlang:integer_to_binary(CallCount)
                        ),
                        CompileContext10_2;
                    false ->
                        CompileContext10
                end,
            CompileContext12 = markdown_html_compile_context:push(
                CompileContext11, <<"\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">">>
            ),
            CompileContext13 = markdown_html_compile_context:push(
                CompileContext12, erlang:integer_to_binary(CallIndex + 1)
            ),
            CompileContext14 = markdown_html_compile_context:push(CompileContext13, <<"</a></sup>">>),
            CompileContext14
    end.

%% @private
-spec on_exit_gfm_footnote_call__find_call_index(GfmFootnoteDefinitionCalls, Id, Index) -> Index when
    GfmFootnoteDefinitionCalls :: markdown_vec:t({Id, Count}),
    Id :: binary(),
    Index :: markdown_vec:index(),
    Count :: non_neg_integer().
on_exit_gfm_footnote_call__find_call_index(GfmFootnoteDefinitionCalls, Id, Index) when
    Index < ?markdown_vec_size(GfmFootnoteDefinitionCalls)
->
    case markdown_vec:get(GfmFootnoteDefinitionCalls, Index) of
        {Id, _Count} ->
            Index;
        {_CallId, _Count} ->
            on_exit_gfm_footnote_call__find_call_index(GfmFootnoteDefinitionCalls, Id, Index + 1)
    end;
on_exit_gfm_footnote_call__find_call_index(_GfmFootnoteDefinitionCalls, _Id, Index) ->
    Index.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmFootnoteDefinitionLabelString`][Name::GfmFootnoteDefinitionLabelString].
""".
-spec on_exit_gfm_footnote_definition_label_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_footnote_definition_label_string(
    CompileContext1 = #markdown_html_compile_context{
        events = Events,
        gfm_footnote_definition_stack = GfmFootnoteDefinitionStack1,
        index = Index
    }
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Indices = markdown_position:to_indices(Position),
    GfmFootnoteDefinitionStack2 = markdown_vec:push(GfmFootnoteDefinitionStack1, Indices),
    CompileContext2 = CompileContext1#markdown_html_compile_context{
        gfm_footnote_definition_stack = GfmFootnoteDefinitionStack2
    },
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmFootnoteDefinitionPrefix`][Name::GfmFootnoteDefinitionPrefix].
""".
-spec on_exit_gfm_footnote_definition_prefix(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_footnote_definition_prefix(CompileContext1 = #markdown_html_compile_context{}) ->
    %% Drop the prefix.
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    %% Capture everything until end of definition.
    CompileContext3 = markdown_html_compile_context:buffer(CompileContext2),
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmFootnoteDefinition`][Name::GfmFootnoteDefinition].
""".
-spec on_exit_gfm_footnote_definition(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_footnote_definition(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes,
        gfm_footnote_definition_stack = GfmFootnoteDefinitionStack1,
        gfm_footnote_definitions = GfmFootnoteDefinitions1,
        tight_stack = TightStack1
    }
) ->
    {CompileContext2, Value} = markdown_html_compile_context:resume(CompileContext1),
    {GfmFootnoteDefinitionStack2, Indices} = markdown_vec:pop(GfmFootnoteDefinitionStack1),
    {TightStack2, _} = markdown_vec:pop(TightStack1),
    Slice = markdown_slice:from_indices(Bytes, Indices),
    Id = markdown_util_normalize_identifier:normalize_identifier(markdown_slice:as_binary(Slice)),
    GfmFootnoteDefinitions2 = markdown_vec:push(GfmFootnoteDefinitions1, {Id, Value}),
    CompileContext3 = CompileContext2#markdown_html_compile_context{
        gfm_footnote_definition_stack = GfmFootnoteDefinitionStack2,
        gfm_footnote_definitions = GfmFootnoteDefinitions2,
        tight_stack = TightStack2
    },
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmStrikethrough`][Name::GfmStrikethrough].
""".
-spec on_exit_gfm_strikethrough(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_strikethrough(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"</del>"/utf8>>),
    CompileContext2;
on_exit_gfm_strikethrough(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTable`][Name::GfmTable].
""".
-spec on_exit_gfm_table(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_table(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{gfm_table_align = none},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"</table>"/utf8>>),
    CompileContext4.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTableBody`][Name::GfmTableBody].
""".
-spec on_exit_gfm_table_body(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_table_body(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"</tbody>"/utf8>>),
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTableCell`][Name::GfmTableCell].
""".
-spec on_exit_gfm_table_cell(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_table_cell(
    CompileContext1 = #markdown_html_compile_context{
        gfm_table_column = Column,
        gfm_table_align = {some, Align}
    }
) ->
    CompileContext2 =
        case Column < ?markdown_vec_size(Align) of
            true ->
                case CompileContext1#markdown_html_compile_context.gfm_table_in_head of
                    true ->
                        markdown_html_compile_context:push(CompileContext1, <<"</th>"/utf8>>);
                    false ->
                        markdown_html_compile_context:push(CompileContext1, <<"</td>"/utf8>>)
                end;
            false ->
                %% Stop capturing.
                {CompileContext1_1, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
                CompileContext1_1
        end,
    CompileContext3 = CompileContext2#markdown_html_compile_context{gfm_table_column = Column + 1},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTableHead`][Name::GfmTableHead].
""".
-spec on_exit_gfm_table_head(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_table_head(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{gfm_table_in_head = false},
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"</thead>"/utf8>>),
    CompileContext4.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTableRow`][Name::GfmTableRow].
""".
-spec on_exit_gfm_table_row(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_table_row(
    CompileContext1 = #markdown_html_compile_context{
        gfm_table_column = Column,
        gfm_table_align = {some, Align}
    }
) ->
    %% Add "phantom" cells, for body rows that are shorter than the delimiter
    %% row (which is equal to the head row).
    CompileContext2 = on_exit_gfm_table_row__phantom_cells(CompileContext1, Column, ?markdown_vec_size(Align)),
    CompileContext3 = CompileContext2#markdown_html_compile_context{gfm_table_column = 0},
    CompileContext4 = markdown_html_compile_context:line_ending_if_needed(CompileContext3),
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"</tr>"/utf8>>),
    CompileContext5.

%% @private
-spec on_exit_gfm_table_row__phantom_cells(CompileContext, Column, Len) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    Column :: non_neg_integer(),
    Len :: non_neg_integer().
on_exit_gfm_table_row__phantom_cells(CompileContext1 = #markdown_html_compile_context{}, Column, Len) when
    Column < Len
->
    CompileContext2 = on_enter_gfm_table_cell(CompileContext1),
    CompileContext3 = on_exit_gfm_table_cell(CompileContext2),
    on_exit_gfm_table_row__phantom_cells(CompileContext3, Column + 1, Len);
on_exit_gfm_table_row__phantom_cells(CompileContext, _Column, _Len) ->
    CompileContext.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTaskListItemCheck`][Name::GfmTaskListItemCheck].
""".
-spec on_exit_gfm_task_list_item_check(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_task_list_item_check(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"/>"/utf8>>),
    CompileContext2;
on_exit_gfm_task_list_item_check(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`GfmTaskListItemValueChecked`][Name::GfmTaskListItemValueChecked].
""".
-spec on_exit_gfm_task_list_item_value_checked(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_gfm_task_list_item_value_checked(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"checked=\"\" "/utf8>>),
    CompileContext2;
on_exit_gfm_task_list_item_value_checked(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingAtx`][Name::HeadingAtx].
""".
-spec on_exit_heading_atx(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_heading_atx(CompileContext1 = #markdown_html_compile_context{heading_atx_rank = {some, Rank}}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{heading_atx_rank = none},
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"</h">>),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, erlang:integer_to_binary(Rank)),
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<">">>),
    CompileContext5.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingAtxSequence`][Name::HeadingAtxSequence].
""".
-spec on_exit_heading_atx_sequence(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_heading_atx_sequence(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, events = Events, heading_atx_rank = none, index = Index
    }
) ->
    %% First fence we see.
    Rank = markdown_slice:size(markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index))),
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = CompileContext2#markdown_html_compile_context{heading_atx_rank = {some, Rank}},
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"<h">>),
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, erlang:integer_to_binary(Rank)),
    CompileContext6 = markdown_html_compile_context:push(CompileContext5, <<">">>),
    CompileContext6;
on_exit_heading_atx_sequence(CompileContext1 = #markdown_html_compile_context{heading_atx_rank = {some, _}}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingAtxText`][Name::HeadingAtxText].
""".
-spec on_exit_heading_atx_text(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_heading_atx_text(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, Value} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, Value),
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingSetextText`][Name::HeadingSetextText].
""".
-spec on_exit_heading_setext_text(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_heading_setext_text(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, Value} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext3 = CompileContext2#markdown_html_compile_context{
        heading_setext_buffer = {some, Value},
        slurp_one_line_ending = true
    },
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`HeadingSetextUnderlineSequence`][Name::HeadingSetextUnderlineSequence].
""".
-spec on_exit_heading_setext_underline_sequence(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_heading_setext_underline_sequence(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, events = Events, heading_setext_buffer = {some, Text}, index = Index
    }
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{heading_setext_buffer = none},
    Position = markdown_position:from_exit_event(Events, Index),
    Head = binary:at(Bytes, Position#markdown_position.start#markdown_point.offset),
    Rank =
        case Head =:= $- of
            true -> <<"2">>;
            false -> <<"1">>
        end,
    CompileContext3 = markdown_html_compile_context:line_ending_if_needed(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(
        CompileContext3, <<"<h", Rank/bytes, ">", Text/bytes, "</h", Rank/bytes, ">">>
    ),
    CompileContext4.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`HtmlFlow`][Name::HtmlFlow],[`HtmlText`][Name::HtmlText]}.
""".
-spec on_exit_html(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_html(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{encode_html = true},
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`HtmlFlowData`][Name::HtmlFlowData],[`HtmlTextData`][Name::HtmlTextData]}.
""".
-spec on_exit_html_data(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_html_data(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes,
        encode_html = EncodeHtml,
        events = Events,
        index = Index,
        options = #markdown_compile_options{allow_dangerous_html = AllowDangerousHtml, gfm_tagfilter = GfmTagfilter}
    }
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    Value = markdown_slice:as_binary(Slice),
    EncodedValue =
        case GfmTagfilter andalso AllowDangerousHtml of
            true ->
                GfmTagfilteredValue = markdown_util_gfm_tagfilter:gfm_tagfilter(Value),
                markdown_util_encode:encode(GfmTagfilteredValue, EncodeHtml);
            false ->
                markdown_util_encode:encode(Value, EncodeHtml)
        end,
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, EncodedValue),
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Label`][Name::Label].
""".
-spec on_exit_label(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_label(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, Buffer} = markdown_html_compile_context:resume(CompileContext1),
    #markdown_html_compile_context{media_stack = MediaStack1} = CompileContext2,
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(Media) ->
        Media#markdown_html_media{label = {some, Buffer}}
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`LabelText`][Name::LabelText].
""".
-spec on_exit_label_text(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_label_text(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, media_stack = MediaStack1}
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Indices = markdown_position:to_indices(Position),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(Media) ->
        Media#markdown_html_media{label_id = {some, Indices}}
    end),
    CompileContext2 = CompileContext1#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext2.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`LineEnding`][Name::LineEnding].
""".
-spec on_exit_line_ending(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_line_ending(CompileContext1 = #markdown_html_compile_context{raw_text_inside = true}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<" ">>),
    CompileContext2;
on_exit_line_ending(CompileContext1 = #markdown_html_compile_context{slurp_one_line_ending = true}) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{slurp_one_line_ending = false},
    CompileContext2;
on_exit_line_ending(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, encode_html = EncodeHtml, events = Events, index = Index
    }
) ->
    case (Index > 1) andalso markdown_vec:get(Events, Index - 2) of
        #markdown_event{name = Name} when Name =:= 'definition' orelse Name =:= 'gfm_footnote_definition' ->
            CompileContext2 = CompileContext1#markdown_html_compile_context{slurp_one_line_ending = false},
            CompileContext2;
        _ ->
            Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
            SliceBytes = markdown_slice:as_binary(Slice),
            Value = markdown_util_encode:encode(SliceBytes, EncodeHtml),
            CompileContext2 = markdown_html_compile_context:push(CompileContext1, Value),
            CompileContext2
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`ListOrdered`][Name::ListOrdered],[`ListUnordered`][Name::ListUnordered]}.
""".
-spec on_exit_list(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_list(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, tight_stack = TightStack1}
) ->
    {TightStack2, _} = markdown_vec:pop_option(TightStack1),
    CompileContext2 = CompileContext1#markdown_html_compile_context{tight_stack = TightStack2},
    CompileContext3 = markdown_html_compile_context:line_ending(CompileContext2),
    CompileContext4 = markdown_html_compile_context:push(
        CompileContext3,
        case markdown_vec:get(Events, Index) of
            #markdown_event{name = 'list_ordered'} ->
                <<"</ol>">>;
            _ ->
                <<"</ul>">>
        end
    ),
    CompileContext4.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ListItem`][Name::ListItem].
""".
-spec on_exit_list_item(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_list_item(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, tight_stack = TightStack}
) ->
    Tight =
        case markdown_vec:last_option(TightStack) of
            none ->
                false;
            {some, TightLast} ->
                TightLast
        end,
    BeforeItem = markdown_util_skip:opt_back(Events, Index - 1, [
        blank_line_ending,
        block_quote_prefix,
        line_ending,
        space_or_tab,
        %% Also ignore things that don't contribute to the document.
        definition,
        gfm_footnote_definition
    ]),
    Previous = markdown_vec:get(Events, BeforeItem),
    TightParagraph = Tight andalso Previous#markdown_event.name =:= paragraph,
    EmptyItem = Previous#markdown_event.name =:= list_item_prefix,
    CompileContext2 = CompileContext1#markdown_html_compile_context{slurp_one_line_ending = false},
    CompileContext3 =
        case not TightParagraph andalso not EmptyItem of
            true ->
                markdown_html_compile_context:line_ending_if_needed(CompileContext2);
            false ->
                CompileContext2
        end,
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"</li>">>),
    CompileContext4.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ListItemValue`][Name::ListItemValue].
""".
-spec on_exit_list_item_value(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_list_item_value(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, events = Events, index = Index, list_expect_first_marker = {some, ListExpectFirstMarker}
    }
) ->
    case ListExpectFirstMarker of
        true ->
            Slice = markdown_slice:from_position(Bytes, markdown_position:from_exit_event(Events, Index)),
            SliceBytes = markdown_slice:as_binary(Slice),
            Value = erlang:binary_to_integer(SliceBytes),
            case Value =/= 1 of
                true ->
                    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<" start=\"">>),
                    CompileContext3 = markdown_html_compile_context:push(
                        CompileContext2, erlang:integer_to_binary(Value)
                    ),
                    CompileContext4 = markdown_html_compile_context:push(CompileContext3, <<"\"">>),
                    CompileContext4;
                false ->
                    CompileContext1
            end;
        false ->
            CompileContext1
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`Image`][Name::Image],[`Link`][Name::Link]}.
""".
-spec on_exit_media(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_media(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, definitions = Definitions, media_stack = MediaStack1
    }
) ->
    %% Skip current.
    End = markdown_vec:size(MediaStack1) - 1,
    IsInImage = markdown_vec:reduce_while(MediaStack1, false, fun(Index, MediaEntry, IsInImageAcc) ->
        case Index < End of
            true ->
                case MediaEntry of
                    #markdown_html_media{image = true} ->
                        {halt, true};
                    _ ->
                        {cont, IsInImageAcc}
                end;
            false ->
                {halt, IsInImageAcc}
        end
    end),
    {MediaStack2, Media} = markdown_vec:pop(MediaStack1),
    #markdown_html_media{label = {some, Label}} = Media,
    CompileContext2 = CompileContext1#markdown_html_compile_context{
        image_alt_inside = IsInImage, media_stack = MediaStack2
    },
    OptionId =
        case Media of
            #markdown_html_media{reference_id = {some, ReferenceId}} ->
                ReferenceIdSlice = markdown_slice:from_indices(Bytes, ReferenceId),
                {some,
                    markdown_util_normalize_identifier:normalize_identifier(markdown_slice:as_binary(ReferenceIdSlice))};
            #markdown_html_media{label_id = {some, LabelId}} ->
                LabelIdSlice = markdown_slice:from_indices(Bytes, LabelId),
                {some, markdown_util_normalize_identifier:normalize_identifier(markdown_slice:as_binary(LabelIdSlice))};
            _ ->
                none
        end,
    OptionDestinationIndex =
        case Media of
            #markdown_html_media{destination = none} ->
                markdown_option:map(OptionId, fun(Id) ->
                    OptionIndex = markdown_vec:reduce_while(Definitions, none, fun(Index, Definition, OptionIndexAcc) ->
                        case Definition#markdown_html_definition.id =:= Id of
                            true ->
                                {halt, {some, Index}};
                            false ->
                                {cont, OptionIndexAcc}
                        end
                    end),
                    case OptionIndex of
                        {some, Index} -> Index
                    end
                end);
            _ ->
                none
        end,
    CompileContext3 = on_exit_media__maybe_open_tag(CompileContext2, IsInImage, Media, OptionDestinationIndex),
    CompileContext4 =
        case Media#markdown_html_media.image of
            true ->
                markdown_html_compile_context:push(CompileContext3, Label);
            false ->
                CompileContext3
        end,
    CompileContext5 = on_exit_media__maybe_close_tag(CompileContext4, IsInImage, Media, OptionDestinationIndex),
    CompileContext6 =
        case not Media#markdown_html_media.image of
            true ->
                CompileContext5_1 = CompileContext5,
                CompileContext5_2 = markdown_html_compile_context:push(CompileContext5_1, Label),
                case not IsInImage of
                    true ->
                        markdown_html_compile_context:push(CompileContext5_2, <<"</a>">>);
                    false ->
                        CompileContext5_2
                end;
            false ->
                CompileContext5
        end,
    CompileContext6.

%% @private
-spec on_exit_media__maybe_close_tag(CompileContext, IsInImage, Media, OptionDestinationIndex) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    IsInImage :: boolean(),
    Media :: markdown_html_media:t(),
    OptionDestinationIndex :: markdown_option:t(DestinationIndex),
    DestinationIndex :: markdown_vec:index().
on_exit_media__maybe_close_tag(
    CompileContext1 = #markdown_html_compile_context{
        definitions = Definitions
    },
    false,
    Media,
    OptionDestinationIndex
) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"\"">>),
    OptionTitle =
        case OptionDestinationIndex of
            {some, DestinationIndex} ->
                (markdown_vec:get(Definitions, DestinationIndex))#markdown_html_definition.title;
            none ->
                Media#markdown_html_media.title
        end,
    CompileContext3 =
        case OptionTitle of
            {some, Title} ->
                CompileContext2_1 = CompileContext2,
                CompileContext2_2 = markdown_html_compile_context:push(CompileContext2_1, <<" title=\"">>),
                CompileContext2_3 = markdown_html_compile_context:push(CompileContext2_2, Title),
                CompileContext2_4 = markdown_html_compile_context:push(CompileContext2_3, <<"\"">>),
                CompileContext2_4;
            none ->
                CompileContext2
        end,
    CompileContext4 =
        case Media#markdown_html_media.image of
            true -> markdown_html_compile_context:push(CompileContext3, <<" /">>);
            false -> CompileContext3
        end,
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<">">>),
    CompileContext5;
on_exit_media__maybe_close_tag(
    CompileContext1 = #markdown_html_compile_context{}, true, _Media, _OptionDestinationIndex
) ->
    CompileContext1.

%% @private
-spec on_exit_media__maybe_open_tag(CompileContext, IsInImage, Media, OptionDestinationIndex) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t(),
    IsInImage :: boolean(),
    Media :: markdown_html_media:t(),
    OptionDestinationIndex :: markdown_option:t(DestinationIndex),
    DestinationIndex :: markdown_vec:index().
on_exit_media__maybe_open_tag(
    CompileContext1 = #markdown_html_compile_context{
        definitions = Definitions,
        options = #markdown_compile_options{
            allow_any_img_src = AllowAnyImgSrc, allow_dangerous_protocol = AllowDangerousProtocol
        }
    },
    false,
    Media,
    OptionDestinationIndex
) ->
    CompileContext2 =
        case Media of
            #markdown_html_media{image = true} ->
                markdown_html_compile_context:push(CompileContext1, <<"<img src=\"">>);
            _ ->
                markdown_html_compile_context:push(CompileContext1, <<"<a href=\"">>)
        end,
    OptionDestination =
        case OptionDestinationIndex of
            {some, DestinationIndex} ->
                (markdown_vec:get(Definitions, DestinationIndex))#markdown_html_definition.destination;
            none ->
                Media#markdown_html_media.destination
        end,
    CompileContext3 =
        case OptionDestination of
            {some, Destination} ->
                AllowDangerous = AllowDangerousProtocol orelse (AllowAnyImgSrc andalso Media#markdown_html_media.image),
                Url =
                    case AllowDangerous of
                        true ->
                            markdown_util_sanitize_uri:sanitize(Destination);
                        false ->
                            markdown_util_sanitize_uri:sanitize_with_protocols(
                                Destination,
                                case Media#markdown_html_media.image of
                                    true -> ?SAFE_PROTOCOL_SRC;
                                    false -> ?SAFE_PROTOCOL_HREF
                                end
                            )
                    end,
                markdown_html_compile_context:push(CompileContext2, Url);
            none ->
                CompileContext2
        end,
    CompileContext4 =
        case Media#markdown_html_media.image of
            true -> markdown_html_compile_context:push(CompileContext3, <<"\" alt=\"">>);
            false -> CompileContext3
        end,
    CompileContext4;
on_exit_media__maybe_open_tag(
    CompileContext1 = #markdown_html_compile_context{}, true, _Media, _OptionDestinationIndex
) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Paragraph`][Name::Paragraph].
""".
-spec on_exit_paragraph(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_paragraph(CompileContext1 = #markdown_html_compile_context{tight_stack = TightStack}) ->
    Tight =
        case markdown_vec:last_option(TightStack) of
            none ->
                false;
            {some, TightLast} ->
                TightLast
        end,
    case Tight of
        true ->
            CompileContext2 = CompileContext1#markdown_html_compile_context{slurp_one_line_ending = true},
            CompileContext2;
        false ->
            CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"</p>">>),
            CompileContext2
    end.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFenced`][Name::CodeFenced],[`CodeIndented`][Name::CodeIndented],[`MathFlow`][Name::MathFlow]}.
""".
-spec on_exit_raw_flow(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_raw_flow(CompileContext1 = #markdown_html_compile_context{}) ->
    %% One special case is if we are inside a container, and the raw (flow) was
    %% not closed (meaning it runs to the end).
    %% In that case, the following line ending, is considered *outside* the
    %% fenced code and block quote by `markdown-rs`, but CM wants to treat that
    %% ending as part of the code.
    CompileContext2 = on_exit_raw_flow__maybe_run_to_end(CompileContext1),
    %% But in most cases, it's simpler: when we've seen some data, emit an extra
    %% line ending when needed.
    #markdown_html_compile_context{raw_flow_seen_data = {some, RawFlowSeenData}} = CompileContext2,
    CompileContext3 = CompileContext2#markdown_html_compile_context{raw_flow_seen_data = none},
    CompileContext4 =
        case RawFlowSeenData of
            true ->
                markdown_html_compile_context:line_ending_if_needed(CompileContext3);
            false ->
                CompileContext3
        end,
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"</code></pre>">>),
    CompileContext6 =
        case CompileContext5 of
            #markdown_html_compile_context{raw_flow_fences_count = {some, Count}} ->
                CompileContext5_1 = CompileContext5,
                CompileContext5_2 = CompileContext5_1#markdown_html_compile_context{raw_flow_fences_count = none},
                case Count < 2 of
                    true ->
                        markdown_html_compile_context:line_ending_if_needed(CompileContext5_2);
                    false ->
                        CompileContext5_2
                end;
            _ ->
                CompileContext5
        end,
    CompileContext7 = CompileContext6#markdown_html_compile_context{slurp_one_line_ending = false},
    CompileContext7.

%% @private
-spec on_exit_raw_flow__maybe_run_to_end(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_raw_flow__maybe_run_to_end(
    CompileContext1 = #markdown_html_compile_context{
        events = Events, index = Index, raw_flow_fences_count = {some, Count}, tight_stack = TightStack
    }
) ->
    %% No closing fence.
    case
        Count =:= 1 andalso
            %% In a container.
            (not markdown_vec:is_empty(TightStack)) andalso
            %% Empty (as the closing is right at the opening fence)
            (not lists:member((markdown_vec:get(Events, Index - 1))#markdown_event.name, [
                code_fenced_fence, math_flow_fence
            ]))
    of
        true ->
            CompileContext2 = markdown_html_compile_context:line_ending(CompileContext1),
            CompileContext2;
        false ->
            CompileContext1
    end;
on_exit_raw_flow__maybe_run_to_end(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFlowChunk`][Name::CodeFlowChunk],[`MathFlowChunk`][Name::MathFlowChunk]}.
""".
-spec on_exit_raw_flow_chunk(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_raw_flow_chunk(
    CompileContext1 = #markdown_html_compile_context{
        bytes = Bytes, events = Events, index = Index, encode_html = EncodeHtml
    }
) ->
    CompileContext2 = CompileContext1#markdown_html_compile_context{raw_flow_seen_data = {some, true}},
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    %% Must serialize to get virtual spaces.
    Serialized = markdown_slice:serialize(Slice),
    Encoded = markdown_util_encode:encode(Serialized, EncodeHtml),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, Encoded),
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeFencedFence`][Name::CodeFencedFence],[`MathFlowFence`][Name::MathFlowFence]}.
""".
-spec on_exit_raw_flow_fence(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_raw_flow_fence(CompileContext1 = #markdown_html_compile_context{raw_flow_fences_count = {some, Count}}) ->
    CompileContext2 =
        case Count =:= 0 of
            true ->
                CompileContext1_1 = CompileContext1,
                CompileContext1_2 = markdown_html_compile_context:push(CompileContext1_1, <<">">>),
                CompileContext1_3 = CompileContext1_2#markdown_html_compile_context{slurp_one_line_ending = true},
                CompileContext1_3;
            false ->
                CompileContext1
        end,
    CompileContext3 = CompileContext2#markdown_html_compile_context{raw_flow_fences_count = {some, Count + 1}},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`CodeFencedFenceInfo`][Name::CodeFencedFenceInfo].

Note: math (flow) does not support `info`.
""".
-spec on_exit_raw_flow_fence_info(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_raw_flow_fence_info(CompileContext1 = #markdown_html_compile_context{}) ->
    {CompileContext2, Value} = markdown_html_compile_context:resume(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<" class=\"language-">>),
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, Value),
    CompileContext5 = markdown_html_compile_context:push(CompileContext4, <<"\"">>),
    CompileContext5.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:{[`CodeText`][Name::CodeText],[`MathText`][Name::MathText]}.
""".
-spec on_exit_raw_text(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_raw_text(
    CompileContext1 = #markdown_html_compile_context{
        gfm_table_align = OptionGfmTableAlign, image_alt_inside = ImageAltInside
    }
) ->
    {CompileContext2, Bytes1} = markdown_html_compile_context:resume(CompileContext1),
    %% TODO: share with `to_mdast`.
    %% If we are in a GFM table, we need to decode escaped pipes.
    %% This is a rather weird GFM feature.
    Bytes2 =
        case OptionGfmTableAlign of
            {some, _} -> on_exit_raw_text__gfm_table(Bytes1, <<>>);
            none -> Bytes1
        end,
    Trim1 = false,
    Index1 = 0,
    End1 = byte_size(Bytes2),
    Bytes3 =
        case End1 > 2 andalso binary:at(Bytes2, Index1) =:= $\s andalso binary:at(Bytes2, End1 - 1) =:= $\s of
            true ->
                Index2 = Index1 + 1,
                End2 = End1 - 1,
                on_exit_raw_text__trim(Bytes2, Trim1, Index2, End2);
            false ->
                Bytes2
        end,
    CompileContext3 = CompileContext2#markdown_html_compile_context{raw_text_inside = false},
    CompileContext4 = markdown_html_compile_context:push(CompileContext3, Bytes3),
    CompileContext5 =
        case not ImageAltInside of
            true ->
                markdown_html_compile_context:push(CompileContext4, <<"</code>">>);
            false ->
                CompileContext4
        end,
    CompileContext5.

%% @private
-spec on_exit_raw_text__gfm_table(Input, Output) -> Output when Input :: binary(), Output :: binary().
on_exit_raw_text__gfm_table(<<"\\|", Input/bytes>>, Output1) ->
    Output2 = <<Output1/bytes, "|">>,
    on_exit_raw_text__gfm_table(Input, Output2);
on_exit_raw_text__gfm_table(<<C/utf8, Input/bytes>>, Output1) ->
    Output2 = <<Output1/bytes, C/utf8>>,
    on_exit_raw_text__gfm_table(Input, Output2);
on_exit_raw_text__gfm_table(<<>>, Output) ->
    Output.

%% @private
-spec on_exit_raw_text__trim(Input, Trim, Index, End) -> Output when
    Input :: binary(), Trim :: boolean(), Index :: non_neg_integer(), End :: non_neg_integer(), Output :: binary().
on_exit_raw_text__trim(Input, Trim1 = false, Index1, End) when Index1 < End ->
    case binary:at(Input, Index1) =:= $\s of
        true ->
            Index2 = Index1 + 1,
            on_exit_raw_text__trim(Input, Trim1, Index2, End);
        false ->
            %% Found non-space, we should trim
            Trim2 = true,
            Index2 = Index1 + 1,
            on_exit_raw_text__trim(Input, Trim2, Index2, End)
    end;
on_exit_raw_text__trim(Input, _Trim = false, _Index, _End) ->
    %% All spaces between first and last, don't trim
    Output = Input,
    Output;
on_exit_raw_text__trim(Input, _Trim = true, _Index, _End) ->
    %% Found non-space content, trim first and last spaces
    Output = binary:part(Input, 1, byte_size(Input) - 2),
    Output.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ReferenceString`][Name::ReferenceString].
""".
-spec on_exit_reference_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_reference_string(
    CompileContext1 = #markdown_html_compile_context{events = Events, index = Index, media_stack = MediaStack1}
) ->
    %% Drop stuff.
    {CompileContext2, _Buffer} = markdown_html_compile_context:resume(CompileContext1),
    ReferenceIdPosition = markdown_position:from_exit_event(Events, Index),
    ReferenceId = markdown_position:to_indices(ReferenceIdPosition),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            reference_id = {some, ReferenceId}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ResourceDestinationString`][Name::ResourceDestinationString].
""".
-spec on_exit_resource_destination_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_resource_destination_string(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    {CompileContext2, Buffer} = markdown_html_compile_context:resume(CompileContext1),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            destination = {some, Buffer}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{encode_html = true, media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ResourceTitleString`][Name::ResourceTitleString].
""".
-spec on_exit_resource_title_string(CompileContext) -> CompileContext when
    CompileContext :: markdown_html_compile_context:t().
on_exit_resource_title_string(CompileContext1 = #markdown_html_compile_context{media_stack = MediaStack1}) ->
    {CompileContext2, Buffer} = markdown_html_compile_context:resume(CompileContext1),
    MediaStack2 = markdown_vec:update_last(MediaStack1, fun(MediaEntry) ->
        MediaEntry#markdown_html_media{
            title = {some, Buffer}
        }
    end),
    CompileContext3 = CompileContext2#markdown_html_compile_context{media_stack = MediaStack2},
    CompileContext3.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`Strong`][Name::Strong].
""".
-spec on_exit_strong(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_strong(CompileContext1 = #markdown_html_compile_context{image_alt_inside = false}) ->
    CompileContext2 = markdown_html_compile_context:push(CompileContext1, <<"</strong>">>),
    CompileContext2;
on_exit_strong(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext1.

%% @private
-doc """
Handle [`Exit`][Kind::Exit]:[`ThematicBreak`][Name::ThematicBreak].
""".
-spec on_exit_thematic_break(CompileContext) -> CompileContext when CompileContext :: markdown_html_compile_context:t().
on_exit_thematic_break(CompileContext1 = #markdown_html_compile_context{}) ->
    CompileContext2 = markdown_html_compile_context:line_ending_if_needed(CompileContext1),
    CompileContext3 = markdown_html_compile_context:push(CompileContext2, <<"<hr />">>),
    CompileContext3.
