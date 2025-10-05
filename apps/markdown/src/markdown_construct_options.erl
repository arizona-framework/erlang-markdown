%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_construct_options).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    commonmark/0,
    commonmark/1,
    default/0,
    default/1,
    gfm/0,
    gfm/1,
    mdx/0,
    mdx/1,
    new/1,
    new/2,
    put/3,
    put/4
]).

%% Types
-type castable() ::
    castable_keyword()
    | castable_map().
-type castable_keyword() :: [{key(), boolean()}].
-type castable_map() :: #{key() => boolean()}.
-type config() :: #{extend := extend()}.
-type extend() :: commonmark | default | gfm | mdx | t().
-type key() ::
    attention
    | attribute_list_flow
    | attribute_list_text
    | autolink
    | block_quote
    | character_escape
    | character_reference
    | code_indented
    | code_fenced
    | code_text
    | definition
    % | exp_attribute_list
    % | exp_kramdown_attribute_list
    | frontmatter
    | gfm_autolink_literal
    | gfm_footnote_definition
    | gfm_label_start_footnote
    | gfm_strikethrough
    | gfm_table
    | gfm_task_list_item
    | hard_break_escape
    | hard_break_trailing
    | heading_atx
    | heading_setext
    | html_flow
    | html_text
    | label_start_image
    | label_start_link
    | label_end
    | list_item
    | math_flow
    | math_text
    | mdx_esm
    | mdx_expression_flow
    | mdx_expression_text
    | mdx_jsx_flow
    | mdx_jsx_text
    | thematic_break.
-type t() :: #markdown_construct_options{}.

-export_type([
    castable/0,
    castable_keyword/0,
    castable_map/0,
    config/0,
    extend/0,
    key/0,
    t/0
]).

%% Macros
-define(DEFAULT_CONFIG, #{extend => default}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-compile({inline, [commonmark/0]}).
-spec commonmark() -> Constructs when Constructs :: t().
commonmark() ->
    #markdown_construct_options{
        attention = true,
        attribute_list_flow = false,
        attribute_list_text = false,
        autolink = true,
        block_quote = true,
        character_escape = true,
        character_reference = true,
        code_indented = true,
        code_fenced = true,
        code_text = true,
        definition = true,
        % exp_attribute_list = false,
        % exp_kramdown_attribute_list = false,
        frontmatter = false,
        % TODO: need IAL related functionality from `kramdown` https://kramdown.gettalong.org/syntax.html#block-ials
        %       would need to convince José if we wanted to explore using MDX instead of IAL
        gfm_autolink_literal = false,
        gfm_label_start_footnote = false,
        gfm_footnote_definition = false,
        gfm_strikethrough = false,
        gfm_table = false,
        gfm_task_list_item = false,
        hard_break_escape = true,
        hard_break_trailing = true,
        heading_atx = true,
        heading_setext = true,
        html_flow = true,
        html_text = true,
        label_start_image = true,
        label_start_link = true,
        label_end = true,
        list_item = true,
        math_flow = false,
        math_text = false,
        mdx_esm = false,
        mdx_expression_flow = false,
        mdx_expression_text = false,
        mdx_jsx_flow = false,
        mdx_jsx_text = false,
        thematic_break = true
    }.

-spec commonmark(Castable) -> Constructs when Constructs :: t(), Castable :: castable().
commonmark(Castable) ->
    new(Castable, #{extend => commonmark}).

-compile({inline, [default/0]}).
-spec default() -> Constructs when Constructs :: t().
default() ->
    commonmark().

-spec default(Castable) -> Constructs when Constructs :: t(), Castable :: castable().
default(Castable) ->
    new(Castable, #{extend => default}).

-compile({inline, [gfm/0]}).
-spec gfm() -> Constructs when Constructs :: t().
gfm() ->
    CommonMark = commonmark(),
    CommonMark#markdown_construct_options{
        gfm_autolink_literal = true,
        gfm_footnote_definition = true,
        gfm_label_start_footnote = true,
        gfm_strikethrough = true,
        gfm_table = true,
        gfm_task_list_item = true
    }.

-spec gfm(Castable) -> Constructs when Constructs :: t(), Castable :: castable().
gfm(Castable) ->
    new(Castable, #{extend => gfm}).

-compile({inline, [mdx/0]}).
-spec mdx() -> Constructs when Constructs :: t().
mdx() ->
    CommonMark = commonmark(),
    CommonMark#markdown_construct_options{
        autolink = false,
        code_indented = false,
        html_flow = false,
        html_text = false,
        mdx_esm = true,
        mdx_expression_flow = true,
        mdx_expression_text = true,
        mdx_jsx_flow = true,
        mdx_jsx_text = true
    }.

-spec mdx(Castable) -> Constructs when Constructs :: t(), Castable :: castable().
mdx(Castable) ->
    new(Castable, #{extend => mdx}).

% -spec iterator(Constructs) -> Iterator when Constructs :: t(), Iterator :: iterator().
% iterator(Constructs = #markdown_construct_options{}) ->
%     HeadKeyOrder = ?HEAD_KEY_ORDER,
%     TailKeyOrder = ?TAIL_KEY_ORDER,
%     KeyFun = fun(K) ->
%         case HeadKeyOrder of
%             #{K := KHeadOrder} ->
%                 {0, KHeadOrder};
%             #{} ->
%                 case TailKeyOrder of
%                     #{K := KTailOrder} ->
%                         {2, KTailOrder};
%                     #{} ->
%                         {1, K}
%                 end
%         end
%     end,
%     KeyOrderFun = fun(A, B) ->
%         AOrder = KeyFun(A),
%         BOrder = KeyFun(B),
%         AOrder =< BOrder
%     end,
%     maps:iterator(maps:without(['__struct__'], Env), KeyOrderFun).

-spec new(Constructs | Castable) -> Constructs when Constructs :: t(), Castable :: castable().
new(Constructs = #markdown_construct_options{}) ->
    Constructs;
new(Castable) ->
    new(Castable, ?DEFAULT_CONFIG).

-spec new(Constructs | Castable, Config) -> Constructs when
    Constructs :: t(), Castable :: castable(), Config :: config().
new(Constructs = #markdown_construct_options{}, _Config) ->
    Constructs;
new(Castable, Config = #{extend := Extend}) ->
    Constructs =
        case Extend of
            #markdown_construct_options{} ->
                Extend;
            commonmark ->
                commonmark();
            default ->
                default();
            gfm ->
                gfm();
            mdx ->
                mdx()
        end,
    case Castable of
        _Keyword = [] ->
            Constructs;
        Keyword = [{_, _} | _] ->
            {Constructs2, Config} = lists:foldl(
                fun cast_from_keyword_fold/2, {Constructs, Config}, markdown_types:dynamic_cast(Keyword)
            ),
            Constructs2;
        Map when is_map(Map) ->
            {Constructs2, Config} = maps:fold(
                fun cast_from_maps_fold/3, {Constructs, Config}, markdown_types:dynamic_cast(Map)
            ),
            Constructs2;
        _ ->
            erlang:error(badarg, [Castable, Config])
    end.

% -spec merge(ConstructsA, ConstructsB | CastableB) -> ConstructsC when
%     ConstructsA :: t(),
%     ConstructsB :: t(),
%     CastableB :: castable(),
%     ConstructsC :: t().
% merge(ConstructsA = #markdown_construct_options{}, ConstructsB = #markdown_construct_options{}) ->
%     IteratorB = iterator(dynamic_cast(EnvB)),
%     merge_non_null_from_maps_iterator(dynamic_cast(EnvA), IteratorB);
% merge(EnvA = #{'__struct__' := ?MODULE}, CastableB) ->
%     PartialB = cast_partial(CastableB),
%     IteratorB = maps:iterator(PartialB),
%     merge_from_maps_iterator(dynamic_cast(EnvA), IteratorB).

% new(Map) when is_map(Map) ->
%     Defaults = #markdown_construct_options{},
%     Defaults#markdown_construct_options{
%         attention = maps:get(attention, Map, false),
%         autolink = maps:get(autolink, Map, false),
%         block_quote = maps:get(block_quote, Map, false),
%         character_escape = maps:get(character_escape, Map, false),
%         character_reference = maps:get(character_reference, Map, false),
%         code_indented = maps:get(code_indented, Map, false),
%         code_fenced = maps:get(code_fenced, Map, false),
%         code_text = maps:get(code_text, Map, false),
%         definition = maps:get(definition, Map, false),
%         frontmatter = maps:get(frontmatter, Map, false),
%         gfm_autolink_literal = maps:get(gfm_autolink_literal, Map, false),
%         gfm_footnote_definition = maps:get(gfm_footnote_definition, Map, false),
%         gfm_label_start_footnote = maps:get(gfm_label_start_footnote, Map, false),
%         gfm_strikethrough = maps:get(gfm_strikethrough, Map, false),
%         gfm_table = maps:get(gfm_table, Map, false),
%         gfm_task_list_item = maps:get(gfm_task_list_item, Map, false),
%         hard_break_escape = maps:get(hard_break_escape, Map, false),
%         hard_break_trailing = maps:get(hard_break_trailing, Map, false),
%         heading_atx = maps:get(heading_atx, Map, false),
%         heading_setext = maps:get(heading_setext, Map, false),
%         html_flow = maps:get(html_flow, Map, false),
%         html_text = maps:get(html_text, Map, false),
%         label_start_image = maps:get(label_start_image, Map, false),
%         label_start_link = maps:get(label_start_link, Map, false),
%         label_end = maps:get(label_end, Map, false),
%         list_item = maps:get(list_item, Map, false),
%         math_flow = maps:get(math_flow, Map, false),
%         math_text = maps:get(math_text, Map, false),
%         mdx_esm = maps:get(mdx_esm, Map, false),
%         mdx_expression_flow = maps:get(mdx_expression_flow, Map, false),
%         mdx_expression_text = maps:get(mdx_expression_text, Map, false),
%         mdx_jsx_flow = maps:get(mdx_jsx_flow, Map, false),
%         mdx_jsx_text = maps:get(mdx_jsx_text, Map, false),
%         thematic_break = maps:get(thematic_break, Map, false)
%     }.

-spec put(Constructs, Key, Value) -> Constructs when
    Constructs :: t(), Key :: key(), Value :: boolean().
put(Constructs = #markdown_construct_options{}, Key, Value) when is_boolean(Value) ->
    put(Constructs, Key, Value, ?DEFAULT_CONFIG).

-spec put(Constructs, Key, Value, Config) -> Constructs when
    Constructs :: t(), Key :: key(), Value :: boolean(), Config :: config().
put(Constructs = #markdown_construct_options{}, Key, Value, _Config) when is_boolean(Value) ->
    Index = key_to_index(Key),
    setelement(Index, Constructs, Value).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, {Constructs, Config}) -> {Constructs, Config} when
    Key :: key(), Value :: boolean(), Constructs :: t(), Config :: config().
cast_from_keyword_fold({Key, Value}, {Constructs, Config}) ->
    Constructs2 = put(Constructs, Key, Value, Config),
    {Constructs2, Config}.

%% @private
-spec cast_from_maps_fold(Key, Value, {Constructs, Config}) -> {Constructs, Config} when
    Key :: key(), Value :: boolean(), Constructs :: t(), Config :: config().
cast_from_maps_fold(Key, Value, {Constructs, Config}) ->
    Constructs2 = put(Constructs, Key, Value, Config),
    {Constructs2, Config}.

% %% @private
% -compile({inline, [index_to_key/1]}).
% -spec index_to_key(Index) -> Key when Index :: pos_integer(), Key :: key().
% index_to_key(#markdown_construct_options.attention) -> attention;
% index_to_key(#markdown_construct_options.autolink) -> autolink;
% index_to_key(#markdown_construct_options.block_quote) -> block_quote;
% index_to_key(#markdown_construct_options.character_escape) -> character_escape;
% index_to_key(#markdown_construct_options.character_reference) -> character_reference;
% index_to_key(#markdown_construct_options.code_indented) -> code_indented;
% index_to_key(#markdown_construct_options.code_fenced) -> code_fenced;
% index_to_key(#markdown_construct_options.code_text) -> code_text;
% index_to_key(#markdown_construct_options.definition) -> definition;
% index_to_key(#markdown_construct_options.frontmatter) -> frontmatter;
% index_to_key(#markdown_construct_options.gfm_autolink_literal) -> gfm_autolink_literal;
% index_to_key(#markdown_construct_options.gfm_footnote_definition) -> gfm_footnote_definition;
% index_to_key(#markdown_construct_options.gfm_label_start_footnote) -> gfm_label_start_footnote;
% index_to_key(#markdown_construct_options.gfm_strikethrough) -> gfm_strikethrough;
% index_to_key(#markdown_construct_options.gfm_table) -> gfm_table;
% index_to_key(#markdown_construct_options.gfm_task_list_item) -> gfm_task_list_item;
% index_to_key(#markdown_construct_options.hard_break_escape) -> hard_break_escape;
% index_to_key(#markdown_construct_options.hard_break_trailing) -> hard_break_trailing;
% index_to_key(#markdown_construct_options.heading_atx) -> heading_atx;
% index_to_key(#markdown_construct_options.heading_setext) -> heading_setext;
% index_to_key(#markdown_construct_options.html_flow) -> html_flow;
% index_to_key(#markdown_construct_options.html_text) -> html_text;
% index_to_key(#markdown_construct_options.label_start_image) -> label_start_image;
% index_to_key(#markdown_construct_options.label_start_link) -> label_start_link;
% index_to_key(#markdown_construct_options.label_end) -> label_end;
% index_to_key(#markdown_construct_options.list_item) -> list_item;
% index_to_key(#markdown_construct_options.math_flow) -> math_flow;
% index_to_key(#markdown_construct_options.math_text) -> math_text;
% index_to_key(#markdown_construct_options.mdx_esm) -> mdx_esm;
% index_to_key(#markdown_construct_options.mdx_expression_flow) -> mdx_expression_flow;
% index_to_key(#markdown_construct_options.mdx_expression_text) -> mdx_expression_text;
% index_to_key(#markdown_construct_options.mdx_jsx_flow) -> mdx_jsx_flow;
% index_to_key(#markdown_construct_options.mdx_jsx_text) -> mdx_jsx_text;
% index_to_key(#markdown_construct_options.thematic_break) -> thematic_break.

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(attention) -> #markdown_construct_options.attention;
key_to_index(attribute_list_flow) -> #markdown_construct_options.attribute_list_flow;
key_to_index(attribute_list_text) -> #markdown_construct_options.attribute_list_text;
key_to_index(autolink) -> #markdown_construct_options.autolink;
key_to_index(block_quote) -> #markdown_construct_options.block_quote;
key_to_index(character_escape) -> #markdown_construct_options.character_escape;
key_to_index(character_reference) -> #markdown_construct_options.character_reference;
key_to_index(code_indented) -> #markdown_construct_options.code_indented;
key_to_index(code_fenced) -> #markdown_construct_options.code_fenced;
key_to_index(code_text) -> #markdown_construct_options.code_text;
key_to_index(definition) -> #markdown_construct_options.definition;
% key_to_index(exp_attribute_list) -> #markdown_construct_options.exp_attribute_list;
% key_to_index(exp_kramdown_attribute_list) -> #markdown_construct_options.exp_kramdown_attribute_list;
key_to_index(frontmatter) -> #markdown_construct_options.frontmatter;
key_to_index(gfm_autolink_literal) -> #markdown_construct_options.gfm_autolink_literal;
key_to_index(gfm_footnote_definition) -> #markdown_construct_options.gfm_footnote_definition;
key_to_index(gfm_label_start_footnote) -> #markdown_construct_options.gfm_label_start_footnote;
key_to_index(gfm_strikethrough) -> #markdown_construct_options.gfm_strikethrough;
key_to_index(gfm_table) -> #markdown_construct_options.gfm_table;
key_to_index(gfm_task_list_item) -> #markdown_construct_options.gfm_task_list_item;
key_to_index(hard_break_escape) -> #markdown_construct_options.hard_break_escape;
key_to_index(hard_break_trailing) -> #markdown_construct_options.hard_break_trailing;
key_to_index(heading_atx) -> #markdown_construct_options.heading_atx;
key_to_index(heading_setext) -> #markdown_construct_options.heading_setext;
key_to_index(html_flow) -> #markdown_construct_options.html_flow;
key_to_index(html_text) -> #markdown_construct_options.html_text;
key_to_index(label_start_image) -> #markdown_construct_options.label_start_image;
key_to_index(label_start_link) -> #markdown_construct_options.label_start_link;
key_to_index(label_end) -> #markdown_construct_options.label_end;
key_to_index(list_item) -> #markdown_construct_options.list_item;
key_to_index(math_flow) -> #markdown_construct_options.math_flow;
key_to_index(math_text) -> #markdown_construct_options.math_text;
key_to_index(mdx_esm) -> #markdown_construct_options.mdx_esm;
key_to_index(mdx_expression_flow) -> #markdown_construct_options.mdx_expression_flow;
key_to_index(mdx_expression_text) -> #markdown_construct_options.mdx_expression_text;
key_to_index(mdx_jsx_flow) -> #markdown_construct_options.mdx_jsx_flow;
key_to_index(mdx_jsx_text) -> #markdown_construct_options.mdx_jsx_text;
key_to_index(thematic_break) -> #markdown_construct_options.thematic_break.

% %% @private
% -spec new(Constructs, Castable) -> Constructs when Extend :: Constructs, Castable :: castable(), Constructs :: t().
% new(Constructs = #markdown_construct_options{}, Castable) ->
%     case Castable of
%         _Keyword = [] ->
%             Constructs;
%         Keyword = [{_, _} | _] ->
%             lists:foldl(fun cast_from_keyword_fold/2, Constructs, markdown_types:dynamic_cast(Keyword));
%         Map when is_map(Map) ->
%             maps:fold(fun cast_from_maps_fold/3, Constructs, markdown_types:dynamic_cast(Map));
%         _ ->
%             erlang:error(badarg, [Castable])
%     end.
