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
-module(markdown_parse_options).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    cast_boolean/1,
    cast_constructs/1,
    cast_key_value/2,
    cast_mdx_esm_parse/1,
    cast_mdx_expression_parse/1,
    commonmark/0,
    default/0,
    gfm/0,
    mdx/0,
    new/1,
    put/3
]).

%% Types
-type castable() ::
    castable_keyword()
    | castable_map().
-type castable_keyword() :: [{key(), castable_value()}].
-type castable_map() :: #{key() => castable_value()}.
-type castable_value() :: value() | markdown_construct_options:castable().
-type key() ::
    constructs
    | gfm_strikethrough_single_tilde
    | math_text_single_dollar
    | mdx_expression_parse
    | mdx_esm_parse.
-type mdx_esm_parse() :: dynamic().
-type mdx_expression_parse() :: dynamic().
-type t() :: #markdown_parse_options{}.
-type value() ::
    markdown_construct_options:t()
    | boolean()
    | markdown_types:option(mdx_expression_parse())
    | markdown_types:option(mdx_esm_parse()).

-export_type([
    castable/0,
    castable_keyword/0,
    castable_map/0,
    castable_value/0,
    key/0,
    mdx_esm_parse/0,
    mdx_expression_parse/0,
    t/0,
    value/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec cast_boolean(From) -> To when From :: boolean(), To :: boolean().
cast_boolean(From) when is_boolean(From) ->
    To = From,
    To.

-spec cast_constructs(From) -> To when
    From :: markdown_construct_options:t() | markdown_construct_options:castable(),
    To :: markdown_construct_options:t().
cast_constructs(From) ->
    To = markdown_construct_options:new(From),
    To.

-spec cast_key_value(key(), castable_value()) -> {key(), value()}.
cast_key_value(K = constructs, V) -> {K, cast_constructs(markdown_types:dynamic_cast(V))};
cast_key_value(K = gfm_strikethrough_single_tilde, V) -> {K, cast_boolean(markdown_types:dynamic_cast(V))};
cast_key_value(K = math_text_single_dollar, V) -> {K, cast_boolean(markdown_types:dynamic_cast(V))};
cast_key_value(K = mdx_expression_parse, V) -> {K, cast_mdx_expression_parse(markdown_types:dynamic_cast(V))};
cast_key_value(K = mdx_esm_parse, V) -> {K, cast_mdx_esm_parse(markdown_types:dynamic_cast(V))}.

-spec cast_mdx_esm_parse(markdown_types:option(From)) -> markdown_types:option(To) when
    From :: mdx_esm_parse(), To :: mdx_esm_parse().
cast_mdx_esm_parse(none) ->
    none;
cast_mdx_esm_parse({some, From}) ->
    To = From,
    {some, To}.

-spec cast_mdx_expression_parse(markdown_types:option(From)) -> markdown_types:option(To) when
    From :: mdx_expression_parse(), To :: mdx_expression_parse().
cast_mdx_expression_parse(none) ->
    none;
cast_mdx_expression_parse({some, From}) ->
    To = From,
    {some, To}.

-spec commonmark() -> ParseOpts when ParseOpts :: t().
commonmark() ->
    #markdown_parse_options{
        constructs = markdown_construct_options:commonmark(),
        gfm_strikethrough_single_tilde = true,
        math_text_single_dollar = true,
        mdx_expression_parse = none,
        mdx_esm_parse = none
    }.

-compile({inline, [default/0]}).
-spec default() -> ParseOpts when ParseOpts :: t().
default() ->
    commonmark().

-spec gfm() -> ParseOpts when ParseOpts :: t().
gfm() ->
    #markdown_parse_options{
        constructs = markdown_construct_options:gfm(),
        gfm_strikethrough_single_tilde = true,
        math_text_single_dollar = true,
        mdx_expression_parse = none,
        mdx_esm_parse = none
    }.

-spec mdx() -> ParseOpts when ParseOpts :: t().
mdx() ->
    #markdown_parse_options{
        constructs = markdown_construct_options:mdx(),
        gfm_strikethrough_single_tilde = true,
        math_text_single_dollar = true,
        mdx_expression_parse = none,
        mdx_esm_parse = none
    }.

-spec new(ParseOpts | Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
new(ParseOpts = #markdown_parse_options{}) ->
    ParseOpts;
new(Castable) ->
    ParseOpts = default(),
    case Castable of
        _Keyword = [] ->
            ParseOpts;
        Keyword = [{_, _} | _] ->
            lists:foldl(fun cast_from_keyword_fold/2, ParseOpts, markdown_types:dynamic_cast(Keyword));
        Map when is_map(Map) ->
            maps:fold(fun cast_from_maps_fold/3, ParseOpts, markdown_types:dynamic_cast(Map));
        _ ->
            erlang:error(badarg, [Castable])
    end.

-spec put(ParseOpts, Key, Value) -> ParseOpts when
    ParseOpts :: t(), Key :: key(), Value :: castable_value().
put(ParseOpts = #markdown_parse_options{}, Key, Value) ->
    {CastKey, CastValue} = cast_key_value(Key, Value),
    Index = key_to_index(CastKey),
    setelement(Index, ParseOpts, CastValue).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, ParseOpts) -> ParseOpts when
    Key :: key(), Value :: boolean(), ParseOpts :: t().
cast_from_keyword_fold({Key, Value}, ParseOpts) ->
    put(ParseOpts, Key, Value).

%% @private
-spec cast_from_maps_fold(Key, Value, ParseOpts) -> ParseOpts when
    Key :: key(), Value :: boolean(), ParseOpts :: t().
cast_from_maps_fold(Key, Value, ParseOpts) ->
    put(ParseOpts, Key, Value).

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(constructs) -> #markdown_parse_options.constructs;
key_to_index(gfm_strikethrough_single_tilde) -> #markdown_parse_options.gfm_strikethrough_single_tilde;
key_to_index(math_text_single_dollar) -> #markdown_parse_options.math_text_single_dollar;
key_to_index(mdx_expression_parse) -> #markdown_parse_options.mdx_expression_parse;
key_to_index(mdx_esm_parse) -> #markdown_parse_options.mdx_esm_parse.
