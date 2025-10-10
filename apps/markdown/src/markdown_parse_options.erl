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
    cast_constructs/2,
    cast_key_value/2,
    cast_key_value/3,
    cast_mdx_esm_parse/1,
    cast_mdx_esm_parse/2,
    cast_mdx_expression_parse/1,
    cast_mdx_expression_parse/2,
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
-type castable_keyword() :: [{key(), castable_value()}].
-type castable_map() :: #{key() => castable_value()}.
-type castable_value() :: value() | markdown_construct_options:castable().
-type config() :: #{extend := extend()}.
-type extend() :: commonmark | default | gfm | mdx | t().
-type key() ::
    constructs
    | gfm_strikethrough_single_tilde
    | math_text_single_dollar
    | mdx_expression_parse
    | mdx_esm_parse.
-type mdx_esm_parse() :: markdown_mdx:esm_parse().
-type mdx_expression_parse() :: markdown_mdx:expression_parse().
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
    config/0,
    extend/0,
    key/0,
    mdx_esm_parse/0,
    mdx_expression_parse/0,
    t/0,
    value/0
]).

%% Macros
-define(DEFAULT_CONFIG, #{extend => default}).

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
    cast_constructs(From, ?DEFAULT_CONFIG).

-spec cast_constructs(From, Config) -> To when
    From :: markdown_construct_options:t() | markdown_construct_options:castable(),
    Config :: config(),
    To :: markdown_construct_options:t().
cast_constructs(From, Config) ->
    ConstructConfig =
        case Config of
            #{extend := #markdown_parse_options{constructs = Constructs}} -> Config#{extend := Constructs};
            _ -> Config
        end,
    To = markdown_construct_options:new(From, ConstructConfig),
    To.

-spec cast_key_value(key(), castable_value()) -> {key(), value()}.
cast_key_value(Key, Value) ->
    cast_key_value(Key, Value, ?DEFAULT_CONFIG).

-spec cast_key_value(key(), castable_value(), config()) -> {key(), value()}.
cast_key_value(K = constructs, V, C) -> {K, cast_constructs(markdown_types:dynamic_cast(V), C)};
cast_key_value(K = gfm_strikethrough_single_tilde, V, _C) -> {K, cast_boolean(markdown_types:dynamic_cast(V))};
cast_key_value(K = math_text_single_dollar, V, _C) -> {K, cast_boolean(markdown_types:dynamic_cast(V))};
cast_key_value(K = mdx_expression_parse, V, C) -> {K, cast_mdx_expression_parse(markdown_types:dynamic_cast(V), C)};
cast_key_value(K = mdx_esm_parse, V, C) -> {K, cast_mdx_esm_parse(markdown_types:dynamic_cast(V), C)}.

-spec cast_mdx_esm_parse(markdown_types:option(From)) -> markdown_types:option(To) when
    From :: mdx_esm_parse(), To :: mdx_esm_parse().
cast_mdx_esm_parse(OptionFrom) ->
    cast_mdx_esm_parse(OptionFrom, ?DEFAULT_CONFIG).

-spec cast_mdx_esm_parse(markdown_types:option(From), config()) -> markdown_types:option(To) when
    From :: mdx_esm_parse(), To :: mdx_esm_parse().
cast_mdx_esm_parse(none, _Config) ->
    none;
cast_mdx_esm_parse({some, From}, _Config) when is_function(From, 1) ->
    To = From,
    {some, To}.

-spec cast_mdx_expression_parse(markdown_types:option(From)) -> markdown_types:option(To) when
    From :: mdx_expression_parse(), To :: mdx_expression_parse().
cast_mdx_expression_parse(OptionFrom) ->
    cast_mdx_expression_parse(OptionFrom, ?DEFAULT_CONFIG).

-spec cast_mdx_expression_parse(markdown_types:option(From), config()) -> markdown_types:option(To) when
    From :: mdx_expression_parse(), To :: mdx_expression_parse().
cast_mdx_expression_parse(none, _Config) ->
    none;
cast_mdx_expression_parse({some, From}, _Config) when is_function(From, 2) ->
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

-spec commonmark(Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
commonmark(Castable) ->
    new(Castable, #{extend => commonmark}).

-compile({inline, [default/0]}).
-spec default() -> ParseOpts when ParseOpts :: t().
default() ->
    commonmark().

-spec default(Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
default(Castable) ->
    new(Castable, #{extend => default}).

-spec gfm() -> ParseOpts when ParseOpts :: t().
gfm() ->
    #markdown_parse_options{
        constructs = markdown_construct_options:gfm(),
        gfm_strikethrough_single_tilde = true,
        math_text_single_dollar = true,
        mdx_expression_parse = none,
        mdx_esm_parse = none
    }.

-spec gfm(Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
gfm(Castable) ->
    new(Castable, #{extend => gfm}).

-spec mdx() -> ParseOpts when ParseOpts :: t().
mdx() ->
    #markdown_parse_options{
        constructs = markdown_construct_options:mdx(),
        gfm_strikethrough_single_tilde = true,
        math_text_single_dollar = true,
        mdx_expression_parse = none,
        mdx_esm_parse = none
    }.

-spec mdx(Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
mdx(Castable) ->
    new(Castable, #{extend => mdx}).

-spec new(ParseOpts | Castable) -> ParseOpts when ParseOpts :: t(), Castable :: castable().
new(ParseOpts = #markdown_parse_options{}) ->
    ParseOpts;
new(Castable) ->
    new(Castable, ?DEFAULT_CONFIG).

-spec new(ParseOpts | Castable, Config) -> ParseOpts when ParseOpts :: t(), Castable :: castable(), Config :: config().
new(ParseOpts = #markdown_parse_options{}, _Config) ->
    ParseOpts;
new(Castable, Config = #{extend := Extend}) ->
    ParseOpts =
        case Extend of
            #markdown_parse_options{} ->
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
            ParseOpts;
        Keyword = [{_, _} | _] ->
            {ParseOpts2, Config} = lists:foldl(
                fun cast_from_keyword_fold/2, {ParseOpts, Config}, markdown_types:dynamic_cast(Keyword)
            ),
            ParseOpts2;
        Map when is_map(Map) ->
            {ParseOpts2, Config} = maps:fold(
                fun cast_from_maps_fold/3, {ParseOpts, Config}, markdown_types:dynamic_cast(Map)
            ),
            ParseOpts2;
        _ ->
            erlang:error(badarg, [Castable, Config])
    end.

-spec put(ParseOpts, Key, Value) -> ParseOpts when
    ParseOpts :: t(), Key :: key(), Value :: castable_value().
put(ParseOpts = #markdown_parse_options{}, Key, Value) ->
    put(ParseOpts, Key, Value, ?DEFAULT_CONFIG).

-spec put(ParseOpts, Key, Value, Config) -> ParseOpts when
    ParseOpts :: t(), Key :: key(), Value :: castable_value(), Config :: config().
put(ParseOpts = #markdown_parse_options{}, Key, Value, Config) ->
    {CastKey, CastValue} = cast_key_value(Key, Value, Config),
    Index = key_to_index(CastKey),
    setelement(Index, ParseOpts, CastValue).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, {ParseOpts, Config}) -> {ParseOpts, Config} when
    Key :: key(), Value :: boolean(), ParseOpts :: t(), Config :: config().
cast_from_keyword_fold({Key, Value}, {ParseOpts, Config}) ->
    ParseOpts2 = put(ParseOpts, Key, Value, Config),
    {ParseOpts2, Config}.

%% @private
-spec cast_from_maps_fold(Key, Value, {ParseOpts, Config}) -> {ParseOpts, Config} when
    Key :: key(), Value :: boolean(), ParseOpts :: t(), Config :: config().
cast_from_maps_fold(Key, Value, {ParseOpts, Config}) ->
    ParseOpts2 = put(ParseOpts, Key, Value, Config),
    {ParseOpts2, Config}.

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(constructs) -> #markdown_parse_options.constructs;
key_to_index(gfm_strikethrough_single_tilde) -> #markdown_parse_options.gfm_strikethrough_single_tilde;
key_to_index(math_text_single_dollar) -> #markdown_parse_options.math_text_single_dollar;
key_to_index(mdx_expression_parse) -> #markdown_parse_options.mdx_expression_parse;
key_to_index(mdx_esm_parse) -> #markdown_parse_options.mdx_esm_parse.
