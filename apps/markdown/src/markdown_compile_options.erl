%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_compile_options).
-moduledoc """
Configuration that describes how to compile to HTML.

You likely either want to turn on the dangerous options
(`allow_dangerous_html`, `allow_dangerous_protocol`) when dealing with
input you trust, or want to customize how GFM footnotes are compiled
(typically because the input markdown is not in English).

## Examples

```
use markdown::CompileOptions;
# fn main() {

// Use the default trait to get safe defaults:
let safe = CompileOptions::default();

// Live dangerously / trust the author:
let danger = CompileOptions {
  allow_dangerous_html: true,
  allow_dangerous_protocol: true,
  ..CompileOptions::default()
};

// In French:
let enFrançais = CompileOptions {
  gfm_footnote_label: Some("Notes de bas de page".into()),
  gfm_footnote_back_label: Some("Arrière".into()),
  ..CompileOptions::default()
};
# }
```
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    default/0,
    default/1,
    gfm/0,
    gfm/1,
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
-type castable_value() :: boolean() | markdown_line_ending:t() | markdown_option:t(binary()).
-type config() :: #{extend := extend()}.
-type extend() :: default | gfm | t().
-type key() ::
    allow_dangerous_html
    | allow_dangerous_protocol
    | allow_any_img_src
    | default_line_ending
    | gfm_footnote_label
    | gfm_footnote_label_tag_name
    | gfm_footnote_label_attributes
    | gfm_footnote_back_label
    | gfm_footnote_clobber_prefix
    | gfm_task_list_item_checkable
    | gfm_tagfilter.
-type t() :: #markdown_compile_options{}.
-type value() :: boolean() | markdown_line_ending:t() | markdown_option:t(binary()).

-export_type([
    castable/0,
    castable_keyword/0,
    castable_map/0,
    castable_value/0,
    config/0,
    extend/0,
    key/0,
    t/0,
    value/0
]).

%% Macros
-define(DEFAULT_CONFIG, #{extend => default}).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-compile({inline, [default/0]}).
-spec default() -> CompileOptions when CompileOptions :: t().
default() ->
    #markdown_compile_options{
        allow_dangerous_html = false,
        allow_dangerous_protocol = false,
        allow_any_img_src = false,
        default_line_ending = markdown_line_ending:default(),
        gfm_footnote_label = none,
        gfm_footnote_label_tag_name = none,
        gfm_footnote_label_attributes = none,
        gfm_footnote_back_label = none,
        gfm_footnote_clobber_prefix = none,
        gfm_task_list_item_checkable = false,
        gfm_tagfilter = false
    }.

-spec default(Castable) -> CompileOpts when CompileOpts :: t(), Castable :: castable().
default(Castable) ->
    new(Castable, #{extend => default}).

-doc """
GFM.

GFM stands for **GitHub flavored markdown**.
On the compilation side, GFM turns on the GFM tag filter.
The tagfilter is useless, but it’s included here for consistency, and
this method exists for parity to parse options.

For more information, see the GFM specification:
<https://github.github.com/gfm/>.
""".
-compile({inline, [gfm/0]}).
-spec gfm() -> CompileOpts when CompileOpts :: t().
gfm() ->
    Default = default(),
    Default#markdown_compile_options{
        gfm_tagfilter = true
    }.

-spec gfm(Castable) -> CompileOpts when CompileOpts :: t(), Castable :: castable().
gfm(Castable) ->
    new(Castable, #{extend => gfm}).

-spec new(CompileOpts | Castable) -> CompileOpts when CompileOpts :: t(), Castable :: castable().
new(CompileOpts = #markdown_compile_options{}) ->
    CompileOpts;
new(Castable) ->
    new(Castable, ?DEFAULT_CONFIG).

-spec new(CompileOpts | Castable, Config) -> CompileOpts when
    CompileOpts :: t(), Castable :: castable(), Config :: config().
new(CompileOpts = #markdown_compile_options{}, _Config) ->
    CompileOpts;
new(Castable, Config = #{extend := Extend}) ->
    CompileOpts =
        case Extend of
            #markdown_compile_options{} ->
                Extend;
            default ->
                default();
            gfm ->
                gfm()
        end,
    case Castable of
        _Keyword = [] ->
            CompileOpts;
        Keyword = [{_, _} | _] ->
            {CompileOpts2, Config} = lists:foldl(
                fun cast_from_keyword_fold/2, {CompileOpts, Config}, markdown_types:dynamic_cast(Keyword)
            ),
            CompileOpts2;
        Map when is_map(Map) ->
            {CompileOpts2, Config} = maps:fold(
                fun cast_from_maps_fold/3, {CompileOpts, Config}, markdown_types:dynamic_cast(Map)
            ),
            CompileOpts2;
        _ ->
            erlang:error(badarg, [Castable])
    end.

-spec put(CompileOpts, Key, CastableValue) -> CompileOpts when
    CompileOpts :: t(), Key :: key(), CastableValue :: castable_value().
put(CompileOpts = #markdown_compile_options{}, Key, CastableValue) ->
    put(CompileOpts, Key, CastableValue, ?DEFAULT_CONFIG).

-spec put(CompileOpts, Key, Value, Config) -> CompileOpts when
    CompileOpts :: t(), Key :: key(), Value :: castable_value(), Config :: config().
put(CompileOpts = #markdown_compile_options{}, Key, CastableValue, _Config) ->
    Value = cast_value(Key, CastableValue),
    Index = key_to_index(Key),
    setelement(Index, CompileOpts, Value).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, {CompileOpts, Config}) -> {CompileOpts, Config} when
    Key :: key(), Value :: boolean(), CompileOpts :: t(), Config :: config().
cast_from_keyword_fold({Key, Value}, {CompileOpts, Config}) ->
    CompileOpts2 = put(CompileOpts, Key, Value, Config),
    {CompileOpts2, Config}.

%% @private
-spec cast_from_maps_fold(Key, Value, {CompileOpts, Config}) -> {CompileOpts, Config} when
    Key :: key(), Value :: boolean(), CompileOpts :: t(), Config :: config().
cast_from_maps_fold(Key, Value, {CompileOpts, Config}) ->
    CompileOpts2 = put(CompileOpts, Key, Value, Config),
    {CompileOpts2, Config}.

%% @private
-spec cast_value(Key, CastableValue) -> Value when Key :: key(), CastableValue :: castable_value(), Value :: value().
cast_value(allow_dangerous_html, V) when is_boolean(V) -> V;
cast_value(allow_dangerous_protocol, V) when is_boolean(V) -> V;
cast_value(allow_any_img_src, V) when is_boolean(V) -> V;
cast_value(default_line_ending, V) when ?is_markdown_line_ending(V) -> V;
cast_value(gfm_footnote_label, V) when ?is_option_binary(V) -> V;
cast_value(gfm_footnote_label_tag_name, V) when ?is_option_binary(V) -> V;
cast_value(gfm_footnote_label_attributes, V) when ?is_option_binary(V) -> V;
cast_value(gfm_footnote_back_label, V) when ?is_option_binary(V) -> V;
cast_value(gfm_footnote_clobber_prefix, V) when ?is_option_binary(V) -> V;
cast_value(gfm_task_list_item_checkable, V) when is_boolean(V) -> V;
cast_value(gfm_tagfilter, V) when is_boolean(V) -> V.

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(allow_dangerous_html) -> #markdown_compile_options.allow_dangerous_html;
key_to_index(allow_dangerous_protocol) -> #markdown_compile_options.allow_dangerous_protocol;
key_to_index(allow_any_img_src) -> #markdown_compile_options.allow_any_img_src;
key_to_index(default_line_ending) -> #markdown_compile_options.default_line_ending;
key_to_index(gfm_footnote_label) -> #markdown_compile_options.gfm_footnote_label;
key_to_index(gfm_footnote_label_tag_name) -> #markdown_compile_options.gfm_footnote_label_tag_name;
key_to_index(gfm_footnote_label_attributes) -> #markdown_compile_options.gfm_footnote_label_attributes;
key_to_index(gfm_footnote_back_label) -> #markdown_compile_options.gfm_footnote_back_label;
key_to_index(gfm_footnote_clobber_prefix) -> #markdown_compile_options.gfm_footnote_clobber_prefix;
key_to_index(gfm_task_list_item_checkable) -> #markdown_compile_options.gfm_task_list_item_checkable;
key_to_index(gfm_tagfilter) -> #markdown_compile_options.gfm_tagfilter.
