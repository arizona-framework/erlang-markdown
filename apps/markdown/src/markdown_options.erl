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
-module(markdown_options).
-moduledoc """
Configuration that describes how to parse from markdown and compile to
HTML.

In most cases, you will want to use the default trait or `gfm` method.

## Examples

```
use markdown::Options;
# fn main() {

// Use the default trait to compile markdown to HTML according to `CommonMark`:
let commonmark = Options::default();

// Use the `gfm` method to compile markdown to HTML according to GFM:
let gfm = Options::gfm();
# }
```
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    cast_compile/1,
    cast_compile/2,
    cast_key_value/2,
    cast_key_value/3,
    cast_parse/1,
    cast_parse/2,
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
-type castable_value() :: value() | markdown_compile_options:castable() | markdown_parse_options:castable().
-type config() :: #{extend := extend()}.
-type extend() :: commonmark | default | gfm | mdx | t().
-type key() ::
    compile
    | parse.
-type t() :: #markdown_options{}.
-type value() ::
    markdown_compile_options:t()
    | markdown_parse_options:t().

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

-spec cast_compile(From) -> To when
    From :: markdown_compile_options:t() | markdown_compile_options:castable(),
    To :: markdown_compile_options:t().
cast_compile(From) ->
    cast_compile(From, ?DEFAULT_CONFIG).

-spec cast_compile(From, Config) -> To when
    From :: markdown_compile_options:t() | markdown_compile_options:castable(),
    Config :: config(),
    To :: markdown_compile_options:t().
cast_compile(From, Config) ->
    CompileOptionsConfig =
        case Config of
            #{extend := #markdown_options{compile = CompileOptions}} -> Config#{extend := CompileOptions};
            #{extend := default} -> Config;
            #{extend := gfm} -> Config;
            #{extend := _} -> Config#{extend := default}
        end,
    To = markdown_compile_options:new(From, CompileOptionsConfig),
    To.

-spec cast_key_value(key(), castable_value()) -> {key(), value()}.
cast_key_value(Key, Value) ->
    cast_key_value(Key, Value, ?DEFAULT_CONFIG).

-spec cast_key_value(key(), castable_value(), config()) -> {key(), value()}.
cast_key_value(K = compile, V, C) -> {K, cast_compile(markdown_types:dynamic_cast(V), C)};
cast_key_value(K = parse, V, C) -> {K, cast_parse(markdown_types:dynamic_cast(V), C)}.

-spec cast_parse(From) -> To when
    From :: markdown_parse_options:t() | markdown_parse_options:castable(),
    To :: markdown_parse_options:t().
cast_parse(From) ->
    cast_parse(From, ?DEFAULT_CONFIG).

-spec cast_parse(From, Config) -> To when
    From :: markdown_parse_options:t() | markdown_parse_options:castable(),
    Config :: config(),
    To :: markdown_parse_options:t().
cast_parse(From, Config) ->
    ParseOptionsConfig =
        case Config of
            #{extend := #markdown_options{parse = ParseOptions}} -> Config#{extend := ParseOptions};
            #{extend := _} -> Config
        end,
    To = markdown_parse_options:new(From, ParseOptionsConfig),
    To.

-spec commonmark() -> Options when Options :: t().
commonmark() ->
    #markdown_options{
        parse = markdown_parse_options:commonmark(),
        compile = markdown_compile_options:default()
    }.

-spec commonmark(Castable) -> Options when Options :: t(), Castable :: castable().
commonmark(Castable) ->
    new(Castable, #{extend => commonmark}).

-compile({inline, [default/0]}).
-spec default() -> Options when Options :: t().
default() ->
    #markdown_options{
        parse = markdown_parse_options:default(),
        compile = markdown_compile_options:default()
    }.

-spec default(Castable) -> Options when Options :: t(), Castable :: castable().
default(Castable) ->
    new(Castable, #{extend => default}).

-doc """
GFM.

GFM stands for GitHub flavored markdown.
GFM extends `CommonMark` and adds support for autolink literals,
footnotes, strikethrough, tables, and tasklists.
On the compilation side, GFM turns on the GFM tag filter.
The tagfilter is useless, but it’s included here for consistency.

For more information, see the GFM specification:
<https://github.github.com/gfm/>
""".
-compile({inline, [gfm/0]}).
-spec gfm() -> Options when Options :: t().
gfm() ->
    #markdown_options{
        parse = markdown_parse_options:gfm(),
        compile = markdown_compile_options:gfm()
    }.

-spec gfm(Castable) -> Options when Options :: t(), Castable :: castable().
gfm(Castable) ->
    new(Castable, #{extend => gfm}).

-spec mdx() -> Options when Options :: t().
mdx() ->
    #markdown_options{
        parse = markdown_parse_options:mdx(),
        compile = markdown_compile_options:default()
    }.

-spec mdx(Castable) -> Options when Options :: t(), Castable :: castable().
mdx(Castable) ->
    new(Castable, #{extend => mdx}).

-spec new(Options | Castable) -> Options when Options :: t(), Castable :: castable().
new(Options = #markdown_options{}) ->
    Options;
new(Castable) ->
    new(Castable, ?DEFAULT_CONFIG).

-spec new(Options | Castable, Config) -> Options when Options :: t(), Castable :: castable(), Config :: config().
new(Options = #markdown_options{}, _Config) ->
    Options;
new(Castable, Config = #{extend := Extend}) ->
    Options =
        case Extend of
            #markdown_options{} ->
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
            Options;
        Keyword = [{_, _} | _] ->
            {Options2, Config} = lists:foldl(
                fun cast_from_keyword_fold/2, {Options, Config}, markdown_types:dynamic_cast(Keyword)
            ),
            Options2;
        Map when is_map(Map) ->
            {Options2, Config} = maps:fold(
                fun cast_from_maps_fold/3, {Options, Config}, markdown_types:dynamic_cast(Map)
            ),
            Options2;
        _ ->
            erlang:error(badarg, [Castable, Config])
    end.

-spec put(Options, Key, Value) -> Options when
    Options :: t(), Key :: key(), Value :: castable_value().
put(Options = #markdown_options{}, Key, Value) ->
    put(Options, Key, Value, ?DEFAULT_CONFIG).

-spec put(Options, Key, Value, Config) -> Options when
    Options :: t(), Key :: key(), Value :: castable_value(), Config :: config().
put(Options = #markdown_options{}, Key, Value, Config) ->
    {CastKey, CastValue} = cast_key_value(Key, Value, Config),
    Index = key_to_index(CastKey),
    setelement(Index, Options, CastValue).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, {Options, Config}) -> {Options, Config} when
    Key :: key(), Value :: castable_value(), Options :: t(), Config :: config().
cast_from_keyword_fold({Key, Value}, {Options, Config}) ->
    Options2 = put(Options, Key, Value, Config),
    {Options2, Config}.

%% @private
-spec cast_from_maps_fold(Key, Value, {Options, Config}) -> {Options, Config} when
    Key :: key(), Value :: castable_value(), Options :: t(), Config :: config().
cast_from_maps_fold(Key, Value, {Options, Config}) ->
    Options2 = put(Options, Key, Value, Config),
    {Options2, Config}.

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(parse) -> #markdown_options.parse;
key_to_index(compile) -> #markdown_options.compile.
