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
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    cast_compile/1,
    cast_key_value/2,
    cast_parse/1,
    default/0,
    gfm/0,
    new/1,
    put/3
]).

%% Types
-type castable() ::
    castable_keyword()
    | castable_map().
-type castable_keyword() :: [{key(), castable_value()}].
-type castable_map() :: #{key() => castable_value()}.
-type castable_value() :: value() | markdown_compile_options:castable() | markdown_parse_options:castable().
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
    key/0,
    t/0,
    value/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec cast_compile(From) -> To when
    From :: markdown_compile_options:t() | markdown_compile_options:castable(),
    To :: markdown_compile_options:t().
cast_compile(From) ->
    To = markdown_compile_options:new(From),
    To.

-spec cast_key_value(key(), castable_value()) -> {key(), value()}.
cast_key_value(K = compile, V) -> {K, cast_compile(markdown_types:dynamic_cast(V))};
cast_key_value(K = parse, V) -> {K, cast_parse(markdown_types:dynamic_cast(V))}.

-spec cast_parse(From) -> To when
    From :: markdown_parse_options:t() | markdown_parse_options:castable(),
    To :: markdown_parse_options:t().
cast_parse(From) ->
    To = markdown_parse_options:new(From),
    To.

-compile({inline, [default/0]}).
-spec default() -> Options when Options :: t().
default() ->
    #markdown_options{
        parse = markdown_parse_options:default(),
        compile = markdown_compile_options:default()
    }.

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

-spec new(Options | Castable) -> Options when Options :: t(), Castable :: castable().
new(Options = #markdown_options{}) ->
    Options;
new(Castable) ->
    Options = default(),
    case Castable of
        _Keyword = [] ->
            Options;
        Keyword = [{_, _} | _] ->
            lists:foldl(fun cast_from_keyword_fold/2, Options, markdown_types:dynamic_cast(Keyword));
        Map when is_map(Map) ->
            maps:fold(fun cast_from_maps_fold/3, Options, markdown_types:dynamic_cast(Map));
        _ ->
            erlang:error(badarg, [Castable])
    end.

-spec put(Options, Key, Value) -> Options when
    Options :: t(), Key :: key(), Value :: castable_value().
put(Options = #markdown_options{}, Key, Value) ->
    {CastKey, CastValue} = cast_key_value(Key, Value),
    Index = key_to_index(CastKey),
    setelement(Index, Options, CastValue).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec cast_from_keyword_fold({Key, Value}, Options) -> Options when
    Key :: key(), Value :: castable_value(), Options :: t().
cast_from_keyword_fold({Key, Value}, Options) ->
    put(Options, Key, Value).

%% @private
-spec cast_from_maps_fold(Key, Value, Options) -> Options when
    Key :: key(), Value :: castable_value(), Options :: t().
cast_from_maps_fold(Key, Value, Options) ->
    put(Options, Key, Value).

%% @private
-compile({inline, [key_to_index/1]}).
-spec key_to_index(Key) -> Index when Key :: key(), Index :: pos_integer().
key_to_index(parse) -> #markdown_options.parse;
key_to_index(compile) -> #markdown_options.compile.
