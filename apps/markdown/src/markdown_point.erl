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
-module(markdown_point).
-moduledoc """
One place in a source file.

The interface for the location in the document comes from unist
[`Point`](https://github.com/syntax-tree/unist#point).
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    fmt/1,
    new/4,
    to_index/1,
    to_unist/1
]).

%% Types
-doc "1-indexed integer representing a line in a source file.".
-type column() :: pos_integer().
-doc """
1-indexed integer representing a column in a source file.

This is increased up to a tab stop for tabs.
""".
-type line() :: pos_integer().
-doc "0-indexed integer representing a byte in a source file.".
-type offset() :: non_neg_integer().
-doc """
One place in a source file.
""".
-type t() :: #markdown_point{}.
-doc "0-indexed integer representing a virtual byte in a source file.".
-type virtual() :: non_neg_integer().

-export_type([
    column/0,
    line/0,
    offset/0,
    t/0,
    virtual/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec fmt(Point) -> {Format, Data} when Point :: t(), Format :: io:format(), Data :: [term()].
fmt(Point = #markdown_point{}) ->
    {"~w:~w (~w)", [
        Point#markdown_point.line, Point#markdown_point.column, Point#markdown_point.offset
    ]}.

-spec new(Line, Column, Offset, Virtual) -> Point when
    Line :: line(), Column :: column(), Offset :: offset(), Virtual :: virtual(), Point :: t().
new(Line, Column, Offset, Virtual) when
    ?is_pos_integer(Line) andalso ?is_pos_integer(Column) andalso ?is_non_neg_integer(Offset) andalso
        ?is_non_neg_integer(Virtual)
->
    #markdown_point{line = Line, column = Column, offset = Offset, virtual = Virtual}.

-spec to_index(Point) -> Index when Point :: t(), Index :: markdown_index:t().
to_index(Point = #markdown_point{}) ->
    #markdown_index{offset = Point#markdown_point.offset, virtual = Point#markdown_point.virtual}.

-doc """
Create a unist point.
""".
-spec to_unist(Point) -> UnistPoint when Point :: t(), UnistPoint :: markdown_unist_point:t().
to_unist(Point = #markdown_point{}) ->
    markdown_unist_point:new(Point#markdown_point.line, Point#markdown_point.column, Point#markdown_point.offset).
