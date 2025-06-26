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
-module(markdown_unist_point).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_unist.hrl").

%% API
-export([
    fmt/1,
    new/3
]).

%% Types
-doc """
1-indexed integer representing a column in a source file.
""".
-type column() :: pos_integer().
-doc """
1-indexed integer representing a line in a source file.
""".
-type line() :: pos_integer().
-doc """
0-indexed integer representing a character in a source file.
""".
-type offset() :: non_neg_integer().
-doc """
One place in a source file.
""".
-type t() :: #markdown_unist_point{}.

-export_type([
    column/0,
    line/0,
    offset/0,
    t/0
]).

%% Macros
-define(is_non_neg_integer(X), (is_integer(X) andalso X >= 0)).
-define(is_pos_integer(X), (is_integer(X) andalso X >= 1)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec fmt(Point) -> {Format, Data} when Point :: t(), Format :: io:format(), Data :: [term()].
fmt(Point = #markdown_unist_point{}) ->
    {"~w:~w (~w)", [
        Point#markdown_unist_point.line, Point#markdown_unist_point.column, Point#markdown_unist_point.offset
    ]}.

-spec new(Line, Column, Offset) -> Point when Line :: line(), Column :: column(), Offset :: offset(), Point :: t().
new(Line, Column, Offset) when
    ?is_pos_integer(Line) andalso ?is_pos_integer(Column) andalso ?is_non_neg_integer(Offset)
->
    #markdown_unist_point{line = Line, column = Column, offset = Offset}.
