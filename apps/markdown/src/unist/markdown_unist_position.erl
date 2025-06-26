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
-module(markdown_unist_position).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_unist.hrl").

%% API
-export([
    fmt/1,
    new/2,
    new/6
]).

%% Types
-doc """
Location of a node in a source file.
""".
-type t() :: #markdown_unist_position{}.

-export_type([
    t/0
]).

%% Macros
-define(is_non_neg_integer(X), (is_integer(X) andalso X >= 0)).
-define(is_pos_integer(X), (is_integer(X) andalso X >= 1)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec fmt(Position) -> {Format, Data} when Position :: t(), Format :: io:format(), Data :: [term()].
fmt(Position = #markdown_unist_position{}) ->
    Start = Position#markdown_unist_position.start,
    End = Position#markdown_unist_position.'end',
    {"~w:~w-~w:~w (~w-~w)", [
        Start#markdown_unist_point.line,
        Start#markdown_unist_point.column,
        End#markdown_unist_point.line,
        End#markdown_unist_point.column,
        Start#markdown_unist_point.offset,
        End#markdown_unist_point.offset
    ]}.

-spec new(Start, End) -> Position when
    Start :: markdown_unist_point:t(), End :: markdown_unist_point:t(), Position :: t().
new(Start = #markdown_unist_point{}, End = #markdown_unist_point{}) ->
    #markdown_unist_position{start = Start, 'end' = End}.

-spec new(StartLine, StartColumn, StartOffset, EndLine, EndColumn, EndOffset) -> Position when
    StartLine :: markdown_unist_point:line(),
    StartColumn :: markdown_unist_point:column(),
    StartOffset :: markdown_unist_point:offset(),
    EndLine :: markdown_unist_point:line(),
    EndColumn :: markdown_unist_point:column(),
    EndOffset :: markdown_unist_point:offset(),
    Position :: t().
new(StartLine, StartColumn, StartOffset, EndLine, EndColumn, EndOffset) when
    ?is_pos_integer(StartLine) andalso ?is_pos_integer(StartColumn) andalso ?is_non_neg_integer(StartOffset) andalso
        ?is_pos_integer(EndLine) andalso ?is_pos_integer(EndColumn) andalso ?is_non_neg_integer(EndOffset)
->
    Start = markdown_unist_point:new(StartLine, StartColumn, StartOffset),
    End = markdown_unist_point:new(EndLine, EndColumn, EndOffset),
    new(Start, End).
