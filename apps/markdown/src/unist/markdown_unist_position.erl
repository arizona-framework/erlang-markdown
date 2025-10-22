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
-module(markdown_unist_position).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
