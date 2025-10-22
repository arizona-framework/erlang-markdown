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
-module(markdown_place).
-moduledoc """
Somewhere.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_unist.hrl").

%% API
-export([
    fmt/1,
    point/1,
    position/1
]).

%% Types
-type inner() :: markdown_unist_point:t() | markdown_unist_position:t().

-doc """
Somewhere.
""".
-type t() :: #markdown_place{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec fmt(Place) -> {Format, Data} when Place :: t(), Format :: io:format(), Data :: [term()].
fmt(#markdown_place{inner = Point = #markdown_unist_point{}}) ->
    markdown_unist_point:fmt(Point);
fmt(#markdown_place{inner = Position = #markdown_unist_position{}}) ->
    markdown_unist_position:fmt(Position).

-doc """
At a point.
""".
-spec point(Point) -> Place when Point :: markdown_unist_point:t(), Place :: t().
point(Point = #markdown_unist_point{}) ->
    #markdown_place{inner = Point}.

-doc """
Between two points.
""".
-spec position(Position) -> Place when Position :: markdown_unist_position:t(), Place :: t().
position(Position = #markdown_unist_position{}) ->
    #markdown_place{inner = Position}.
