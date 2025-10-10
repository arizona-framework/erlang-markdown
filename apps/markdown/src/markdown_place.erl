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
-module(markdown_place).
-moduledoc """
Somewhere.
""".
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
