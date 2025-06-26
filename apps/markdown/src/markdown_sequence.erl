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
-module(markdown_sequence).
-moduledoc """
Attention sequence that we can take markers from.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
% -export([
% ]).

%% Types
-doc """
Attention sequence that we can take markers from.
""".
-type t() :: #markdown_sequence{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

% -spec fmt(Point) -> {Format, Data} when Point :: t(), Format :: io:format(), Data :: [term()].
% fmt(Point = #markdown_point{}) ->
%     {"~w:~w (~w)", [
%         Point#markdown_point.line, Point#markdown_point.column, Point#markdown_point.offset
%     ]}.

% -spec new(Line, Column, Offset, Virtual) -> Point when
%     Line :: line(), Column :: column(), Offset :: offset(), Virtual :: virtual(), Point :: t().
% new(Line, Column, Offset, Virtual) when
%     ?is_pos_integer(Line) andalso ?is_pos_integer(Column) andalso ?is_non_neg_integer(Offset) andalso
%         ?is_non_neg_integer(Virtual)
% ->
%     #markdown_point{line = Line, column = Column, offset = Offset, virtual = Virtual}.

% -spec to_index(Point) -> Index when Point :: t(), Index :: markdown_index:t().
% to_index(Point = #markdown_point{}) ->
%     #markdown_index{offset = Point#markdown_point.offset, virtual = Point#markdown_point.virtual}.
