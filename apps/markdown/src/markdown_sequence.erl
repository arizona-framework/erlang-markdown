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
-module(markdown_sequence).
-moduledoc """
Attention sequence that we can take markers from.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
