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
-module(markdown_subresult).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    append/2,
    default/0
]).

%% Types

-doc """

""".
-type t() :: #markdown_subresult{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec append(SubresultA, SubresultB) -> SubresultC when SubresultA :: t(), SubresultB :: t(), SubresultC :: t().
append(
    SubresultA = #markdown_subresult{gfm_footnote_definitions = FnDefsA, definitions = DefsA},
    _SubresultB = #markdown_subresult{gfm_footnote_definitions = FnDefsB, definitions = DefsB}
) ->
    FnDefsC = markdown_vec:append(FnDefsA, FnDefsB),
    DefsC = markdown_vec:append(DefsA, DefsB),
    SubresultC = SubresultA#markdown_subresult{gfm_footnote_definitions = FnDefsC, definitions = DefsC},
    SubresultC.

-spec default() -> Subresult when Subresult :: t().
default() ->
    #markdown_subresult{
        done = false,
        gfm_footnote_definitions = markdown_vec:new(),
        definitions = markdown_vec:new()
    }.
