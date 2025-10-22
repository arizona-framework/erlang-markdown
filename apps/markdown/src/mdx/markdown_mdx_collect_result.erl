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
-module(markdown_mdx_collect_result).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-10", modified => "2025-10-10"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    new/0,
    push/3
]).

%% Types
-type t() :: #markdown_mdx_collect_result{}.

-export_type([
    t/0
]).

%% Macros

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> Result when Result :: t().
new() ->
    #markdown_mdx_collect_result{
        value = <<>>,
        stops = markdown_vec:new()
    }.

-spec push(Result, Value, Stop) -> Result when
    Result :: t(), Value :: unicode:unicode_binary(), Stop :: markdown_stop:t().
push(Result1 = #markdown_mdx_collect_result{value = Value1, stops = Stops1}, Value, Stop = #markdown_stop{}) when
    is_binary(Value)
->
    Stops2 = markdown_vec:push(Stops1, Stop),
    Value2 = <<Value1/bytes, Value/bytes>>,
    Result2 = Result1#markdown_mdx_collect_result{value = Value2, stops = Stops2},
    Result2.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
