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
-module(markdown_parse_state).
-moduledoc """
Info needed, in all content types, when parsing markdown.

Importantly, this contains a set of known definitions.
It also references the input value as bytes (`u8`).
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    append/2,
    bytes/1,
    new/3
]).

%% Types
-doc """
Info needed, in all content types, when parsing markdown.

Importantly, this contains a set of known definitions.
It also references the input value as bytes (`u8`).
""".
-type t() :: #markdown_parse_state{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec append(ParseState, Subresult) -> ParseState when ParseState :: t(), Subresult :: markdown_subresult:t().
append(
    ParseState = #markdown_parse_state{gfm_footnote_definitions = FnDefsA, definitions = DefsA},
    _Subresult = #markdown_subresult{gfm_footnote_definitions = FnDefsB, definitions = DefsB}
) ->
    FnDefsC = markdown_vec:append(FnDefsA, FnDefsB),
    DefsC = markdown_vec:append(DefsA, DefsB),
    ParseState#markdown_parse_state{gfm_footnote_definitions = FnDefsC, definitions = DefsC}.

-spec bytes(ParseState) -> Bytes when ParseState :: t(), Bytes :: binary().
bytes(#markdown_parse_state{bytes = Bytes}) -> Bytes.

-spec new(OptionLocation, ParseOptions, Bytes) -> ParseState when
    OptionLocation :: markdown_types:option(Location),
    Location :: markdown_location:t(),
    ParseOptions :: markdown_parse_options:t(),
    Bytes :: binary(),
    ParseState :: t().
new(OptionLocation, ParseOptions = #markdown_parse_options{}, Bytes) when
    ?is_option_record(OptionLocation, markdown_location) andalso is_binary(Bytes)
->
    #markdown_parse_state{
        location = OptionLocation,
        options = ParseOptions,
        bytes = Bytes,
        definitions = markdown_vec:new(),
        gfm_footnote_definitions = markdown_vec:new()
    }.
