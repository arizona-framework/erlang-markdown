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
-module(markdown_parse_state).
-moduledoc """
Info needed, in all content types, when parsing markdown.

Importantly, this contains a set of known definitions.
It also references the input value as bytes (`u8`).
""".
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
