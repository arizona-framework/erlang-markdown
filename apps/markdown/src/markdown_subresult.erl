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
-module(markdown_subresult).
-moduledoc """

""".
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
