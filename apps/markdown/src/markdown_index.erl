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
-module(markdown_index).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/2
]).

%% Types
-doc "0-indexed integer representing a byte in a source file.".
-type offset() :: non_neg_integer().
-doc """
One place in a source file.
""".
-type t() :: #markdown_index{}.
-doc "0-indexed integer representing a virtual byte in a source file.".
-type virtual() :: non_neg_integer().

-export_type([
    offset/0,
    t/0,
    virtual/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Offset, Virtual) -> Point when
    Offset :: offset(), Virtual :: virtual(), Point :: t().
new(Offset, Virtual) when ?is_non_neg_integer(Offset) andalso ?is_non_neg_integer(Virtual) ->
    #markdown_index{offset = Offset, virtual = Virtual}.
