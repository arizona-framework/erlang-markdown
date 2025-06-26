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
-module(markdown_label_start).
-moduledoc """
Label start, looking for an end.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/3
]).

%% Types

-doc "Label start kind.".
-type kind() :: markdown_label:kind().

-doc """
Label start, looking for an end.
""".
-type t() :: #markdown_label_start{}.

-export_type([
    kind/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Kind, Start, Inactive) -> LabelStart when
    Kind :: kind(), Start :: markdown_indices:t(), Inactive :: boolean(), LabelStart :: t().
new(Kind, Start = #markdown_indices{}, Inactive) when ?is_markdown_label_kind(Kind) andalso is_boolean(Inactive) ->
    #markdown_label_start{kind = Kind, start = Start, inactive = Inactive}.
