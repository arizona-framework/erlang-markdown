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
-module(markdown_tokenize_state).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    default/0
]).

%% Types

-doc """

""".
-type t() :: #markdown_tokenize_state{}.

-export_type([
    t/0
]).

%% Macros

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec default() -> TokenizeState when TokenizeState :: t().
default() ->
    #markdown_tokenize_state{}.
