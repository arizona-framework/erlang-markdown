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
-module(markdown_mdx_collect_result).
-moduledoc """

""".
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
