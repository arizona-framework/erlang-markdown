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
-module(markdown_mdast_attribute_value_expression).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #markdown_mdast_attribute_value_expression{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Value, Stops) -> AttributeValueExpression when
    Value :: binary(), Stops :: markdown_vec:t(Stop), Stop :: markdown_mdast_stop:t(), AttributeValueExpression :: t().
new(Value, Stops = #markdown_vec{}) when is_binary(Value) ->
    #markdown_mdast_attribute_value_expression{
        value = Value,
        stops = Stops
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
