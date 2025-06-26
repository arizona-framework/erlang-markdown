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
-module(markdown_mdast_stop).
-moduledoc """
MDX: relative byte index into a string, to an absolute byte index into the
whole document.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #markdown_mdast_stop{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Relative, Absolute) -> Stop when Relative :: non_neg_integer(), Absolute :: non_neg_integer(), Stop :: t().
new(Relative, Absolute) when ?is_non_neg_integer(Relative) andalso ?is_non_neg_integer(Absolute) ->
    #markdown_mdast_stop{
        relative = Relative,
        absolute = Absolute
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
