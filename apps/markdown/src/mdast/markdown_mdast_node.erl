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
-module(markdown_mdast_node).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").

%% New API
-export([
    blockquote/1,
    root/1
]).

%% Types
-type inner() ::
    % Document:
    %% Root.
    markdown_mdast_root:t()
    % Container:
    %% Block quote.
    | markdown_mdast_blockquote:t().
-type t() :: #markdown_mdast_node{}.

-export_type([
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec blockquote(Inner) -> Node when Inner :: markdown_mdast_blockquote:t(), Node :: t().
blockquote(Inner = #markdown_mdast_blockquote{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec root(Inner) -> Node when Inner :: markdown_mdast_root:t(), Node :: t().
root(Inner = #markdown_mdast_root{}) ->
    #markdown_mdast_node{inner = Inner}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
