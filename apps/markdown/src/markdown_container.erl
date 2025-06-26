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
-module(markdown_container).
-moduledoc """
Containers.

Containers are found when tokenizing
[document content][crate::construct::document].
They parse a portion at the start of one or more lines.
The rest of those lines is a different content type (specifically, flow),
which they “contain”.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    block_quote/2,
    list_item/2,
    gfm_footnote_definition/2
]).

%% Types

-doc "Container kind.".
-type kind() ::
    %% [Block quote][crate::construct::block_quote].
    block_quote
    %% [List item][crate::construct::list_item].
    | list_item
    %% [GFM: Footnote definition][crate::construct::gfm_footnote_definition].
    | gfm_footnote_definition.

-doc """
Info used to tokenize a container.

Practically, these fields are only used for list items.
""".
-type t() :: #markdown_container{}.

-export_type([
    kind/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec block_quote(BlankInitial, Size) -> Container when
    BlankInitial :: boolean(), Size :: markdown_types:usize(), Container :: t().
block_quote(BlankInitial, Size) when is_boolean(BlankInitial) andalso ?is_usize(Size) ->
    #markdown_container{kind = ?FUNCTION_NAME, blank_initial = BlankInitial, size = Size}.

-spec list_item(BlankInitial, Size) -> Container when
    BlankInitial :: boolean(), Size :: markdown_types:usize(), Container :: t().
list_item(BlankInitial, Size) when is_boolean(BlankInitial) andalso ?is_usize(Size) ->
    #markdown_container{kind = ?FUNCTION_NAME, blank_initial = BlankInitial, size = Size}.

-spec gfm_footnote_definition(BlankInitial, Size) -> Container when
    BlankInitial :: boolean(), Size :: markdown_types:usize(), Container :: t().
gfm_footnote_definition(BlankInitial, Size) when is_boolean(BlankInitial) andalso ?is_usize(Size) ->
    #markdown_container{kind = ?FUNCTION_NAME, blank_initial = BlankInitial, size = Size}.
