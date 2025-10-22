%%%-----------------------------------------------------------------------------
%%% %CopyrightBegin%
%%%
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
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
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
