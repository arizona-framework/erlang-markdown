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
-module(markdown_mdast_reference).
-moduledoc """
A reference to something.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").

%% New API
-export([
    default/0,
    new/3
]).

%% Types
-doc """
The reference is explicit, its identifier inferred from its content.
""".
-type collapsed() :: collapsed.
-doc """
The reference is explicit, its identifier explicitly set.
""".
-type full() :: full.
-doc """
Explicitness of a reference.
""".
-type kind() :: shortcut() | collapsed() | full().
-doc """
The reference is implicit, its identifier inferred from its content.
""".
-type shortcut() :: shortcut.
-type t() :: #markdown_mdast_reference{}.

-export_type([
    collapsed/0,
    full/0,
    kind/0,
    shortcut/0,
    t/0
]).

%% Macros
-define(is_kind(X), ((X) =:= shortcut orelse (X) =:= collapsed orelse (X) =:= full)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Assume shortcut: removed on a resource, changed on a reference.
""".
-spec default() -> Reference when Reference :: t().
default() ->
    #markdown_mdast_reference{
        kind = {some, shortcut},
        identifier = <<>>,
        label = <<>>
    }.

-spec new(OptionKind, Identifier, Label) -> Reference when
    OptionKind :: markdown_option:t(Kind), Kind :: kind(), Identifier :: binary(), Label :: binary(), Reference :: t().
new(OptionKind = none, Identifier, Label) when is_binary(Identifier) andalso is_binary(Label) ->
    #markdown_mdast_reference{
        kind = OptionKind,
        identifier = Identifier,
        label = Label
    };
new(OptionKind = {some, Kind}, Identifier, Label) when
    ?is_kind(Kind) andalso is_binary(Identifier) andalso is_binary(Label)
->
    #markdown_mdast_reference{
        kind = OptionKind,
        identifier = Identifier,
        label = Label
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
