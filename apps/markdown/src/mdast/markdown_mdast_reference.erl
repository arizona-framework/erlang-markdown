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
-module(markdown_mdast_reference).
-moduledoc """
A reference to something.
""".
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
