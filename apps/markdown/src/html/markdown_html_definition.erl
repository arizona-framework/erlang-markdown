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
-module(markdown_html_definition).
-moduledoc """
Representation of a definition.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_html.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/3
]).

%% Types
-type t() :: #markdown_html_definition{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Id, OptionDestination, OptionTitle) -> Definition when
    Id :: binary(),
    OptionDestination :: markdown_option:t(Destination),
    Destination :: binary(),
    OptionTitle :: markdown_option:t(Title),
    Title :: binary(),
    Definition :: t().
new(Id, OptionDestination, OptionTitle) when
    is_binary(Id) andalso ?is_option_binary(OptionDestination) andalso ?is_option_binary(OptionTitle)
->
    #markdown_html_definition{
        id = Id,
        destination = OptionDestination,
        title = OptionTitle
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
