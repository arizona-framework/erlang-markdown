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
-module(markdown_html_media).
-moduledoc """
Link, image, or footnote call.
Resource or reference.
Reused for temporary definitions as well, in the first pass.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_html.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/6
]).

%% Types
-type t() :: #markdown_html_media{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Image, OptionLabelId, OptionLabel, OptionReferenceId, OptionDestination, OptionTitle) -> Media when
    Image :: boolean(),
    OptionLabelId :: markdown_option:t(LabelId),
    LabelId :: markdown_indices:t(),
    OptionLabel :: markdown_option:t(Label),
    Label :: binary(),
    OptionReferenceId :: markdown_option:t(ReferenceId),
    ReferenceId :: markdown_indices:t(),
    OptionDestination :: markdown_option:t(Destination),
    Destination :: binary(),
    OptionTitle :: markdown_option:t(Title),
    Title :: binary(),
    Media :: t().
new(Image, OptionLabelId, OptionLabel, OptionReferenceId, OptionDestination, OptionTitle) when
    is_boolean(Image) andalso ?is_option_record(OptionLabelId, markdown_indices) andalso ?is_option_binary(OptionLabel) andalso
        ?is_option_record(OptionReferenceId, markdown_indices) andalso ?is_option_binary(OptionDestination) andalso
        ?is_option_binary(OptionTitle)
->
    #markdown_html_media{
        image = Image,
        label_id = OptionLabelId,
        label = OptionLabel,
        reference_id = OptionReferenceId,
        destination = OptionDestination,
        title = OptionTitle
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
