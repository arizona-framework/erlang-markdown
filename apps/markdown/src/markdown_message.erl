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
-module(markdown_message).
-moduledoc """
Message.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/4
]).

%% Types
-doc """
Message.
""".
-type t() :: #markdown_message{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionPlace, Reason, RuleId, Source) -> Message when
    OptionPlace :: markdown_types:option(Place),
    Place :: markdown_place:t(),
    Reason :: unicode:unicode_binary(),
    RuleId :: unicode:unicode_binary(),
    Source :: unicode:unicode_binary(),
    Message :: t().
new(OptionPlace, Reason, RuleId, Source) when
    ?is_option_record(OptionPlace, markdown_place) andalso is_binary(Reason) andalso is_binary(RuleId) andalso
        is_binary(Source)
->
    #markdown_message{
        place = OptionPlace,
        reason = Reason,
        rule_id = RuleId,
        source = Source
    }.
