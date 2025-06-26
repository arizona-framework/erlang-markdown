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
-module(markdown_progress).
-moduledoc """
The internal state of a tokenizer.

Not to be confused with states from the state machine, this instead is all
the information on where we currently are and what's going on.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/5
]).

%% Types

-doc """
The internal state of a tokenizer.

Not to be confused with states from the state machine, this instead is all
the information on where we currently are and what's going on.
""".
-type t() :: #markdown_progress{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(EventsLen, StackLen, OptionPrevious, OptionCurrent, Point) -> Progress when
    EventsLen :: markdown_types:usize(),
    StackLen :: markdown_types:usize(),
    OptionPrevious :: markdown_types:option(byte()),
    OptionCurrent :: markdown_types:option(byte()),
    Point :: markdown_point:t(),
    Progress :: t().
new(EventsLen, StackLen, OptionPrevious, OptionCurrent, Point = #markdown_point{}) when
    ?is_usize(EventsLen) andalso ?is_usize(StackLen) andalso ?is_option_u8(OptionPrevious) andalso
        ?is_option_u8(OptionCurrent)
->
    #markdown_progress{
        events_len = EventsLen,
        stack_len = StackLen,
        previous = OptionPrevious,
        current = OptionCurrent,
        point = Point
    }.
