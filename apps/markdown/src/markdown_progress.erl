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
-module(markdown_progress).
-moduledoc """
The internal state of a tokenizer.

Not to be confused with states from the state machine, this instead is all
the information on where we currently are and what's going on.
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
