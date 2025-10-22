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
-module(markdown_event_link).
-moduledoc """
Link to another event.
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
    new/3
]).

%% Types
-doc "Link to another event.".
-type t() :: #markdown_event_link{}.

-export_type([
    t/0
]).

%% Macros
-define(is_content(X), ((X) =:= flow orelse (X) =:= content orelse (X) =:= string orelse (X) =:= text)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionPrevious, OptionNext, Content) -> Link when
    OptionPrevious :: markdown_types:option(Previous),
    OptionNext :: markdown_types:option(Next),
    Previous :: markdown_types:usize(),
    Next :: markdown_types:usize(),
    Content :: markdown_event:content(),
    Link :: t().
new(OptionPrevious, OptionNext, Content) when
    ?is_option_usize(OptionPrevious) andalso ?is_option_usize(OptionNext) andalso ?is_content(Content)
->
    #markdown_event_link{
        previous = OptionPrevious,
        next = OptionNext,
        content = Content
    }.
