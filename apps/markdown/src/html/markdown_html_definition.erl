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
-module(markdown_html_definition).
-moduledoc """
Representation of a definition.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
