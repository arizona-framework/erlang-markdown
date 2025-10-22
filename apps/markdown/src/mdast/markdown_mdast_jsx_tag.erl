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
-module(markdown_mdast_jsx_tag).
-moduledoc """
Info on a tag.

JSX tags are parsed on their own.
They’re matched together here.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    new/6
]).

%% Types
-type t() :: #markdown_mdast_jsx_tag{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionName, Attributes, Close, SelfClosing, Start, End) -> JsxTag when
    OptionName :: markdown_option:t(Name),
    Name :: binary(),
    Attributes :: markdown_vec:t(Attribute),
    Attribute :: markdown_mdast_attribute_content:t(),
    Close :: boolean(),
    SelfClosing :: boolean(),
    Start :: markdown_unist_point:t(),
    End :: markdown_unist_point:t(),
    JsxTag :: t().
new(
    OptionName,
    Attributes = #markdown_vec{},
    Close,
    SelfClosing,
    Start = #markdown_unist_point{},
    End = #markdown_unist_point{}
) when ?is_option_binary(OptionName) andalso is_boolean(Close) andalso is_boolean(SelfClosing) ->
    #markdown_mdast_jsx_tag{
        name = OptionName,
        attributes = Attributes,
        close = Close,
        self_closing = SelfClosing,
        start = Start,
        'end' = End
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
