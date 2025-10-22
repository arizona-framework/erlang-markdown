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
-module(markdown_label).
-moduledoc """
Valid label.
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

-doc "Label start kind.".
-type kind() ::
    %% Label (image) start.
    %%
    %% ```markdown
    %% > | a ![b] c
    %%       ^^
    %% ```
    %%
    %% Construct: [Label start (image)][crate::construct::label_start_image].
    image
    %% Label (image) link.
    %%
    %% ```markdown
    %% > | a [b] c
    %%       ^
    %% ```
    %%
    %% Construct: [Label start (link)][crate::construct::label_start_link].
    | link
    %% GFM: Label (footnote) link.
    %%
    %% ```markdown
    %% > | a [^b] c
    %%       ^^
    %% ```
    %%
    %% Construct: [GFM: Label start (footnote)][crate::construct::gfm_label_start_footnote].
    | gfm_footnote
    %% GFM: Label (footnote) link, not matching a footnote definition, so
    %% handled as a label (link) start.
    %%
    %% ```markdown
    %% > | a [^b](c) d
    %%       ^^
    %% ```
    %%
    %% Construct: [Label end][crate::construct::label_end].
    | gfm_undefined_footnote.

-doc """
Valid label.
""".
-type t() :: #markdown_label{}.

-export_type([
    kind/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Kind, Start, End) -> Label when
    Kind :: kind(),
    Start :: markdown_indices:t(),
    End :: markdown_indices:t(),
    Label :: t().
new(Kind, Start = #markdown_indices{}, End = #markdown_indices{}) when ?is_markdown_label_kind(Kind) ->
    #markdown_label{kind = Kind, start = Start, 'end' = End}.
