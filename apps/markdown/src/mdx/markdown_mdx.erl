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
-module(markdown_mdx).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-10", modified => "2025-10-10"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Types
-doc """
Signature of a function that parses MDX ESM.

Can be passed as `mdx_esm_parse` in
[`ParseOptions`][crate::configuration::ParseOptions] to support
ESM according to a certain grammar (typically, a programming language).
""".
-type esm_parse() :: fun((unicode:unicode_binary()) -> signal()).
-doc """
Expression kind.
""".
-type expression_kind() :: markdown_mdx_expression_kind:t().
-doc """
Signature of a function that parses MDX expressions.

Can be passed as `mdx_expression_parse` in
[`ParseOptions`][crate::configuration::ParseOptions] to support
expressions according to a certain grammar (typically, a programming
language).
""".
-type expression_parse() :: fun((unicode:unicode_binary(), expression_kind()) -> signal()).
-doc """
Signal used as feedback when parsing MDX ESM/expressions.
""".
-type signal() :: markdown_mdx_signal:t().

-export_type([
    esm_parse/0,
    expression_kind/0,
    expression_parse/0,
    signal/0
]).
