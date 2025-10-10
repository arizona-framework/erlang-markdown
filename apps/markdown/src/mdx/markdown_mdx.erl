%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
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
