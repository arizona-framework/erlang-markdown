%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_mdx_expression_kind).
-moduledoc """
Expression kind.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-10", modified => "2025-10-10"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    attribute_expression/0,
    attribute_value_expression/0,
    expression/0
]).

%% Types
-doc """
Kind of expressions as attributes.

```mdx
> | <a {...b}>
       ^^^^^^
```
""".
-type attribute_expression() :: attribute_expression.
-doc """
Kind of expressions as attribute values.

```mdx
> | <a b={c}>
         ^^^
```
""".
-type attribute_value_expression() :: attribute_value_expression.
-doc """
Kind of expressions in prose.

```mdx
> | # {Math.PI}
      ^^^^^^^^^
  |
> | {Math.PI}
    ^^^^^^^^^
```
""".
-type expression() :: expression.
-type t() :: attribute_expression() | attribute_value_expression() | expression().

-export_type([
    attribute_expression/0,
    attribute_value_expression/0,
    expression/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Kind of expressions as attributes.

```mdx
> | <a {...b}>
       ^^^^^^
```
""".
-compile({inline, [attribute_expression/0]}).
-spec attribute_expression() -> ExpressionKind when ExpressionKind :: t().
attribute_expression() -> attribute_expression.

-doc """
Kind of expressions as attribute values.

```mdx
> | <a b={c}>
         ^^^
```
""".
-compile({inline, [attribute_value_expression/0]}).
-spec attribute_value_expression() -> ExpressionKind when ExpressionKind :: t().
attribute_value_expression() -> attribute_value_expression.

-doc """
Kind of expressions in prose.

```mdx
> | # {Math.PI}
      ^^^^^^^^^
  |
> | {Math.PI}
    ^^^^^^^^^
```
""".
-compile({inline, [expression/0]}).
-spec expression() -> ExpressionKind when ExpressionKind :: t().
expression() -> expression.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
