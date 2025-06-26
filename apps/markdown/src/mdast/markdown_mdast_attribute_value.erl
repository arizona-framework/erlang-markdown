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
-module(markdown_mdast_attribute_value).
-moduledoc """
MDX: attribute content.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").

%% New API
-export([
    expression/1,
    literal/1
]).

%% Types
-doc """
Expression value.

```markdown
> | <a b={c} />
         ^^^
```
""".
-type expression() :: markdown_mdast_attribute_value_expression:t().
-type inner() :: {expression, expression()} | {literal, literal()}.
-doc """
Static value.

```markdown
> | <a b="c" />
         ^^^
```
""".
-type literal() :: binary().
-type t() :: #markdown_mdast_attribute_value{}.

-export_type([
    expression/0,
    inner/0,
    literal/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-doc """
Expression value.

```markdown
> | <a b={c} />
         ^^^
```
""".
-spec expression(AttributeValueExpression) -> AttributeValue when
    AttributeValueExpression :: expression(), AttributeValue :: t().
expression(AttributeValueExpression = #markdown_mdast_attribute_value_expression{}) ->
    #markdown_mdast_attribute_value{
        inner = {expression, AttributeValueExpression}
    }.

-doc """
Static value.

```markdown
> | <a b="c" />
         ^^^
```
""".
-spec literal(String) -> AttributeValue when String :: literal(), AttributeValue :: t().
literal(String) when is_binary(String) ->
    #markdown_mdast_attribute_value{
        inner = {literal, String}
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
