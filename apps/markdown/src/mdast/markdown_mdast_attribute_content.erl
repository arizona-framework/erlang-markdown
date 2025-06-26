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
-module(markdown_mdast_attribute_content).
-moduledoc """
MDX: attribute content.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").

%% New API
-export([
    expression/1,
    property/1
]).

%% Types
-doc """
JSX expression.

```markdown
> | <a {...b} />
       ^^^^^^
```
""".
-type expression() :: {expression, markdown_mdast_mdx_jsx_expression_attribute:t()}.
-type inner() :: expression() | property().
-doc """
JSX property.

```markdown
> | <a b />
       ^
```
""".
-type property() :: {property, markdown_mdast_mdx_jsx_attribute:t()}.
-type t() :: #markdown_mdast_attribute_content{}.

-export_type([
    expression/0,
    inner/0,
    property/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-doc """
JSX expression.

```markdown
> | <a {...b} />
       ^^^^^^
```
""".
-spec expression(MdxJsxExpressionAttribute) -> AttributeContent when
    MdxJsxExpressionAttribute :: expression(), AttributeContent :: t().
expression(MdxJsxExpressionAttribute = #markdown_mdast_mdx_jsx_expression_attribute{}) ->
    #markdown_mdast_attribute_content{
        inner = {expression, MdxJsxExpressionAttribute}
    }.

-doc """
JSX property.

```markdown
> | <a b />
       ^
```
""".
-spec property(MdxJsxAttribute) -> AttributeContent when MdxJsxAttribute :: property(), AttributeContent :: t().
property(MdxJsxAttribute = #markdown_mdast_mdx_jsx_attribute{}) ->
    #markdown_mdast_attribute_content{
        inner = {property, MdxJsxAttribute}
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
