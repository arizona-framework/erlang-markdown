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
-module(markdown_mdast_attribute_content).
-moduledoc """
MDX: attribute content.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
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
