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
-module(markdown_mdast_attribute_value).
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
