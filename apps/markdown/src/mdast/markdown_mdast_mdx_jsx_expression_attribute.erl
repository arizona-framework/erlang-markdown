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
-module(markdown_mdast_mdx_jsx_expression_attribute).
-moduledoc """
MDX: JSX expression attribute.

```markdown
> | <a {...b} />
       ^
```
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    new/2
]).

%% Types
-type t() :: #markdown_mdast_mdx_jsx_expression_attribute{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Value, Stops) -> MdxJsxExpressionAttribute when
    Value :: binary(), Stops :: markdown_vec:t(Stop), Stop :: markdown_mdast_stop:t(), MdxJsxExpressionAttribute :: t().
new(Value, Stops = #markdown_vec{}) when is_binary(Value) ->
    #markdown_mdast_mdx_jsx_expression_attribute{
        value = Value,
        stops = Stops
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
