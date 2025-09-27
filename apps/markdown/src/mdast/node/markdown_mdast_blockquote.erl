%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  26 Jun 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_mdast_blockquote).
-moduledoc """
Block quote.

```markdown
> | > a
    ^^^
```
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").

%% API
-export([
    new/0
]).

%% Types
-type t() :: #markdown_mdast_blockquote{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> Blockquote when
    Blockquote :: t().
new() ->
    #markdown_mdast_blockquote{
        children = markdown_vec:new(),
        position = none
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
