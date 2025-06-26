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
-module(markdown_construct_frontmatter).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of frontmatter.

```markdown
> | ---
    ^
  | title: "Venus"
  | ---
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{frontmatter = true}}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $+ orelse Current =:= $- ->
    %% Indent is not allowed.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Current},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, frontmatter),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, frontmatter_fence),
    Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, frontmatter_sequence),
    State = markdown_state:retry(frontmatter_open_sequence),
    {Tokenizer5, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
