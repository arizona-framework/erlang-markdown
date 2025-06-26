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
-module(markdown_construct_partial_non_lazy_continuation).
-moduledoc """
Non-lazy continuation.

This is a tiny helper that [flow][] constructs can use to make sure that
the following line is not lazy.
For example, [html (flow)][html_flow] and ([raw (flow)][raw_flow],
[indented][code_indented]), stop when the next line is lazy.

[flow]: crate::construct::flow
[raw_flow]: crate::construct::raw_flow
[code_indented]: crate::construct::code_indented
[html_flow]: crate::construct::html_flow
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At eol, before continuation.

```markdown
> | * ```js
           ^
  | b
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(non_lazy_continuation_after),
    {Tokenizer4, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
A continuation.

```markdown
  | * ```js
> | b
    ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer = #markdown_tokenizer{lazy = true}) ->
    State = markdown_state:nok(),
    {Tokenizer, State};
'after'(Tokenizer = #markdown_tokenizer{lazy = false}) ->
    State = markdown_state:ok(),
    {Tokenizer, State}.
