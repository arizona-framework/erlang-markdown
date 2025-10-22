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
-module(markdown_construct_partial_bom).
-moduledoc """
Byte order mark occurs at the start of the document.

## Grammar

Byte order mark forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
byte_order_mark ::= 0xEF 0xBB 0xBF
```

## Recommendation

Don’t use BOMs.

## Tokens

*   [`ByteOrderMark`][Name::ByteOrderMark]

## References

*   [`micromark/lib/preprocess.js` in `micromark`](https://github.com/micromark/micromark/blob/ed23453/packages/micromark/dev/lib/preprocess.js#L54-L60)
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    inside/1
]).

%% Macros
-define(BOM_0, 16#EF).
-define(BOM_1, 16#BB).
-define(BOM_2, 16#BF).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Before BOM.

```text
> | 0xEF 0xBB 0xBF
    ^^^^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, ?BOM_0}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, byte_order_mark),
    State = markdown_state:retry(bom_inside),
    {Tokenizer2, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In BOM.

```text
> | 0xEF 0xBB 0xBF
    ^^^^ ^^^^ ^^^^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, ?BOM_0}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = 0}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 1},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(bom_inside),
    {Tokenizer3, State};
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, ?BOM_1}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = 1}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(bom_inside),
    {Tokenizer3, State};
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, ?BOM_2}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = 2}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, byte_order_mark),
    State = markdown_state:ok(),
    {Tokenizer4, State};
inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.
