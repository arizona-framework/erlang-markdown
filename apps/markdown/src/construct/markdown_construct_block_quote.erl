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
-module(markdown_construct_block_quote).
-moduledoc """
Block quotes occur in the [document][] content type.

## Grammar

Block quotes form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
block_quote_start ::= '>' [ space_or_tab ]
block_quote_cont ::= '>' [ space_or_tab ]
```

Further lines that are not prefixed with `block_quote_cont` cause the block
quote to be exited, except when those lines are lazy continuation.
Like so many things in markdown, block quotes too are complex.
See [*§ Phase 1: block structure* in `CommonMark`][commonmark-block] for
more on parsing details.

As block quote is a container, it takes several bytes from the start of the
line, while the rest of the line includes more containers or flow.

## HTML

Block quote relates to the `<blockquote>` element in HTML.
See [*§ 4.4.4 The `blockquote` element*][html-blockquote] in the HTML spec
for more info.

## Recommendation

Always use a single space after a block quote marker (`>`).
Never use lazy continuation.

## Tokens

*   [`BlockQuote`][Name::BlockQuote]
*   [`BlockQuoteMarker`][Name::BlockQuoteMarker]
*   [`BlockQuotePrefix`][Name::BlockQuotePrefix]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`block-quote.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/block-quote.js)
*   [*§ 5.1 Block quotes* in `CommonMark`](https://spec.commonmark.org/0.31/#block-quotes)

[document]: crate::construct::document
[html-blockquote]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-blockquote-element
[commonmark-block]: https://spec.commonmark.org/0.31/#phase-1-block-structure
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    cont_start/1,
    cont_before/1,
    cont_after/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of block quote.

```markdown
> | > a
   ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{block_quote = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, block_quote),
    State = markdown_state:retry(block_quote_cont_start),
    {Tokenizer2, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
Start of block quote continuation.

Also used to parse the first block quote opening.

```markdown
| > a
> | > b
    ^
```
""".
-spec cont_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{options = #markdown_parse_options{constructs = Constructs}}
    }
) when Current =:= $\t orelse Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(block_quote_cont_before),
        markdown_state:nok()
    ),
    MaxIndent =
        case Constructs#markdown_construct_options.code_indented of
            true -> ?TAB_SIZE - 1;
            false -> infinity
        end,
    {Tokenizer3, StateName} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(Tokenizer2, 1, MaxIndent),
    State = markdown_state:retry(StateName),
    {Tokenizer3, State};
cont_start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(block_quote_cont_before),
    {Tokenizer, State}.

-doc """
At `>`, after optional whitespace.

Also used to parse the first block quote opening.

```markdown
    | > a
> | > b
    ^
```
""".
-spec cont_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_before(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, block_quote_prefix),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, block_quote_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, block_quote_marker),
    State = markdown_state:next(block_quote_cont_after),
    {Tokenizer5, State};
cont_before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `>`, before optional whitespace.

```markdown
> | > a
     ^
> | >b
     ^
```
""".
-spec cont_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, space_or_tab),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, space_or_tab),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, block_quote_prefix),
    State = markdown_state:ok(),
    {Tokenizer5, State};
cont_after(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, block_quote_prefix),
    State = markdown_state:ok(),
    {Tokenizer2, State}.
