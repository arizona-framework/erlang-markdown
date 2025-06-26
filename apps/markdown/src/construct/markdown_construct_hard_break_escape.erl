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
-module(markdown_construct_hard_break_escape).
-moduledoc """
Hard break (escape) occurs in the  [text][] content type.

## Grammar

Hard break (escape) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: followed by a line ending  (that is part of the content
; instead of ending it).
hard_break_escape ::= '\\'
```

It is also possible to create a hard break with a
[hard break (trailing)][hard_break_trailing].

Punctuation characters can be escaped with a similar
construct: a [character escape][character_escape] is a backslash followed
by an ASCII punctuation character.
Arbitrary characters can be escaped with
[character references][character_reference].

## HTML

Hard breaks in markdown relate to the HTML element `<br>`.
See [*§ 4.5.27 The `br` element* in the HTML spec][html] for more info.

## Recommendation

Always use hard break (escape), never hard break (trailing).

## Tokens

*   [`HardBreakEscape`][Name::HardBreakEscape]

## References

*   [`hard-break-escape.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/hard-break-escape.js)
*   [*§ 6.7 Hard line breaks* in `CommonMark`](https://spec.commonmark.org/0.31/#hard-line-breaks)

[text]: crate::construct::text
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[hard_break_trailing]: crate::construct::partial_whitespace
[html]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-br-element
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
Start of hard break (escape).

```markdown
> | a\\
    ^
 | b
\```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $\\},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{hard_break_escape = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, hard_break_escape),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(hard_break_escape_after),
    {Tokenizer3, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `\\`, at eol.

```markdown
> | a\\
     ^
 | b
\```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, hard_break_escape),
    State = markdown_state:ok(),
    {Tokenizer2, State};
'after'(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
