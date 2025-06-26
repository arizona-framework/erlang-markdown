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
-module(markdown_construct_paragraph).
-moduledoc """
Paragraph occurs in the [content][] content type.

## Grammar

Paragraph forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: lines cannot start other flow constructs.
; Restriction: lines cannot be blank.
paragraph ::= 1*line *(eol 1*line)
```

This construct must be followed by an eol (line ending) or eof (end of
file), like flow constructs.

Paragraphs can contain line endings and whitespace, but they are not
allowed to contain blank lines, or to be blank themselves.

The paragraph is interpreted as the [text][] content type.
That means that [autolinks][autolink], [code (text)][raw_text], etc are
allowed.

## HTML

Paragraphs in markdown relate to the `<p>` element in HTML.
See [*§ 4.4.1 The `p` element* in the HTML spec][html] for more info.

## Tokens

*   [`Paragraph`][Name::Paragraph]

## References

*   [`content.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/content.js)
*   [*§ 4.8 Paragraphs* in `CommonMark`](https://spec.commonmark.org/0.31/#paragraphs)

[content]: crate::construct::content
[text]: crate::construct::text
[autolink]: crate::construct::autolink
[raw_text]: crate::construct::raw_text
[html]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-p-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    line_start/1,
    inside/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Paragraph start.

```markdown
> | abc
    ^
  | def
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, _}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, paragraph),
    {Tokenizer2, markdown_state:retry(paragraph_line_start)}.

-doc """
Start of a line in a paragraph.

```markdown
> | abc
    ^
> | def
    ^
```
""".
-spec line_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
line_start(Tokenizer1 = #markdown_tokenizer{current = {some, _}}) ->
    Tokenizer2 =
        #markdown_tokenizer{events = Events1, tokenize_state = TokenizeState1} = markdown_tokenizer:enter_link(
            Tokenizer1,
            data,
            markdown_event_link:new(none, none, text)
        ),
    Tokenizer3 =
        case TokenizeState1#markdown_tokenize_state.connect of
            true ->
                Index = markdown_vec:size(Events1) - 1,
                Events1_1 = Events1,
                Events1_2 = markdown_subtokenizer:link(Events1_1, Index),
                Tokenizer2#markdown_tokenizer{events = Events1_2};
            false ->
                TokenizeState1_1 = TokenizeState1,
                TokenizeState1_2 = TokenizeState1_1#markdown_tokenize_state{connect = true},
                Tokenizer2_1 = Tokenizer2,
                Tokenizer2_2 = Tokenizer2_1#markdown_tokenizer{tokenize_state = TokenizeState1_2},
                Tokenizer2_2
        end,
    {Tokenizer3, markdown_state:retry(paragraph_inside)}.

-doc """
In paragraph.

```markdown
> | abc
    ^^^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = none, tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{connect = false},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, data),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, paragraph),
    {Tokenizer4, markdown_state:ok()};
inside(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, data),
    {Tokenizer3, markdown_state:next(paragraph_line_start)};
inside(Tokenizer1 = #markdown_tokenizer{current = {some, _}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    {Tokenizer2, markdown_state:next(paragraph_inside)}.
