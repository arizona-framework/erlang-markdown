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
-module(markdown_construct_code_indented).
-moduledoc """
Code (indented) occurs in the [flow][] content type.

## Grammar

Code (indented) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
code_indented ::= filled_line *( eol *( blank_line eol ) filled_line )

; Restriction: at least one `line` byte must be `text`.
filled_line ::= 4(space_or_tab) *line
blank_line ::= *space_or_tab
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

In markdown, it is also possible to use [code (text)][raw_text] in the
[text][] content type.
It is also possible to create code with the [code (fenced)][raw_flow]
construct.

## HTML

Code (indented) relates to both the `<pre>` and the `<code>` elements in
HTML.
See [*§ 4.4.3 The `pre` element*][html_pre] and the [*§ 4.5.15 The `code`
element*][html_code] in the HTML spec for more info.

## Recommendation

It is recommended to use code (fenced) instead of code (indented).
Code (fenced) is more explicit, similar to code (text), and has support
for specifying the programming language.

## Tokens

*   [`CodeIndented`][Name::CodeIndented]
*   [`CodeFlowChunk`][Name::CodeFlowChunk]
*   [`LineEnding`][Name::LineEnding]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`code-indented.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/code-indented.js)
*   [*§ 4.4 Indented code blocks* in `CommonMark`](https://spec.commonmark.org/0.31/#indented-code-blocks)

[flow]: crate::construct::flow
[text]: crate::construct::text
[raw_flow]: crate::construct::raw_flow
[raw_text]: crate::construct::raw_text
[html_code]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-code-element
[html_pre]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-pre-element
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
    at_break/1,
    inside/1,
    'after'/1,
    further_start/1,
    further_begin/1,
    further_after/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of code (indented).

> **Parsing note**: it is not needed to check if this first line is a
> filled line (that it has a non-whitespace character), because blank lines
> are parsed already, so we never run into that.

```markdown
> |     aaa
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        interrupt = false,
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{code_indented = true}}
        },
        current = {some, Current}
    }
) when Current =:= $\t orelse Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, code_indented),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(code_indented_at_break), markdown_state:nok()
    ),
    {Tokenizer4, State} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer3, ?TAB_SIZE, ?TAB_SIZE
    ),
    {Tokenizer4, markdown_state:retry(State)};
start(_Tokenizer = #markdown_tokenizer{}) ->
    {_Tokenizer, markdown_state:nok()}.

-doc """
At a break.

```markdown
> |     aaa
        ^  ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    State = markdown_state:retry(code_indented_after),
    {Tokenizer1, State};
at_break(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(code_indented_at_break),
        markdown_state:next(code_indented_after)
    ),
    State = markdown_state:retry(code_indented_further_start),
    {Tokenizer2, State};
at_break(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, code_flow_chunk),
    State = markdown_state:retry(code_indented_inside),
    {Tokenizer2, State}.

-doc """
In code content.

```markdown
> |     aaa
        ^^^^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, code_flow_chunk),
    State = markdown_state:retry(code_indented_at_break),
    {Tokenizer2, State};
inside(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, code_flow_chunk),
    State = markdown_state:retry(code_indented_at_break),
    {Tokenizer2, State};
inside(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(code_indented_inside),
    {Tokenizer2, State}.

-doc """
After indented code.

```markdown
> |     aaa
           ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, code_indented),
    %% Feel free to interrupt.
    Tokenizer3 = Tokenizer2#markdown_tokenizer{interrupt = false},
    State = markdown_state:ok(),
    {Tokenizer3, State}.

-doc """
At eol, trying to parse another indent.

```markdown
> |     aaa
           ^
  |     bbb
```
""".
-spec further_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
further_start(Tokenizer1 = #markdown_tokenizer{lazy = true}) ->
    {Tokenizer1, markdown_state:nok()};
further_start(Tokenizer1 = #markdown_tokenizer{pierce = true}) ->
    {Tokenizer1, markdown_state:nok()};
further_start(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(code_indented_further_start),
    {Tokenizer4, State};
further_start(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:ok(), markdown_state:next(code_indented_further_begin)
    ),
    {Tokenizer3, SpaceState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer2, ?TAB_SIZE, ?TAB_SIZE
    ),
    {Tokenizer3, markdown_state:retry(SpaceState)}.

-doc """
At the beginning of a line that is not indented enough.

```markdown
  |     aaa
> |   bbb
    ^
```
""".
-spec further_begin(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
further_begin(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(code_indented_further_after), markdown_state:nok()
    ),
    {Tokenizer3, SpaceState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    {Tokenizer3, markdown_state:retry(SpaceState)};
further_begin(_Tokenizer = #markdown_tokenizer{}) ->
    {_Tokenizer, markdown_state:nok()}.

-doc """
After whitespace, not indented enough.

```markdown
  |     aaa
> |   bbb
      ^
```
""".
-spec further_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
further_after(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    State = markdown_state:retry(code_indented_further_start),
    {Tokenizer1, State};
further_after(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.
