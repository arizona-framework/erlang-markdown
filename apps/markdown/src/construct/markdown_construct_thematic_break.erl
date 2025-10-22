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
-module(markdown_construct_thematic_break).
-moduledoc """
Thematic break occurs in the [flow][] content type.

## Grammar

Thematic break forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: all markers must be identical.
; Restriction: at least 3 markers must be used.
thematic_break ::= *space_or_tab 1*(1*marker *space_or_tab)

marker ::= '*' | '-' | '_'
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

## HTML

Thematic breaks in markdown typically relate to the HTML element `<hr>`.
See [*§ 4.4.2 The `hr` element* in the HTML spec][html] for more info.

## Recommendation

It is recommended to use exactly three asterisks without whitespace when
writing markdown.
As using more than three markers has no effect other than wasting space,
it is recommended to use exactly three markers.
Thematic breaks formed with asterisks or dashes can interfere with
[list][list-item]s if there is whitespace between them: `* * *` and `- - -`.
For these reasons, it is recommend to not use spaces or tabs between the
markers.
Thematic breaks formed with dashes (without whitespace) can also form
[heading (setext)][heading_setext].
As dashes and underscores frequently occur in natural language and URLs, it
is recommended to use asterisks for thematic breaks to distinguish from
such use.
Because asterisks can be used to form the most markdown constructs, using
them has the added benefit of making it easier to gloss over markdown: you
can look for asterisks to find syntax while not worrying about other
characters.

## Tokens

*   [`ThematicBreak`][Name::ThematicBreak]
*   [`ThematicBreakSequence`][Name::ThematicBreakSequence]

## References

*   [`thematic-break.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/thematic-break.js)
*   [*§ 4.1 Thematic breaks* in `CommonMark`](https://spec.commonmark.org/0.31/#thematic-breaks)

[flow]: crate::construct::flow
[heading_setext]: crate::construct::heading_setext
[list-item]: crate::construct::list_item
[html]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-hr-element
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
    before/1,
    at_break/1,
    sequence/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of thematic break.

```markdown
> | ***
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{thematic_break = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, thematic_break),
    case Current of
        C when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(thematic_break_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true ->
                        ?TAB_SIZE - 1;
                    false ->
                        infinity
                end,
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxIndent
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(thematic_break_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After optional whitespace, at marker.

```markdown
> | ***
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}, tokenize_state = TokenizeState1}) when
    Current =:= $* orelse Current =:= $- orelse Current =:= $_
->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Current},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(thematic_break_at_break),
    {Tokenizer2, State};
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After something, before something else.

```markdown
> | ***
    ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(
    Tokenizer = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = #markdown_tokenize_state{marker = Marker}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer, thematic_break_sequence),
    State = markdown_state:retry(thematic_break_sequence),
    {Tokenizer2, State};
at_break(
    Tokenizer = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{size = Size}
    }
) when
    Size >= ?THEMATIC_BREAK_MARKER_COUNT_MIN, (Current =:= none orelse Current =:= {some, $\n})
->
    TokenizeState2 = TokenizeState#markdown_tokenize_state{marker = 0, size = 0},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, thematic_break),
    %% Feel free to interrupt.
    Tokenizer4 = Tokenizer3#markdown_tokenizer{interrupt = false},
    State = markdown_state:ok(),
    {Tokenizer4, State};
at_break(Tokenizer = #markdown_tokenizer{tokenize_state = TokenizeState}) ->
    TokenizeState2 = TokenizeState#markdown_tokenize_state{marker = 0, size = 0},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In sequence.

```markdown
> | ***
    ^
```
""".
-spec sequence(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence(
    Tokenizer = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState = #markdown_tokenize_state{marker = Marker, size = Size}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer),
    TokenizeState2 = TokenizeState#markdown_tokenize_state{size = Size + 1},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(thematic_break_sequence),
    {Tokenizer3, State};
sequence(Tokenizer = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t; Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer, thematic_break_sequence),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(thematic_break_at_break), markdown_state:nok()
    ),
    {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer3),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer4, State};
sequence(Tokenizer) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer, thematic_break_sequence),
    State = markdown_state:retry(thematic_break_at_break),
    {Tokenizer2, State}.
