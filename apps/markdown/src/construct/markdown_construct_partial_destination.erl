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
-module(markdown_construct_partial_destination).
-moduledoc """
Destination occurs in [definition][] and [label end][label_end].

## Grammar

Destination forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
destination ::= destination_enclosed | destination_raw

destination_enclosed ::= '<' *(destination_enclosed_byte | destination_enclosed_escape) '>'
destination_enclosed_byte ::= line - '<' - '\\' - '>'
destination_enclosed_escape ::= '\\' ['<' | '\\' | '>']

destination_raw ::= 1*(destination_raw_byte | destination_raw_escape)
; Restriction: unbalanced `)` characters are not allowed.
destination_raw_byte ::= text - '\\' - ascii_control
destination_raw_escape ::= '\\' ['(' | ')' | '\\']
```

Balanced parens allowed in raw destinations.
They are counted with a counter that starts at `0`, and is incremented
every time `(` occurs and decremented every time `)` occurs.
If `)` is found when the counter is `0`, the destination closes immediately
before it.
Escaped parens do not count in balancing.

The destination is interpreted as the [string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.

The grammar for enclosed destinations (`<x>`) prohibits the use of `<`,
`>`, and line endings to form URLs.
The angle brackets can be encoded as a character reference, character
escape, or percent encoding:

*   `<` as `&lt;`, `\<`, or `%3c`
*   `>` as `&gt;`, `\>`, or `%3e`

The grammar for raw destinations (`x`) prohibits space (` `) and all
[ASCII control][u8::is_ascii_control] characters, which thus must be
encoded.
Unbalanced parens can be encoded as a character reference, character escape,
or percent encoding:

*   `(` as `&lpar;`, `\(`, or `%28`
*   `)` as `&rpar;`, `\)`, or `%29`

There are several cases where incorrect encoding of URLs would, in other
languages, result in a parse error.
In markdown, there are no errors, and URLs are normalized.
In addition, unicode characters are percent encoded
([`sanitize_uri`][sanitize_uri]).
For example:

```markdown
[x]

[x]: <https://a👍b%>
```

Yields:

```html
<p><a href="https://a%F0%9F%91%8Db%25">x</a></p>
```

## Recommendation

It is recommended to use the enclosed variant of destinations, as it allows
the most characters, including arbitrary parens, in URLs.

## References

*   [`micromark-factory-destination/index.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-factory-destination/dev/index.js)

[definition]: crate::construct::definition
[string]: crate::construct::string
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[label_end]: crate::construct::label_end
[sanitize_uri]: crate::util::sanitize_uri
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    enclosed_before/1,
    enclosed/1,
    enclosed_escape/1,
    raw/1,
    raw_escape/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of destination.

```markdown
> | <aa>
    ^
> | aa
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState}) ->
    case Current of
        {some, $<} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_1),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, TokenizeState#markdown_tokenize_state.token_2),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, TokenizeState#markdown_tokenize_state.token_3),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, TokenizeState#markdown_tokenize_state.token_3),
            State = markdown_state:next(destination_enclosed_before),
            {Tokenizer6, State};
        %% ASCII control, space, closing paren, but *not* `\0`.
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, C} when (C >= 16#01 andalso C =< 16#1F) orelse C =:= $\s orelse C =:= $) orelse C =:= 16#7F ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_1),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, TokenizeState#markdown_tokenize_state.token_4),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, TokenizeState#markdown_tokenize_state.token_5),
            Link = markdown_event_link:new(none, none, string),
            Tokenizer5 = markdown_tokenizer:enter_link(Tokenizer4, data, Link),
            State = markdown_state:retry(destination_raw),
            {Tokenizer5, State}
    end.

-doc """
After `<`, at an enclosed destination.

```markdown
> | <aa>
     ^
```
""".
-spec enclosed_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
enclosed_before(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState}) ->
    case Current of
        {some, $>} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_3),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, TokenizeState#markdown_tokenize_state.token_3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, TokenizeState#markdown_tokenize_state.token_2),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, TokenizeState#markdown_tokenize_state.token_1),
            State = markdown_state:ok(),
            {Tokenizer6, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_5),
            Link = markdown_event_link:new(none, none, string),
            Tokenizer3 = markdown_tokenizer:enter_link(Tokenizer2, data, Link),
            State = markdown_state:retry(destination_enclosed),
            {Tokenizer3, State}
    end.

-doc """
In enclosed destination.

```markdown
> | <aa>
     ^
```
""".
-spec enclosed(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
enclosed(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $\n} ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $<} ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $>} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState#markdown_tokenize_state.token_5),
            State = markdown_state:retry(destination_enclosed_before),
            {Tokenizer3, State};
        {some, $\\} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_enclosed_escape),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_enclosed),
            {Tokenizer2, State}
    end.

-doc """
After `\\`, at a special character.

```markdown
> | <a\\*a>
       ^
```
""".
-spec enclosed_escape(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
enclosed_escape(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when C =:= $< orelse C =:= $> orelse C =:= $\\ ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_enclosed),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(destination_enclosed),
            {Tokenizer1, State}
    end.

-doc """
In raw destination.

```markdown
> | aa
    ^
```
""".
-spec raw(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
raw(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size, size_b = SizeB}
    }
) ->
    RawCase =
        case Current of
            none when Size =:= 0 -> raw_case_1;
            {some, C} when Size =:= 0 andalso (C =:= $\t orelse C =:= $\n orelse C =:= $\s orelse C =:= $)) ->
                raw_case_1;
            {some, $(} when Size < SizeB -> raw_case_2;
            {some, $)} ->
                raw_case_3;
            none ->
                raw_case_4;
            {some, C} when (C >= 16#01 andalso C =< 16#1F) orelse (C =:= $\s orelse C =:= $( orelse C =:= 16#7F) ->
                raw_case_4;
            {some, $\\} ->
                raw_case_5;
            {some, _} ->
                raw_case_6
        end,
    case RawCase of
        raw_case_1 ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState1#markdown_tokenize_state.token_5),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, TokenizeState1#markdown_tokenize_state.token_4),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, TokenizeState1#markdown_tokenize_state.token_1),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:ok(),
            {Tokenizer6, State};
        raw_case_2 ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:next(destination_raw),
            {Tokenizer3, State};
        raw_case_3 ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size - 1},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:next(destination_raw),
            {Tokenizer3, State};
        raw_case_4 ->
            %% ASCII control (but *not* `\0`) and space and `(`.
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        raw_case_5 ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_raw_escape),
            {Tokenizer2, State};
        raw_case_6 ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_raw),
            {Tokenizer2, State}
    end.

-doc """
After `\\`, at special character.

```markdown
> | a\\*a
      ^
```
""".
-spec raw_escape(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
raw_escape(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when (C =:= $( orelse C =:= $) orelse C =:= $\\) ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(destination_raw),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(destination_raw),
            {Tokenizer1, State}
    end.
