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
-module(markdown_construct_character_reference).
-moduledoc """
Character references occur in the [string][] and [text][] content types.

## Grammar

Character references form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
character_reference ::= '&' (numeric | named) ';'

numeric ::= '#' (hexadecimal | decimal)
; Note: Limit of `6` imposed, as all bigger numbers are invalid.
hexadecimal ::= ('x' | 'X') 1*6(ascii_hexdigit)
; Note: Limit of `7` imposed, as all bigger numbers are invalid.
decimal ::= 1*7(ascii_digit)
; Note: Limit of `31` imposed, for `CounterClockwiseContourIntegral`.
; Note: Limited to any known named character reference (see `constants.rs`)
named ::= 1*31(ascii_alphanumeric)
```

Like much of markdown, there are no “invalid” character references.
However, for security reasons, several numeric character references parse
fine but are not rendered as their corresponding character.
They are instead replaced by a U+FFFD REPLACEMENT CHARACTER (`�`).
See [`decode_numeric`][decode_numeric] for more info.

To escape ASCII punctuation characters, use the terser
[character escape][character_escape] construct instead (as in, `\&`).

Character references in markdown are not the same as character references
in HTML.
Notably, HTML allows several character references without a closing
semicolon.
See [*§ 13.2.5.72 Character reference state* in the HTML spec][html] for more info.

Character references are parsed insensitive to casing.
The casing of hexadecimal numeric character references has no effect.
The casing of named character references does not matter when parsing, but
does affect whether they match.
Depending on the name, one or more cases are allowed, such as that `AMP`
and `amp` are both allowed but other cases are not.
See [`CHARACTER_REFERENCES`][character_references] for which
names match.

## Recommendation

If possible, use a character escape.
Otherwise, use a character reference.

## Tokens

*   [`CharacterReference`][Name::CharacterReference]
*   [`CharacterReferenceMarker`][Name::CharacterReferenceMarker]
*   [`CharacterReferenceMarkerHexadecimal`][Name::CharacterReferenceMarkerHexadecimal]
*   [`CharacterReferenceMarkerNumeric`][Name::CharacterReferenceMarkerNumeric]
*   [`CharacterReferenceMarkerSemi`][Name::CharacterReferenceMarkerSemi]
*   [`CharacterReferenceValue`][Name::CharacterReferenceValue]

## References

*   [`character-reference.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/character-reference.js)
*   [*§ 2.5 Entity and numeric character references* in `CommonMark`](https://spec.commonmark.org/0.31/#entity-and-numeric-character-references)

[string]: crate::construct::string
[text]: crate::construct::text
[character_escape]: crate::construct::character_reference
[decode_numeric]: crate::util::character_reference::decode_numeric
[character_references]: crate::util::constant::CHARACTER_REFERENCES
[html]: https://html.spec.whatwg.org/multipage/parsing.html#character-reference-state
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
    open/1,
    numeric/1,
    value/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of character reference.

```markdown
> | a&amp;b
     ^
> | a&#123;b
     ^
> | a&#x9;b
     ^
\```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $&},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{character_reference = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_reference),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, character_reference_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, character_reference_marker),
    State = markdown_state:next(character_reference_open),
    {Tokenizer5, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `&`, at `#` for numeric references or alphanumeric for named
references.

```markdown
> | a&amp;b
      ^
> | a&#123;b
      ^
> | a&#x9;b
      ^
\```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = {some, $#}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_reference_marker_numeric),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, character_reference_marker_numeric),
    State = markdown_state:next(character_reference_numeric),
    {Tokenizer4, State};
open(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = $&},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, character_reference_value),
    State = markdown_state:retry(character_reference_value),
    {Tokenizer3, State}.

-doc """
After `#`, at `x` for hexadecimals or digit for decimals.

```markdown
> | a&#123;b
       ^
> | a&#x9;b
       ^
\```
""".
-spec numeric(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
numeric(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $x orelse Current =:= $X ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_reference_marker_hexadecimal),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, character_reference_marker_hexadecimal),
    Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, character_reference_value),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = $x},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(character_reference_value),
    {Tokenizer6, State};
numeric(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_reference_value),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = $#},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(character_reference_value),
    {Tokenizer3, State}.

-doc """
After markers (`&#x`, `&#`, or `&`), in value, before `;`.

The character reference kind defines what and how many characters are
allowed.

```markdown
> | a&amp;b
      ^^^
> | a&#123;b
       ^^^
> | a&#x9;b
        ^
\```
""".
-spec value(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
value(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $;},
        parse_state = #markdown_parse_state{bytes = Bytes},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size}
    }
) when Size > 0 ->
    %% Named.
    case Marker of
        $& ->
            %% Guaranteed to be valid ASCII bytes.
            Slice = markdown_slice:from_indices(
                Bytes,
                Tokenizer1#markdown_tokenizer.point#markdown_point.offset - Size,
                Tokenizer1#markdown_tokenizer.point#markdown_point.offset
            ),
            case markdown_util_character_reference:decode_named(markdown_slice:as_binary(Slice), true) of
                none ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:nok(),
                    {Tokenizer2, State};
                _ ->
                    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, character_reference_value),
                    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, character_reference_marker_semi),
                    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
                    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, character_reference_marker_semi),
                    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, character_reference),
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
                    Tokenizer7 = Tokenizer6#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:ok(),
                    {Tokenizer7, State}
            end;
        _ ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, character_reference_value),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, character_reference_marker_semi),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, character_reference_marker_semi),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, character_reference),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
            Tokenizer7 = Tokenizer6#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:ok(),
            {Tokenizer7, State}
    end;
value(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size}
    }
) ->
    ValueMax = markdown_util_character_reference:value_max(Marker),
    case Size < ValueMax andalso markdown_util_character_reference:value_test(Marker, Current) of
        true ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(character_reference_value),
            {Tokenizer3, State};
        false ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end;
value(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.
