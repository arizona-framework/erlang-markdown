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
-module(markdown_construct_character_escape).
-moduledoc """
Character escapes occur in the [string][] and [text][] content types.

## Grammar

Character escapes form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
character_escape ::= '\\' ascii_punctuation
```

Like much of markdown, there are no “invalid” character escapes: just a
slash, or a slash followed by anything other than an ASCII punctuation
character, is just a slash.

To escape other characters, use a [character reference][character_reference]
instead (as in, `&amp;`, `&#123;`, or say `&#x9;`).

It is also possible to escape a line ending in text with a similar
construct: a [hard break (escape)][hard_break_escape] is a backslash followed
by a line ending (that is part of the construct instead of ending it).

## Recommendation

If possible, use a character escape.
Otherwise, use a character reference.

## Tokens

*   [`CharacterEscape`][Name::CharacterEscape]
*   [`CharacterEscapeMarker`][Name::CharacterEscapeMarker]
*   [`CharacterEscapeValue`][Name::CharacterEscapeValue]

## References

*   [`character-escape.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/character-escape.js)
*   [*§ 2.4 Backslash escapes* in `CommonMark`](https://spec.commonmark.org/0.31/#backslash-escapes)

[string]: crate::construct::string
[text]: crate::construct::text
[character_reference]: crate::construct::character_reference
[hard_break_escape]: crate::construct::hard_break_escape
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    inside/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of character escape.

```markdown
> | a\*b
     ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $\\},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{character_escape = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_escape),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, character_escape_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, character_escape_marker),
    State = markdown_state:next(character_escape_inside),
    {Tokenizer5, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `\`, at punctuation.

```markdown
> | a\*b
      ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) ->
    %% ASCII punctuation.
    case
        (Current >= $! andalso Current =< $/) orelse
            (Current >= $: andalso Current =< $@) orelse
            (Current >= $[ andalso Current =< $`) orelse
            (Current >= ${ andalso Current =< $~)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, character_escape_value),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, character_escape_value),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, character_escape),
            State = markdown_state:ok(),
            {Tokenizer5, State};
        false ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end;
inside(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
