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
-module(markdown_construct_raw_text).
-moduledoc """
Raw (text) occurs in the [text][] content type.
It forms code (text) and math (text).

## Grammar

Raw (text) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: the number of markers in the closing sequence must be equal
; to the number of markers in the opening sequence.
raw_text ::= sequence 1*byte sequence

; Restriction: not preceded or followed by the same marker.
sequence ::= 1*'`' | 1*'$'
```

The above grammar shows that it is not possible to create empty raw (text).
It is possible to include the sequence marker (grave accent for code,
dollar for math) in raw (text), by wrapping it in bigger or smaller
sequences:

```markdown
Include more: `a``b` or include less: ``a`b``.
```

It is also possible to include just one marker:

```markdown
Include just one: `` ` ``.
```

Sequences are “gready”, in that they cannot be preceded or followed by
more markers.
To illustrate:

```markdown
Not code: ``x`.

Not code: `x``.

Escapes work, this is code: \``x`.

Escapes work, this is code: `x`\`.
```

Yields:

```html
<p>Not code: ``x`.</p>
<p>Not code: `x``.</p>
<p>Escapes work, this is code: `<code>x</code>.</p>
<p>Escapes work, this is code: <code>x</code>`.</p>
```

That is because, when turning markdown into HTML, the first and last space,
if both exist and there is also a non-space in the code, are removed.
Line endings, at that stage, are considered as spaces.

In markdown, it is possible to create code or math with the
[raw (flow)][raw_flow] (or [code (indented)][code_indented]) constructs
in the [flow][] content type.

## HTML

Code (text) relates to the `<code>` element in HTML.
See [*§ 4.5.15 The `code` element*][html_code] in the HTML spec for more
info.

Math (text) does not relate to HTML elements.
`MathML`, which is sort of like SVG but for math, exists but it doesn’t work
well and isn’t widely supported.
Instead, it is recommended to use client side JavaScript with something like
`KaTeX` or `MathJax` to process the math
For that, the math is compiled as a `<code>` element with two classes:
`language-math` and `math-inline`.
Client side JavaScript can look for these classes to process them further.

When turning markdown into HTML, each line ending in raw (text) is turned
into a space.

## Recommendations

When authoring markdown with math, keep in mind that math doesn’t work in
most places.
Notably, GitHub currently has a really weird crappy client-side regex-based
thing.
But on your own (math-heavy?) site it can be great!
You can set [`parse_options.math_text_single_dollar: false`][parse_options]
to improve this, as it prevents single dollars from being seen as math, and
thus prevents normal dollars in text from being seen as math.

## Tokens

*   [`CodeText`][Name::CodeText]
*   [`CodeTextData`][Name::CodeTextData]
*   [`CodeTextSequence`][Name::CodeTextSequence]
*   [`MathText`][Name::MathText]
*   [`MathTextData`][Name::MathTextData]
*   [`MathTextSequence`][Name::MathTextSequence]
*   [`LineEnding`][Name::LineEnding]

## References

*   [`code-text.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/code-text.js)
*   [`micromark-extension-math`](https://github.com/micromark/micromark-extension-math)
*   [*§ 6.1 Code spans* in `CommonMark`](https://spec.commonmark.org/0.31/#code-spans)

> 👉 **Note**: math is not specified anywhere.

[flow]: crate::construct::flow
[text]: crate::construct::text
[code_indented]: crate::construct::code_indented
[raw_flow]: crate::construct::raw_flow
[html_code]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-code-element
[parse_options]: crate::ParseOptions
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    sequence_open/1,
    between/1,
    data/1,
    sequence_close/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of raw (text).

```markdown
> | `a`
    ^
> | \`a`
     ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        previous = Previous,
        parse_state = #markdown_parse_state{options = #markdown_parse_options{constructs = Constructs}},
        events = Events
    }
) ->
    %% Code (text):
    case
        ((Constructs#markdown_construct_options.code_text =:= true andalso Current =:= {some, $`}) orelse
            %% Math (text):
            (Constructs#markdown_construct_options.math_text =:= true andalso Current =:= {some, $$})) andalso
            %% Not the same marker (except when escaped).
            (Previous =/= Current orelse
                ((not markdown_vec:is_empty(Events)) andalso
                    (markdown_vec:last(Events))#markdown_event.name =:= character_escape))
    of
        true ->
            {some, Marker} = Current,
            {Token1, Token2, Token3} =
                case Marker of
                    $` ->
                        {code_text, code_text_sequence, code_text_data};
                    _ ->
                        {math_text, math_text_sequence, math_text_data}
                end,
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                token_1 = Token1,
                token_2 = Token2,
                token_3 = Token3,
                marker = Marker
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, Token1),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, Token2),
            State = markdown_state:retry(raw_text_sequence_open),
            {Tokenizer4, State};
        false ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In opening sequence.

```markdown
> | `a`
    ^
```
""".
-spec sequence_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
sequence_open(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size, token_2 = Token2},
        parse_state = #markdown_parse_state{options = Options}
    }
) ->
    case Current of
        {some, CurrentChar} when CurrentChar =:= Marker ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(raw_text_sequence_open),
            {Tokenizer3, State};
        %% Not enough markers in the sequence.
        _ when
            Marker =:= $$ andalso Size =:= 1 andalso
                Options#markdown_parse_options.math_text_single_dollar =:= false
        ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                marker = 0,
                size = 0,
                token_1 = data,
                token_2 = data,
                token_3 = data
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token2),
            State = markdown_state:retry(raw_text_between),
            {Tokenizer2, State}
    end.

-doc """
Between something and something else.

```markdown
> | `a`
     ^^
```
""".
-spec between(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
between(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, token_2 = Token2, token_3 = Token3}
    }
) ->
    case Current of
        none ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                marker = 0,
                size = 0,
                token_1 = data,
                token_2 = data,
                token_3 = data
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
            State = markdown_state:next(raw_text_between),
            {Tokenizer4, State};
        _ ->
            case Current of
                {some, CurrentChar} when CurrentChar =:= Marker ->
                    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token2),
                    State = markdown_state:retry(raw_text_sequence_close),
                    {Tokenizer2, State};
                _ ->
                    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token3),
                    State = markdown_state:retry(raw_text_data),
                    {Tokenizer2, State}
            end
    end.

-doc """
In data.

```markdown
> | `a`
     ^
```
""".
-spec data(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
data(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = #markdown_tokenize_state{marker = Marker, token_3 = Token3}
    }
) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token3),
            State = markdown_state:retry(raw_text_between),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token3),
            State = markdown_state:retry(raw_text_between),
            {Tokenizer2, State};
        {some, CurrentChar} when CurrentChar =:= Marker ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token3),
            State = markdown_state:retry(raw_text_between),
            {Tokenizer2, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(raw_text_data),
            {Tokenizer2, State}
    end.

-doc """
In closing sequence.

```markdown
> | `a`
      ^
```
""".
-spec sequence_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
sequence_close(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                marker = Marker, size = Size, size_b = SizeB, token_1 = Token1, token_2 = Token2, token_3 = Token3
            }
    }
) ->
    case Current of
        {some, CurrentChar} when CurrentChar =:= Marker ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = SizeB + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(raw_text_sequence_close),
            {Tokenizer3, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token2),
            case Size =:= SizeB of
                true ->
                    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, Token1),
                    #markdown_tokenizer{tokenize_state = TokenizeState3} = Tokenizer3,
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{
                        marker = 0,
                        size = 0,
                        size_b = 0,
                        token_1 = data,
                        token_2 = data,
                        token_3 = data
                    },
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
                    State = markdown_state:ok(),
                    {Tokenizer4, State};
                false ->
                    %% More or less accents: mark as data.
                    Len = markdown_vec:size(Tokenizer2#markdown_tokenizer.events),
                    Tokenizer3 = markdown_tokenizer:update_event(Tokenizer2, Len - 2, fun(EventEntry) ->
                        EventEntry#markdown_event{name = Token3}
                    end),
                    Tokenizer4 = markdown_tokenizer:update_event(Tokenizer3, Len - 1, fun(EventEntry) ->
                        EventEntry#markdown_event{name = Token3}
                    end),
                    #markdown_tokenizer{tokenize_state = TokenizeState4} = Tokenizer4,
                    TokenizeState5 = TokenizeState4#markdown_tokenize_state{size_b = 0},
                    Tokenizer5 = Tokenizer4#markdown_tokenizer{tokenize_state = TokenizeState5},
                    State = markdown_state:retry(raw_text_between),
                    {Tokenizer5, State}
            end
    end.
