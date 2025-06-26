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
-module(markdown_construct_partial_title).
-moduledoc """
Title occurs in [definition][] and [label end][label_end].

## Grammar

Title forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: no blank lines.
; Restriction: markers must match (in case of `(` with `)`).
title ::= marker *(title_byte | title_escape) marker
title_byte ::= code - '\\' - marker
title_escape ::= '\\' ['\\' | marker]
marker ::= '"' | '\'' | '('
```

Titles can be double quoted (`"a"`), single quoted (`'a'`), or
parenthesized (`(a)`).

Titles can contain line endings and whitespace, but they are not allowed to
contain blank lines.
They are allowed to be blank themselves.

The title is interpreted as the [string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.

## References

*   [`micromark-factory-title/index.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-factory-title/dev/index.js)

[definition]: crate::construct::definition
[string]: crate::construct::string
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[label_end]: crate::construct::label_end
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    'begin'/1,
    at_break/1,
    after_eol/1,
    nok/1,
    inside/1,
    escape/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of title.

```markdown
> | "a"
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Marker},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = Token1, token_2 = Token2}
    }
) when Marker =:= $\" orelse Marker =:= $\' orelse Marker =:= $( ->
    CloseMarker =
        case Marker of
            $( -> $);
            _ -> Marker
        end,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = CloseMarker},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, Token1),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, Token2),
    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, Token2),
    State = markdown_state:next(title_begin),
    {Tokenizer6, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After opening marker.

This is also used at the closing marker.

```markdown
> | "a"
     ^
```
""".
-spec 'begin'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'begin'(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, token_1 = Token1, token_2 = Token2}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token2),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, Token2),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, Token1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, connect = false},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:ok(),
    {Tokenizer6, State};
'begin'(Tokenizer1 = #markdown_tokenizer{tokenize_state = #markdown_tokenize_state{token_3 = Token3}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token3),
    State = markdown_state:retry(title_at_break),
    {Tokenizer2, State}.

-doc """
At something, before something else.

```markdown
> | "a"
     ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = #markdown_tokenize_state{marker = Marker, token_3 = Token3, connect = Connect}
    }
) ->
    case Current of
        Current when Current =:= Marker ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token3),
            State = markdown_state:retry(title_begin),
            {Tokenizer2, State};
        $\n ->
            Options = #markdown_space_or_tab_eol_options{
                content = {some, string},
                connect = Connect
            },
            {Tokenizer2, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol_with_options(
                Tokenizer1, Options
            ),
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(title_after_eol), markdown_state:next(title_nok)
            ),
            State = markdown_state:retry(SpaceOrTabEolState),
            {Tokenizer3, State};
        _ ->
            Link = markdown_event_link:new(none, none, string),
            Tokenizer2 = markdown_tokenizer:enter_link(Tokenizer1, data, Link),
            Tokenizer3 =
                case Connect of
                    true ->
                        Events2 = Tokenizer2#markdown_tokenizer.events,
                        Index = markdown_vec:size(Events2) - 1,
                        Events3 = markdown_subtokenizer:link(Events2, Index),
                        Tokenizer2#markdown_tokenizer{events = Events3};
                    false ->
                        TokenizeState2 = Tokenizer2#markdown_tokenizer.tokenize_state,
                        TokenizeState3 = TokenizeState2#markdown_tokenize_state{space_or_tab_eol_connect = true},
                        Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3}
                end,
            State = markdown_state:retry(title_inside),
            {Tokenizer3, State}
    end;
at_break(Tokenizer) ->
    State = markdown_state:retry(title_nok),
    {Tokenizer, State}.

-doc """
In title, after whitespace.

```markdown
  | "a␊
> | b"
    ^
```
""".
-spec after_eol(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_eol(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{connect = true},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(title_at_break),
    {Tokenizer2, State}.

-doc """
In title, at something that isn't allowed.

```markdown
> | "a
      ^
```
""".
-spec nok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
nok(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, connect = false},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In text.

```markdown
> | "a"
     ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{marker = Marker}}) ->
    case Current of
        {some, Byte} when Byte =:= Marker ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(title_at_break),
            {Tokenizer2, State};
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(title_at_break),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(title_at_break),
            {Tokenizer2, State};
        {some, $\\} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(title_escape),
            {Tokenizer2, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(title_inside),
            {Tokenizer2, State}
    end.

-doc """
After `\`, at a special character.

```markdown
> | "a\*b"
     ^
```
""".
-spec escape(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
escape(Tokenizer1 = #markdown_tokenizer{current = {some, Byte}}) when
    Byte =:= $\" orelse Byte =:= $\' orelse Byte =:= $) orelse Byte =:= $\\
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(title_inside),
    {Tokenizer2, State};
escape(Tokenizer) ->
    State = markdown_state:retry(title_inside),
    {Tokenizer, State}.
