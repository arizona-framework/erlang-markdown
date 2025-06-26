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
-module(markdown_construct_partial_label).
-moduledoc """
Label occurs in [definition][] and [label end][label_end].

## Grammar

Label forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: maximum `999` codes allowed between brackets.
; Restriction: no blank lines.
; Restriction: at least 1 `text` byte must exist.
label ::= '[' *(label_byte | label_escape) ']'
label_byte ::= code - '[' - '\\' - ']'
label_escape ::= '\\' ['[' | '\\' | ']']
```

The maximum allowed size of the label, without the brackets, is `999`
(inclusive), which is defined in
[`LINK_REFERENCE_SIZE_MAX`][].

Labels can contain line endings and whitespace, but they are not allowed to
contain blank lines, and they must not be blank themselves.

The label is interpreted as the [string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.

> 👉 **Note**: this label relates to, but is not, the initial “label” of
> what is know as a reference in markdown:
>
> | Kind      | Link     | Image     |
> | --------- | -------- | --------- |
> | Shortcut  | `[x]`    | `![x]`    |
> | Collapsed | `[x][]`  | `![x][]`  |
> | Full      | `[x][y]` | `![x][y]` |
>
> The 6 above things are references, in the three kinds they come in, as
> links and images.
> The label that this module focusses on is only the thing that contains
> `y`.
>
> The thing that contains `x` is not a single thing when parsing markdown,
> but instead constists of an opening
> ([label start (image)][label_start_image] or
> [label start (link)][label_start_link]) and a closing
> ([label end][label_end]), so as to allow further phrasing such as
> [code (text)][raw_text] or [attention][].

## References

*   [`micromark-factory-label/index.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-factory-label/dev/index.js)

[definition]: crate::construct::definition
[string]: crate::construct::string
[attention]: crate::construct::attention
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[label_start_image]: crate::construct::label_start_image
[label_start_link]: crate::construct::label_start_link
[label_end]: crate::construct::label_end
[raw_text]: crate::construct::raw_text
[link_reference_size_max]: crate::util::constant::LINK_REFERENCE_SIZE_MAX
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    at_break/1,
    eol_after/1,
    nok/1,
    inside/1,
    escape/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of label.

```markdown
> | [a]
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, $[}, tokenize_state = TokenizeState}) ->
    #markdown_tokenize_state{token_1 = Token1, token_2 = Token2, token_3 = Token3} = TokenizeState,
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token1),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, Token2),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, Token2),
    Tokenizer6 = markdown_tokenizer:enter(Tokenizer5, Token3),
    State = markdown_state:next(label_at_break),
    {Tokenizer6, State}.

-doc """
In label, at something, before something else.

```markdown
> | [a]
     ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(
    Tokenizer = #markdown_tokenizer{
        current = Current, tokenize_state = #markdown_tokenize_state{seen = Seen, size = Size}
    }
) when
    Size > ?LINK_REFERENCE_SIZE_MAX orelse (Current =:= none orelse Current =:= {some, $[}) orelse
        (Current =:= {some, $]} andalso not Seen)
->
    State = markdown_state:retry(label_nok),
    {Tokenizer, State};
at_break(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = #markdown_tokenize_state{
            connect = Connect, token_1 = Token1, token_2 = Token2, token_3 = Token3
        }
    }
) ->
    case Current of
        {some, $\n} ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(label_eol_after), markdown_state:next(label_nok)
            ),
            {Tokenizer4, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol_with_options(
                Tokenizer3, #markdown_space_or_tab_eol_options{
                    connect = Connect,
                    content = {some, string}
                }
            ),
            State = markdown_state:retry(SpaceOrTabEolState),
            {Tokenizer4, State};
        {some, $]} ->
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer1, Token3),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, Token2),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, Token2),
            Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, Token1),
            #markdown_tokenizer{tokenize_state = TokenizeState6} = Tokenizer7,
            TokenizeState7 = TokenizeState6#markdown_tokenize_state{
                connect = false,
                seen = false,
                size = 0
            },
            Tokenizer8 = Tokenizer7#markdown_tokenizer{tokenize_state = TokenizeState7},
            State = markdown_state:ok(),
            {Tokenizer8, State};
        _ ->
            Link = markdown_event_link:new(none, none, string),
            Tokenizer3 = markdown_tokenizer:enter_link(Tokenizer1, data, Link),
            Tokenizer4 =
                case Connect of
                    true ->
                        Events3 = Tokenizer3#markdown_tokenizer.events,
                        Index = markdown_vec:size(Events3) - 1,
                        Events4 = markdown_subtokenizer:link(Events3, Index),
                        Tokenizer3#markdown_tokenizer{events = Events4};
                    false ->
                        #markdown_tokenizer{tokenize_state = TokenizeState3} = Tokenizer3,
                        TokenizeState4 = TokenizeState3#markdown_tokenize_state{connect = true},
                        Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4}
                end,
            State = markdown_state:retry(label_inside),
            {Tokenizer4, State}
    end.

-doc """
In label, after whitespace.

```markdown
  | [a␊
> | b]
    ^
```
""".
-spec eol_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
eol_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{connect = true},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(label_at_break),
    {Tokenizer2, State}.

-doc """
In label, on something disallowed.

```markdown
> | []
     ^
```
""".
-spec nok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
nok(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        connect = false,
        seen = false,
        size = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In label, in text.

```markdown
> | [a]
     ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(label_at_break),
            {Tokenizer2, State};
        {some, C} when C =:= $\n orelse C =:= $[ orelse C =:= $] ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(label_at_break),
            {Tokenizer2, State};
        {some, Byte} ->
            case TokenizeState1#markdown_tokenize_state.size > ?LINK_REFERENCE_SIZE_MAX of
                true ->
                    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
                    State = markdown_state:retry(label_at_break),
                    {Tokenizer2, State};
                false ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    Seen =
                        case TokenizeState1#markdown_tokenize_state.seen of
                            false -> Byte =/= $\t andalso Byte =/= $\s;
                            true -> true
                        end,
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        size = TokenizeState1#markdown_tokenize_state.size + 1,
                        seen = Seen
                    },
                    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
                    NextState =
                        case Byte of
                            $\\ -> label_escape;
                            _ -> label_inside
                        end,
                    State = markdown_state:next(NextState),
                    {Tokenizer3, State}
            end
    end.

-doc """
After `\\`, at a special character.

```markdown
> | [a\\*a]
       ^
```
""".
-spec escape(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
escape(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1}) ->
    case Current of
        {some, C} when C =:= $[ orelse C =:= $\\ orelse C =:= $] ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                size = TokenizeState1#markdown_tokenize_state.size + 1
            },
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:next(label_inside),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(label_inside),
            {Tokenizer1, State}
    end.
