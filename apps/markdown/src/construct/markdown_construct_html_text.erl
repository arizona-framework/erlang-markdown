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
-module(markdown_construct_html_text).
-moduledoc """
HTML (text) occurs in the [text][] content type.

## Grammar

HTML (text) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
html_text ::= comment | instruction | declaration | cdata | tag_close | tag_open

; Restriction: the text is not allowed to start with `>`, `->`, or to contain `--`.
comment ::= '<!--' *byte '-->'
instruction ::= '<?' *byte '?>'
declaration ::= '<!' ascii_alphabetic *byte '>'
; Restriction: the text is not allowed to contain `]]`.
cdata ::= '<![CDATA[' *byte ']]>'
tag_close ::= '</' tag_name [space_or_tab_eol] '>'
opening_tag ::= '<' tag_name *(space_or_tab_eol attribute) [[space_or_tab_eol] '/'] [space_or_tab_eol] '>'

tag_name ::= ascii_alphabetic *( '-' | ascii_alphanumeric )
attribute ::= attribute_name [[space_or_tab_eol] '=' [space_or_tab_eol] attribute_value]
attribute_name ::= (':' | '_' | ascii_alphabetic) *('-' | '.' | ':' | '_' | ascii_alphanumeric)
attribute_value ::= '"' *(byte - '"') '"' | "'" *(byte - "'")  "'" | 1*(text - '"' - "'" - '/' - '<' - '=' - '>' - '`')
```

The grammar for HTML in markdown does not follow the rules of parsing
HTML according to the [*§ 13.2 Parsing HTML documents* in the HTML
spec][html_parsing].
See the related flow construct [HTML (flow)][html_flow] for more info.

Because the **tag open** and **tag close** productions in the grammar form
with just tags instead of complete elements, it is possible to interleave
(a word for switching between languages) markdown and HTML together.
For example:

```markdown
This is equivalent to <code>*emphasised* code</code>.
```

## Tokens

*   [`HtmlText`][Name::HtmlText]
*   [`HtmlTextData`][Name::HtmlTextData]

## References

*   [`html-text.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/html-text.js)
*   [*§ 6.6 Raw HTML* in `CommonMark`](https://spec.commonmark.org/0.31/#raw-html)

[text]: crate::construct::text
[html_flow]: crate::construct::html_flow
[html_parsing]: https://html.spec.whatwg.org/multipage/parsing.html#parsing
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    open/1,
    declaration_open/1,
    comment_open_inside/1,
    comment/1,
    comment_close/1,
    comment_end/1,
    cdata_open_inside/1,
    cdata/1,
    cdata_close/1,
    cdata_end/1,
    declaration/1,
    instruction/1,
    instruction_close/1,
    tag_close_start/1,
    tag_close/1,
    tag_close_between/1,
    tag_open/1,
    tag_open_between/1,
    tag_open_attribute_name/1,
    tag_open_attribute_name_after/1,
    tag_open_attribute_value_before/1,
    tag_open_attribute_value_quoted/1,
    tag_open_attribute_value_unquoted/1,
    tag_open_attribute_value_quoted_after/1,
    'end'/1,
    line_ending_before/1,
    line_ending_after/1,
    line_ending_after_prefix/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of HTML (text).

```markdown
> | a <b> c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $<},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{html_text = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, html_text),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, html_text_data),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    State = markdown_state:next(html_text_open),
    {Tokenizer4, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `<`, at tag name or other stuff.

```markdown
> | a <b> c
       ^
> | a <!doctype> c
       ^
> | a <!--b--> c
       ^
```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $!} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_declaration_open),
            {Tokenizer2, State};
        {some, $/} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_close_start),
            {Tokenizer2, State};
        {some, $?} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_instruction),
            {Tokenizer2, State};
        %% ASCII alphabetical.
        {some, Char} when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
After `<!`, at declaration, comment, or CDATA.

```markdown
> | a <!doctype> c
        ^
> | a <!--b--> c
        ^
> | a <![CDATA[>&<]]> c
        ^
```
""".
-spec declaration_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
declaration_open(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_comment_open_inside),
            {Tokenizer2, State};
        %% ASCII alphabetical.
        {some, Char} when
            (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_declaration),
            {Tokenizer2, State};
        {some, $[} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_cdata_open_inside),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In a comment, after `<!-`, at another `-`.

```markdown
> | a <!--b--> c
         ^
```
""".
-spec comment_open_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
comment_open_inside(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_comment_end),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In comment.

```markdown
> | a <!--b--> c
          ^
```
""".
-spec comment(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
comment(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(html_text_comment), markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_comment_close),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_comment),
            {Tokenizer2, State}
    end.

-doc """
In comment, after `-`.

```markdown
> | a <!--b--> c
            ^
```
""".
-spec comment_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
comment_close(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_comment_end),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_comment),
            {Tokenizer1, State}
    end.

-doc """
In comment, after `-`.

```markdown
> | a <!--b--> c
            ^
```
""".
-spec comment_end(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
comment_end(Tokenizer = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $>} ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer, State};
        {some, $-} ->
            State = markdown_state:retry(html_text_comment_close),
            {Tokenizer, State};
        _ ->
            State = markdown_state:retry(html_text_comment),
            {Tokenizer, State}
    end.

-doc """
After `<![`, in CDATA, expecting `CDATA[`.

```markdown
> | a <![CDATA[>&<]]> b
         ^^^^^^
```
""".
-spec cdata_open_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
cdata_open_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) ->
    case Current =:= binary:at(?HTML_CDATA_PREFIX, Size) of
        true ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            case Size + 1 =:= byte_size(?HTML_CDATA_PREFIX) of
                true ->
                    TokenizeState3 = TokenizeState2#markdown_tokenize_state{size = 0},
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState3},
                    State = markdown_state:next(html_text_cdata),
                    {Tokenizer4, State};
                false ->
                    State = markdown_state:next(html_text_cdata_open_inside),
                    {Tokenizer3, State}
            end;
        false ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end;
cdata_open_inside(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In CDATA.

```markdown
> | a <![CDATA[>&<]]> b
               ^^^
```
""".
-spec cdata(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
cdata(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(html_text_cdata), markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, $]} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_cdata_close),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_cdata),
            {Tokenizer2, State}
    end.

-doc """
In CDATA, after `]`, at another `]`.

```markdown
> | a <![CDATA[>&<]]> b
                   ^
```
""".
-spec cdata_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
cdata_close(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $]} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_cdata_end),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_cdata),
            {Tokenizer1, State}
    end.

-doc """
In CDATA, after `]]`, at `>`.

```markdown
> | a <![CDATA[>&<]]> b
                    ^
```
""".
-spec cdata_end(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
cdata_end(Tokenizer = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $>} ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer, State};
        {some, $]} ->
            State = markdown_state:retry(html_text_cdata_close),
            {Tokenizer, State};
        _ ->
            State = markdown_state:retry(html_text_cdata),
            {Tokenizer, State}
    end.

-doc """
In declaration.

```markdown
> | a <!b> c
         ^
```
""".
-spec declaration(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
declaration(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer1, State};
        {some, $>} ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer1, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(html_text_declaration), markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_declaration),
            {Tokenizer2, State}
    end.

-doc """
In instruction.

```markdown
> | a <?b?> c
        ^
```
""".
-spec instruction(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
instruction(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(html_text_instruction), markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, $?} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_instruction_close),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_instruction),
            {Tokenizer2, State}
    end.

-doc """
In instruction, after `?`, at `>`.

```markdown
> | a <?b?> c
          ^
```
""".
-spec instruction_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
instruction_close(Tokenizer = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $>} ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer, State};
        _ ->
            State = markdown_state:retry(html_text_instruction),
            {Tokenizer, State}
    end.

-doc """
After `</`, in closing tag, at tag name.

```markdown
> | a </b> c
        ^
```
""".
-spec tag_close_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_close_start(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    %% ASCII alphabetical.
    case Current of
        {some, Char} when
            (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_close),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
After `</x`, in a tag name.

```markdown
> | a </b> c
         ^
```
""".
-spec tag_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_close(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    %% ASCII alphanumerical and `-`.
    case Current of
        {some, Char} when
            Char =:= $- orelse
                (Char >= $0 andalso Char =< $9) orelse
                (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_close),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_tag_close_between),
            {Tokenizer1, State}
    end.

-doc """
In closing tag, after tag name.

```markdown
> | a </b> c
         ^
```
""".
-spec tag_close_between(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_close_between(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_tag_close_between),
                markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_close_between),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer1, State}
    end.

-doc """
After `<x`, in opening tag name.

```markdown
> | a <b> c
        ^
```
""".
-spec tag_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    %% ASCII alphanumerical and `-`.
    case Current of
        {some, Char} when
            Char =:= $- orelse
                (Char >= $0 andalso Char =< $9) orelse
                (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open),
            {Tokenizer2, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\n orelse Char =:= $\s orelse Char =:= $/ orelse Char =:= $> ->
            State = markdown_state:retry(html_text_tag_open_between),
            {Tokenizer1, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In opening tag, after tag name.

```markdown
> | a <b> c
        ^
```
""".
-spec tag_open_between(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_between(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_tag_open_between),
                markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_between),
            {Tokenizer2, State};
        {some, $/} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_end),
            {Tokenizer2, State};
        %% ASCII alphabetical and `:` and `_`.
        {some, Char} when
            Char =:= $: orelse
                (Char >= $A andalso Char =< $Z) orelse
                Char =:= $_ orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_name),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_end),
            {Tokenizer1, State}
    end.

-doc """
In attribute name.

```markdown
> | a <b c> d
         ^
```
""".
-spec tag_open_attribute_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_name(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    %% ASCII alphabetical and `-`, `.`, `:`, and `_`.
    case Current of
        {some, Char} when
            Char =:= $- orelse
                Char =:= $. orelse
                (Char >= $0 andalso Char =< $9) orelse
                Char =:= $: orelse
                (Char >= $A andalso Char =< $Z) orelse
                Char =:= $_ orelse
                (Char >= $a andalso Char =< $z)
        ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_name),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_tag_open_attribute_name_after),
            {Tokenizer1, State}
    end.

-doc """
After attribute name, before initializer, the end of the tag, or
whitespace.

```markdown
> | a <b c> d
          ^
```
""".
-spec tag_open_attribute_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_name_after(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_tag_open_attribute_name_after),
                markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_name_after),
            {Tokenizer2, State};
        {some, $=} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_value_before),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(html_text_tag_open_between),
            {Tokenizer1, State}
    end.

-doc """
Before unquoted, double quoted, or single quoted attribute value, allowing
whitespace.

```markdown
> | a <b c=d> e
           ^
```
""".
-spec tag_open_attribute_value_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_value_before(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, Char} when Char =:= $< orelse Char =:= $= orelse Char =:= $> orelse Char =:= $` ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_tag_open_attribute_value_before),
                markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_value_before),
            {Tokenizer2, State};
        {some, Char} when Char =:= $" orelse Char =:= $' ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Char},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(html_text_tag_open_attribute_value_quoted),
            {Tokenizer3, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_value_unquoted),
            {Tokenizer2, State}
    end.

-doc """
In double or single quoted attribute value.

```markdown
> | a <b c="d"> e
            ^
```
""".
-spec tag_open_attribute_value_quoted(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_value_quoted(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker}
    }
) ->
    case Current of
        {some, Char} when Char =:= Marker ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(html_text_tag_open_attribute_value_quoted_after),
            {Tokenizer3, State};
        none ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_tag_open_attribute_value_quoted),
                markdown_state:nok()
            ),
            State = markdown_state:retry(html_text_line_ending_before),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_value_quoted),
            {Tokenizer2, State}
    end.

-doc """
In unquoted attribute value.

```markdown
> | a <b c=d> e
           ^
```
""".
-spec tag_open_attribute_value_unquoted(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_value_unquoted(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, Char} when Char =:= $" orelse Char =:= $' orelse Char =:= $< orelse Char =:= $= orelse Char =:= $` ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, Char} when Char =:= $\t orelse Char =:= $\n orelse Char =:= $\s orelse Char =:= $/ orelse Char =:= $> ->
            State = markdown_state:retry(html_text_tag_open_between),
            {Tokenizer1, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_text_tag_open_attribute_value_unquoted),
            {Tokenizer2, State}
    end.

-doc """
After double or single quoted attribute value, before whitespace or the end
of the tag.

```markdown
> | a <b c="d"> e
              ^
```
""".
-spec tag_open_attribute_value_quoted_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_open_attribute_value_quoted_after(Tokenizer = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, Char} when Char =:= $\t orelse Char =:= $\n orelse Char =:= $\s orelse Char =:= $/ orelse Char =:= $> ->
            State = markdown_state:retry(html_text_tag_open_between),
            {Tokenizer, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer, State}
    end.

-doc """
In certain circumstances of a tag where only an `>` is allowed.

```markdown
> | a <b c="d"> e
              ^
```
""".
-spec 'end'(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
'end'(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $>} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, html_text_data),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, html_text),
            State = markdown_state:ok(),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
At eol.

> 👉 **Note**: we can't have blank lines in text, so no need to worry about
> empty tokens.

```markdown
> | a <!--a
           ^
  | b-->
```
""".
-spec line_ending_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
line_ending_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_text_data),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, line_ending),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, line_ending),
            State = markdown_state:next(html_text_line_ending_after),
            {Tokenizer5, State};
        _ ->
            error("expected eol")
    end.

-doc """
After eol, at optional whitespace.

> 👉 **Note**: we can't have blank lines in text, so no need to worry about
> empty tokens.

```markdown
  | a <!--a
> | b-->
    ^
```
""".
-spec line_ending_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
line_ending_after(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(html_text_line_ending_after_prefix),
                markdown_state:nok()
            ),
            {SpaceOrTabTokenizer, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
            State = markdown_state:retry(SpaceOrTabState),
            {SpaceOrTabTokenizer, State};
        _ ->
            State = markdown_state:retry(html_text_line_ending_after_prefix),
            {Tokenizer1, State}
    end.

-doc """
After eol, after optional whitespace.

> 👉 **Note**: we can't have blank lines in text, so no need to worry about
> empty tokens.

```markdown
  | a <!--a
> | b-->
    ^
```
""".
-spec line_ending_after_prefix(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
line_ending_after_prefix(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, html_text_data),
    State = markdown_state:ok(),
    {Tokenizer2, State}.
