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
-module(markdown_construct_html_flow).
-moduledoc """
HTML (flow) occurs in the [flow][] content type.

## Grammar

HTML (flow) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
html_flow ::= raw | comment | instruction | declaration | cdata | basic | complete

; Note: closing tag name does not need to match opening tag name.
raw ::= '<' raw_tag_name [[space_or_tab *line | '>' *line] eol] *(*line eol) ['</' raw_tag_name *line]
comment ::= '<!--' [*'-' '>' *line | *line *(eol *line) ['-->' *line]]
instruction ::= '<?' ['>' *line | *line *(eol *line) ['?>' *line]]
declaration ::= '<!' ascii_alphabetic *line *(eol *line) ['>' *line]
cdata ::= '<![CDATA[' *line *(eol *line) [']]>' *line]
basic ::= '< ['/'] basic_tag_name [['/'] '>' *line *(eol 1*line)]
complete ::= (opening_tag | closing_tag) [*space_or_tab *(eol 1*line)]

raw_tag_name ::= 'pre' | 'script' | 'style' | 'textarea' ; Note: case-insensitive.
basic_tag_name ::= 'address' | 'article' | 'aside' | ... ; See `constants.rs`, and note: case-insensitive.
opening_tag ::= '<' tag_name *(1*space_or_tab attribute) [*space_or_tab '/'] *space_or_tab '>'
closing_tag ::= '</' tag_name *space_or_tab '>'
tag_name ::= ascii_alphabetic *('-' | ascii_alphanumeric)
attribute ::= attribute_name [*space_or_tab '=' *space_or_tab attribute_value]
attribute_name ::= (':' | '_' | ascii_alphabetic) *('-' | '.' | ':' | '_' | ascii_alphanumeric)
attribute_value ::= '"' *(line - '"') '"' | "'" *(line - "'")  "'" | 1*(text - '"' - "'" - '/' - '<' - '=' - '>' - '`')
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

The grammar for HTML in markdown does not follow the rules of parsing
HTML according to the [*§ 13.2 Parsing HTML documents* in the HTML
spec][html_parsing].
As such, HTML in markdown *resembles* HTML, but is instead a (naïve?)
attempt to parse an XML-like language.
By extension, another notable property of the grammar is that it can
result in invalid HTML, in that it allows things that wouldn’t work or
wouldn’t work well in HTML, such as mismatched tags.

Interestingly, most of the productions above have a clear opening and
closing condition (raw, comment, insutrction, declaration, cdata), but the
closing condition does not need to be satisfied.
In this case, the parser never has to backtrack.

Because the **basic** and **complete** productions in the grammar form with
a tag, followed by more stuff, and stop at a blank line, it is possible to
interleave (a word for switching between languages) markdown and HTML
together, by placing the opening and closing tags on their own lines,
with blank lines between them and markdown.
For example:

```markdown
<div>This is <code>code</code> but this is not *emphasis*.</div>

<div>

This is a paragraph in a `div` and with `code` and *emphasis*.

</div>
```

The **complete** production of HTML (flow) is not allowed to interrupt
content.
That means that a blank line is needed between a [paragraph][] and it.
However, [HTML (text)][html_text] has a similar production, which will
typically kick-in instead.

The list of tag names allowed in the **raw** production are defined in
[`HTML_RAW_NAMES`][].
This production exists because there are a few cases where markdown
*inside* some elements, and hence interleaving, does not make sense.

The list of tag names allowed in the **basic** production are defined in
[`HTML_BLOCK_NAMES`][].
This production exists because there are a few cases where we can decide
early that something is going to be a flow (block) element instead of a
phrasing (inline) element.
We *can* interrupt and don’t have to care too much about it being
well-formed.

## Tokens

*   [`HtmlFlow`][Name::HtmlFlow]
*   [`HtmlFlowData`][Name::HtmlFlowData]
*   [`LineEnding`][Name::LineEnding]

## References

*   [`html-flow.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/html-flow.js)
*   [*§ 4.6 HTML blocks* in `CommonMark`](https://spec.commonmark.org/0.31/#html-blocks)

[flow]: crate::construct::flow
[html_text]: crate::construct::html_text
[paragraph]: crate::construct::paragraph
[html_raw_names]: crate::util::constant::HTML_RAW_NAMES
[html_block_names]: crate::util::constant::HTML_BLOCK_NAMES
[html_parsing]: https://html.spec.whatwg.org/multipage/parsing.html#parsing
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
    open/1,
    declaration_open/1,
    comment_open_inside/1,
    cdata_open_inside/1,
    tag_close_start/1,
    tag_name/1,
    basic_self_closing/1,
    complete_closing_tag_after/1,
    complete_attribute_name_before/1,
    complete_attribute_name/1,
    complete_attribute_name_after/1,
    complete_attribute_value_before/1,
    complete_attribute_value_quoted/1,
    complete_attribute_value_unquoted/1,
    complete_attribute_value_quoted_after/1,
    complete_end/1,
    complete_after/1,
    continuation/1,
    continuation_start/1,
    continuation_start_non_lazy/1,
    continuation_before/1,
    continuation_comment_inside/1,
    continuation_raw_tag_open/1,
    continuation_raw_end_tag/1,
    continuation_cdata_inside/1,
    continuation_declaration_inside/1,
    continuation_close/1,
    continuation_after/1,
    blank_line_before/1
]).

%% Macros
%% Symbol for `<script>` (condition 1).
-define(RAW, 1).
%% Symbol for `<!---->` (condition 2).
-define(COMMENT, 2).
%% Symbol for `<?php?>` (condition 3).
-define(INSTRUCTION, 3).
%% Symbol for `<!doctype>` (condition 4).
-define(DECLARATION, 4).
%% Symbol for `<![CDATA[]]>` (condition 5).
-define(CDATA, 5).
%% Symbol for `<div` (condition 6).
-define(BASIC, 6).
%% Symbol for `<x>` (condition 7).
-define(COMPLETE, 7).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of HTML (flow).

```markdown
> | <x />
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{html_flow = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, html_flow),
    case Current of
        C when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(html_flow_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true -> ?TAB_SIZE - 1;
                    false -> infinity
                end,
            Options = #markdown_space_or_tab_options{
                kind = html_flow_data,
                min = 0,
                max = MaxIndent,
                connect = false,
                content = none
            },
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_with_options(
                Tokenizer3, Options
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(html_flow_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
At `<`, after optional whitespace.

```markdown
> | <x />
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, $<}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, html_flow_data),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(html_flow_open),
    {Tokenizer3, State};
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `<`, at tag name or other stuff.

```markdown
> | <x />
     ^
> | <!doctype>
     ^
> | <!--xxx-->
     ^
```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = {some, $!}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_declaration_open),
    {Tokenizer2, State};
open(Tokenizer1 = #markdown_tokenizer{current = {some, $/}}) ->
    Tokenizer2 =
        #markdown_tokenizer{point = Point2, tokenize_state = TokenizeState2} =
        markdown_tokenizer:consume(Tokenizer1),
    TokenizeState3 = TokenizeState2#markdown_tokenize_state{seen = true, start = Point2#markdown_point.offset},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
    State = markdown_state:next(html_flow_tag_close_start),
    {Tokenizer3, State};
open(
    Tokenizer1 = #markdown_tokenizer{current = {some, $?}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = ?INSTRUCTION},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2, concrete = true},
    %% While we're in an instruction instead of a declaration, we're on a `?`
    %% right now, so we do need to search for `>`, similar to declarations.
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer3, State};
open(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, point = Point, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z) ->
    %% ASCII alphabetical.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = Point#markdown_point.offset},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(html_flow_tag_name),
    {Tokenizer2, State};
open(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `<!`, at declaration, comment, or CDATA.

```markdown
> | <!doctype>
      ^
> | <!--xxx-->
      ^
> | <![CDATA[>&<]]>
      ^
```
""".
-spec declaration_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
declaration_open(
    Tokenizer1 = #markdown_tokenizer{current = {some, $-}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = ?COMMENT},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(html_flow_comment_open_inside),
    {Tokenizer3, State};
declaration_open(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = ?DECLARATION},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2, concrete = true},
    %% Do not form containers.
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer3, State};
declaration_open(
    Tokenizer1 = #markdown_tokenizer{current = {some, $[}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = ?CDATA},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(html_flow_cdata_open_inside),
    {Tokenizer3, State};
declaration_open(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `<!-`, inside a comment, at another `-`.

```markdown
> | <!--xxx-->
       ^
```
""".
-spec comment_open_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
comment_open_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $-}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    %% Do not form containers.
    Tokenizer3 = Tokenizer2#markdown_tokenizer{concrete = true},
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer3, State};
comment_open_inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After `<![`, inside CDATA, expecting `CDATA[`.

```markdown
> | <![CDATA[>&<]]>
       ^^^^^^
```
""".
-spec cdata_open_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cdata_open_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size1}
    }
) ->
    case Current =:= binary:at(?HTML_CDATA_PREFIX, Size1) of
        true ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            Size2 = Size1 + 1,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size2},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            case Size2 =:= byte_size(?HTML_CDATA_PREFIX) of
                true ->
                    TokenizeState3 = TokenizeState2#markdown_tokenize_state{size = 0},
                    %% Do not form containers.
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState3, concrete = true},
                    State = markdown_state:next(html_flow_continuation),
                    {Tokenizer4, State};
                false ->
                    State = markdown_state:next(html_flow_cdata_open_inside),
                    {Tokenizer3, State}
            end;
        false ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.

-doc """
After `</`, in closing tag, at tag name.

```markdown
> | </x>
      ^
```
""".
-spec tag_close_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
tag_close_start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z)
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_tag_name),
    {Tokenizer2, State};
tag_close_start(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = false, start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In tag name.

```markdown
> | <ab>
     ^^
> | </ab>
      ^^
```
""".
-spec tag_name(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
tag_name(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    case Current of
        none ->
            tag_name__continue(Tokenizer1);
        {some, C} when (C =:= $\t orelse C =:= $\n orelse C =:= $\s orelse C =:= $/ orelse C =:= $>) ->
            tag_name__continue(Tokenizer1);
        {some, C} when
            (C =:= $-) orelse (C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $Z) orelse
                (C >= $a andalso C =< $z)
        ->
            %% ASCII alphanumerical and `-`.
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(html_flow_tag_name),
            {Tokenizer2, State};
        {some, _} ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = false},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.

-doc """
After closing slash of a basic tag name.

```markdown
> | <div/>
         ^
```
""".
-spec basic_self_closing(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
basic_self_closing(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    %% Do not form containers.
    Tokenizer3 = Tokenizer2#markdown_tokenizer{concrete = true},
    State = markdown_state:next(html_flow_continuation),
    {Tokenizer3, State};
basic_self_closing(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After closing slash of a complete tag name.

```markdown
> | <x/>
       ^
```
""".
-spec complete_closing_tag_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_closing_tag_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_closing_tag_after),
    {Tokenizer2, State};
complete_closing_tag_after(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_complete_end),
    {Tokenizer, State}.

-doc """
At an attribute name.

At first, this state is used after a complete tag name, after whitespace,
where it expects optional attributes or the end of the tag.
It is also reused after attributes, when expecting more optional
attributes.

```markdown
> | <a />
       ^
> | <a :b>
       ^
> | <a _b>
       ^
> | <a b>
       ^
> | <a >
       ^
```
""".
-spec complete_attribute_name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_name_before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_name_before),
    {Tokenizer2, State};
complete_attribute_name_before(Tokenizer1 = #markdown_tokenizer{current = {some, $/}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_end),
    {Tokenizer2, State};
complete_attribute_name_before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    (Current >= $0 andalso Current =< $9) orelse Current =:= $: orelse (Current >= $A andalso Current =< $Z) orelse
        Current =:= $_ orelse (Current >= $a andalso Current =< $z)
->
    %% ASCII alphanumerical and `:` and `_`.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_name),
    {Tokenizer2, State};
complete_attribute_name_before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_complete_end),
    {Tokenizer, State}.

-doc """
In attribute name.

```markdown
> | <a :b>
        ^
> | <a _b>
        ^
> | <a b>
        ^
```
""".
-spec complete_attribute_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_name(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $- orelse Current =:= $. orelse (Current >= $0 andalso Current =< $9) orelse Current =:= $: orelse
        (Current >= $A andalso Current =< $Z) orelse Current =:= $_ orelse (Current >= $a andalso Current =< $z)
->
    %% ASCII alphanumerical and `-`, `.`, `:`, and `_`.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_name),
    {Tokenizer2, State};
complete_attribute_name(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_complete_attribute_name_after),
    {Tokenizer, State}.

-doc """
After attribute name, at an optional initializer, the end of the tag, or
whitespace.

```markdown
> | <a b>
        ^
> | <a b=c>
        ^
```
""".
-spec complete_attribute_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_name_after),
    {Tokenizer2, State};
complete_attribute_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $=}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_value_before),
    {Tokenizer2, State};
complete_attribute_name_after(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_complete_attribute_name_before),
    {Tokenizer, State}.

-doc """
Before unquoted, double quoted, or single quoted attribute value, allowing
whitespace.

```markdown
> | <a b=c>
         ^
> | <a b="c">
         ^
```
""".
-spec complete_attribute_value_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_value_before(
    Tokenizer1 = #markdown_tokenizer{current = none, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State};
complete_attribute_value_before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $< orelse Current =:= $= orelse Current =:= $> orelse Current =:= $` ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State};
complete_attribute_value_before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_value_before),
    {Tokenizer2, State};
complete_attribute_value_before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $" orelse Current =:= $' ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker_b = Current},
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}),
    State = markdown_state:next(html_flow_complete_attribute_value_quoted),
    {Tokenizer2, State};
complete_attribute_value_before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_complete_attribute_value_unquoted),
    {Tokenizer, State}.

-doc """
In double or single quoted attribute value.

```markdown
> | <a b="c">
          ^
> | <a b='c'>
          ^
```
""".
-spec complete_attribute_value_quoted(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_value_quoted(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker_b = MarkerB}
    }
) when Current =:= MarkerB ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker_b = 0},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(html_flow_complete_attribute_value_quoted_after),
    {Tokenizer3, State};
complete_attribute_value_quoted(
    Tokenizer1 = #markdown_tokenizer{current = none, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, marker_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State};
complete_attribute_value_quoted(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $\n}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, marker_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State};
complete_attribute_value_quoted(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_value_quoted),
    {Tokenizer2, State}.

-doc """
In unquoted attribute value.

```markdown
> | <a b=c>
         ^
```
""".
-spec complete_attribute_value_unquoted(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_value_unquoted(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:retry(html_flow_complete_attribute_name_after),
    {Tokenizer, State};
complete_attribute_value_unquoted(Tokenizer = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\n orelse Current =:= $\s orelse Current =:= $" orelse Current =:= $' orelse
        Current =:= $/ orelse Current =:= $< orelse Current =:= $= orelse Current =:= $> orelse Current =:= $`
->
    State = markdown_state:retry(html_flow_complete_attribute_name_after),
    {Tokenizer, State};
complete_attribute_value_unquoted(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_attribute_value_unquoted),
    {Tokenizer2, State}.

-doc """
After double or single quoted attribute value, before whitespace or the
end of the tag.

```markdown
> | <a b="c">
           ^
```
""".
-spec complete_attribute_value_quoted_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_attribute_value_quoted_after(Tokenizer = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s orelse Current =:= $/ orelse Current =:= $>
->
    State = markdown_state:retry(html_flow_complete_attribute_name_before),
    {Tokenizer, State};
complete_attribute_value_quoted_after(
    Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In certain circumstances of a complete tag where only an `>` is allowed.

```markdown
> | <a b="c">
            ^
```
""".
-spec complete_end(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_end(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_after),
    {Tokenizer2, State};
complete_end(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After `>` in a complete tag.

```markdown
> | <x>
       ^
```
""".
-spec complete_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
complete_after(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    %% Do not form containers.
    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = true},
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer2, State};
complete_after(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    %% Do not form containers.
    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = true},
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer2, State};
complete_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_complete_after),
    {Tokenizer2, State};
complete_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In continuation of any HTML kind.

```markdown
> | <!--xxx-->
         ^
```
""".
-spec continuation(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation(
    Tokenizer1 = #markdown_tokenizer{current = {some, $-}, tokenize_state = #markdown_tokenize_state{marker = ?COMMENT}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_comment_inside),
    {Tokenizer2, State};
continuation(
    Tokenizer1 = #markdown_tokenizer{current = {some, $<}, tokenize_state = #markdown_tokenize_state{marker = ?RAW}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_raw_tag_open),
    {Tokenizer2, State};
continuation(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $>}, tokenize_state = #markdown_tokenize_state{marker = ?DECLARATION}
    }
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_close),
    {Tokenizer2, State};
continuation(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $?}, tokenize_state = #markdown_tokenize_state{marker = ?INSTRUCTION}
    }
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer2, State};
continuation(
    Tokenizer1 = #markdown_tokenizer{current = {some, $]}, tokenize_state = #markdown_tokenize_state{marker = ?CDATA}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_cdata_inside),
    {Tokenizer2, State};
continuation(
    Tokenizer1 = #markdown_tokenizer{current = {some, $\n}, tokenize_state = #markdown_tokenize_state{marker = Marker}}
) when Marker =:= ?BASIC orelse Marker =:= ?COMPLETE ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow_data),
    Tokenizer3 = markdown_tokenizer:check(
        Tokenizer2,
        markdown_state:next(html_flow_continuation_after),
        markdown_state:next(html_flow_continuation_start)
    ),
    State2 = markdown_state:retry(html_flow_blank_line_before),
    {Tokenizer3, State2};
continuation(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow_data),
    State = markdown_state:retry(html_flow_continuation_start),
    {Tokenizer2, State};
continuation(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow_data),
    State = markdown_state:retry(html_flow_continuation_start),
    {Tokenizer2, State};
continuation(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation),
    {Tokenizer2, State}.

-doc """
In continuation, at eol.

```markdown
> | <x>
       ^
  | asd
```
""".
-spec continuation_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_start(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:check(
        Tokenizer1,
        markdown_state:next(html_flow_continuation_start_non_lazy),
        markdown_state:next(html_flow_continuation_after)
    ),
    State2 = markdown_state:retry(non_lazy_continuation_start),
    {Tokenizer2, State2}.

-doc """
In continuation, at eol, before non-lazy content.

```markdown
> | <x>
       ^
  | asd
```
""".
-spec continuation_start_non_lazy(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_start_non_lazy(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(html_flow_continuation_before),
    {Tokenizer4, State};
continuation_start_non_lazy(_) ->
    erlang:error({badmatch, "expected eol"}).

-doc """
In continuation, before non-lazy content.

```markdown
  | <x>
> | asd
    ^
```
""".
-spec continuation_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_before(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:retry(html_flow_continuation_start),
    {Tokenizer, State};
continuation_before(Tokenizer = #markdown_tokenizer{current = {some, $\n}}) ->
    State = markdown_state:retry(html_flow_continuation_start),
    {Tokenizer, State};
continuation_before(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, html_flow_data),
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer2, State}.

-doc """
In comment continuation, after one `-`, expecting another.

```markdown
> | <!--xxx-->
            ^
```
""".
-spec continuation_comment_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_comment_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $-}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer2, State};
continuation_comment_inside(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer, State}.

-doc """
In raw continuation, after `<`, at `/`.

```markdown
> | <script>console.log(1)</script>
                           ^
```
""".
-spec continuation_raw_tag_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_raw_tag_open(Tokenizer1 = #markdown_tokenizer{current = {some, $/}}) ->
    Tokenizer2 =
        #markdown_tokenizer{point = Point2, tokenize_state = TokenizeState2} =
        markdown_tokenizer:consume(Tokenizer1),
    TokenizeState3 = TokenizeState2#markdown_tokenize_state{start = Point2#markdown_point.offset},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
    State = markdown_state:next(html_flow_continuation_raw_end_tag),
    {Tokenizer3, State};
continuation_raw_tag_open(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer, State}.

-doc """
In raw continuation, after `</`, in a raw tag name.

```markdown
> | <script>console.log(1)</script>
                            ^^^^^^
```
""".
-spec continuation_raw_end_tag(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_raw_end_tag(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $>},
        parse_state = #markdown_parse_state{bytes = Bytes},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{start = Start},
        point = Point
    }
) ->
    %% Guaranteed to be valid ASCII bytes.
    TagSlice = markdown_slice:from_indices(Bytes, Start, Point#markdown_point.offset),
    TagName = string:lowercase(markdown_slice:as_binary(TagSlice)),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case maps:is_key(TagName, ?HTML_RAW_NAMES) of
        true ->
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(html_flow_continuation_close),
            {Tokenizer3, State};
        false ->
            State = markdown_state:retry(html_flow_continuation),
            {Tokenizer2, State}
    end;
continuation_raw_end_tag(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = #markdown_tokenize_state{start = Start}, point = Point
    }
) when
    ((Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z)) andalso
        (Point#markdown_point.offset - Start < ?HTML_RAW_SIZE_MAX)
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_raw_end_tag),
    {Tokenizer2, State};
continuation_raw_end_tag(
    Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer2, State}.

-doc """
In cdata continuation, after `]`, expecting `]>`.

```markdown
> | <![CDATA[>&<]]>
                 ^
```
""".
-spec continuation_cdata_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_cdata_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $]}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer2, State};
continuation_cdata_inside(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer, State}.

-doc """
In declaration or instruction continuation, at `>`.

```markdown
> | <!-->
        ^
> | <?>
      ^
> | <!q>
       ^
> | <!--ab-->
            ^
> | <![CDATA[>&<]]>
                  ^
```
""".
-spec continuation_declaration_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_declaration_inside(
    Tokenizer1 = #markdown_tokenizer{current = {some, $-}, tokenize_state = #markdown_tokenize_state{marker = ?COMMENT}}
) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_declaration_inside),
    {Tokenizer2, State};
continuation_declaration_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_close),
    {Tokenizer2, State};
continuation_declaration_inside(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(html_flow_continuation),
    {Tokenizer, State}.

-doc """
In closed continuation: everything we get until the eol/eof is part of it.

```markdown
> | <!doctype>
              ^
```
""".
-spec continuation_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_close(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow_data),
    State = markdown_state:retry(html_flow_continuation_after),
    {Tokenizer2, State};
continuation_close(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow_data),
    State = markdown_state:retry(html_flow_continuation_after),
    {Tokenizer2, State};
continuation_close(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(html_flow_continuation_close),
    {Tokenizer2, State}.

-doc """
Done.

```markdown
> | <!doctype>
              ^
```
""".
-spec continuation_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, html_flow),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    %% Feel free to interrupt.
    %% No longer concrete.
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2, interrupt = false, concrete = false},
    State = markdown_state:ok(),
    {Tokenizer3, State}.

-doc """
Before eol, expecting blank line.

```markdown
> | <div>
         ^
  |
```
""".
-spec blank_line_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
blank_line_before(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(blank_line_start),
    {Tokenizer4, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec tag_name__continue(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
tag_name__continue(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        parse_state = #markdown_parse_state{bytes = Bytes},
        point = Point,
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{seen = Seen, start = Start}
    }
) ->
    ClosingTag = Seen,
    Slash =
        case Current of
            {some, $/} -> true;
            _ -> false
        end,
    %% Guaranteed to be valid ASCII bytes.
    Slice = markdown_slice:from_indices(Bytes, Start, Point#markdown_point.offset),
    %% The line ending case might result in a `\r` that is already accounted for.
    Name = string:lowercase(string:trim(markdown_slice:as_binary(Slice))),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        seen = false,
        start = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case (not Slash) andalso (not ClosingTag) andalso maps:is_key(Name, ?HTML_RAW_NAMES) of
        true ->
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{marker = ?RAW},
            %% Do not form containers.
            Tokenizer3 = Tokenizer2#markdown_tokenizer{concrete = true, tokenize_state = TokenizeState3},
            State = markdown_state:retry(html_flow_continuation),
            {Tokenizer3, State};
        false ->
            case maps:is_key(Name, ?HTML_BLOCK_NAMES) of
                true ->
                    TokenizeState3 = TokenizeState2#markdown_tokenize_state{marker = ?BASIC},
                    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
                    case Slash of
                        true ->
                            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
                            State = markdown_state:next(html_flow_basic_self_closing),
                            {Tokenizer4, State};
                        false ->
                            %% Do not form containers.
                            Tokenizer4 = Tokenizer3#markdown_tokenizer{concrete = true},
                            State = markdown_state:retry(html_flow_continuation),
                            {Tokenizer4, State}
                    end;
                false ->
                    TokenizeState3 = TokenizeState2#markdown_tokenize_state{marker = ?COMPLETE},
                    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
                    %% Do not support complete HTML when interrupting.
                    case Tokenizer3#markdown_tokenizer.interrupt andalso not Tokenizer3#markdown_tokenizer.lazy of
                        true ->
                            TokenizeState4 = TokenizeState3#markdown_tokenize_state{marker = 0},
                            Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
                            State = markdown_state:nok(),
                            {Tokenizer4, State};
                        false when ClosingTag =:= true ->
                            State = markdown_state:retry(html_flow_complete_closing_tag_after),
                            {Tokenizer3, State};
                        false ->
                            State = markdown_state:retry(html_flow_complete_attribute_name_before),
                            {Tokenizer3, State}
                    end
            end
    end.
