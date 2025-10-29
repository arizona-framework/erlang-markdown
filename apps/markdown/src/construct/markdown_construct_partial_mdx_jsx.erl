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
-module(markdown_construct_partial_mdx_jsx).
-moduledoc """
MDX JSX occurs in [MDX JSX (flow)][mdx_jsx_flow] and
[MDX JSX (text)][mdx_jsx_text].

## Grammar

MDX JSX forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; constraint: markdown whitespace (`space_or_tab | eol`) is NOT
; allowed directly after `<` in order to allow `1 < 3` in markdown.
mdx_jsx ::=
  '<' [closing]
  [*whitespace name [attributes_after_identifier] [closing]]
  *whitespace '>'

attributes_after_identifier ::=
  1*whitespace (attributes_boolean | attributes_value) |
  *whitespace attributes_expression |
attributes_after_value ::=
  *whitespace (attributes_boolean | attributes_expression | attributes_value)
attributes_boolean ::= key [attributes_after_identifier]
; Note: in gnostic mode the value of the expression must instead be a single valid ES spread
; expression
attributes_expression ::= expression [attributes_after_value]
attributes_value ::= key initializer [attributes_after_value]

closing ::= *whitespace '/'

name ::= identifier [local | members]
key ::= identifier [local]
local ::= *whitespace ':' *whitespace identifier
members ::= member *member
member ::= *whitespace '.' *whitespace identifier

identifier ::= identifier_start *identifier_part
initializer ::= *whitespace '=' *whitespace value
value ::= double_quoted | single_quoted | expression
; Note: in gnostic mode the value must instead be a single valid ES expression
expression ::= '{' *(expression_text | expression) '}'

double_quoted ::= '"' *double_quoted_text '"'
single_quoted ::= "'" *single_quoted_text "'"

text ::= char - '<' - '{'
whitespace ::= es_whitespace
double_quoted_text ::= char - '"'
single_quoted_text ::= char - "'"
expression_text ::= char - '{' - '}'
identifier_start ::= es_identifier_start
identifier_part ::= es_identifier_part | '-'

; ECMAScript
; See “Identifier_start”: <https://tc39.es/ecma262/#prod-IdentifierStart>
es_identifier_start ::= ?
; See “Identifier_part”: <https://tc39.es/ecma262/#prod-IdentifierPart>
es_identifier_part ::= ?
; See “Whitespace”: <https://tc39.es/ecma262/#prod-WhiteSpace>
es_whitespace ::= ?
```

The grammar for JSX in markdown is much stricter than that of HTML in
markdown.
The primary benefit of this is that tags are parsed into tokens, and thus
can be processed.
Another, arguable, benefit of this is that it comes with syntax errors: if
an author types something that is nonsensical, an error is thrown with
information about where it happened, what occurred, and what was expected
instead.

## Tokens

*   [`LineEnding`][Name::LineEnding]
*   [`MdxJsxEsWhitespace`][Name::MdxJsxEsWhitespace]
*   [`MdxJsxTagMarker`][Name::MdxJsxTagMarker]
*   [`MdxJsxTagClosingMarker`][Name::MdxJsxTagClosingMarker]
*   [`MdxJsxTagName`][Name::MdxJsxTagName]
*   [`MdxJsxTagNamePrimary`][Name::MdxJsxTagNamePrimary]
*   [`MdxJsxTagNameMemberMarker`][Name::MdxJsxTagNameMemberMarker]
*   [`MdxJsxTagNamePrefixMarker`][Name::MdxJsxTagNamePrefixMarker]
*   [`MdxJsxTagNameMember`][Name::MdxJsxTagNameMember]
*   [`MdxJsxTagNameLocal`][Name::MdxJsxTagNameLocal]
*   [`MdxJsxTagAttribute`][Name::MdxJsxTagAttribute]
*   [`MdxJsxTagAttributeName`][Name::MdxJsxTagAttributeName]
*   [`MdxJsxTagAttributePrimaryName`][Name::MdxJsxTagAttributePrimaryName]
*   [`MdxJsxTagAttributeNamePrefixMarker`][Name::MdxJsxTagAttributeNamePrefixMarker]
*   [`MdxJsxTagAttributeNameLocal`][Name::MdxJsxTagAttributeNameLocal]
*   [`MdxJsxTagAttributeInitializerMarker`][Name::MdxJsxTagAttributeInitializerMarker]
*   [`MdxJsxTagAttributeValueLiteral`][Name::MdxJsxTagAttributeValueLiteral]
*   [`MdxJsxTagAttributeValueLiteralMarker`][Name::MdxJsxTagAttributeValueLiteralMarker]
*   [`MdxJsxTagAttributeValueLiteralValue`][Name::MdxJsxTagAttributeValueLiteralValue]
*   [`MdxJsxTagSelfClosingMarker`][Name::MdxJsxTagSelfClosingMarker]

## Recommendation

When authoring markdown with JSX, keep in mind that MDX is a whitespace
sensitive and line-based language, while JavaScript is insensitive to
whitespace.
This affects how markdown and JSX interleave with eachother in MDX.
For more info on how it works, see [§ Interleaving][interleaving] on the
MDX site.

###### Comments inside tags

JavaScript comments in JSX are not supported.

Incorrect:

```jsx
<hi/*comment!*//>
<hello// comment!
/>
```

Correct:

```jsx
<hi/>
<hello
/>
```

A PR that adds support for them would be accepted.

###### Element or fragment attribute values

JSX elements or JSX fragments as attribute values are not supported.
The reason for this change is that it would be confusing whether markdown
would work.

Incorrect:

```jsx
<welcome name=<>Venus</> />
<welcome name=<span>Pluto</span> />
```

Correct:

```jsx
<welcome name='Mars' />
<welcome name={<span>Jupiter</span>} />
```

###### Greater than (`>`) and right curly brace (`}`)

JSX does not allow U+003E GREATER THAN (`>`) or U+007D RIGHT CURLY BRACE
(`}`) literally in text, they need to be encoded as character references
(or expressions).
There is no good reason for this (some JSX parsers agree with us and don’t
crash either).
Therefore, in MDX, U+003E GREATER THAN (`>`) and U+007D RIGHT CURLY BRACE
(`}`) are fine literally and don’t need to be encoded.

## References

*   [`jsx-flow.js` in `micromark-extension-mdx-jsx`](https://github.com/micromark/micromark-extension-mdx-jsx/blob/main/dev/lib/jsx-flow.js)
*   [`mdxjs.com`](https://mdxjs.com)

[mdx_jsx_flow]: crate::construct::mdx_jsx_flow
[mdx_jsx_text]: crate::construct::mdx_jsx_text
[interleaving]: https://mdxjs.com/docs/what-is-mdx/#interleaving
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-28", modified => "2025-10-28"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    start/1,
    start_after/1,
    name_before/1,
    closing_tag_name_before/1,
    primary_name/1,
    primary_name_after/1,
    member_name_before/1,
    member_name/1,
    member_name_after/1,
    local_name_before/1,
    local_name/1,
    local_name_after/1,
    attribute_before/1,
    attribute_expression_after/1,
    attribute_primary_name/1,
    attribute_primary_name_after/1,
    attribute_local_name_before/1,
    attribute_local_name/1,
    attribute_local_name_after/1,
    attribute_value_before/1,
    attribute_value_expression_after/1,
    attribute_value_quoted_start/1,
    attribute_value_quoted/1,
    self_closing/1,
    tag_end/1,
    es_whitespace_start/1,
    es_whitespace_inside/1,
    es_whitespace_eol_after/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of MDX: JSX.

```markdown
> | a <B /> c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{current = {some, $<}, tokenize_state = #markdown_tokenize_state{token_1 = Token1}}
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token1),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, mdx_jsx_tag_marker),
    State = markdown_state:next(mdx_jsx_start_after),
    {Tokenizer5, State}.

-doc """
After `<`.

```markdown
> | a <B /> c
       ^
```
""".
-spec start_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\n orelse Current =:= $\s
->
    %% Deviate from JSX, which allows arbitrary whitespace.
    %% See: <https://github.com/micromark/micromark-extension-mdx-jsx/issues/7>.
    State = markdown_state:nok(),
    {Tokenizer1, State};
start_after(Tokenizer1 = #markdown_tokenizer{}) ->
    OkState = markdown_state:next(mdx_jsx_name_before),
    NokState = markdown_state:nok(),
    Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, OkState, NokState),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer2, State}.

-doc """
Before name, self slash, or end of tag for fragments.

```markdown
> | a <B> c
       ^
> | a </B> c
       ^
> | a <> b
       ^
```
""".
-spec name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
name_before(Tokenizer1 = #markdown_tokenizer{current = {some, $/}}) ->
    %% Closing tag.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_closing_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_closing_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_closing_tag_name_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
name_before(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    %% Fragment opening tag.
    State = markdown_state:retry(mdx_jsx_tag_end),
    {Tokenizer1, State};
name_before(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_name_primary),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            State = markdown_state:next(mdx_jsx_primary_name),
            {Tokenizer4, State};
        false ->
            Message =
                case Current of
                    {some, $!} ->
                        <<"a character that can start a name, such as a letter, `$`, or `_` (note: to create a comment in MDX, use `{/* text */}`)">>;
                    _ ->
                        <<"a character that can start a name, such as a letter, `$`, or `_`">>
                end,
            crash(Tokenizer1, <<"before name">>, Message)
    end.

-doc """
Before name of closing tag or end of closing fragment tag.

```markdown
> | a </> b
        ^
> | a </B> c
        ^
```
""".
-spec closing_tag_name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
closing_tag_name_before(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    %% Fragment closing tag.
    State = markdown_state:retry(mdx_jsx_tag_end),
    {Tokenizer1, State};
closing_tag_name_before(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            %% Start of a closing tag name.
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_name_primary),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            State = markdown_state:next(mdx_jsx_primary_name),
            {Tokenizer4, State};
        false ->
            Message =
                case Current of
                    {some, Char} when Char =:= $* orelse Char =:= $/ ->
                        <<"a character that can start a name, such as a letter, `$`, or `_` (note: JS comments in JSX tags are not supported in MDX)">>;
                    _ ->
                        <<"a character that can start a name, such as a letter, `$`, or `_`">>
                end,
            crash(Tokenizer1, <<"before name">>, Message)
    end.

-doc """
In primary name.

```markdown
> | a <Bc> d
        ^
```
""".
-spec primary_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
primary_name(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    %% End of name.
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $.) orelse
                ?is_option_some(Current, $.) orelse
                ?is_option_some(Current, $:) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${))
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name_primary),
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(mdx_jsx_primary_name_after), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer3, State};
        false ->
            %% Continuation of name: remain.
            %% Allow continuation bytes.
            CharAfter = markdown_util_char:after_index(Bytes, Index),
            case (Current >= {some, 16#80} andalso Current =< {some, 16#BF}) orelse id_cont_opt(CharAfter) of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(mdx_jsx_primary_name),
                    {Tokenizer2, State};
                false ->
                    Message =
                        case Current of
                            {some, $@} ->
                                <<"a name character such as letters, digits, `$`, or `_`; whitespace before attributes; or the end of the tag (note: to create a link in MDX, use `[text](url)`)">>;
                            _ ->
                                <<"a name character such as letters, digits, `$`, or `_`; whitespace before attributes; or the end of the tag">>
                        end,
                    crash(Tokenizer1, <<"in name">>, Message)
            end
    end.

-doc """
After primary name.

```markdown
> | a <b.c> d
        ^
> | a <b:c> d
        ^
```
""".
-spec primary_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
primary_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $.}}) ->
    %% Start of a member name.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name_member_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_name_member_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_member_name_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
primary_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $:}}) ->
    %% Start of a local name.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name_prefix_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_name_prefix_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_local_name_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
primary_name_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% End of name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case
        (Current =:= {some, $/} orelse Current =:= {some, $>} orelse Current =:= {some, ${}) orelse
            id_start_opt(CharAfter)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name),
            State = markdown_state:retry(mdx_jsx_attribute_before),
            {Tokenizer2, State};
        false ->
            crash(
                Tokenizer1,
                <<"after name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; whitespace before attributes; or the end of the tag">>
            )
    end.

-doc """
Before member name.

```markdown
> | a <b.c> d
         ^
```
""".
-spec member_name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
member_name_before(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Start of a member name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name_member),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(mdx_jsx_member_name),
            {Tokenizer3, State};
        false ->
            crash(
                Tokenizer1,
                <<"before member name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; whitespace before attributes; or the end of the tag">>
            )
    end.

-doc """
In member name.

```markdown
> | a <b.cd> e
          ^
```
""".
-spec member_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
member_name(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    %% End of name.
    %% Note: no `:` allowed here.
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $.) orelse
                ?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${))
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name_member),
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(mdx_jsx_member_name_after), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer3, State};
        false ->
            %% Continuation of name: remain.
            %% Allow continuation bytes.
            CharAfter = markdown_util_char:after_index(Bytes, Index),
            case (Current >= {some, 16#80} andalso Current =< {some, 16#BF}) orelse id_cont_opt(CharAfter) of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(mdx_jsx_member_name),
                    {Tokenizer2, State};
                false ->
                    Message =
                        case Current of
                            {some, $@} ->
                                <<"a name character such as letters, digits, `$`, or `_`; whitespace before attributes; or the end of the tag (note: to create a link in MDX, use `[text](url)`)">>;
                            _ ->
                                <<"a name character such as letters, digits, `$`, or `_`; whitespace before attributes; or the end of the tag">>
                        end,
                    crash(Tokenizer1, <<"in member name">>, Message)
            end
    end.

-doc """
After member name.

```markdown
> | a <b.c> d
          ^
> | a <b.c.d> e
          ^
```
""".
-spec member_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
member_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $.}}) ->
    %% Start of another member name.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name_member_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_name_member_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_member_name_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
member_name_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% End of name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case
        (?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${)) orelse
            id_start_opt(CharAfter)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name),
            State = markdown_state:retry(mdx_jsx_attribute_before),
            {Tokenizer2, State};
        false ->
            crash(
                Tokenizer1,
                <<"after member name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; whitespace before attributes; or the end of the tag">>
            )
    end.

-doc """
Local member name.

```markdown
> | a <b:c> d
         ^
```
""".
-spec local_name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
local_name_before(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Start of a local name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_name_local),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(mdx_jsx_local_name),
            {Tokenizer3, State};
        false ->
            Message =
                case Current of
                    {some, Char} when Char =:= $+ orelse (Char >= $/ andalso Char =< $9) ->
                        <<"a character that can start a name, such as a letter, `$`, or `_` (note: to create a link in MDX, use `[text](url)`)">>;
                    _ ->
                        <<"a character that can start a name, such as a letter, `$`, or `_`">>
                end,
            crash(Tokenizer1, <<"before local name">>, Message)
    end.

-doc """
In local name.

```markdown
> | a <b:cd> e
          ^
```
""".
-spec local_name(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
local_name(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    %% End of local name (note that we don't expect another colon, or a member).
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${))
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name_local),
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(mdx_jsx_local_name_after), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer3, State};
        false ->
            %% Continuation of name: remain.
            %% Allow continuation bytes.
            CharAfter = markdown_util_char:after_index(Bytes, Index),
            case (Current >= {some, 16#80} andalso Current =< {some, 16#BF}) orelse id_cont_opt(CharAfter) of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(mdx_jsx_local_name),
                    {Tokenizer2, State};
                false ->
                    crash(
                        Tokenizer1,
                        <<"in local name">>,
                        <<"a name character such as letters, digits, `$`, or `_`; whitespace before attributes; or the end of the tag">>
                    )
            end
    end.

-doc """
After local name.

This is like as `primary_name_after`, but we don't expect colons or
periods.

```markdown
> | a <b.c> d
          ^
> | a <b.c.d> e
          ^
```
""".
-spec local_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
local_name_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% End of name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case
        (?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${)) orelse
            id_start_opt(CharAfter)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_name),
            State = markdown_state:retry(mdx_jsx_attribute_before),
            {Tokenizer2, State};
        false ->
            crash(
                Tokenizer1,
                <<"after local name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; whitespace before attributes; or the end of the tag">>
            )
    end.

-doc """
Before attribute.

```markdown
> | a <b /> c
         ^
> | a <b > c
         ^
> | a <b {...c}> d
         ^
> | a <b c> d
         ^
```
""".
-spec attribute_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_before(Tokenizer1 = #markdown_tokenizer{current = {some, $/}}) ->
    %% Self-closing.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_self_closing_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_self_closing_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_self_closing), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
attribute_before(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    %% End of tag.
    State = markdown_state:retry(mdx_jsx_tag_end),
    {Tokenizer1, State};
attribute_before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Char},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = Token1, token_2 = _Token2}
    }
) when Char =:= ${ ->
    %% Attribute expression.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_2 = Token1, token_1 = mdx_jsx_tag_attribute_expression
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(mdx_jsx_attribute_expression_after), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_expression_start),
    {Tokenizer3, State};
attribute_before(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Start of an attribute name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_attribute),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_attribute_name),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, mdx_jsx_tag_attribute_primary_name),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            State = markdown_state:next(mdx_jsx_attribute_primary_name),
            {Tokenizer5, State};
        false ->
            crash(
                Tokenizer1,
                <<"before attribute name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; whitespace before attributes; or the end of the tag">>
            )
    end.

-doc """
After attribute expression.

```markdown
> | a <b {c} d/> e
            ^
```
""".
-spec attribute_expression_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_expression_after(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = _Token1, token_2 = Token2}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = Token2, token_2 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(mdx_jsx_attribute_before), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer3, State}.

-doc """
In primary attribute name.

```markdown
> | a <b cd/> e
          ^
> | a <b c:d> e
          ^
> | a <b c=d> e
          ^
```
""".
-spec attribute_primary_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_primary_name(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    %% End of attribute name or tag.
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $/) orelse
                ?is_option_some(Current, $:) orelse
                ?is_option_some(Current, $=) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${))
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute_primary_name),
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(mdx_jsx_attribute_primary_name_after), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer3, State};
        false ->
            %% Continuation of name: remain.
            %% Allow continuation bytes.
            CharAfter = markdown_util_char:after_index(Bytes, Index),
            case (Current >= {some, 16#80} andalso Current =< {some, 16#BF}) orelse id_cont_opt(CharAfter) of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(mdx_jsx_attribute_primary_name),
                    {Tokenizer2, State};
                false ->
                    crash(
                        Tokenizer1,
                        <<"in attribute name">>,
                        <<"an attribute name character such as letters, digits, `$`, or `_`; `=` to initialize a value; whitespace before attributes; or the end of the tag">>
                    )
            end
    end.

-doc """
After primary attribute name.

```markdown
> | a <b c/> d
          ^
> | a <b c:d> e
          ^
> | a <b c=d> e
          ^
```
""".
-spec attribute_primary_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_primary_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $:}}) ->
    %% Start of a local name.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_attribute_name_prefix_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_attribute_name_prefix_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_attribute_local_name_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
attribute_primary_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $=}}) ->
    %% Initializer: start of an attribute value.
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute_name),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_attribute_initializer_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, mdx_jsx_tag_attribute_initializer_marker),
    Tokenizer6 = markdown_tokenizer:attempt(
        Tokenizer5, markdown_state:next(mdx_jsx_attribute_value_before), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer6, State};
attribute_primary_name_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% End of tag / new attribute.
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${)) orelse
            id_start_opt(CharAfter)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute_name),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, mdx_jsx_tag_attribute),
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(mdx_jsx_attribute_before), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer4, State};
        false ->
            crash(
                Tokenizer1,
                <<"after attribute name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; `=` to initialize a value; or the end of the tag">>
            )
    end.

-doc """
Before local attribute name.

```markdown
> | a <b c:d/> e
           ^
```
""".
-spec attribute_local_name_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_local_name_before(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Start of a local name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case id_start_opt(CharAfter) of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_attribute_name_local),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(mdx_jsx_attribute_local_name),
            {Tokenizer3, State};
        false ->
            crash(
                Tokenizer1,
                <<"before local attribute name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; `=` to initialize a value; or the end of the tag">>
            )
    end.

-doc """
In local attribute name.

```markdown
> | a <b c:de/> f
            ^
> | a <b c:d=e/> f
            ^
```
""".
-spec attribute_local_name(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_local_name(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    %% End of local name (note that we don't expect another colon).
    case
        Kind =:= whitespace orelse
            (?is_option_some(Current, $/) orelse
                ?is_option_some(Current, $=) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${))
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute_name_local),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, mdx_jsx_tag_attribute_name),
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(mdx_jsx_attribute_local_name_after), markdown_state:nok()
            ),
            State = markdown_state:retry(mdx_jsx_es_whitespace_start),
            {Tokenizer4, State};
        false ->
            %% Continuation of name: remain.
            %% Allow continuation bytes.
            CharAfter = markdown_util_char:after_index(Bytes, Index),
            case (Current >= {some, 16#80} andalso Current =< {some, 16#BF}) orelse id_cont_opt(CharAfter) of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(mdx_jsx_attribute_local_name),
                    {Tokenizer2, State};
                false ->
                    crash(
                        Tokenizer1,
                        <<"in local attribute name">>,
                        <<"an attribute name character such as letters, digits, `$`, or `_`; `=` to initialize a value; whitespace before attributes; or the end of the tag">>
                    )
            end
    end.

-doc """
After local attribute name.

```markdown
> | a <b c:d/> f
            ^
> | a <b c:d=e/> f
            ^
```
""".
-spec attribute_local_name_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_local_name_after(Tokenizer1 = #markdown_tokenizer{current = {some, $=}}) ->
    %% Start of an attribute value.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_attribute_initializer_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_attribute_initializer_marker),
    Tokenizer5 = markdown_tokenizer:attempt(
        Tokenizer4, markdown_state:next(mdx_jsx_attribute_value_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer5, State};
attribute_local_name_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% End of name.
    CharAfter = markdown_util_char:after_index(Bytes, Index),
    case
        (?is_option_some(Current, $/) orelse ?is_option_some(Current, $>) orelse ?is_option_some(Current, ${)) orelse
            id_start_opt(CharAfter)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute),
            State = markdown_state:retry(mdx_jsx_attribute_before),
            {Tokenizer2, State};
        false ->
            crash(
                Tokenizer1,
                <<"after local attribute name">>,
                <<"a character that can start an attribute name, such as a letter, `$`, or `_`; `=` to initialize a value; or the end of the tag">>
            )
    end.

-doc """
After `=`, before value.

```markdown
> | a <b c="d"/> e
           ^
> | a <b c={d}/> e
           ^
```
""".
-spec attribute_value_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_value_before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Char}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Char =:= $" orelse Char =:= $' ->
    %% Start of double- or single quoted value.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Char},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_attribute_value_literal),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, mdx_jsx_tag_attribute_value_literal_marker),
    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, mdx_jsx_tag_attribute_value_literal_marker),
    State = markdown_state:next(mdx_jsx_attribute_value_quoted_start),
    {Tokenizer6, State};
attribute_value_before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Char},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = Token1, token_2 = _Token2}
    }
) when Char =:= ${ ->
    %% Attribute value expression.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_2 = Token1, token_1 = mdx_jsx_tag_attribute_value_expression
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(mdx_jsx_attribute_value_expression_after), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_expression_start),
    {Tokenizer3, State};
attribute_value_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    Message =
        case Current of
            {some, $<} ->
                <<"a character that can start an attribute value, such as `\"`, `'`, or `{` (note: to use an element or fragment as a prop value in MDX, use `{<element />}`)">>;
            _ ->
                <<"a character that can start an attribute value, such as `\"`, `'`, or `{`">>
        end,
    crash(Tokenizer1, <<"before attribute value">>, Message).

-doc """
After attribute value expression.

```markdown
> | a <b c={d} e/> f
              ^
```
""".
-spec attribute_value_expression_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_value_expression_after(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = _Token1, token_2 = Token2}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = Token2, token_2 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, mdx_jsx_tag_attribute),
    Tokenizer4 = markdown_tokenizer:attempt(
        Tokenizer3, markdown_state:next(mdx_jsx_attribute_before), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer4, State}.

-doc """
Before quoted literal attribute value.

```markdown
> | a <b c="d"/> e
           ^
```
""".
-spec attribute_value_quoted_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_value_quoted_start(
    Tokenizer1 = #markdown_tokenizer{current = {some, Byte}, tokenize_state = #markdown_tokenize_state{marker = Marker}}
) when Byte =:= Marker ->
    TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_jsx_tag_attribute_value_literal_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, mdx_jsx_tag_attribute_value_literal_marker),
    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, mdx_jsx_tag_attribute_value_literal),
    Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, mdx_jsx_tag_attribute),
    Tokenizer8 = markdown_tokenizer:attempt(
        Tokenizer7, markdown_state:next(mdx_jsx_attribute_before), markdown_state:nok()
    ),
    State = markdown_state:next(mdx_jsx_es_whitespace_start),
    {Tokenizer8, State};
attribute_value_quoted_start(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(mdx_jsx_attribute_value_quoted_start), markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer2, State};
attribute_value_quoted_start(Tokenizer1 = #markdown_tokenizer{current = {some, _Byte}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_attribute_value_literal_value),
    State = markdown_state:retry(mdx_jsx_attribute_value_quoted),
    {Tokenizer2, State};
attribute_value_quoted_start(
    Tokenizer1 = #markdown_tokenizer{current = none, tokenize_state = #markdown_tokenize_state{marker = Marker}}
) ->
    Message = <<"a corresponding closing quote ", (markdown_util_char:format_byte(Marker))/bytes>>,
    crash(Tokenizer1, <<"in attribute value">>, Message).

-doc """
In quoted literal attribute value.

```markdown
> | a <b c="d"/> e
            ^
```
""".
-spec attribute_value_quoted(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
attribute_value_quoted(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{marker = Marker}}
) ->
    case Current =:= {some, Marker} orelse Current =:= none orelse Current =:= {some, $\n} of
        true ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_tag_attribute_value_literal_value),
            State = markdown_state:retry(mdx_jsx_attribute_value_quoted_start),
            {Tokenizer2, State};
        false ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(mdx_jsx_attribute_value_quoted),
            {Tokenizer2, State}
    end.

-doc """
After self-closing slash.

```markdown
> | a <b/> c
         ^
```
""".
-spec self_closing(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
self_closing(Tokenizer1 = #markdown_tokenizer{current = {some, $>}}) ->
    State = markdown_state:retry(mdx_jsx_tag_end),
    {Tokenizer1, State};
self_closing(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    Message =
        case Current of
            {some, Char} when Char =:= $* orelse Char =:= $/ ->
                <<"`>` to end the tag (note: JS comments in JSX tags are not supported in MDX)">>;
            _ ->
                <<"`>` to end the tag">>
        end,
    crash(Tokenizer1, <<"after self-closing slash">>, Message).

-doc """
At final `>`.

```markdown
> | a <b> c
        ^
```
""".
-spec tag_end(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
tag_end(
    Tokenizer1 = #markdown_tokenizer{current = {some, $>}, tokenize_state = #markdown_tokenize_state{token_1 = Token1}}
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_tag_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_jsx_tag_marker),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, Token1),
    State = markdown_state:ok(),
    {Tokenizer5, State}.

-doc """
Before optional ECMAScript whitespace.

```markdown
> | a <a b> c
        ^
```
""".
-spec es_whitespace_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
es_whitespace_start(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(mdx_jsx_es_whitespace_eol_after),
    {Tokenizer4, State};
es_whitespace_start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    case Kind =:= whitespace of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_jsx_es_whitespace),
            State = markdown_state:retry(mdx_jsx_es_whitespace_inside),
            {Tokenizer2, State};
        false ->
            State = markdown_state:ok(),
            {Tokenizer1, State}
    end.

-doc """
In ECMAScript whitespace.

```markdown
> | a <a  b> c
         ^
```
""".
-spec es_whitespace_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
es_whitespace_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_es_whitespace),
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer2, State};
es_whitespace_inside(Tokenizer1 = #markdown_tokenizer{current = {some, Byte}}) when
    Byte >= 16#80 andalso Byte =< 16#BF
->
    %% Allow continuation bytes.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(mdx_jsx_es_whitespace_inside),
    {Tokenizer2, State};
es_whitespace_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, _Byte},
        parse_state = #markdown_parse_state{bytes = Bytes},
        point = #markdown_point{offset = Index}
    }
) ->
    Kind = markdown_util_char:kind_after_index(Bytes, Index),
    case Kind =:= whitespace of
        true ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(mdx_jsx_es_whitespace_inside),
            {Tokenizer2, State};
        false ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_jsx_es_whitespace),
            State = markdown_state:ok(),
            {Tokenizer2, State}
    end;
es_whitespace_inside(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    %% Handle EOF.
    State = markdown_state:nok(),
    {Tokenizer1, State}.

-doc """
After eol in whitespace.

```markdown
> | a <a\nb> c
         ^
```
""".
-spec es_whitespace_eol_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
es_whitespace_eol_after(
    Tokenizer1 = #markdown_tokenizer{
        lazy = true, tokenize_state = #markdown_tokenize_state{token_1 = mdx_jsx_flow_tag}, point = Point
    }
) ->
    %% Lazy continuation in a flow tag is a syntax error.
    Place = markdown_place:point(markdown_point:to_unist(Point)),
    Message = markdown_message:new(
        {some, Place},
        <<"Unexpected lazy line in jsx in container, expected line to be prefixed with `>` when in a block quote, whitespace when in a list, etc">>,
        <<"unexpected-lazy">>,
        <<"erlang-markdown">>
    ),
    State = markdown_state:error(Message),
    {Tokenizer1, State};
es_whitespace_eol_after(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:retry(mdx_jsx_es_whitespace_start),
    {Tokenizer1, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Check if a character can start a JSX identifier.
""".
-spec id_start_opt(OptionCode) -> boolean() when OptionCode :: markdown_option:t(Code), Code :: char().
id_start_opt(none) ->
    false;
id_start_opt({some, Code}) ->
    markdown_util_identifier:id_start(Code).

%% @private
-doc """
Check if a character can continue a JSX identifier.
""".
-spec id_cont_opt(OptionCode) -> boolean() when OptionCode :: markdown_option:t(Code), Code :: char().
id_cont_opt(none) ->
    false;
id_cont_opt({some, Code}) ->
    markdown_util_identifier:id_cont(Code, true).

%% @private
-doc """
Crash because something happened `at`, with info on what was `expect`ed
instead.
""".
-spec crash(Tokenizer, At, Expect) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), At :: binary(), Expect :: binary(), State :: markdown_state:t().
crash(
    Tokenizer = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = Point
    },
    At,
    Expect
) ->
    CharOpt =
        case Current of
            none ->
                none;
            {some, _} ->
                markdown_util_char:after_index(Bytes, Point#markdown_point.offset)
        end,
    RuleId =
        case Current of
            none ->
                <<"unexpected-eof">>;
            {some, _} ->
                <<"unexpected-character">>
        end,
    Reason =
        <<"Unexpected ", (markdown_util_char:format_opt(CharOpt))/bytes, " ", At/bytes, " , expected ", Expect/bytes>>,
    Message = markdown_message:new(
        {some, markdown_place:point(markdown_point:to_unist(Point))},
        Reason,
        RuleId,
        <<"erlang-markdown">>
    ),
    State = markdown_state:error(Message),
    {Tokenizer, State}.
