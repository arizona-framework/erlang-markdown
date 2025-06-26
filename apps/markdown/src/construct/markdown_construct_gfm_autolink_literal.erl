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
-module(markdown_construct_gfm_autolink_literal).
-moduledoc """
GFM: autolink literal occurs in the [text][] content type.

## Grammar

Autolink literals form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
gfm_autolink_literal ::= gfm_protocol_autolink | gfm_www_autolink | gfm_email_autolink

; Restriction: the code before must be `www_autolink_before`.
; Restriction: the code after `.` must not be eof.
www_autolink ::= 3('w' | 'W') '.' [domain [path]]
www_autolink_before ::= eof | eol | space_or_tab | '(' | '*' | '_' | '[' | ']' | '~'

; Restriction: the code before must be `http_autolink_before`.
; Restriction: the code after the protocol must be `http_autolink_protocol_after`.
http_autolink ::= ('h' | 'H') 2('t' | 'T') ('p' | 'P') ['s' | 'S'] ':' 2'/' domain [path]
http_autolink_before ::= byte - ascii_alpha
http_autolink_protocol_after ::= byte - eof - eol - ascii_control - unicode_whitespace - unicode_punctuation

; Restriction: the code before must be `email_autolink_before`.
; Restriction: `ascii_digit` may not occur in the last label part of the label.
email_autolink ::= 1*('+' | '-' | '.' | '_' | ascii_alphanumeric) '@' 1*(1*label_segment label_dot_cont) 1*label_segment
email_autolink_before ::= byte - ascii_alpha - '/'

; Restriction: `_` may not occur in the last two domain parts.
domain ::= 1*(url_ampt_cont | domain_punct_cont | '-' | byte - eof - ascii_control - unicode_whitespace - unicode_punctuation)
; Restriction: must not be followed by `punct`.
domain_punct_cont ::= '.' | '_'
; Restriction: must not be followed by `char-ref`.
url_ampt_cont ::= '&'

; Restriction: a counter `balance = 0` is increased for every `(`, and decreased for every `)`.
; Restriction: `)` must not be `paren_at_end`.
path ::= 1*(url_ampt_cont | path_punctuation_cont | '(' | ')' | byte - eof - eol - space_or_tab)
; Restriction: must not be followed by `punct`.
path_punctuation_cont ::= trailing_punctuation - '<'
; Restriction: must be followed by `punct` and `balance` must be less than `0`.
paren_at_end ::= ')'

label_segment ::= label_dash_underscore_cont | ascii_alpha | ascii_digit
; Restriction: if followed by `punct`, the whole email autolink is invalid.
label_dash_underscore_cont ::= '-' | '_'
; Restriction: must not be followed by `punct`.
label_dot_cont ::= '.'

punct ::= *trailing_punctuation ( byte - eof - eol - space_or_tab - '<' )
char_ref ::= *ascii_alpha ';' path_end
trailing_punctuation ::= '!' | '"' | '\'' | ')' | '*' | ',' | '.' | ':' | ';' | '<' | '?' | '_' | '~'
```

The grammar for GFM autolink literal is very relaxed: basically anything
except for whitespace is allowed after a prefix.
To use whitespace characters and otherwise impossible characters, in URLs,
you can use percent encoding:

```markdown
https://example.com/alpha%20bravo
```

Yields:

```html
<p><a href="https://example.com/alpha%20bravo">https://example.com/alpha%20bravo</a></p>
```

There are several cases where incorrect encoding of URLs would, in other
languages, result in a parse error.
In markdown, there are no errors, and URLs are normalized.
In addition, many characters are percent encoded
([`sanitize_uri`][sanitize_uri]).
For example:

```markdown
www.a👍b%
```

Yields:

```html
<p><a href="http://www.a%F0%9F%91%8Db%25">www.a👍b%</a></p>
```

There is a big difference between how www and protocol literals work
compared to how email literals work.
The first two are done when parsing, and work like anything else in
markdown.
But email literals are handled afterwards: when everything is parsed, we
look back at the events to figure out if there were email addresses.
This particularly affects how they interleave with character escapes and
character references.

## HTML

GFM autolink literals relate to the `<a>` element in HTML.
See [*§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.
When an email autolink is used, the string `mailto:` is prepended when
generating the `href` attribute of the hyperlink.
When a www autolink is used, the string `http:` is prepended.

## Recommendation

It is recommended to use labels ([label start link][label_start_link],
[label end][label_end]), either with a resource or a definition
([definition][]), instead of autolink literals, as those allow relative
URLs and descriptive text to explain the URL in prose.

## Bugs

GitHub’s own algorithm to parse autolink literals contains three bugs.
A smaller bug is left unfixed in this project for consistency.
Two main bugs are not present in this project.
The issues relating to autolink literals are:

*   [GFM autolink extension (`www.`, `https?://` parts): links don’t work when after bracket](https://github.com/github/cmark-gfm/issues/278)\
    fixed here ✅
*   [GFM autolink extension (`www.` part): uppercase does not match on issues/PRs/comments](https://github.com/github/cmark-gfm/issues/280)\
    fixed here ✅
*   [GFM autolink extension (`www.` part): the word `www` matches](https://github.com/github/cmark-gfm/issues/279)\
    present here for consistency

## Tokens

*   [`GfmAutolinkLiteralEmail`][Name::GfmAutolinkLiteralEmail]
*   [`GfmAutolinkLiteralMailto`][Name::GfmAutolinkLiteralMailto]
*   [`GfmAutolinkLiteralProtocol`][Name::GfmAutolinkLiteralProtocol]
*   [`GfmAutolinkLiteralWww`][Name::GfmAutolinkLiteralWww]
*   [`GfmAutolinkLiteralXmpp`][Name::GfmAutolinkLiteralXmpp]

## References

*   [`micromark-extension-gfm-autolink-literal`](https://github.com/micromark/micromark-extension-gfm-autolink-literal)
*   [*§ 6.9 Autolinks (extension)* in `GFM`](https://github.github.com/gfm/#autolinks-extension-)

> 👉 **Note**: `mailto:` and `xmpp:` protocols before email autolinks were
> added in `cmark-gfm@0.29.0.gfm.5` and are as of yet undocumented.

[text]: crate::construct::text
[definition]: crate::construct::definition
[attention]: crate::construct::attention
[label_start_link]: crate::construct::label_start_link
[label_end]: crate::construct::label_end
[sanitize_uri]: crate::util::sanitize_uri
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    protocol_start/1,
    protocol_after/1,
    protocol_prefix_inside/1,
    protocol_slashes_inside/1,
    www_start/1,
    www_after/1,
    www_prefix_inside/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of protocol autolink literal.

```markdown
> | https://example.com/a?b#c
    ^
```
""".
-spec protocol_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
protocol_start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{
                    gfm_autolink_literal = true
                }
            }
        },
        current = {some, Current},
        previous = Previous,
        point = Point,
        tokenize_state = TokenizeState1
    }
) when
    (Current =:= $H orelse Current =:= $h) andalso
        not (Previous =:= {some, $A} orelse Previous =:= {some, $B} orelse Previous =:= {some, $C} orelse
            Previous =:= {some, $D} orelse Previous =:= {some, $E} orelse Previous =:= {some, $F} orelse
            Previous =:= {some, $G} orelse Previous =:= {some, $H} orelse Previous =:= {some, $I} orelse
            Previous =:= {some, $J} orelse Previous =:= {some, $K} orelse Previous =:= {some, $L} orelse
            Previous =:= {some, $M} orelse Previous =:= {some, $N} orelse Previous =:= {some, $O} orelse
            Previous =:= {some, $P} orelse Previous =:= {some, $Q} orelse Previous =:= {some, $R} orelse
            Previous =:= {some, $S} orelse Previous =:= {some, $T} orelse Previous =:= {some, $U} orelse
            Previous =:= {some, $V} orelse Previous =:= {some, $W} orelse Previous =:= {some, $X} orelse
            Previous =:= {some, $Y} orelse Previous =:= {some, $Z} orelse Previous =:= {some, $a} orelse
            Previous =:= {some, $b} orelse Previous =:= {some, $c} orelse Previous =:= {some, $d} orelse
            Previous =:= {some, $e} orelse Previous =:= {some, $f} orelse Previous =:= {some, $g} orelse
            Previous =:= {some, $h} orelse Previous =:= {some, $i} orelse Previous =:= {some, $j} orelse
            Previous =:= {some, $k} orelse Previous =:= {some, $l} orelse Previous =:= {some, $m} orelse
            Previous =:= {some, $n} orelse Previous =:= {some, $o} orelse Previous =:= {some, $p} orelse
            Previous =:= {some, $q} orelse Previous =:= {some, $r} orelse Previous =:= {some, $s} orelse
            Previous =:= {some, $t} orelse Previous =:= {some, $u} orelse Previous =:= {some, $v} orelse
            Previous =:= {some, $w} orelse Previous =:= {some, $x} orelse Previous =:= {some, $y} orelse
            Previous =:= {some, $z})
->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_autolink_literal_protocol),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(gfm_autolink_literal_protocol_after),
        markdown_state:nok()
    ),
    Tokenizer4 = markdown_tokenizer:attempt(
        Tokenizer3,
        markdown_state:next(gfm_autolink_literal_domain_inside),
        markdown_state:nok()
    ),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = Point#markdown_point.offset},
    Tokenizer5 = Tokenizer4#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer5, markdown_state:retry(gfm_autolink_literal_protocol_prefix_inside)};
protocol_start(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
After a protocol autolink literal.

```markdown
> | https://example.com/a?b#c
                             ^
```
""".
-spec protocol_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
protocol_after(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, gfm_autolink_literal_protocol),
    {Tokenizer2, markdown_state:ok()}.

-doc """
In protocol.

```markdown
> | https://example.com/a?b#c
    ^^^^^
```
""".
-spec protocol_prefix_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
protocol_prefix_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        point = #markdown_point{offset = Index},
        tokenize_state = #markdown_tokenize_state{start = Start}
    }
) when
    ((Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z)) andalso
        (Index - Start < 5)
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    {Tokenizer2, markdown_state:next(gfm_autolink_literal_protocol_prefix_inside)};
protocol_prefix_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $:},
        parse_state = #markdown_parse_state{bytes = Bytes},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{start = Start},
        point = #markdown_point{offset = Index}
    }
) ->
    Slice = markdown_slice:from_indices(Bytes, Start, Index),
    Name = markdown_slice:as_binary(Slice),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    case Name of
        <<"http">> ->
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            {Tokenizer3, markdown_state:next(gfm_autolink_literal_protocol_slashes_inside)};
        <<"https">> ->
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            {Tokenizer3, markdown_state:next(gfm_autolink_literal_protocol_slashes_inside)};
        _ ->
            {Tokenizer2, markdown_state:nok()}
    end;
protocol_prefix_inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, markdown_state:nok()}.

-doc """
In protocol slashes.

```markdown
> | https://example.com/a?b#c
          ^^
```
""".
-spec protocol_slashes_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
protocol_slashes_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $/}, tokenize_state = TokenizeState1}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    case TokenizeState1#markdown_tokenize_state.size of
        0 ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 1},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            {Tokenizer3, markdown_state:next(gfm_autolink_literal_protocol_slashes_inside)};
        _ ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            {Tokenizer3, markdown_state:ok()}
    end;
protocol_slashes_inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, markdown_state:nok()}.

-doc """
Start of www autolink literal.

```markdown
> | www.example.com/a?b#c
    ^
```
""".
-spec www_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
www_start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{
                    gfm_autolink_literal = true
                }
            }
        },
        current = {some, Current},
        previous = Previous
    }
) when
    (Current =:= $W orelse Current =:= $w) andalso
        (Previous =:= none orelse Previous =:= {some, $\t} orelse Previous =:= {some, $\n} orelse
            Previous =:= {some, $\s} orelse Previous =:= {some, $(} orelse Previous =:= {some, $*} orelse
            Previous =:= {some, $_} orelse Previous =:= {some, $[} orelse Previous =:= {some, $]} orelse
            Previous =:= {some, $~})
->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_autolink_literal_www),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(gfm_autolink_literal_www_after),
        markdown_state:nok()
    ),
    %% Note: we *check*, so we can discard the `www.` we parsed.
    %% If it worked, we consider it as a part of the domain.
    Tokenizer4 = markdown_tokenizer:check(
        Tokenizer3,
        markdown_state:next(gfm_autolink_literal_domain_inside),
        markdown_state:nok()
    ),
    {Tokenizer4, markdown_state:retry(gfm_autolink_literal_www_prefix_inside)};
www_start(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
After a www autolink literal.

```markdown
> | www.example.com/a?b#c
                         ^
```
""".
-spec www_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
www_after(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, gfm_autolink_literal_www),
    State = markdown_state:ok(),
    {Tokenizer2, State}.

-doc """
In www prefix.

```markdown
> | www.example.com
    ^^^^
```
""".
-spec www_prefix_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
www_prefix_inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1}) ->
    case Current of
        {some, $.} when TokenizeState1#markdown_tokenize_state.size =:= 3 ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(gfm_autolink_literal_www_prefix_after),
            {Tokenizer3, State};
        {some, C} when (C =:= $W orelse C =:= $w) andalso TokenizeState1#markdown_tokenize_state.size < 3 ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                size = TokenizeState1#markdown_tokenize_state.size + 1
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(gfm_autolink_literal_www_prefix_inside),
            {Tokenizer3, State};
        _ ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.
