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
    www_prefix_inside/1,
    www_prefix_after/1,
    domain_inside/1,
    domain_at_punctuation/1,
    domain_after/1,
    path_inside/1,
    path_at_punctuation/1,
    path_after/1,
    trail/1,
    trail_bracket_after/1,
    trail_char_ref_start/1,
    trail_char_ref_inside/1,
    resolve/1
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

-doc """
After www prefix.

```markdown
> | www.example.com
        ^
```
""".
-spec www_prefix_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
www_prefix_after(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:nok(),
    {Tokenizer, State};
www_prefix_after(Tokenizer = #markdown_tokenizer{current = {some, _Current}}) ->
    %% If there is *anything*, we can link.
    State = markdown_state:ok(),
    {Tokenizer, State}.

-doc """
In domain.

```markdown
> | https://example.com/a
            ^^^^^^^^^^^
```
""".
-spec domain_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
domain_inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $. orelse Current =:= $_ ->
    %% Check whether this marker, which is a trailing punctuation
    %% marker, optionally followed by more trailing markers, and then
    %% followed by an end.
    Tokenizer2 = markdown_tokenizer:check(
        Tokenizer1,
        markdown_state:next(gfm_autolink_literal_domain_after),
        markdown_state:next(gfm_autolink_literal_domain_at_punctuation)
    ),
    State = markdown_state:retry(gfm_autolink_literal_trail),
    {Tokenizer2, State};
domain_inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $- orelse (Current >= 16#80 andalso Current =< 16#BF)
->
    %% Dashes and continuation bytes are fine.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_domain_inside),
    {Tokenizer2, State};
domain_inside(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = Point, tokenize_state = TokenizeState1
    }
) ->
    %% Source: <https://github.com/github/cmark-gfm/blob/ef1cfcb/extensions/autolink.c#L12>.
    case markdown_util_char:kind_after_index(Bytes, Point#markdown_point.offset) of
        other ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = true},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(gfm_autolink_literal_domain_inside),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(gfm_autolink_literal_domain_after),
            {Tokenizer1, State}
    end.

-doc """
In domain, at potential trailing punctuation, that was not trailing.

```markdown
> | https://example.com
                   ^
```
""".
-spec domain_at_punctuation(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
domain_at_punctuation(Tokenizer1 = #markdown_tokenizer{current = {some, $_}, tokenize_state = TokenizeState1}) ->
    %% There is an underscore in the last segment of the domain
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = $_},
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}),
    State = markdown_state:next(gfm_autolink_literal_domain_inside),
    {Tokenizer2, State};
domain_at_punctuation(
    Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker}}
) ->
    %% Otherwise, it's a `.`: save the last segment underscore in the
    %% penultimate segment slot.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker_b = Marker, marker = 0},
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}),
    State = markdown_state:next(gfm_autolink_literal_domain_inside),
    {Tokenizer2, State}.

-doc """
After domain

```markdown
> | https://example.com/a
                       ^
```
""".
-spec domain_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
domain_after(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, marker_b = MarkerB, seen = Seen}
    }
) ->
    %% No underscores allowed in last two segments.
    Result =
        case (MarkerB =:= $_ orelse Marker =:= $_) orelse not Seen of
            true ->
                %% At least one character must be seen.
                %% Note: that's GH says a dot is needed, but it's not true:
                %% <https://github.com/github/cmark-gfm/issues/279>
                markdown_state:nok();
            false ->
                markdown_state:retry(gfm_autolink_literal_path_inside)
        end,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = false, marker = 0, marker_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, Result}.

-doc """
In path.

```markdown
> | https://example.com/a
                       ^^
```
""".
-spec path_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
path_inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current >= 16#80 andalso Current =< 16#BF
->
    %% Continuation bytes are fine, we've already checked the first one.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_path_inside),
    {Tokenizer2, State};
path_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $(}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size1}
    }
) ->
    %% Count opening parens.
    Size2 = Size1 + 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size2},
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}),
    State = markdown_state:next(gfm_autolink_literal_path_inside),
    {Tokenizer2, State};
path_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = #markdown_tokenize_state{size = Size, size_b = SizeB}
    }
) when
    Current =:= $! orelse Current =:= $" orelse Current =:= $& orelse Current =:= $' orelse Current =:= $) orelse
        Current =:= $* orelse Current =:= $, orelse Current =:= $. orelse Current =:= $: orelse Current =:= $; orelse
        Current =:= $< orelse Current =:= $? orelse Current =:= $] orelse Current =:= $_ orelse Current =:= $~
->
    %% Check whether this trailing punctuation marker is optionally
    %% followed by more trailing markers, and then followed
    %% by an end.
    %% If this is a paren (followed by trailing, then the end), we
    %% *continue* if we saw less closing parens than opening parens.
    Next =
        case Current =:= $) andalso SizeB < Size of
            true ->
                gfm_autolink_literal_path_at_punctuation;
            false ->
                gfm_autolink_literal_path_after
        end,
    Tokenizer2 = markdown_tokenizer:check(
        Tokenizer1, markdown_state:next(Next), markdown_state:next(gfm_autolink_literal_path_at_punctuation)
    ),
    State = markdown_state:retry(gfm_autolink_literal_trail),
    {Tokenizer2, State};
path_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Source: <https://github.com/github/cmark-gfm/blob/ef1cfcb/extensions/autolink.c#L12>.
    case Current =:= none orelse markdown_util_char:kind_after_index(Bytes, Index) =:= whitespace of
        true ->
            State = markdown_state:retry(gfm_autolink_literal_path_after),
            {Tokenizer1, State};
        false ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(gfm_autolink_literal_path_inside),
            {Tokenizer2, State}
    end.

-doc """
In path, at potential trailing punctuation, that was not trailing.

```markdown
> | https://example.com/a"b
                         ^
```
""".
-spec path_at_punctuation(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
path_at_punctuation(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $)}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size_b = SizeB1}
    }
) ->
    %% Count closing parens.
    SizeB2 = SizeB1 + 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = SizeB2},
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}),
    State = markdown_state:next(gfm_autolink_literal_path_inside),
    {Tokenizer2, State};
path_at_punctuation(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_path_inside),
    {Tokenizer2, State}.

-doc """
At end of path, reset parens.

```markdown
> | https://example.com/asd(qwe).
                                ^
```
""".
-spec path_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
path_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0, size_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:ok(),
    {Tokenizer2, State}.

-doc """
In trail of domain or path.

```markdown
> | https://example.com").
                       ^
```
""".
-spec trail(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
trail(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $! orelse Current =:= $" orelse Current =:= $' orelse Current =:= $) orelse Current =:= $* orelse
        Current =:= $, orelse Current =:= $. orelse Current =:= $: orelse Current =:= $; orelse Current =:= $? orelse
        Current =:= $_ orelse Current =:= $~
->
    %% Regular trailing punctuation.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_trail),
    {Tokenizer2, State};
trail(Tokenizer1 = #markdown_tokenizer{current = {some, $&}}) ->
    %% `&` followed by one or more alphabeticals and then a `;`, is
    %% as a whole considered as trailing punctuation.
    %% In all other cases, it is considered as continuation of the URL.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_trail_char_ref_start),
    {Tokenizer2, State};
trail(Tokenizer1 = #markdown_tokenizer{current = {some, $<}}) ->
    %% `<` is an end.
    State = markdown_state:ok(),
    {Tokenizer1, State};
trail(Tokenizer1 = #markdown_tokenizer{current = {some, $]}}) ->
    %% Needed because we allow literals after `[`, as we fix:
    %% <https://github.com/github/cmark-gfm/issues/278>.
    %% Check that it is not followed by `(` or `[`.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_trail_bracket_after),
    {Tokenizer2, State};
trail(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{bytes = Bytes}, point = #markdown_point{offset = Index}
    }
) ->
    %% Whitespace is the end of the URL, anything else is continuation.
    case markdown_util_char:kind_after_index(Bytes, Index) of
        whitespace ->
            State = markdown_state:ok(),
            {Tokenizer1, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In trail, after `]`.

> 👉 **Note**: this deviates from `cmark-gfm` to fix a bug.
> See end of <https://github.com/github/cmark-gfm/issues/278> for more.

```markdown
> | https://example.com](
                        ^
```
""".
-spec trail_bracket_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
trail_bracket_after(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =:= none orelse Current =:= {some, $\t} orelse Current =:= {some, $\n} orelse Current =:= {some, $\s} orelse
        Current =:= {some, $(} orelse Current =:= {some, $[}
->
    %% Whitespace or something that could start a resource or reference is the end.
    %% Switch back to trail otherwise.
    State = markdown_state:ok(),
    {Tokenizer1, State};
trail_bracket_after(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:retry(gfm_autolink_literal_trail),
    {Tokenizer1, State}.

-doc """
In character-reference like trail, after `&`.

```markdown
> | https://example.com&amp;).
                        ^
```
""".
-spec trail_char_ref_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
trail_char_ref_start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z)
->
    State = markdown_state:retry(gfm_autolink_literal_trail_char_ref_inside),
    {Tokenizer1, State};
trail_char_ref_start(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer1, State}.

-doc """
In character-reference like trail.

```markdown
> | https://example.com&amp;).
                        ^
```
""".
-spec trail_char_ref_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
trail_char_ref_inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z)
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_trail_char_ref_inside),
    {Tokenizer2, State};
trail_char_ref_inside(Tokenizer1 = #markdown_tokenizer{current = {some, $;}}) ->
    %% Switch back to trail if this is well-formed.
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_autolink_literal_trail),
    {Tokenizer2, State};
trail_char_ref_inside(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer1, State}.

-doc """
Resolve: postprocess text to find email autolink literals.
""".
-spec resolve(Tokenizer) -> Tokenizer when Tokenizer :: markdown_tokenizer:t().
resolve(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:map_consume(Tokenizer1),
    Events2 = Tokenizer2#markdown_tokenizer.events,
    EventsIterator = markdown_vec:iterator(Events2),
    Tokenizer3 = resolve_loop(EventsIterator, Tokenizer2, 0),
    Tokenizer3.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(EventsIterator, Tokenizer, Links) -> Tokenizer when
    EventsIterator :: markdown_vec:iterator(Event),
    Event :: markdown_event:t(),
    Tokenizer :: markdown_tokenizer:t(),
    Links :: non_neg_integer().
resolve_loop(EventsIterator1, Tokenizer1, Links1) ->
    case markdown_vec:next(EventsIterator1) of
        none ->
            Tokenizer1;
        {_Index, #markdown_event{kind = enter, name = link}, EventsIterator2} ->
            Links2 = Links1 + 1,
            resolve_loop(EventsIterator2, Tokenizer1, Links2);
        {Index, #markdown_event{kind = exit, name = data}, EventsIterator2} when Links1 =:= 0 ->
            Tokenizer2 = resolve_data(Tokenizer1, Index),
            resolve_loop(EventsIterator2, Tokenizer2, Links1);
        {_Index, #markdown_event{kind = exit, name = link}, EventsIterator2} ->
            Links2 = Links1 - 1,
            resolve_loop(EventsIterator2, Tokenizer1, Links2);
        {_Index, _Event, EventsIterator2} ->
            resolve_loop(EventsIterator2, Tokenizer1, Links1)
    end.

%% @private
-spec resolve_data(Tokenizer, Index) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(), Index :: markdown_vec:index().
resolve_data(
    Tokenizer1 = #markdown_tokenizer{events = Events, parse_state = #markdown_parse_state{bytes = Bytes}}, Index
) ->
    Position = markdown_position:from_exit_event(Events, Index),
    Slice = markdown_slice:from_position(Bytes, Position),
    SliceBytes = markdown_slice:as_binary(Slice),
    EnterEvent = markdown_vec:get(Events, Index - 1),
    Point1 = EnterEvent#markdown_event.point,
    StartIndex = Point1#markdown_point.offset,
    Replace = resolve_data_loop(SliceBytes, 0, Point1, 0, Bytes, StartIndex, markdown_vec:new()),
    Tokenizer2 =
        case not markdown_vec:is_empty(Replace) of
            true ->
                %% If there were links.
                markdown_tokenizer:map_add(Tokenizer1, Index - 1, 2, Replace);
            false ->
                Tokenizer1
        end,
    Tokenizer2.

%% @private
-spec resolve_data_loop(SliceBytes, ByteIndex, Point, Min, Bytes, StartIndex, Replace) -> Replace when
    SliceBytes :: binary(),
    ByteIndex :: non_neg_integer(),
    Point :: markdown_point:t(),
    Min :: non_neg_integer(),
    Bytes :: binary(),
    StartIndex :: non_neg_integer(),
    Replace :: markdown_vec:t(Event),
    Event :: markdown_event:t().
resolve_data_loop(SliceBytes, ByteIndex, Point1, Min1, Bytes, StartIndex, Replace1) when
    ByteIndex < byte_size(SliceBytes)
->
    Byte = binary:at(SliceBytes, ByteIndex),
    case Byte of
        $@ ->
            Range1 = {0, 0, gfm_autolink_literal_email},
            Range2 =
                case peek_bytes_atext(SliceBytes, Min1, ByteIndex) of
                    none ->
                        Range1;
                    {some, Start1} ->
                        {Start2, Kind1} = peek_protocol(SliceBytes, Min1, Start1),
                        case peek_bytes_email_domain(SliceBytes, ByteIndex + 1, Kind1 =:= gfm_autolink_literal_xmpp) of
                            none ->
                                Range1;
                            {some, End} ->
                                %% Note: normally we’d truncate trailing
                                %% punctuation from the link.
                                %% However, email autolink literals cannot
                                %% contain any of those markers, except for
                                %% `.`, but that can only occur if it isn’t
                                %% trailing.
                                %% So we can ignore truncating while
                                %% postprocessing!
                                {Start2, End, Kind1}
                        end
                end,
            {_RangeStart, RangeEnd, _RangeName} = Range2,
            case RangeEnd =/= 0 of
                true ->
                    {RangeStart, RangeEnd, RangeName} = Range2,
                    {Replace2, Point2} =
                        case Min1 =/= RangeStart of
                            true ->
                                %% If there is something between the last link
                                %% (or `min`) and this link.
                                Replace1_1 = markdown_vec:push(Replace1, #markdown_event{
                                    kind = enter, name = data, point = Point1, link = none
                                }),
                                Point1_1 = markdown_point:shift_to(Point1, Bytes, StartIndex + RangeStart),
                                Replace1_2 = markdown_vec:push(Replace1_1, #markdown_event{
                                    kind = exit, name = data, point = Point1_1, link = none
                                }),
                                {Replace1_2, Point1_1};
                            false ->
                                {Replace1, Point1}
                        end,
                    Replace3 = markdown_vec:push(Replace2, #markdown_event{
                        kind = enter, name = RangeName, point = Point2, link = none
                    }),
                    Point3 = markdown_point:shift_to(Point2, Bytes, StartIndex + RangeEnd),
                    Replace4 = markdown_vec:push(Replace3, #markdown_event{
                        kind = exit, name = RangeName, point = Point3, link = none
                    }),
                    resolve_data_loop(SliceBytes, RangeEnd, Point3, RangeEnd, Bytes, StartIndex, Replace4);
                false ->
                    resolve_data_loop(SliceBytes, ByteIndex + 1, Point1, Min1, Bytes, StartIndex, Replace1)
            end;
        _ ->
            resolve_data_loop(SliceBytes, ByteIndex + 1, Point1, Min1, Bytes, StartIndex, Replace1)
    end;
resolve_data_loop(SliceBytes, _ByteIndex, Point1, Min1, Bytes, StartIndex, Replace1) ->
    Replace2 =
        case Min1 =/= 0 andalso Min1 < byte_size(SliceBytes) of
            true ->
                Replace1_1 = markdown_vec:push(Replace1, #markdown_event{
                    kind = enter, name = data, point = Point1, link = none
                }),
                Point2 = markdown_point:shift_to(Point1, Bytes, StartIndex + byte_size(SliceBytes)),
                Replace1_2 = markdown_vec:push(Replace1_1, #markdown_event{
                    kind = exit, name = data, point = Point2, link = none
                }),
                Replace1_2;
            false ->
                Replace1
        end,
    Replace2.

%% @private
-doc """
Move back past atext.

Moving back is only used when post processing text: so for the email address
algorithm.

```markdown
> | a contact@example.org b
             ^-- from
      ^-- to
```
""".
-spec peek_bytes_atext(Bytes, Min, End) -> none | {some, Index} when
    Bytes :: binary(), Min :: non_neg_integer(), End :: non_neg_integer(), Index :: non_neg_integer().
peek_bytes_atext(Bytes, Min, End) ->
    Index = peek_bytes_atext_loop(Bytes, Min, End),
    case Index =:= End orelse (Index > Min andalso binary:at(Bytes, Index - 1) =:= $/) of
        true ->
            none;
        false ->
            {some, Index}
    end.

%% @private
-spec peek_bytes_atext_loop(Bytes, Min, Index) -> Index when
    Bytes :: binary(), Min :: non_neg_integer(), Index :: non_neg_integer().
peek_bytes_atext_loop(Bytes, Min, Index) when Index > Min ->
    Byte = binary:at(Bytes, Index - 1),
    case Byte of
        $+ -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        $- -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        $. -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        B when B >= $0 andalso B =< $9 -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        B when B >= $A andalso B =< $Z -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        $_ -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        B when B >= $a andalso B =< $z -> peek_bytes_atext_loop(Bytes, Min, Index - 1);
        _ -> Index
    end;
peek_bytes_atext_loop(_Bytes, _Min, Index) ->
    Index.

%% @private
-doc """
Move back past a `mailto:` or `xmpp:` protocol.

Moving back is only used when post processing text: so for the email address
algorithm.

```markdown
> | a mailto:contact@example.org b
             ^-- from
      ^-- to
```
""".
-spec peek_protocol(Bytes, Min, End) -> {Index, Name} when
    Bytes :: binary(),
    Min :: non_neg_integer(),
    End :: non_neg_integer(),
    Index :: non_neg_integer(),
    Name :: markdown_token:name().
peek_protocol(Bytes, Min, End) when End > Min ->
    case binary:at(Bytes, End - 1) of
        $: ->
            Index1 = End - 1,
            Index2 = peek_protocol_loop(Bytes, Min, Index1),
            Slice = markdown_slice:from_indices(Bytes, Index2, End - 1),
            SliceStr = markdown_slice:as_string(Slice),
            Name = string:lowercase(SliceStr),
            case Name of
                "xmpp" ->
                    {Index2, gfm_autolink_literal_xmpp};
                "mailto" ->
                    {Index2, gfm_autolink_literal_mailto};
                _ ->
                    {End, gfm_autolink_literal_email}
            end;
        _ ->
            {End, gfm_autolink_literal_email}
    end;
peek_protocol(_Bytes, _Min, End) ->
    {End, gfm_autolink_literal_email}.

%% @private
-spec peek_protocol_loop(Bytes, Min, Index) -> Index when
    Bytes :: binary(), Min :: non_neg_integer(), Index :: non_neg_integer().
peek_protocol_loop(Bytes, Min, Index) when Index > Min ->
    Byte = binary:at(Bytes, Index - 1),
    case Byte of
        B when (B >= $0 andalso B =< $9) orelse (B >= $A andalso B =< $Z) orelse (B >= $a andalso B =< $z) ->
            peek_protocol_loop(Bytes, Min, Index - 1);
        _ ->
            Index
    end;
peek_protocol_loop(_Bytes, _Min, Index) ->
    Index.

%% @private
-doc """
Move past email domain.

Peeking like this only used when post processing text: so for the email
address algorithm.

```markdown
> | a contact@example.org b
              ^-- from
                        ^-- to
```
""".
-spec peek_bytes_email_domain(Bytes, Start, Xmpp) -> none | {some, Index} when
    Bytes :: binary(), Start :: non_neg_integer(), Xmpp :: boolean(), Index :: non_neg_integer().
peek_bytes_email_domain(Bytes, Start, Xmpp) ->
    {Index, Dot} = peek_bytes_email_domain_loop(Bytes, Start, Xmpp, false),
    case Index > Start andalso Dot of
        true ->
            LastByte = binary:at(Bytes, Index - 1),
            case LastByte of
                $. -> {some, Index};
                B when (B >= $A andalso B =< $Z) orelse (B >= $a andalso B =< $z) -> {some, Index};
                _ -> none
            end;
        false ->
            none
    end.

%% @private
-spec peek_bytes_email_domain_loop(Bytes, Index, Xmpp, Dot) -> {Index, Dot} when
    Bytes :: binary(), Index :: non_neg_integer(), Xmpp :: boolean(), Dot :: boolean().
peek_bytes_email_domain_loop(Bytes, Index, Xmpp, Dot) when Index < byte_size(Bytes) ->
    Byte = binary:at(Bytes, Index),
    case Byte of
        $- ->
            peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        B when B >= $0 andalso B =< $9 -> peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        B when B >= $A andalso B =< $Z -> peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        $_ ->
            peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        B when B >= $a andalso B =< $z -> peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        $/ when Xmpp -> peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, Dot);
        $. when Index + 1 < byte_size(Bytes) ->
            NextByte = binary:at(Bytes, Index + 1),
            case NextByte of
                B when (B >= $0 andalso B =< $9) orelse (B >= $A andalso B =< $Z) orelse (B >= $a andalso B =< $z) ->
                    peek_bytes_email_domain_loop(Bytes, Index + 1, Xmpp, true);
                _ ->
                    {Index, Dot}
            end;
        _ ->
            {Index, Dot}
    end;
peek_bytes_email_domain_loop(_Bytes, Index, _Xmpp, Dot) ->
    {Index, Dot}.
