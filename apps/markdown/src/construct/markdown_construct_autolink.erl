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
-module(markdown_construct_autolink).
-moduledoc """
Autolink occurs in the [text][] content type.

## Grammar

Autolink forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
autolink ::= '<' (url | email) '>'

url ::= protocol *url_byte
protocol ::= ascii_alphabetic 0*31(protocol_byte) ':'
protocol_byte ::= '+' '-' '.' ascii_alphanumeric
url_byte ::= byte - ascii_control - ' '

email ::= 1*ascii_atext '@' email_domain *('.' email_domain)
; Restriction: up to (including) 63 character are allowed in each domain.
email_domain ::= ascii_alphanumeric *(ascii_alphanumeric | '-' ascii_alphanumeric)

ascii_atext ::= ascii_alphanumeric | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
```

The maximum allowed size of a scheme is `31` (inclusive), which is defined
in [`AUTOLINK_SCHEME_SIZE_MAX`][].
The maximum allowed size of a domain is `63` (inclusive), which is defined
in [`AUTOLINK_DOMAIN_SIZE_MAX`][].

The grammar for autolinks is quite strict and prohibits the use of ASCII control
characters or spaces.
To use non-ascii characters and otherwise impossible characters in URLs,
you can use percent encoding:

```markdown
<https://example.com/alpha%20bravo>
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
<https://a👍b%>
```

Yields:

```html
<p><a href="https://a%F0%9F%91%8Db%25">https://a👍b%</a></p>
```

Interestingly, there are a couple of things that are valid autolinks in
markdown but in HTML would be valid tags, such as `<svg:rect>` and
`<xml:lang/>`.
However, because `CommonMark` employs a naïve HTML parsing algorithm, those
are not considered HTML.

While `CommonMark` restricts links from occurring in other links in the
case of labels (see [label end][label_end]), this restriction is not in
place for autolinks inside labels:

```markdown
[<https://example.com>](#)
```

Yields:

```html
<p><a href="#"><a href="https://example.com">https://example.com</a></a></p>
```

The generated output, in this case, is invalid according to HTML.
When a browser sees that markup, it will instead parse it as:

```html
<p><a href="#"></a><a href="https://example.com">https://example.com</a></p>
```

## HTML

Autolinks relate to the `<a>` element in HTML.
See [*§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.
When an email autolink is used (so, without a protocol), the string
`mailto:` is prepended before the email, when generating the `href`
attribute of the hyperlink.

## Recommendation

It is recommended to use labels ([label start link][label_start_link],
[label end][label_end]), either with a resource or a definition
([definition][]), instead of autolinks, as those allow more characters in
URLs, and allow relative URLs and `www.` URLs.
They also allow for descriptive text to explain the URL in prose.

## Tokens

*   [`Autolink`][Name::Autolink]
*   [`AutolinkEmail`][Name::AutolinkEmail]
*   [`AutolinkMarker`][Name::AutolinkMarker]
*   [`AutolinkProtocol`][Name::AutolinkProtocol]

## References

*   [`autolink.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/autolink.js)
*   [*§ 6.4 Autolinks* in `CommonMark`](https://spec.commonmark.org/0.31/#autolinks)

[text]: crate::construct::text
[definition]: crate::construct::definition
[label_start_link]: crate::construct::label_start_link
[label_end]: crate::construct::label_end
[autolink_scheme_size_max]: crate::util::constant::AUTOLINK_SCHEME_SIZE_MAX
[autolink_domain_size_max]: crate::util::constant::AUTOLINK_DOMAIN_SIZE_MAX
[sanitize_uri]: crate::util::sanitize_uri
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    open/1,
    scheme_or_email_atext/1,
    scheme_inside_or_email_atext/1,
    url_inside/1,
    email_atext/1,
    email_at_sign_or_dot/1,
    email_label/1,
    email_value/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of an autolink.

```markdown
> | a<https://example.com>b
     ^
> | a<user@example.com>b
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
            options = #markdown_parse_options{constructs = #markdown_construct_options{autolink = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, autolink),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, autolink_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, autolink_marker),
    Tokenizer6 = markdown_tokenizer:enter(Tokenizer5, autolink_protocol),
    State = markdown_state:next(autolink_open),
    {Tokenizer6, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `<`, at protocol or atext.

```markdown
> | a<https://example.com>b
      ^
> | a<user@example.com>b
      ^
```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) ->
    %% ASCII alphabetic.
    case (Current >= $A andalso Current =< $Z) orelse (Current >= $a andalso Current =< $z) of
        true ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(autolink_scheme_or_email_atext),
            {Tokenizer2, State};
        false ->
            case Current of
                $@ ->
                    State = markdown_state:nok(),
                    {Tokenizer1, State};
                _ ->
                    State = markdown_state:retry(autolink_email_atext),
                    {Tokenizer1, State}
            end
    end;
open(Tokenizer) ->
    State = markdown_state:retry(autolink_email_atext),
    {Tokenizer, State}.

-doc """
At second byte of protocol or atext.

```markdown
> | a<https://example.com>b
       ^
> | a<user@example.com>b
       ^
```
""".
-spec scheme_or_email_atext(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
scheme_or_email_atext(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) ->
    %% ASCII alphanumeric and `+`, `-`, and `.`.
    case
        Current =:= $+ orelse Current =:= $- orelse Current =:= $. orelse
            (Current >= $0 andalso Current =< $9) orelse
            (Current >= $A andalso Current =< $Z) orelse
            (Current >= $a andalso Current =< $z)
    of
        true ->
            %% Count the previous alphabetical from `open` too.
            TokenizeState = Tokenizer1#markdown_tokenizer.tokenize_state,
            Tokenizer2 = Tokenizer1#markdown_tokenizer{
                tokenize_state = TokenizeState#markdown_tokenize_state{size = 1}
            },
            State = markdown_state:retry(autolink_scheme_inside_or_email_atext),
            {Tokenizer2, State};
        false ->
            State = markdown_state:retry(autolink_email_atext),
            {Tokenizer1, State}
    end;
scheme_or_email_atext(Tokenizer) ->
    State = markdown_state:retry(autolink_email_atext),
    {Tokenizer, State}.

-doc """
In ambiguous protocol or atext.

```markdown
> | a<https://example.com>b
       ^
> | a<user@example.com>b
       ^
```
""".
-spec scheme_inside_or_email_atext(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
scheme_inside_or_email_atext(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) ->
    case Current of
        $: ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:next(autolink_url_inside),
            {Tokenizer3, State};
        _ ->
            %% ASCII alphanumeric and `+`, `-`, and `.`.
            case
                (Current =:= $+ orelse Current =:= $- orelse Current =:= $. orelse
                    (Current >= $0 andalso Current =< $9) orelse
                    (Current >= $A andalso Current =< $Z) orelse
                    (Current >= $a andalso Current =< $z)) andalso
                    Size < ?AUTOLINK_SCHEME_SIZE_MAX
            of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
                    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:next(autolink_scheme_inside_or_email_atext),
                    {Tokenizer3, State};
                false ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:retry(autolink_email_atext),
                    {Tokenizer2, State}
            end
    end;
scheme_inside_or_email_atext(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(autolink_email_atext),
    {Tokenizer2, State}.

-doc """
After protocol, in URL.

```markdown
> | a<https://example.com>b
            ^
```
""".
-spec url_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
url_inside(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $>} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, autolink_protocol),
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, autolink_marker),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, autolink_marker),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, autolink),
            State = markdown_state:ok(),
            {Tokenizer6, State};
        %% ASCII control, space, or `<`.
        none ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, Current2} when
            (Current2 >= 16#00 andalso Current2 =< 16#1F) orelse Current2 =:= $\s orelse Current2 =:= $< orelse
                Current2 =:= 16#7F
        ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(autolink_url_inside),
            {Tokenizer2, State}
    end.

-doc """
In email atext.

```markdown
> | a<user.name@example.com>b
             ^
```
""".
-spec email_atext(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
email_atext(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $@} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(autolink_email_at_sign_or_dot),
            {Tokenizer2, State};
        %% ASCII atext.
        %%
        %% atext is an ASCII alphanumeric (see [`is_ascii_alphanumeric`][]), or
        %% a byte in the inclusive ranges U+0023 NUMBER SIGN (`#`) to U+0027
        %% APOSTROPHE (`'`), U+002A ASTERISK (`*`), U+002B PLUS SIGN (`+`),
        %% U+002D DASH (`-`), U+002F SLASH (`/`), U+003D EQUALS TO (`=`),
        %% U+003F QUESTION MARK (`?`), U+005E CARET (`^`) to U+0060 GRAVE
        %% ACCENT (`` ` ``), or U+007B LEFT CURLY BRACE (`{`) to U+007E TILDE
        %% (`~`).
        %%
        %% See:
        %% **\[RFC5322]**:
        %% [Internet Message Format](https://tools.ietf.org/html/rfc5322).
        %% P. Resnick.
        %% IETF.
        %%
        %% [`is_ascii_alphanumeric`]: char::is_ascii_alphanumeric
        {some, Current2} ->
            case
                (Current2 >= $# andalso Current2 =< $') orelse
                    Current2 =:= $* orelse
                    Current2 =:= $+ orelse
                    (Current2 >= $- andalso Current2 =< $9) orelse
                    Current2 =:= $= orelse
                    Current2 =:= $? orelse
                    (Current2 >= $A andalso Current2 =< $Z) orelse
                    (Current2 >= $^ andalso Current2 =< $~)
            of
                true ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(autolink_email_atext),
                    {Tokenizer2, State};
                false ->
                    State = markdown_state:nok(),
                    {Tokenizer1, State}
            end;
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In label, after at-sign or dot.

```markdown
> | a<user.name@example.com>b
                ^       ^
```
""".
-spec email_at_sign_or_dot(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
email_at_sign_or_dot(Tokenizer = #markdown_tokenizer{current = Current}) ->
    %% ASCII alphanumeric.
    case Current of
        {some, Char} when
            (Char >= $0 andalso Char =< $9) orelse
                (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)
        ->
            State = markdown_state:retry(autolink_email_value),
            {Tokenizer, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer, State}
    end.

-doc """
In label, where `.` and `>` are allowed.

```markdown
> | a<user.name@example.com>b
                  ^
```
""".
-spec email_label(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
email_label(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $.} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            #markdown_tokenizer{tokenize_state = TokenizeState2} = Tokenizer2,
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{size = 0},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
            State = markdown_state:next(autolink_email_at_sign_or_dot),
            {Tokenizer3, State};
        {some, $>} ->
            Index = markdown_vec:size(Tokenizer1#markdown_tokenizer.events),
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, autolink_protocol),
            %% Change the event name.
            Tokenizer3 = markdown_tokenizer:update_event(Tokenizer2, Index - 1, fun(EventEntry) ->
                EventEntry#markdown_event{name = autolink_email}
            end),
            Tokenizer4 = markdown_tokenizer:update_event(Tokenizer3, Index, fun(EventEntry) ->
                EventEntry#markdown_event{name = autolink_email}
            end),
            Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, autolink_marker),
            Tokenizer6 = markdown_tokenizer:consume(Tokenizer5),
            Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, autolink_marker),
            Tokenizer8 = markdown_tokenizer:exit(Tokenizer7, autolink),
            #markdown_tokenizer{tokenize_state = TokenizeState8} = Tokenizer8,
            TokenizeState9 = TokenizeState8#markdown_tokenize_state{size = 0},
            Tokenizer9 = Tokenizer8#markdown_tokenizer{tokenize_state = TokenizeState9},
            State = markdown_state:ok(),
            {Tokenizer9, State};
        _ ->
            State = markdown_state:retry(autolink_email_value),
            {Tokenizer1, State}
    end.

-doc """
In label, where `.` and `>` are *not* allowed.

Though, this is also used in `email_label` to parse other values.

```markdown
> | a<user.name@ex-ample.com>b
                   ^
```
""".
-spec email_value(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
email_value(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) ->
    %% ASCII alphanumeric or `-`.
    case Current of
        {some, Char} when
            (Char =:= $- orelse
                (Char >= $0 andalso Char =< $9) orelse
                (Char >= $A andalso Char =< $Z) orelse
                (Char >= $a andalso Char =< $z)) andalso
                Size < ?AUTOLINK_DOMAIN_SIZE_MAX
        ->
            Name =
                case Char of
                    $- ->
                        autolink_email_value;
                    _ ->
                        autolink_email_label
                end,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(Name),
            {Tokenizer3, State};
        _ ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.
