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
-module(markdown_construct_label_end).
-moduledoc """
Label end occurs in the [text][] content type.

## Grammar

Label end forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
label_end ::= ']' [resource | reference_full | reference_collapsed]

resource ::= '(' [space_or_tab_eol] destination [space_or_tab_eol title] [space_or_tab_eol] ')'
reference_full ::= '[' label ']'
reference_collapsed ::= '[' ']'

; See the `destination`, `title`, and `label` constructs for the BNF of
; those parts.
```

See [`destination`][destination], [`label`][label], and [`title`][title]
for grammar, notes, and recommendations on each part.

In the case of a resource, the destination and title are given directly
with the label end.
In the case of a reference, this information is provided by a matched
[definition][].
Full references (`[x][y]`) match to definitions through their explicit,
second, label (`y`).
Collapsed references (`[x][]`) and shortcut references (`[x]`) match by
interpreting the text provided between the first, implicit, label (`x`).
To match, the effective label of the reference must be equal to the label
of the definition after normalizing with
[`normalize_identifier`][].

Importantly, while the label of a full reference *can* include [string][]
content, and in case of collapsed and shortcut references even [text][]
content, that content is not considered when matching.
To illustrate, neither label matches the definition:

```markdown
[a&b]: https://example.com

[x][a&amp;b], [a\&b][]
```

When the resource or reference matches, the destination forms the `href`
attribute in case of a [label start (link)][label_start_link], and an
`src` attribute in case of a [label start (image)][label_start_image].
The title is formed, optionally, on either `<a>` or `<img>`.
When matched with a [gfm label start (footnote)][gfm_label_start_footnote],
no reference or resource can follow the label end.

For info on how to encode characters in URLs, see
[`destination`][destination].
For info on how characters are encoded as `href` on `<a>` or `src` on
`<img>` when compiling, see
[`sanitize_uri`][sanitize_uri].

In case of a matched [gfm label start (footnote)][gfm_label_start_footnote],
a counter is injected.
In case of a matched [label start (link)][label_start_link], the interpreted
content between it and the label end, is placed between the opening and
closing tags.
In case of a matched [label start (image)][label_start_image], the text is
also interpreted, but used *without* the resulting tags:

```markdown
[a *b* c](#)

![a *b* c](#)
```

Yields:

```html
<p><a href="#">a <em>b</em> c</a></p>
<p><img src="#" alt="a b c" /></p>
```

It is possible to use images in links.
It’s somewhat possible to have links in images (the text will be used, not
the HTML, see above).
But it’s not possible to use links (or footnotes, which result in links)
in links.
The “deepest” link (or footnote) wins.
To illustrate:

```markdown
a [b [c](#) d](#) e
```

Yields:

```html
<p>a [b <a href="#">c</a> d](#) e</p>
```

This limitation is imposed because links in links is invalid according to
HTML.
Technically though, in markdown it is still possible to construct them by
using an [autolink][] in a link.
You definitely should not do that.

## HTML

Label end does not, on its own, relate to anything in HTML.
When matched with a [label start (link)][label_start_link], they together
relate to the `<a>` element in HTML.
See [*§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.
It can also match with [label start (image)][label_start_image], in which
case they form an `<img>` element.
See [*§ 4.8.3 The `img` element*][html_img] in the HTML spec for more info.
It can also match with [gfm label start (footnote)][gfm_label_start_footnote],
in which case they form `<sup>` and `<a>` elements in HTML.
See [*§ 4.5.19 The `sub` and `sup` elements*][html_sup] and
[*§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.

## Recommendation

It is recommended to use labels for links instead of [autolinks][autolink].
Labels allow more characters in URLs, and allow relative URLs and `www.`
URLs.
They also allow for descriptive text to explain the URL in prose.

In footnotes, it’s recommended to use words instead of numbers (or letters
or anything with an order) as calls.
That makes it easier to reuse and reorder footnotes.

## Tokens

*   [`Data`][Name::Data]
*   [`GfmFootnoteCall`][Name::GfmFootnoteCall]
*   [`Image`][Name::Image]
*   [`Label`][Name::Label]
*   [`LabelEnd`][Name::LabelEnd]
*   [`LabelMarker`][Name::LabelMarker]
*   [`LabelText`][Name::LabelText]
*   [`LineEnding`][Name::LineEnding]
*   [`Link`][Name::Link]
*   [`Reference`][Name::Reference]
*   [`ReferenceMarker`][Name::ReferenceMarker]
*   [`ReferenceString`][Name::ReferenceString]
*   [`Resource`][Name::Resource]
*   [`ResourceDestination`][Name::ResourceDestination]
*   [`ResourceDestinationLiteral`][Name::ResourceDestinationLiteral]
*   [`ResourceDestinationLiteralMarker`][Name::ResourceDestinationLiteralMarker]
*   [`ResourceDestinationRaw`][Name::ResourceDestinationRaw]
*   [`ResourceDestinationString`][Name::ResourceDestinationString]
*   [`ResourceMarker`][Name::ResourceMarker]
*   [`ResourceTitle`][Name::ResourceTitle]
*   [`ResourceTitleMarker`][Name::ResourceTitleMarker]
*   [`ResourceTitleString`][Name::ResourceTitleString]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`label-end.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/label-end.js)
*   [`micromark-extension-gfm-task-list-item`](https://github.com/micromark/micromark-extension-gfm-footnote)
*   [*§ 4.7 Link reference definitions* in `CommonMark`](https://spec.commonmark.org/0.31/#link-reference-definitions)
*   [*§ 6.3 Links* in `CommonMark`](https://spec.commonmark.org/0.31/#links)
*   [*§ 6.4 Images* in `CommonMark`](https://spec.commonmark.org/0.31/#images)

> 👉 **Note**: Footnotes are not specified in GFM yet.
> See [`github/cmark-gfm#270`](https://github.com/github/cmark-gfm/issues/270)
> for the related issue.

[string]: crate::construct::string
[text]: crate::construct::text
[destination]: crate::construct::partial_destination
[title]: crate::construct::partial_title
[label]: crate::construct::partial_label
[label_start_image]: crate::construct::label_start_image
[label_start_link]: crate::construct::label_start_link
[gfm_label_start_footnote]: crate::construct::gfm_label_start_footnote
[definition]: crate::construct::definition
[autolink]: crate::construct::autolink
[sanitize_uri]: crate::util::sanitize_uri::sanitize
[normalize_identifier]: crate::util::normalize_identifier::normalize_identifier
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
[html_img]: https://html.spec.whatwg.org/multipage/embedded-content.html#the-img-element
[html_sup]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-sub-and-sup-elements
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    'after'/1,
    reference_not_full/1,
    ok/1,
    nok/1,
    resource_start/1,
    resource_before/1,
    resource_before_whitespace/1,
    resource_open/1,
    resource_destination_after/1,
    resource_destination_missing/1,
    resource_between/1,
    resource_title_after/1,
    resource_end/1,
    reference_full/1,
    reference_full_after/1,
    reference_full_missing/1,
    reference_collapsed/1,
    reference_collapsed_open/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of label end.

```markdown
> | [a](b) c
      ^
> | [a][b] c
      ^
> | [a][] b
      ^
> | [a] b
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $]},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{label_end = true}}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{label_starts = LabelStarts},
        events = Events
    }
) ->
    %% If there is an okay opening:
    case markdown_vec:size(LabelStarts) of
        0 ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        _ ->
            LabelStart = markdown_vec:last(LabelStarts),

            TokenizeState2 = TokenizeState1#markdown_tokenize_state{'end' = markdown_vec:size(Events)},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

            %% If the corresponding label (link) start is marked as inactive,
            %% it means we'd be wrapping a link, like this:
            %%
            %% ```markdown
            %% > | a [b [c](d) e](f) g.
            %%                  ^
            %% ```
            %%
            %% We can't have that, so it's just balanced brackets.
            case LabelStart#markdown_label_start.inactive of
                true ->
                    State = markdown_state:retry(label_end_nok),
                    {Tokenizer2, State};
                false ->
                    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, label_end),
                    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, label_marker),
                    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
                    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, label_marker),
                    Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, label_end),

                    State = markdown_state:next(label_end_after),
                    {Tokenizer7, State}
            end
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `]`.

```markdown
> | [a](b) c
      ^
> | [a][b] c
      ^
> | [a][] b
      ^
> | [a] b
      ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{label_starts = LabelStarts1, 'end' = End1},
        events = Events,
        parse_state = ParseState
    }
) ->
    StartIndex = markdown_vec:size(LabelStarts1) - 1,
    Start1 = markdown_vec:get(LabelStarts1, StartIndex),

    IndicesStartEvent = markdown_vec:get(Events, Start1#markdown_label_start.start#markdown_indices.'end'),
    IndicesEndEvent = markdown_vec:get(Events, End1),

    Indices = markdown_indices:new(
        IndicesStartEvent#markdown_event.point#markdown_point.offset,
        IndicesEndEvent#markdown_event.point#markdown_point.offset
    ),

    %% We don’t care about virtual spaces, so `indices` and `as_str` are fine.
    Slice = markdown_slice:from_indices(ParseState#markdown_parse_state.bytes, Indices),
    Id1 = markdown_util_normalize_identifier:normalize_identifier(markdown_slice:as_binary(Slice)),

    %% See if this matches a footnote definition.
    case Start1#markdown_label_start.kind of
        gfm_footnote ->
            case markdown_vec:contains(ParseState#markdown_parse_state.gfm_footnote_definitions, Id1) of
                true ->
                    State = markdown_state:retry(label_end_ok),
                    {Tokenizer1, State};
                false ->
                    %% Nope, this might be a normal link?
                    Start2 = Start1#markdown_label_start{kind = gfm_undefined_footnote},
                    LabelStarts2 = markdown_vec:set(LabelStarts1, StartIndex, Start2),
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{label_starts = LabelStarts2},
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    Id2 = <<"^", Id1/bytes>>,
                    after__continue(Tokenizer2, Id2)
            end;
        _ ->
            after__continue(Tokenizer1, Id1)
    end.

-doc """
After `]`, at `[`, but not at a full reference.

> 👉 **Note**: we only get here if the label is defined.

```markdown
> | [a][] b
       ^
> | [a] b
       ^
```
""".
-spec reference_not_full(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_not_full(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(label_end_ok), markdown_state:next(label_end_nok)
    ),
    State = markdown_state:retry(label_end_reference_collapsed),
    {Tokenizer2, State}.

-doc """
Done, we found something.

```markdown
> | [a](b) c
          ^
> | [a][b] c
          ^
> | [a][] b
         ^
> | [a] b
       ^
```
""".
-spec ok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
ok(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{label_starts = LabelStarts1, labels = Labels1, 'end' = End},
        events = Events
    }
) ->
    %% Remove the start.
    {LabelStarts2, LabelStart} = markdown_vec:pop(LabelStarts1),
    %% If this is a link or footnote, we need to mark earlier link starts as no
    %% longer viable for use (as they would otherwise contain a link).
    %% These link starts are still looking for balanced closing brackets, so
    %% we can't remove them, but we can mark them.
    LabelStarts3 =
        case LabelStart#markdown_label_start.kind of
            image ->
                LabelStarts2;
            _ ->
                markdown_vec:map(LabelStarts2, fun(_, LabelStartEntry) ->
                    case LabelStartEntry of
                        #markdown_label_start{kind = image} ->
                            LabelStartEntry;
                        _ ->
                            LabelStartEntry#markdown_label_start{inactive = true}
                    end
                end)
        end,
    Label = markdown_label:new(
        LabelStart#markdown_label_start.kind,
        LabelStart#markdown_label_start.start,
        markdown_indices:new(End, markdown_vec:size(Events) - 1)
    ),
    Labels2 = markdown_vec:push(Labels1, Label),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        label_starts = LabelStarts3,
        labels = Labels2,
        'end' = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:register_resolver_before(Tokenizer2, label),
    State = markdown_state:ok(),
    {Tokenizer3, State}.

-doc """
Done, it's nothing.

There was an okay opening, but we didn't match anything.

```markdown
> | [a](b c
       ^
> | [a][b c
       ^
> | [a] b
       ^
```
""".
-spec nok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
nok(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                label_starts = LabelStarts1, label_starts_loose = LabelStartsLoose1
            }
    }
) ->
    {LabelStarts2, Start} = markdown_vec:pop(LabelStarts1),
    LabelStartsLoose2 = markdown_vec:push(LabelStartsLoose1, Start),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        label_starts = LabelStarts2,
        label_starts_loose = LabelStartsLoose2,
        'end' = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
At a resource.

```markdown
> | [a](b) c
       ^
```
""".
-spec resource_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_start(Tokenizer1 = #markdown_tokenizer{current = {some, $(}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, resource),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, resource_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, resource_marker),
    State = markdown_state:next(label_end_resource_before),
    {Tokenizer5, State};
resource_start(_Tokenizer) ->
    error("expected `(`").

-doc """
In resource, after `(`, at optional whitespace.

```markdown
> | [a](b) c
        ^
```
""".
-spec resource_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $\t} ->
            resource_before_whitespace(Tokenizer1);
        {some, $\n} ->
            resource_before_whitespace(Tokenizer1);
        {some, $\s} ->
            resource_before_whitespace(Tokenizer1);
        _ ->
            State = markdown_state:retry(label_end_resource_open),
            {Tokenizer1, State}
    end.

%% @private
-spec resource_before_whitespace(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_before_whitespace(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(label_end_resource_open), markdown_state:next(label_end_resource_open)
    ),
    {Tokenizer3, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabEolState),
    {Tokenizer3, State}.

-doc """
In resource, after optional whitespace, at `)` or a destination.

```markdown
> | [a](b) c
        ^
```
""".
-spec resource_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_open(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    case Current of
        {some, $)} ->
            State = markdown_state:retry(label_end_resource_end),
            {Tokenizer1, State};
        _ ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                token_1 = resource_destination,
                token_2 = resource_destination_literal,
                token_3 = resource_destination_literal_marker,
                token_4 = resource_destination_raw,
                token_5 = resource_destination_string,
                size_b = ?RESOURCE_DESTINATION_BALANCE_MAX
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(label_end_resource_destination_after),
                markdown_state:next(label_end_resource_destination_missing)
            ),
            State = markdown_state:retry(destination_start),
            {Tokenizer3, State}
    end.

-doc """
In resource, after destination, at optional whitespace.

```markdown
> | [a](b) c
         ^
```
""".
-spec resource_destination_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_destination_after(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data,
        token_4 = data,
        token_5 = data,
        size_b = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case Current of
        {some, C} when C =:= $\t orelse C =:= $\n orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(label_end_resource_between),
                markdown_state:next(label_end_resource_end)
            ),
            {Tokenizer4, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol(Tokenizer3),
            State = markdown_state:retry(SpaceOrTabEolState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(label_end_resource_end),
            {Tokenizer2, State}
    end.

-doc """
At invalid destination.

```markdown
> | [a](<<) b
        ^
```
""".
-spec resource_destination_missing(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_destination_missing(
    Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data,
        token_4 = data,
        token_5 = data,
        size_b = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In resource, after destination and whitespace, at `(` or title.

```markdown
> | [a](b ) c
          ^
```
""".
-spec resource_between(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_between(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    case Current of
        {some, C} when C =:= $\" orelse C =:= $' orelse C =:= $( ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                token_1 = resource_title,
                token_2 = resource_title_marker,
                token_3 = resource_title_string
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(label_end_resource_title_after),
                markdown_state:nok()
            ),
            State = markdown_state:retry(title_start),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(label_end_resource_end),
            {Tokenizer1, State}
    end.

-doc """
In resource, after title, at optional whitespace.

```markdown
> | [a](b "c") d
             ^
```
""".
-spec resource_title_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_title_after(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case Current of
        {some, C} when C =:= $\t orelse C =:= $\n orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(label_end_resource_end),
                markdown_state:next(label_end_resource_end)
            ),
            {Tokenizer4, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol(Tokenizer3),
            State = markdown_state:retry(SpaceOrTabEolState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(label_end_resource_end),
            {Tokenizer2, State}
    end.

-doc """
In resource, at `)`.

```markdown
> | [a](b) d
         ^
```
""".
-spec resource_end(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
resource_end(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $)} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, resource_marker),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, resource_marker),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, resource),
            State = markdown_state:ok(),
            {Tokenizer5, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In reference (full), at `[`.
```markdown
> | [a][b] d
       ^
```
""".
-spec reference_full(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_full(
    Tokenizer1 = #markdown_tokenizer{current = {some, $[}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = reference, token_2 = reference_marker, token_3 = reference_string
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    OkState = markdown_state:next(label_end_reference_full_after),
    NokState = markdown_state:next(label_end_reference_full_missing),
    Tokenizer3 = markdown_tokenizer:attempt(Tokenizer2, OkState, NokState),
    State = markdown_state:retry(label_start),
    {Tokenizer3, State}.

-doc """
In reference (full), after `]`.
```markdown
> | [a][b] d
         ^
```
""".
-spec reference_full_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_full_after(
    Tokenizer1 = #markdown_tokenizer{
        events = Events,
        parse_state = #markdown_parse_state{bytes = Bytes, definitions = Definitions},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data, token_2 = data, token_3 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    EventsSize = markdown_vec:size(Events),
    SkipIndex = markdown_util_skip:to_back(Events, EventsSize - 1, [reference_string]),
    Position = markdown_position:from_exit_event(Events, SkipIndex),
    Slice = markdown_slice:from_position(Bytes, Position),
    %% We don’t care about virtual spaces, so `as_binary` is fine.
    SliceBytes = markdown_slice:as_binary(Slice),
    NormalizedIdentifier = markdown_util_normalize_identifier:normalize_identifier(SliceBytes),
    State =
        case markdown_vec:contains(Definitions, NormalizedIdentifier) of
            true ->
                markdown_state:ok();
            false ->
                markdown_state:nok()
        end,
    {Tokenizer2, State}.

-doc """
In reference (full) that was missing.

```markdown
> | [a][b d
       ^
```
""".
-spec reference_full_missing(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_full_missing(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data, token_2 = data, token_3 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
In reference (collapsed), at `[`.

> 👉 **Note**: we only get here if the label is defined.

```markdown
> | [a][] d
       ^
```
""".
-spec reference_collapsed(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_collapsed(Tokenizer1 = #markdown_tokenizer{current = {some, $[}}) ->
    %% We only attempt a collapsed label if there's a `[`.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, reference),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, reference_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, reference_marker),
    State = markdown_state:next(label_end_reference_collapsed_open),
    {Tokenizer5, State}.

-doc """
In reference (collapsed), at `]`.

> 👉 **Note**: we only get here if the label is defined.

```markdown
> | [a][] d
        ^
```
""".
-spec reference_collapsed_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
reference_collapsed_open(Tokenizer1 = #markdown_tokenizer{current = {some, $]}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, reference_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, reference_marker),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, reference),
    State = markdown_state:ok(),
    {Tokenizer5, State};
reference_collapsed_open(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
Resolve images, links, and footnotes.

This turns matching label starts and label ends into links, images, and
footnotes, and turns unmatched label starts back into data.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{labels = Labels1}}) ->
    %% Inject labels.
    {Labels2, Labels} = markdown_vec:split_off(Labels1, 0),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{labels = Labels2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = inject_labels(Tokenizer2, Labels),
    %% Handle loose starts.
    #markdown_tokenizer{tokenize_state = TokenizeState3 = #markdown_tokenize_state{label_starts = LabelStarts1}} =
        Tokenizer3,
    {LabelStarts2, LabelStarts} = markdown_vec:split_off(LabelStarts1, 0),
    TokenizeState4 = TokenizeState3#markdown_tokenize_state{label_starts = LabelStarts2},
    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
    Tokenizer5 = mark_as_data(Tokenizer4, LabelStarts),
    #markdown_tokenizer{
        tokenize_state = TokenizeState5 = #markdown_tokenize_state{label_starts_loose = LabelStartsLoose1}
    } = Tokenizer5,
    {LabelStartsLoose2, LabelStartsLoose} = markdown_vec:split_off(LabelStartsLoose1, 0),
    TokenizeState6 = TokenizeState5#markdown_tokenize_state{label_starts_loose = LabelStartsLoose2},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState6},
    Tokenizer7 = mark_as_data(Tokenizer6, LabelStartsLoose),
    Tokenizer8 = markdown_tokenizer:map_consume(Tokenizer7),
    {Tokenizer8, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec after__continue(Tokenizer, Id) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), Id :: unicode:unicode_binary(), State :: markdown_state:t().
after__continue(
    Tokenizer1 = #markdown_tokenizer{current = Current, parse_state = #markdown_parse_state{definitions = Definitions}},
    Id
) ->
    Defined = markdown_vec:contains(Definitions, Id),
    case Current of
        %% Resource (`[asd](fgh)`)?
        {some, $(} ->
            OkState = markdown_state:next(label_end_ok),
            NokState =
                case Defined of
                    true -> markdown_state:next(label_end_ok);
                    false -> markdown_state:next(label_end_nok)
                end,
            Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, OkState, NokState),
            State = markdown_state:retry(label_end_resource_start),
            {Tokenizer2, State};
        %% Full (`[asd][fgh]`) or collapsed (`[asd][]`) reference?
        {some, $[} ->
            OkState = markdown_state:next(label_end_ok),
            NokState =
                case Defined of
                    true -> markdown_state:next(label_end_reference_not_full);
                    false -> markdown_state:next(label_end_nok)
                end,
            Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, OkState, NokState),
            State = markdown_state:retry(label_end_reference_full),
            {Tokenizer2, State};
        %% Shortcut (`[asd]`) reference?
        _ ->
            State =
                case Defined of
                    true -> markdown_state:retry(label_end_ok);
                    false -> markdown_state:retry(label_end_nok)
                end,
            {Tokenizer1, State}
    end.

%% @private
-doc """
Inject links/images/footnotes.
""".
-spec inject_labels(Tokenizer, Labels) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    Labels :: markdown_vec:t(Label),
    Label :: markdown_label:t().
inject_labels(Tokenizer, Labels) ->
    %% Add grouping events.
    inject_labels__loop(Tokenizer, Labels, 0).

%% @private
-spec inject_labels__empty_events(Tokenizer, Caret, Label) -> {Tokenizer, Caret} when
    Tokenizer :: markdown_tokenizer:t(),
    Caret :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Label :: markdown_label:t().
inject_labels__empty_events(
    Tokenizer1 = #markdown_tokenizer{}, Caret1 = #markdown_vec{size = Caret1Size}, #markdown_label{
        start = #markdown_indices{'end' = LabelStartEnd}, 'end' = #markdown_indices{start = LabelEndStart}
    }
) when LabelStartEnd =/= LabelEndStart orelse Caret1Size > 0 ->
    #markdown_event{point = LabelStartEndPoint} = markdown_tokenizer:events_get(Tokenizer1, LabelStartEnd),
    #markdown_event{point = LabelEndStartPoint} = markdown_tokenizer:events_get(Tokenizer1, LabelEndStart),
    Tokenizer2 = markdown_tokenizer:map_add_before(Tokenizer1, LabelStartEnd + 1, 0, [
        markdown_event:label_text('enter', LabelStartEndPoint, none)
    ]),
    Tokenizer3 = markdown_tokenizer:map_add(Tokenizer2, LabelEndStart, 0, [
        markdown_event:label_text('exit', LabelEndStartPoint, none)
    ]),
    {Tokenizer3, Caret1};
inject_labels__empty_events(Tokenizer = #markdown_tokenizer{}, Caret = #markdown_vec{}, _Label = #markdown_label{}) ->
    {Tokenizer, Caret}.

%% @private
-spec inject_labels__gfm_footnote(Tokenizer, Caret, Label) -> {Tokenizer, Caret} when
    Tokenizer :: markdown_tokenizer:t(),
    Caret :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Label :: markdown_label:t().
inject_labels__gfm_footnote(
    Tokenizer1 = #markdown_tokenizer{events = Events1, map = EditMap1},
    Caret1 = #markdown_vec{},
    Label = #markdown_label{kind = gfm_undefined_footnote}
) ->
    %% Add caret.
    LabelStartIndex = Label#markdown_label.start#markdown_indices.start,
    LabelEndIndex = Label#markdown_label.start#markdown_indices.'end',
    EnterIndex = LabelEndIndex - 2,
    ExitIndex = LabelEndIndex - 1,
    #markdown_event{point = EnterPoint} = markdown_vec:get(Events1, EnterIndex),
    #markdown_event{point = ExitPoint} = markdown_vec:get(Events1, ExitIndex),
    %% Enter:GfmFootnoteCallMarker.
    Caret2 = markdown_vec:push(Caret1, markdown_event:data('enter', EnterPoint, none)),
    %% Exit:GfmFootnoteCallMarker.
    Caret3 = markdown_vec:push(Caret2, markdown_event:data('exit', ExitPoint, none)),
    %% Change and move label end.
    #markdown_event{point = CaretFirstPoint} = markdown_vec:first(Caret3),
    Events2 = markdown_vec:update(Events1, LabelStartIndex, fun(Event) ->
        Event#markdown_event{name = label_link}
    end),
    Events3 = markdown_vec:update(Events2, LabelEndIndex, fun(Event) ->
        Event#markdown_event{name = label_link, point = CaretFirstPoint}
    end),
    %% Remove the caret.
    %% Enter:GfmFootnoteCallMarker, Exit:GfmFootnoteCallMarker.
    EditMap2 = markdown_edit_map:add(EditMap1, EnterIndex, 2, []),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3, map = EditMap2},
    {Tokenizer2, Caret3};
inject_labels__gfm_footnote(Tokenizer = #markdown_tokenizer{}, Caret = #markdown_vec{}, _Label) ->
    {Tokenizer, Caret}.

%% @private
-spec inject_labels__loop(Tokenizer, Labels, Index) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    Labels :: markdown_vec:t(Label),
    Label :: markdown_label:t(),
    Index :: markdown_vec:index().
inject_labels__loop(Tokenizer1 = #markdown_tokenizer{}, Labels, Index) when
    Index >= 0 andalso Index < ?markdown_vec_size(Labels)
->
    Label = markdown_vec:get(Labels, Index),
    LabelStartStart = Label#markdown_label.start#markdown_indices.start,
    LabelStartEnd = Label#markdown_label.start#markdown_indices.'end',
    LabelEndStart = Label#markdown_label.'end'#markdown_indices.start,
    LabelEndEnd = Label#markdown_label.'end'#markdown_indices.'end',
    GroupName =
        case Label#markdown_label.kind of
            gfm_footnote -> gfm_footnote_call;
            image -> image;
            _ -> link
        end,
    %% If this is a fine link, which starts with a footnote start that did
    %% not match, we need to inject the caret as data.
    Caret1 = markdown_vec:new(),
    {Tokenizer2, Caret2} = inject_labels__gfm_footnote(Tokenizer1, Caret1, Label),
    %% Insert a group enter and label enter.
    #markdown_event{point = GroupEnterPoint} = markdown_tokenizer:events_get(Tokenizer2, LabelStartStart),
    Tokenizer3 = markdown_tokenizer:map_add(Tokenizer2, LabelStartStart, 0, [
        markdown_event:new('enter', GroupName, GroupEnterPoint, none),
        markdown_event:label('enter', GroupEnterPoint, none)
    ]),
    %% Empty events not allowed.
    %% Though: if this was what looked like a footnote, but didn’t match,
    %% it’s a link instead, and we need to inject the `^`.
    {Tokenizer4, Caret3} = inject_labels__empty_events(Tokenizer3, Caret2, Label),
    Tokenizer5 =
        case not markdown_vec:is_empty(Caret3) of
            true ->
                markdown_tokenizer:map_add(Tokenizer4, LabelStartEnd + 1, 0, Caret3);
            false ->
                Tokenizer4
        end,
    %% Insert a label exit.
    Tokenizer6 = markdown_tokenizer:map_add(Tokenizer5, LabelEndStart + 4, 0, [
        markdown_event:label(
            'exit', (markdown_tokenizer:events_get(Tokenizer5, LabelEndStart + 3))#markdown_event.point, none
        )
    ]),
    %% Insert a group exit.
    Tokenizer7 = markdown_tokenizer:map_add(Tokenizer6, LabelEndEnd + 1, 0, [
        markdown_event:new(
            'exit', GroupName, (markdown_tokenizer:events_get(Tokenizer6, LabelEndEnd))#markdown_event.point, none
        )
    ]),
    inject_labels__loop(Tokenizer7, Labels, Index + 1);
inject_labels__loop(Tokenizer, _Labels, _Index) ->
    Tokenizer.

%% @private
-doc """
Remove loose label starts.
""".
-spec mark_as_data(Tokenizer, LabelStarts) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    LabelStarts :: markdown_vec:t(LabelStart),
    LabelStart :: markdown_label_start:t().
mark_as_data(Tokenizer, LabelStarts) ->
    mark_as_data__loop(Tokenizer, LabelStarts, 0).

%% @private
-spec mark_as_data__loop(Tokenizer, LabelStarts, Index) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    LabelStarts :: markdown_vec:t(LabelStart),
    LabelStart :: markdown_label_start:t(),
    Index :: markdown_vec:index().
mark_as_data__loop(Tokenizer1 = #markdown_tokenizer{events = Events, map = EditMap1}, LabelStarts, Index) when
    Index >= 0 andalso Index < ?markdown_vec_size(LabelStarts)
->
    #markdown_label_start{
        start = #markdown_indices{
            start = DataEnterIndex,
            'end' = DataExitIndex
        }
    } = markdown_vec:get(LabelStarts, Index),
    #markdown_event{point = DataEnterPoint} = markdown_vec:get(Events, DataEnterIndex),
    #markdown_event{point = DataExitPoint} = markdown_vec:get(Events, DataExitIndex),
    EditMap2 = markdown_edit_map:add(EditMap1, DataEnterIndex, DataExitIndex - DataEnterIndex + 1, [
        markdown_event:data('enter', DataEnterPoint, none),
        markdown_event:data('exit', DataExitPoint, none)
    ]),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{map = EditMap2},
    mark_as_data__loop(Tokenizer2, LabelStarts, Index + 1);
mark_as_data__loop(Tokenizer = #markdown_tokenizer{}, _LabelStart, _Index) ->
    Tokenizer.
