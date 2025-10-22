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
-module(markdown_construct_gfm_footnote_definition).
-moduledoc """
GFM: Footnote definition occurs in the [document][] content type.

## Grammar

Footnote definitions form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: `label` must start with `^` (and not be empty after it).
; See the `label` construct for the BNF of that part.
gfm_footnote_definition_start ::= label ':' *space_or_tab

; Restriction: blank line allowed.
gfm_footnote_definition_cont ::= 4(space_or_tab)
```

Further lines that are not prefixed with `gfm_footnote_definition_cont`
cause the footnote definition to be exited, except when those lines are
lazy continuation or blank.
Like so many things in markdown, footnote definition too are complex.
See [*§ Phase 1: block structure* in `CommonMark`][commonmark_block] for
more on parsing details.

See [`label`][label] for grammar, notes, and recommendations on that part.

The `label` part is interpreted as the [string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.

Definitions match to calls through identifiers.
To match, both labels must be equal after normalizing with
[`normalize_identifier`][].
One definition can match to multiple calls.
Multiple definitions with the same, normalized, identifier are ignored: the
first definition is preferred.
To illustrate, the definition with the content of `x` wins:

```markdown
[^a]: x
[^a]: y

[^a]
```

Importantly, while labels *can* include [string][] content (character
escapes and character references), these are not considered when matching.
To illustrate, neither definition matches the call:

```markdown
[^a&amp;b]: x
[^a\&b]: y

[^a&b]
```

Because footnote definitions are containers (like block quotes and list
items), they can contain more footnote definitions, and they can include
calls to themselves.

## HTML

GFM footnote definitions do not, on their own, relate to anything in HTML.
When matched with a [label end][label_end], which in turns matches to a
[GFM label start (footnote)][gfm_label_start_footnote], the definition
relates to several elements in HTML.

When one or more definitions are called, a footnote section is generated
at the end of the document, using `<section>`, `<h2>`, and `<ol>` elements:

```html
<section data-footnotes="" class="footnotes"><h2 id="footnote-label" class="sr-only">Footnotes</h2>
<ol>…</ol>
</section>
```

Each definition is generated as a `<li>` in the `<ol>`, in the order they
were first called:

```html
<li id="user-content-fn-1">…</li>
```

Backreferences are injected at the end of the first paragraph, or, when
there is no paragraph, at the end of the definition.
When a definition is called multiple times, multiple backreferences are
generated.
Further backreferences use an extra counter in the `href` attribute and
visually in a `<span>` after `↩`.

```html
<a href="#user-content-fnref-1" data-footnote-backref="" class="data-footnote-backref" aria-label="Back to content">↩</a> <a href="#user-content-fnref-1-2" data-footnote-backref="" class="data-footnote-backref" aria-label="Back to content">↩<sup>2</sup></a>
```

See
[*§ 4.5.1 The `a` element*][html_a],
[*§ 4.3.6 The `h1`, `h2`, `h3`, `h4`, `h5`, and `h6` elements*][html_h],
[*§ 4.4.8 The `li` element*][html_li],
[*§ 4.4.5 The `ol` element*][html_ol],
[*§ 4.4.1 The `p` element*][html_p],
[*§ 4.3.3 The `section` element*][html_section], and
[*§ 4.5.19 The `sub` and `sup` elements*][html_sup]
in the HTML spec for more info.

## Recommendation

When authoring markdown with footnotes, it’s recommended to use words
instead of numbers (or letters or anything with an order) as calls.
That makes it easier to reuse and reorder footnotes.

It’s recommended to place footnotes definitions at the bottom of the document.

## Bugs

GitHub’s own algorithm to parse footnote definitions contains several bugs.
These are not present in this project.
The issues relating to footnote definitions are:

*   [Footnote reference call identifiers are trimmed, but definition identifiers aren’t](https://github.com/github/cmark-gfm/issues/237)\
    — initial and final whitespace in labels causes them not to match
*   [Footnotes are matched case-insensitive, but links keep their casing, breaking them](https://github.com/github/cmark-gfm/issues/239)\
    — using uppercase (or any character that will be percent encoded) in identifiers breaks links
*   [Colons in footnotes generate links w/o `href`](https://github.com/github/cmark-gfm/issues/250)\
    — colons in identifiers generate broken links
*   [Character escape of `]` does not work in footnote identifiers](https://github.com/github/cmark-gfm/issues/240)\
    — some character escapes don’t work
*   [Footnotes in links are broken](https://github.com/github/cmark-gfm/issues/249)\
    — while `CommonMark` prevents links in links, GitHub does not prevent footnotes (which turn into links) in links
*   [Footnote-like brackets around image, break that image](https://github.com/github/cmark-gfm/issues/275)\
    — images can’t be used in what looks like a footnote call
*   [GFM footnotes: line ending in footnote definition label causes text to disappear](https://github.com/github/cmark-gfm/issues/282)\
    — line endings in footnote definitions cause text to disappear

## Tokens

*   [`DefinitionMarker`][Name::DefinitionMarker]
*   [`GfmFootnoteDefinition`][Name::GfmFootnoteDefinition]
*   [`GfmFootnoteDefinitionLabel`][Name::GfmFootnoteDefinitionLabel]
*   [`GfmFootnoteDefinitionLabelMarker`][Name::GfmFootnoteDefinitionLabelMarker]
*   [`GfmFootnoteDefinitionLabelString`][Name::GfmFootnoteDefinitionLabelString]
*   [`GfmFootnoteDefinitionMarker`][Name::GfmFootnoteDefinitionMarker]
*   [`GfmFootnoteDefinitionPrefix`][Name::GfmFootnoteDefinitionPrefix]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`micromark-extension-gfm-footnote`](https://github.com/micromark/micromark-extension-gfm-footnote)

> 👉 **Note**: Footnotes are not specified in GFM yet.
> See [`github/cmark-gfm#270`](https://github.com/github/cmark-gfm/issues/270)
> for the related issue.

[document]: crate::construct::document
[string]: crate::construct::string
[character_reference]: crate::construct::character_reference
[character_escape]: crate::construct::character_escape
[label]: crate::construct::partial_label
[label_end]: crate::construct::label_end
[gfm_label_start_footnote]: crate::construct::gfm_label_start_footnote
[commonmark_block]: https://spec.commonmark.org/0.31/#phase-1-block-structure
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
[html_h]: https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements
[html_li]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-li-element
[html_ol]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-ol-element
[html_p]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-p-element
[html_section]: https://html.spec.whatwg.org/multipage/sections.html#the-section-element
[html_sup]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-sub-and-sup-elements
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    start/1,
    label_before/1,
    label_at_marker/1,
    label_inside/1,
    label_escape/1,
    label_after/1,
    whitespace_after/1,
    cont_start/1,
    cont_blank/1,
    cont_filled/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of GFM footnote definition.

```markdown
> | [^a]: b
   ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{gfm_footnote_definition = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_footnote_definition),

    case Current of
        {some, C} when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(gfm_footnote_definition_label_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true -> ?TAB_SIZE - 1;
                    false -> infinity
                end,
            {Tokenizer4, StateName} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxIndent
            ),
            State = markdown_state:retry(StateName),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(gfm_footnote_definition_label_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
Before definition label (after optional whitespace).

```markdown
> | [^a]: b
    ^
```
""".
-spec label_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_before(Tokenizer1 = #markdown_tokenizer{current = {some, $[}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_footnote_definition_prefix),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_footnote_definition_label),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, gfm_footnote_definition_label_marker),
    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, gfm_footnote_definition_label_marker),
    State = markdown_state:next(gfm_footnote_definition_label_at_marker),
    {Tokenizer6, State};
label_before(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In label, at caret.

```markdown
> | [^a]: b
     ^
```
""".
-spec label_at_marker(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_at_marker(Tokenizer1 = #markdown_tokenizer{current = {some, $^}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_footnote_definition_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_footnote_definition_marker),
    Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, gfm_footnote_definition_label_string),
    Link = markdown_event_link:new(none, none, string),
    Tokenizer6 = markdown_tokenizer:enter_link(Tokenizer5, data, Link),
    State = markdown_state:next(gfm_footnote_definition_label_inside),
    {Tokenizer6, State};
label_at_marker(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In label.

> 👉 **Note**: `cmark-gfm` prevents whitespace from occurring in footnote
> definition labels.

```markdown
> | [^a]: b
      ^
```
""".
-spec label_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) when
    %% Too long.
    Size > ?LINK_REFERENCE_SIZE_MAX orelse
        %% Space or tab is not supported by GFM for some reason (`\n` and
        %% `[` make sense).
        (?is_option_none(Current) orelse
            ?is_option_some(Current, $\t) orelse
            ?is_option_some(Current, $\n) orelse ?is_option_some(Current, $\s) orelse ?is_option_some(Current, $[)) orelse
        %% Closing brace with nothing.
        (?is_option_some(Current, $]) andalso Size =:= 0)
->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State};
label_inside(
    Tokenizer1 = #markdown_tokenizer{current = {some, $]}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, data),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_footnote_definition_label_string),
    Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, gfm_footnote_definition_label_marker),
    Tokenizer6 = markdown_tokenizer:consume(Tokenizer5),
    Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, gfm_footnote_definition_label_marker),
    Tokenizer8 = markdown_tokenizer:exit(Tokenizer7, gfm_footnote_definition_label),
    State = markdown_state:next(gfm_footnote_definition_label_after),
    {Tokenizer8, State};
label_inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) ->
    Next =
        case Current of
            $\\ ->
                gfm_footnote_definition_label_escape;
            _ ->
                gfm_footnote_definition_label_inside
        end,
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(Next),
    {Tokenizer3, State}.

-doc """
After `\\`, at a special character.

> 👉 **Note**: `cmark-gfm` currently does not support escaped brackets:
> <https://github.com/github/cmark-gfm/issues/240>

```markdown
> | [^a\\*b]: c
        ^
```
""".
-spec label_escape(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_escape(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}
    }
) when Current =:= $[ orelse Current =:= $\\ orelse Current =:= $] ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(gfm_footnote_definition_label_inside),
    {Tokenizer3, State};
label_escape(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(gfm_footnote_definition_label_inside),
    {Tokenizer, State}.

-doc """
After definition label.

```markdown
> | [^a]: b
        ^
```
""".
-spec label_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_after(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $:},
        events = Events,
        parse_state = #markdown_parse_state{bytes = Bytes},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{gfm_footnote_definitions = GfmFootnoteDefinitions1}
    }
) ->
    End = markdown_util_skip:to_back(Events, markdown_vec:size(Events) - 1, [gfm_footnote_definition_label_string]),
    IdPosition = markdown_position:from_exit_event(Events, End),
    IdSlice = markdown_slice:from_position(Bytes, IdPosition),
    IdSliceBytes = markdown_slice:as_binary(IdSlice),
    Id = markdown_util_normalize_identifier:normalize_identifier(IdSliceBytes),
    %% Note: we don't care about uniqueness.
    %% It's likely that that doesn't happen very frequently.
    %% It is more likely that it wastes precious time.
    GfmFootnoteDefinitions2 = markdown_vec:push(GfmFootnoteDefinitions1, Id),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{gfm_footnote_definitions = GfmFootnoteDefinitions2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, definition_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, definition_marker),
    OkState = markdown_state:next(gfm_footnote_definition_whitespace_after),
    NokState = markdown_state:nok(),
    Tokenizer6 = markdown_tokenizer:attempt(Tokenizer5, OkState, NokState),
    %% Any whitespace after the marker is eaten, forming indented code
    %% is not possible.
    %% No space is also fine, just like a block quote marker.
    {Tokenizer7, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer6, 0, markdown_types:usize_max()
    ),
    State = markdown_state:next(SpaceOrTabState),
    {Tokenizer7, State};
label_after(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After definition prefix.

```markdown
> | [^a]: b
          ^
```
""".
-spec whitespace_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
whitespace_after(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, gfm_footnote_definition_prefix),
    State = markdown_state:ok(),
    {Tokenizer2, State}.

-doc """
Start of footnote definition continuation.

```markdown
  | [^a]: b
> |     c
    ^
```
""".
-spec cont_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_start(Tokenizer1 = #markdown_tokenizer{}) ->
    OkState = markdown_state:next(gfm_footnote_definition_cont_blank),
    NokState = markdown_state:next(gfm_footnote_definition_cont_filled),
    Tokenizer2 = markdown_tokenizer:check(Tokenizer1, OkState, NokState),
    State = markdown_state:retry(blank_line_start),
    {Tokenizer2, State}.

-doc """
Start of footnote definition continuation, at a blank line.

```markdown
  | [^a]: b
> | ␠␠␊
    ^
```
""".
-spec cont_blank(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_blank(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    {Tokenizer2, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer1, 0, ?TAB_SIZE
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer2, State};
cont_blank(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:ok(),
    {Tokenizer, State}.

-doc """
Start of footnote definition continuation, at a filled line.

```markdown
  | [^a]: b
> |     c
    ^
```
""".
-spec cont_filled(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_filled(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    %% Consume exactly `TAB_SIZE`.
    {Tokenizer2, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer1, ?TAB_SIZE, ?TAB_SIZE
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer2, State};
cont_filled(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
