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
-module(markdown_construct_definition).
-moduledoc """
Definition occurs in the [content] content type.

## Grammar

Definition forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
definition ::= label ':' [ space_or_tab_eol ] destination [ space_or_tab_eol title ] [ space_or_tab ]

; See the `destination`, `title`, and `label` constructs for the BNF of
; those parts.
```

This construct must be followed by an eol (line ending) or eof (end of
file), like flow constructs.

See [`destination`][destination], [`label`][label], and [`title`][title]
for grammar, notes, and recommendations on each part.

The `destination`, `label`, and `title` parts are interpreted as the
[string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.

Definitions match to references through identifiers.
To match, both labels must be equal after normalizing with
[`normalize_identifier`][normalize_identifier].
One definition can match to multiple references.
Multiple definitions with the same, normalized, identifier are ignored: the
first definition is preferred.
To illustrate, the definition with a destination of `x` wins:

```markdown
[a]: x
[a]: y

[a]
```

Importantly, while labels *can* include [string][] content (character
escapes and character references), these are not considered when matching.
To illustrate, neither definition matches the reference:

```markdown
[a&amp;b]: x
[a\&b]: y

[a&b]
```

For info on how to encode characters in URLs, see
[`destination`][destination].
For info on how characters are encoded as `href` on `<a>` or `src` on
`<img>` when compiling, see
[`sanitize_uri`][sanitize_uri].

## HTML

Definitions in markdown do not, on their own, relate to anything in HTML.
When matched with a [label end (reference)][label_end], they together
relate to the `<a>` or `<img>` elements in HTML.
The definition forms its `href` or `src`, and optionally `title`,
attributes.
See [*§ 4.5.1 The `a` element*][html_a] and
[*§ 4.8.3 The `img` element*][html_img] in the HTML spec for more info.

## Tokens

*   [`Definition`][Name::Definition]
*   [`DefinitionDestination`][Name::DefinitionDestination]
*   [`DefinitionDestinationLiteral`][Name::DefinitionDestinationLiteral]
*   [`DefinitionDestinationLiteralMarker`][Name::DefinitionDestinationLiteralMarker]
*   [`DefinitionDestinationRaw`][Name::DefinitionDestinationRaw]
*   [`DefinitionDestinationString`][Name::DefinitionDestinationString]
*   [`DefinitionLabel`][Name::DefinitionLabel]
*   [`DefinitionLabelMarker`][Name::DefinitionLabelMarker]
*   [`DefinitionLabelString`][Name::DefinitionLabelString]
*   [`DefinitionMarker`][Name::DefinitionMarker]
*   [`DefinitionTitle`][Name::DefinitionTitle]
*   [`DefinitionTitleMarker`][Name::DefinitionTitleMarker]
*   [`DefinitionTitleString`][Name::DefinitionTitleString]
*   [`LineEnding`][Name::LineEnding]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`definition.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/definition.js)
*   [*§ 4.7 Link reference definitions* in `CommonMark`](https://spec.commonmark.org/0.31/#link-reference-definitions)

[content]: crate::construct::content
[string]: crate::construct::string
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[destination]: crate::construct::partial_destination
[label]: crate::construct::partial_label
[label_end]: crate::construct::label_end
[title]: crate::construct::partial_title
[sanitize_uri]: crate::util::sanitize_uri::sanitize
[normalize_identifier]: crate::util::normalize_identifier
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
[html_img]: https://html.spec.whatwg.org/multipage/embedded-content.html#the-img-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    before/1,
    label_after/1,
    label_nok/1,
    marker_after/1,
    destination_before/1,
    destination_after/1,
    destination_missing/1,
    'after'/1,
    after_whitespace/1,
    title_before/1,
    title_before_marker/1,
    title_after/1,
    title_after_optional_whitespace/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At start of a definition.

```markdown
> | [a]: b "c"
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{definition = true}}
        },
        interrupt = Interrupt,
        events = Events
    }
) ->
    case
        not Interrupt orelse
            (markdown_vec:size(Events) > 0 andalso
                (markdown_vec:get(
                    Events,
                    markdown_util_skip:opt_back(
                        Events,
                        markdown_vec:size(Events) - 1,
                        [line_ending, space_or_tab]
                    )
                ))#markdown_event.name =:= definition)
    of
        true ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, definition),
            case Tokenizer2#markdown_tokenizer.current of
                {some, Current} when Current =:= $\t orelse Current =:= $\s ->
                    %% Note: arbitrary whitespace allowed even if code (indented) is on.
                    Tokenizer3 = markdown_tokenizer:attempt(
                        Tokenizer2, markdown_state:next(definition_before), markdown_state:nok()
                    ),
                    {SpaceOrTabTokenizer, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(
                        Tokenizer3
                    ),
                    {SpaceOrTabTokenizer, markdown_state:retry(SpaceOrTabState)};
                _ ->
                    {Tokenizer2, markdown_state:retry(definition_before)}
            end;
        false ->
            {Tokenizer1, markdown_state:nok()}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
After optional whitespace, at `[`.

```markdown
> | [a]: b "c"
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, $[}, tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = definition_label,
        token_2 = definition_label_marker,
        token_3 = definition_label_string
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(definition_label_after),
        markdown_state:next(definition_label_nok)
    ),
    {Tokenizer3, markdown_state:retry(label_start)};
before(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
After label.

```markdown
> | [a]: b "c"
       ^
```
""".
-spec label_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, events = Events, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data, token_2 = data, token_3 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    case Current of
        {some, $:} ->
            EventsSize = markdown_vec:size(Events),
            End = markdown_util_skip:to_back(Events, EventsSize - 1, [definition_label_string]),

            TokenizeState3 = Tokenizer2#markdown_tokenizer.tokenize_state#markdown_tokenize_state{'end' = End},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},

            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, definition_marker),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, definition_marker),

            State = markdown_state:next(definition_marker_after),
            {Tokenizer6, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.

-doc """
At a non-label

```markdown
> | []
    ^
```
""".
-spec label_nok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
label_nok(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data, token_2 = data, token_3 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After marker.

```markdown
> | [a]: b "c"
        ^
```
""".
-spec marker_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
marker_after(Tokenizer1) ->
    DestinationBeforeState = markdown_state:next(definition_destination_before),
    Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, DestinationBeforeState, DestinationBeforeState),
    {Tokenizer3, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabEolState),
    {Tokenizer3, State}.

-doc """
Before destination.

```markdown
> | [a]: b "c"
         ^
```
""".
-spec destination_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
destination_before(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = definition_destination,
        token_2 = definition_destination_literal,
        token_3 = definition_destination_literal_marker,
        token_4 = definition_destination_raw,
        token_5 = definition_destination_string,
        size_b = markdown_types:usize_max()
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    DestinationAfterState = markdown_state:next(definition_destination_after),
    DestinationMissingState = markdown_state:next(definition_destination_missing),
    Tokenizer3 = markdown_tokenizer:attempt(Tokenizer2, DestinationAfterState, DestinationMissingState),
    State = markdown_state:retry(destination_start),
    {Tokenizer3, State}.

-doc """
After destination.

```markdown
> | [a]: b "c"
          ^
```
""".
-spec destination_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
destination_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data,
        token_4 = data,
        token_5 = data,
        size_b = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    DefinitionAfterState = markdown_state:next(definition_after),
    Tokenizer3 = markdown_tokenizer:attempt(Tokenizer2, DefinitionAfterState, DefinitionAfterState),
    State = markdown_state:retry(definition_title_before),
    {Tokenizer3, State}.

-doc """
Without destination.
""".
-spec destination_missing(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
destination_missing(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data,
        token_4 = data,
        token_5 = data,
        size_b = 0,
        'end' = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After definition.

```markdown
> | [a]: b
          ^
> | [a]: b "c"
              ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\  ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(definition_after_whitespace), markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
'after'(Tokenizer) ->
    State = markdown_state:retry(definition_after_whitespace),
    {Tokenizer, State}.

-doc """
After definition, after optional whitespace.

```markdown
> | [a]: b
          ^
> | [a]: b "c"
              ^
```
""".
-spec after_whitespace(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_whitespace(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        parse_state = #markdown_parse_state{bytes = Bytes},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{definitions = Definitions1, 'end' = End},
        events = Events
    }
) when Current =:= none orelse Current =:= {some, $\n} ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, definition),
    Position = markdown_position:from_exit_event(Events, End),
    Slice = markdown_slice:from_position(Bytes, Position),
    Id = markdown_slice:as_binary(Slice),
    NormalizedId = markdown_util_normalize_identifier:normalize_identifier(Id),
    %% Note: we don't care about uniqueness.
    %% It's likely that that doesn't happen very frequently.
    %% It is more likely that it wastes precious time.
    Definitions2 = markdown_vec:push(Definitions1, NormalizedId),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{definitions = Definitions2, 'end' = 0},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2, interrupt = true},
    State = markdown_state:ok(),
    {Tokenizer3, State};
after_whitespace(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{'end' = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After destination, at whitespace.

```markdown
> | [a]: b
          ^
> | [a]: b "c"
          ^
```
""".
-spec title_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
title_before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\n orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(definition_title_before_marker), markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabEolState} = markdown_construct_partial_space_or_tab_eol:space_or_tab_eol(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabEolState),
    {Tokenizer3, State};
title_before(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
At title.

```markdown
  | [a]: b
> | "c"
    ^
```
""".
-spec title_before_marker(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
title_before_marker(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = definition_title,
        token_2 = definition_title_marker,
        token_3 = definition_title_string
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(definition_title_after), markdown_state:nok()
    ),
    State = markdown_state:retry(title_start),
    {Tokenizer3, State}.

-doc """
After title.

```markdown
> | [a]: b "c"
              ^
```
""".
-spec title_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
title_after(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        token_1 = data,
        token_2 = data,
        token_3 = data
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case Current =:= {some, $\t} orelse Current =:= {some, $\s} of
        true ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(definition_title_after_optional_whitespace), markdown_state:nok()
            ),
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer3),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        false ->
            State = markdown_state:retry(definition_title_after_optional_whitespace),
            {Tokenizer2, State}
    end.

-doc """
After title, after optional whitespace.

```markdown
> | [a]: b "c"
              ^
```
""".
-spec title_after_optional_whitespace(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
title_after_optional_whitespace(Tokenizer = #markdown_tokenizer{current = Current}) ->
    State =
        case Current of
            none -> markdown_state:ok();
            {some, $\n} -> markdown_state:ok();
            _ -> markdown_state:nok()
        end,
    {Tokenizer, State}.
