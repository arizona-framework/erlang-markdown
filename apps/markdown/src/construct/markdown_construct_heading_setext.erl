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
-module(markdown_construct_heading_setext).
-moduledoc """
Heading (setext) occurs in the [flow][] content type.

## Grammar

Heading (setext) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
heading_setext ::= paragraph eol *space_or_tab (1*'-' | 1*'=') *space_or_tab

; See the `paragraph` construct for the BNF of that part.
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

See [`paragraph`][paragraph] for grammar, notes, and recommendations on
that part.

In markdown, it is also possible to create headings with a
[heading (atx)][heading_atx] construct.
The benefit of setext headings is that their text can include line endings,
and by extensions also hard breaks (e.g., with
[hard break (escape)][hard_break_escape]).
However, their limit is that they cannot form `<h3>` through `<h6>`
headings.

[Thematic breaks][thematic_break] formed with dashes and without whitespace
could be interpreted as a heading (setext).
Which one forms depends on whether there is text directly in fron of the
sequence.

> 🏛 **Background**: the word *setext* originates from a small markup
> language by Ian Feldman from 1991.
> See [*§ Setext* on Wikipedia][wiki_setext] for more info.
> The word *atx* originates from a tiny markup language by Aaron Swartz
> from 2002.
> See [*§ atx, the true structured text format* on `aaronsw.com`][atx] for
> more info.

## HTML

Heading (setext) in markdown relates to the `<h1>` and `<h2>` elements in
HTML.
See [*§ 4.3.6 The `h1`, `h2`, `h3`, `h4`, `h5`, and `h6` elements* in the
HTML spec][html] for more info.

## Recommendation

Always use heading (atx), never heading (setext).

## Tokens

*   [`HeadingSetext`][Name::HeadingSetext]
*   [`HeadingSetextText`][Name::HeadingSetextText]
*   [`HeadingSetextUnderline`][Name::HeadingSetextUnderline]
*   [`HeadingSetextUnderlineSequence`][Name::HeadingSetextUnderlineSequence]

## References

*   [`setext-underline.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/setext-underline.js)
*   [*§ 4.3 Setext headings* in `CommonMark`](https://spec.commonmark.org/0.31/#setext-headings)

[flow]: crate::construct::flow
[paragraph]: crate::construct::paragraph
[heading_atx]: crate::construct::heading_atx
[thematic_break]: crate::construct::thematic_break
[hard_break_escape]: crate::construct::hard_break_escape
[html]: https://html.spec.whatwg.org/multipage/sections.html#the-h1,-h2,-h3,-h4,-h5,-and-h6-elements
[wiki_setext]: https://en.wikipedia.org/wiki/Setext
[atx]: http://www.aaronsw.com/2002/atx/
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
    inside/1,
    'after'/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At start of heading (setext) underline.

```markdown
  | aa
> | ==
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        lazy = false,
        pierce = false,
        events = Events = #markdown_vec{size = EventsSize},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{heading_setext = true, code_indented = CodeIndented}
            }
        }
    }
) when EventsSize > 0 ->
    SkipIndex = markdown_util_skip:opt_back(Events, EventsSize - 1, [line_ending, space_or_tab]),
    case markdown_vec:get(Events, SkipIndex) of
        #markdown_event{name = EventName} when EventName =:= content orelse EventName =:= heading_setext_underline ->
            case markdown_tokenizer:enter(Tokenizer1, heading_setext_underline) of
                Tokenizer2 = #markdown_tokenizer{current = {some, Byte}} when Byte =:= $\t orelse Byte =:= $\s ->
                    Tokenizer3 = markdown_tokenizer:attempt(
                        Tokenizer2,
                        markdown_state:next(heading_setext_before),
                        markdown_state:nok()
                    ),
                    MaxIndent =
                        case CodeIndented of
                            true ->
                                ?TAB_SIZE - 1;
                            false ->
                                infinity
                        end,
                    {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                        Tokenizer3, 0, MaxIndent
                    ),
                    State = markdown_state:retry(SpaceOrTabState),
                    {Tokenizer4, State};
                Tokenizer2 = #markdown_tokenizer{} ->
                    State = markdown_state:retry(heading_setext_before),
                    {Tokenizer2, State}
            end;
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After optional whitespace, at `-` or `=`.

```markdown
  | aa
> | ==
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}, tokenize_state = TokenizeState1}) when
    Current =:= $- orelse Current =:= $=
->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Current},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, heading_setext_underline_sequence),
    {Tokenizer3, markdown_state:retry(heading_setext_inside)};
before(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
In sequence.

```markdown
  | aa
> | ==
    ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = #markdown_tokenize_state{marker = Marker}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    {Tokenizer2, markdown_state:next(heading_setext_inside)};
inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, heading_setext_underline_sequence),
    case Tokenizer3#markdown_tokenizer.current of
        {some, Current} when Current =:= $\t orelse Current =:= $\s ->
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(heading_setext_after), markdown_state:nok()
            ),
            {SpaceOrTabTokenizer, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer4),
            {SpaceOrTabTokenizer, markdown_state:retry(SpaceOrTabState)};
        _ ->
            {Tokenizer3, markdown_state:retry(heading_setext_after)}
    end.

-doc """
After sequence, after optional whitespace.

```markdown
  | aa
> | ==
      ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    %% Feel free to interrupt.
    Tokenizer2 = Tokenizer1#markdown_tokenizer{interrupt = false},
    Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, heading_setext),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, heading_setext_underline),
    {Tokenizer4, markdown_state:ok()};
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    %% Feel free to interrupt.
    Tokenizer2 = Tokenizer1#markdown_tokenizer{interrupt = false},
    Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, heading_setext),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, heading_setext_underline),
    {Tokenizer4, markdown_state:ok()};
'after'(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
Resolve heading (setext).
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{events = Events1, map = Map1}) ->
    % io:format("\n\n[~w] BEFORE resolve:heading_setext:resolve_loop\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer1)]),
    Enter = markdown_util_skip:to(Events1, 0, [heading_setext_underline]),
    {Events2, Map2} = resolve_loop(Events1, Map1, Enter),
    % io:format("\n\n[~w] AFTER resolve:heading_setext:resolve_loop\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer1#markdown_tokenizer{events = Events2, map = Map2})]),
    {Map3, Events3} = markdown_edit_map:consume(Map2, Events2),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3, map = Map3},
    % io:format("\n\n[~w] AFTER resolve:heading_setext:consume\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer2)]),
    {Tokenizer2, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(Events, Map, Enter) -> {Events, Map} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Map :: markdown_edit_map:t(),
    Enter :: non_neg_integer().
resolve_loop(Events, Map, Enter) when Enter >= ?markdown_vec_size(Events) ->
    {Events, Map};
resolve_loop(Events1, Map1, Enter1) ->
    Exit = markdown_util_skip:to(Events1, Enter1 + 1, [heading_setext_underline]),
    %% Find paragraph before
    ParagraphExitBefore = markdown_util_skip:opt_back(Events1, Enter1 - 1, [
        space_or_tab, line_ending, block_quote_prefix
    ]),
    %% There’s a paragraph before: this is a setext heading.
    case markdown_vec:get(Events1, ParagraphExitBefore) of
        #markdown_event{name = paragraph} ->
            ParagraphEnter = markdown_util_skip:to_back(Events1, ParagraphExitBefore - 1, [paragraph]),
            % io:format("\n\n[~w] BEFORE resolve:heading_setext:resolve_loop:paragraph(enter=~w, paragraph_exit_before=~w, paragraph_enter=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), Enter1, ParagraphExitBefore, ParagraphEnter, markdown_debug:rust_debug_string(Events1), markdown_debug:rust_debug_string(Map1)]),
            %% Change types of Enter:Paragraph, Exit:Paragraph.
            Events2 = markdown_vec:update(Events1, ParagraphEnter, fun(EventEntry) ->
                EventEntry#markdown_event{name = heading_setext_text}
            end),
            Events3 = markdown_vec:update(Events2, ParagraphExitBefore, fun(EventEntry) ->
                EventEntry#markdown_event{name = heading_setext_text}
            end),
            %% Add Enter:HeadingSetext, Exit:HeadingSetext.
            HeadingEnter1 = markdown_vec:get(Events3, ParagraphEnter),
            HeadingEnter2 = HeadingEnter1#markdown_event{name = heading_setext},
            Map2 = markdown_edit_map:add(Map1, ParagraphEnter, 0, [HeadingEnter2]),
            HeadingExit1 = markdown_vec:get(Events3, Exit),
            HeadingExit2 = HeadingExit1#markdown_event{name = heading_setext},
            Map3 = markdown_edit_map:add(Map2, Exit + 1, 0, [HeadingExit2]),
            Enter2 = markdown_util_skip:to(Events3, Exit + 1, [heading_setext_underline]),
            % io:format("\n\n[~w] AFTER resolve:heading_setext:resolve_loop:paragraph(enter=~w, paragraph_exit_before=~w, paragraph_enter=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), Enter1, ParagraphExitBefore, ParagraphEnter, markdown_debug:rust_debug_string(Events3), markdown_debug:rust_debug_string(Map3)]),
            resolve_loop(Events3, Map3, Enter2);
        _ ->
            case
                (Exit + 3) < ?markdown_vec_size(Events1) andalso
                    ((markdown_vec:get(Events1, Exit + 1))#markdown_event.name =:= line_ending) andalso
                    ((markdown_vec:get(Events1, Exit + 3))#markdown_event.name =:= paragraph)
            of
                true ->
                    %% Swap type, HeadingSetextUnderline:Enter -> Paragraph:Enter.
                    Events2 = markdown_vec:update(Events1, Enter1, fun(EventEntry) ->
                        EventEntry#markdown_event{name = paragraph}
                    end),
                    %% Swap type, LineEnding -> Data.
                    Events3 = markdown_vec:update(Events2, Exit + 1, fun(EventEntry) ->
                        EventEntry#markdown_event{name = data}
                    end),
                    Events4 = markdown_vec:update(Events3, Exit + 2, fun(EventEntry) ->
                        EventEntry#markdown_event{name = data}
                    end),
                    %% Move new data (was line ending) back to include whole line,
                    %% and link data together.
                    Events5 = markdown_vec:update(Events4, Exit + 1, fun(EventEntry) ->
                        EventEntry#markdown_event{
                            link = {some, markdown_event_link:new(none, {some, Exit + 4}, text)},
                            point = (markdown_vec:get(Events4, Enter1))#markdown_event.point
                        }
                    end),
                    Events6 = markdown_vec:update(Events5, Exit + 4, fun(EventEntry) ->
                        {some, LinkEntry} = EventEntry#markdown_event.link,
                        EventEntry#markdown_event{
                            link = {some, LinkEntry#markdown_event_link{previous = {some, Exit + 1}}}
                        }
                    end),
                    %% Remove *including* HeadingSetextUnderline:Exit, until the line ending.
                    Map2 = markdown_edit_map:add(Map1, Enter1 + 1, Exit - Enter1, []),
                    %% Remove old Paragraph:Enter.
                    Map3 = markdown_edit_map:add(Map2, Exit + 3, 1, []),
                    Enter2 = markdown_util_skip:to(Events3, Exit + 1, [heading_setext_underline]),
                    resolve_loop(Events6, Map3, Enter2);
                false ->
                    %% Swap type.
                    Events2 = markdown_vec:update(Events1, Enter1, fun(EventEntry) ->
                        EventEntry#markdown_event{name = paragraph}
                    end),
                    Events3 = markdown_vec:update(Events2, Exit, fun(EventEntry) ->
                        EventEntry#markdown_event{name = paragraph}
                    end),
                    %% Replace what’s inside the underline (whitespace, sequence).
                    Map2 = markdown_edit_map:add(Map1, Enter1 + 1, Exit - Enter1 - 1, [
                        markdown_event:data(
                            'enter',
                            (markdown_vec:get(Events3, Enter1))#markdown_event.point,
                            {some, markdown_event_link:new(none, none, text)}
                        ),
                        markdown_event:data('exit', (markdown_vec:get(Events3, Exit))#markdown_event.point, none)
                    ]),
                    Enter2 = markdown_util_skip:to(Events3, Exit + 1, [heading_setext_underline]),
                    resolve_loop(Events3, Map2, Enter2)
            end
    end.
