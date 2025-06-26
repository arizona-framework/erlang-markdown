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
-module(markdown_construct_heading_atx).
-moduledoc """
Heading (atx) occurs in the [flow][] content type.

## Grammar

Heading (atx) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
heading_atx ::= 1*6'#' [ 1*space_or_tab line [ 1*space_or_tab 1*'#' ] ] *space_or_tab
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

`CommonMark` introduced the requirement on whitespace existing after the
opening sequence and before text.
In older markdown versions, this was not required, and headings would form
without it.

In markdown, it is also possible to create headings with a
[heading (setext)][heading_setext] construct.
The benefit of setext headings is that their text can include line endings,
and by extensions also hard breaks (e.g., with
[hard break (escape)][hard_break_escape]).
However, their limit is that they cannot form `<h3>` through `<h6>`
headings.

> 🏛 **Background**: the word *setext* originates from a small markup
> language by Ian Feldman from 1991.
> See [*§ Setext* on Wikipedia][wiki_setext] for more info.
> The word *atx* originates from a tiny markup language by Aaron Swartz
> from 2002.
> See [*§ atx, the true structured text format* on `aaronsw.com`][atx] for
> more info.

## HTML

Headings in markdown relate to the `<h1>` through `<h6>` elements in HTML.
See [*§ 4.3.6 The `h1`, `h2`, `h3`, `h4`, `h5`, and `h6` elements* in the
HTML spec][html] for more info.

## Recommendation

Always use heading (atx), never heading (setext).

## Tokens

*   [`HeadingAtx`][Name::HeadingAtx]
*   [`HeadingAtxSequence`][Name::HeadingAtxSequence]
*   [`HeadingAtxText`][Name::HeadingAtxText]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`heading-atx.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/heading-atx.js)
*   [*§ 4.2 ATX headings* in `CommonMark`](https://spec.commonmark.org/0.31/#atx-headings)

[flow]: crate::construct::flow
[heading_setext]: crate::construct::heading_setext
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
    sequence_open/1,
    at_break/1,
    sequence_further/1,
    data/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of a heading (atx).

```markdown
> | ## aa
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{code_indented = CodeIndented, heading_atx = true}
            }
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, heading_atx),
    case Current =:= $\t orelse Current =:= $\s of
        true ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(heading_atx_before),
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
        false ->
            State = markdown_state:retry(heading_atx_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After optional whitespace, at `#`.

```markdown
> | ## aa
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, $#}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, heading_atx_sequence),
    State = markdown_state:retry(heading_atx_sequence_open),
    {Tokenizer2, State};
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In opening sequence.

```markdown
> | ## aa
    ^
```
""".
-spec sequence_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence_open(
    Tokenizer1 = #markdown_tokenizer{current = {some, $#}, tokenize_state = #markdown_tokenize_state{size = Size}}
) when Size < ?HEADING_ATX_OPENING_FENCE_SIZE_MAX ->
    TokenizeState2 = Tokenizer1#markdown_tokenizer.tokenize_state#markdown_tokenize_state{size = Size + 1},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(heading_atx_sequence_open),
    {Tokenizer3, State};
%% Always at least one `#`.
sequence_open(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =:= none orelse Current =:= {some, $\t} orelse Current =:= {some, $\n} orelse Current =:= {some, $\s}
->
    TokenizeState2 = Tokenizer1#markdown_tokenizer.tokenize_state#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, heading_atx_sequence),
    State = markdown_state:retry(heading_atx_at_break),
    {Tokenizer3, State};
sequence_open(Tokenizer1 = #markdown_tokenizer{}) ->
    TokenizeState2 = Tokenizer1#markdown_tokenizer.tokenize_state#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After something, before something else.

```markdown
> | ## aa
      ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(Tokenizer1 = #markdown_tokenizer{current = Current}) when Current =:= none orelse Current =:= {some, $\n} ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, heading_atx),
    Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, heading_atx),
    %% Feel free to interrupt.
    Tokenizer4 = Tokenizer3#markdown_tokenizer{interrupt = false},
    State = markdown_state:ok(),
    {Tokenizer4, State};
at_break(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =:= {some, $\t} orelse Current =:= {some, $\s}
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(heading_atx_at_break),
        markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
at_break(Tokenizer1 = #markdown_tokenizer{current = {some, $#}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, heading_atx_sequence),
    State = markdown_state:retry(heading_atx_sequence_further),
    {Tokenizer2, State};
at_break(Tokenizer1 = #markdown_tokenizer{}) ->
    Link = markdown_event_link:new(none, none, text),
    Tokenizer2 = markdown_tokenizer:enter_link(Tokenizer1, data, Link),
    State = markdown_state:retry(heading_atx_data),
    {Tokenizer2, State}.

-doc """
In further sequence (after whitespace).

Could be normal "visible" hashes in the heading or a final sequence.

```markdown
> | ## aa ##
          ^
```
""".
-spec sequence_further(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence_further(Tokenizer1 = #markdown_tokenizer{current = {some, $#}}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(heading_atx_sequence_further),
    {Tokenizer2, State};
sequence_further(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, heading_atx_sequence),
    State = markdown_state:retry(heading_atx_at_break),
    {Tokenizer2, State}.

-doc """
In text.

```markdown
> | ## aa
       ^
```
""".
-spec data(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
data(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =:= none orelse Current =:= {some, $\t} orelse Current =:= {some, $\n} orelse Current =:= {some, $\s}
->
    %% Note: `#` for closing sequence must be preceded by whitespace, otherwise it's just text.
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
    State = markdown_state:retry(heading_atx_at_break),
    {Tokenizer2, State};
data(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(heading_atx_data),
    {Tokenizer2, State}.

-doc """
Resolve heading (atx).
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{events = Events1, map = Map1}) ->
    % io:format("\n\n[~w] BEFORE heading_atx:resolve_loop\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events1), markdown_debug:rust_debug_string(Map1)]),
    {Events2, Map2} = resolve_loop(Events1, Map1, 0, false, none, none),
    % io:format("\n\n[~w] AFTER heading_atx:resolve_loop\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events2), markdown_debug:rust_debug_string(Map2)]),
    {Map3, Events3} = markdown_edit_map:consume(Map2, Events2),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3, map = Map3},
    {Tokenizer2, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(Events, Map, Index, HeadingInside, DataStart, DataEnd) -> {Events, Map} when
    Events :: markdown_vec:t(markdown_event:t()),
    Map :: markdown_edit_map:t(),
    Index :: non_neg_integer(),
    HeadingInside :: boolean(),
    DataStart :: none | {some, non_neg_integer()},
    DataEnd :: none | {some, non_neg_integer()}.
resolve_loop(Events, Map, Index, _HeadingInside, _DataStart, _DataEnd) when Index >= ?markdown_vec_size(Events) ->
    {Events, Map};
resolve_loop(Events, Map1, Index, HeadingInside, DataStart, DataEnd) ->
    Event = markdown_vec:get(Events, Index),

    {NewHeadingInside, NewDataStart, NewDataEnd, NewMap} =
        case Event#markdown_event.name =:= heading_atx of
            true ->
                case Event#markdown_event.kind =:= enter of
                    true ->
                        {true, DataStart, DataEnd, Map1};
                    false ->
                        case DataStart of
                            {some, Start} ->
                                %% If `Start` is some, `End` is too.
                                {some, End} = DataEnd,

                                StartEvent = markdown_vec:get(Events, Start),
                                EndEvent = markdown_vec:get(Events, End),

                                EnterHeadingAtxTextEvent = markdown_event:heading_atx_text(
                                    enter, StartEvent#markdown_event.point, none
                                ),
                                ExitHeadingAtxTextEvent = markdown_event:heading_atx_text(
                                    exit, EndEvent#markdown_event.point, none
                                ),

                                EnterVec = markdown_vec:from_list([EnterHeadingAtxTextEvent]),
                                EmptyVec = markdown_vec:new(),
                                ExitVec = markdown_vec:from_list([ExitHeadingAtxTextEvent]),

                                Map2 = markdown_edit_map:add(Map1, Start, 0, EnterVec),
                                %% Remove everything between the start and the end.
                                Map3 = markdown_edit_map:add(Map2, Start + 1, End - Start - 1, EmptyVec),
                                Map4 = markdown_edit_map:add(Map3, End + 1, 0, ExitVec),

                                {false, none, none, Map4};
                            none ->
                                {false, none, none, Map1}
                        end
                end;
            false ->
                case HeadingInside andalso Event#markdown_event.name =:= data of
                    true ->
                        case Event#markdown_event.kind =:= enter of
                            true ->
                                case DataStart of
                                    none ->
                                        {HeadingInside, {some, Index}, DataEnd, Map1};
                                    _ ->
                                        {HeadingInside, DataStart, DataEnd, Map1}
                                end;
                            false ->
                                {HeadingInside, DataStart, {some, Index}, Map1}
                        end;
                    false ->
                        {HeadingInside, DataStart, DataEnd, Map1}
                end
        end,

    resolve_loop(Events, NewMap, Index + 1, NewHeadingInside, NewDataStart, NewDataEnd).
