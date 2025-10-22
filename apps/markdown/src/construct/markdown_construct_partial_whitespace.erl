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
-module(markdown_construct_partial_whitespace).
-moduledoc """
Trailing whitespace occurs in [string][] and [text][].

## Grammar

Trailing whitespace forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: the start and end here count as an eol in the case of `text`.
whitespace ::= *space_or_tab eol *space_or_tab
```

It occurs around line endings and, in the case of text content, it also
occurs at the start or end of the whole.

Normally this whitespace is ignored.
In the case of text content, whitespace before a line ending that
consistents solely of spaces, at least 2, forms a hard break (trailing).

The minimum number of those spaces is defined in
[`HARD_BREAK_PREFIX_SIZE_MIN`][].

It is also possible to create a hard break with a similar construct: a
[hard break (escape)][hard_break_escape] is a backslash followed
by a line ending.
That construct is recommended because it is similar to a
[character escape][character_escape] and similar to how line endings can be
“escaped” in other languages.
Trailing spaces are typically invisible in editors, or even automatically
removed, making hard break (trailing) hard to use.

## HTML

Hard breaks in markdown relate to the HTML element `<br>`.
See [*§ 4.5.27 The `br` element* in the HTML spec][html] for more info.

## Recommendation

Do not use trailing whitespace.
It is never needed when using [hard break (escape)][hard_break_escape]
to create hard breaks.

## Tokens

*   [`HardBreakTrailing`][Name::HardBreakTrailing]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`initialize/text.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark/dev/lib/initialize/text.js)
*   [*§ 6.7 Hard line breaks* in `CommonMark`](https://spec.commonmark.org/0.31/#hard-line-breaks)

[string]: crate::construct::string
[text]: crate::construct::text
[hard_break_escape]: crate::construct::hard_break_escape
[character_escape]: crate::construct::character_escape
[hard_break_prefix_size_min]: crate::util::constant::HARD_BREAK_PREFIX_SIZE_MIN
[html]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-br-element
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
    resolve_whitespace/3
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Resolve whitespace.
""".
-spec resolve_whitespace(Tokenizer, HardBreak, TrimWhole) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(), HardBreak :: boolean(), TrimWhole :: boolean().
resolve_whitespace(Tokenizer1 = #markdown_tokenizer{}, HardBreak, TrimWhole) when
    is_boolean(HardBreak) andalso is_boolean(TrimWhole)
->
    % io:format("\n\n[~w] BEFORE resolve_whitespace(hard_break = ~ts, trim_whole = ~ts)\n\n~ts\n\n", [markdown:counter_get(), HardBreak, TrimWhole, markdown_debug:rust_debug_string(Tokenizer1)]),
    Tokenizer2 =
        #markdown_tokenizer{events = Events2, map = EditMap2} = resolve_whitespace__events_loop(
            Tokenizer1, HardBreak, TrimWhole, 0
        ),
    {EditMap3, Events3} = markdown_edit_map:consume(EditMap2, Events2),
    Tokenizer3 = Tokenizer2#markdown_tokenizer{events = Events3, map = EditMap3},
    % io:format("\n\n[~w] AFTER resolve_whitespace(hard_break = ~ts, trim_whole = ~ts)\n\n~ts\n\n", [markdown:counter_get(), HardBreak, TrimWhole, markdown_debug:rust_debug_string(Tokenizer3)]),
    Tokenizer3.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec find_first_non_whitespace(Bytes, Index) -> Index when Bytes :: binary(), Index :: non_neg_integer().
find_first_non_whitespace(Bytes, Index) when Index >= 0 andalso Index < byte_size(Bytes) ->
    case binary:at(Bytes, Index) of
        C when C =:= $\s orelse C =:= $\t ->
            find_first_non_whitespace(Bytes, Index + 1);
        _ ->
            Index
    end;
find_first_non_whitespace(Bytes, Index) when is_binary(Bytes) andalso Index =:= byte_size(Bytes) ->
    Index.

%% @private
-spec find_last_non_whitespace(Bytes, Index, SpacesOnly) -> {Index, SpacesOnly} when
    Bytes :: binary(),
    Index :: non_neg_integer(),
    SpacesOnly :: boolean().
find_last_non_whitespace(_Bytes, 0, SpacesOnly) when is_boolean(SpacesOnly) ->
    {0, SpacesOnly};
find_last_non_whitespace(Bytes, Index, SpacesOnly) when
    Index > 0 andalso Index =< byte_size(Bytes) andalso is_boolean(SpacesOnly)
->
    case binary:at(Bytes, Index - 1) of
        $\s ->
            find_last_non_whitespace(Bytes, Index - 1, SpacesOnly);
        $\t ->
            find_last_non_whitespace(Bytes, Index - 1, false);
        _ ->
            {Index, SpacesOnly}
    end.

%% @private
-spec resolve_whitespace__events_loop(Tokenizer, HardBreak, TrimWhole, Index) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    HardBreak :: boolean(),
    TrimWhole :: boolean(),
    Index :: markdown_vec:index().
resolve_whitespace__events_loop(
    Tokenizer1 = #markdown_tokenizer{events = Events = #markdown_vec{size = TotalSize}}, HardBreak, TrimWhole, Index
) when Index < TotalSize ->
    case markdown_vec:get(Events, Index) of
        #markdown_event{kind = exit, name = data} ->
            TrimStart =
                (TrimWhole andalso Index =:= 1) orelse
                    (Index > 1 andalso (markdown_vec:get(Events, Index - 2))#markdown_event.name =:= line_ending),
            TrimEnd =
                (TrimWhole andalso Index =:= TotalSize - 1) orelse
                    (Index + 1 < TotalSize andalso
                        (markdown_vec:get(Events, Index + 1))#markdown_event.name =:= line_ending),
            Tokenizer2 = trim_data(Tokenizer1, Index, TrimStart, TrimEnd, HardBreak),
            resolve_whitespace__events_loop(Tokenizer2, HardBreak, TrimWhole, Index + 1);
        _ ->
            resolve_whitespace__events_loop(Tokenizer1, HardBreak, TrimWhole, Index + 1)
    end;
resolve_whitespace__events_loop(Tokenizer = #markdown_tokenizer{}, _HardBreak, _TrimWhole, _Index) ->
    Tokenizer.

-doc """
Trim a `Data` event.
""".
-spec trim_data(Tokenizer, ExitIndex, TrimStart, TrimEnd, HardBreak) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    ExitIndex :: markdown_vec:index(),
    TrimStart :: boolean(),
    TrimEnd :: boolean(),
    HardBreak :: boolean().
trim_data(
    Tokenizer1 = #markdown_tokenizer{events = Events = #markdown_vec{}}, ExitIndex, TrimStart, TrimEnd, HardBreak
) ->
    % io:format("EVENTS(~w)\n~ts\n", [ExitIndex, markdown_debug:rust_debug_string(Events)]),
    Position = markdown_position:from_exit_event(Events, ExitIndex),
    Slice1 = markdown_slice:from_position(markdown_tokenizer:bytes(Tokenizer1), Position),
    {Action1, {Tokenizer2, Slice2}} =
        case TrimEnd of
            true ->
                trim_data_end(Tokenizer1, Slice1, ExitIndex, HardBreak);
            false ->
                {cont, {Tokenizer1, Slice1}}
        end,
    case Action1 of
        halt ->
            Tokenizer2;
        cont when TrimStart =:= true ->
            {_Action2, {Tokenizer3, _Slice3}} = trim_data_start(Tokenizer2, Slice2, ExitIndex),
            Tokenizer3;
        cont ->
            Tokenizer2
    end.

%% @private
-spec trim_data_end(Tokenizer, Slice, ExitIndex, HardBreak) -> {cont | halt, {Tokenizer, Slice}} when
    Tokenizer :: markdown_tokenizer:t(),
    Slice :: markdown_slice:t(),
    ExitIndex :: markdown_vec:index(),
    HardBreak :: boolean().
% trim_data_end(SliceData = #markdown_slice{bytes = Bytes, after = After}, Events, Map1, ExitIndex, HardBreak) ->
trim_data_end(
    Tokenizer1 = #markdown_tokenizer{events = Events1 = #markdown_vec{size = EventsLen}, map = EditMap1},
    Slice1 = #markdown_slice{bytes = Bytes, 'after' = After},
    ExitIndex,
    HardBreak
) ->
    BytesLen = byte_size(Bytes),
    {Index, SpacesOnly} = find_last_non_whitespace(Bytes, BytesLen, After =:= 0),
    Diff = BytesLen - Index,
    Name =
        case
            HardBreak andalso SpacesOnly andalso Diff >= ?HARD_BREAK_PREFIX_SIZE_MIN andalso ExitIndex + 1 < EventsLen
        of
            true -> hard_break_trailing;
            false -> space_or_tab
        end,
    case Index of
        0 ->
            %% The whole data is whitespace.
            %% We can be very fast: we only change the event names.
            Events2 = markdown_vec:update(Events1, ExitIndex - 1, fun(Event) -> Event#markdown_event{name = Name} end),
            Events3 = markdown_vec:update(Events2, ExitIndex, fun(Event) -> Event#markdown_event{name = Name} end),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3},
            {halt, {Tokenizer2, Slice1}};
        _ when Diff > 0 orelse After > 0 ->
            ExitEvent = markdown_vec:get(Events1, ExitIndex),
            ExitPoint = ExitEvent#markdown_event.point,
            EnterPoint = ExitPoint#markdown_point{
                offset = ExitPoint#markdown_point.offset - Diff,
                column = ExitPoint#markdown_point.column - Diff,
                virtual = 0
            },
            EditMap2 = markdown_edit_map:add(EditMap1, ExitIndex + 1, 0, [
                markdown_event:Name(enter, EnterPoint, none),
                markdown_event:Name(exit, ExitPoint, none)
            ]),
            Events2 = markdown_vec:set(Events1, ExitIndex, ExitEvent#markdown_event{point = EnterPoint}),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events2, map = EditMap2},
            Slice2 = Slice1#markdown_slice{bytes = binary:part(Bytes, 0, Index)},
            {cont, {Tokenizer2, Slice2}};
        _ ->
            {cont, {Tokenizer1, Slice1}}
    end.

%% @private
-spec trim_data_start(Tokenizer, Slice, ExitIndex) -> {cont | halt, {Tokenizer, Slice}} when
    Tokenizer :: markdown_tokenizer:t(),
    Slice :: markdown_slice:t(),
    ExitIndex :: markdown_vec:index().
trim_data_start(
    Tokenizer1 = #markdown_tokenizer{events = Events1 = #markdown_vec{}, map = EditMap1},
    Slice1 = #markdown_slice{bytes = Bytes, before = Before},
    ExitIndex
) ->
    BytesLen = byte_size(Bytes),
    Index = find_first_non_whitespace(Bytes, 0),
    case Index of
        BytesLen ->
            %% The whole data is whitespace.
            %% We can be very fast: we only change the event names.
            Events2 = markdown_vec:update(Events1, ExitIndex - 1, fun(Event) ->
                Event#markdown_event{name = space_or_tab}
            end),
            Events3 = markdown_vec:update(Events2, ExitIndex, fun(Event) ->
                Event#markdown_event{name = space_or_tab}
            end),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3},
            {halt, {Tokenizer2, Slice1}};
        _ when Index > 0 orelse Before > 0 ->
            EnterEvent = markdown_vec:get(Events1, ExitIndex - 1),
            EnterPoint = EnterEvent#markdown_event.point,
            ExitPoint = EnterPoint#markdown_point{
                offset = EnterPoint#markdown_point.offset + Index,
                column = EnterPoint#markdown_point.column + Index,
                virtual = 0
            },
            EditMap2 = markdown_edit_map:add(EditMap1, ExitIndex + 1, 0, [
                markdown_event:space_or_tab(enter, EnterPoint, none),
                markdown_event:space_or_tab(exit, ExitPoint, none)
            ]),
            Events2 = markdown_vec:set(Events1, ExitIndex - 1, EnterEvent#markdown_event{point = ExitPoint}),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events2, map = EditMap2},
            {cont, {Tokenizer2, Slice1}};
        _ ->
            {cont, {Tokenizer1, Slice1}}
    end.
