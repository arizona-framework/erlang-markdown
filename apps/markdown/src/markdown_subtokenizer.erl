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
-module(markdown_subtokenizer).
-moduledoc """
Deal with content in other content.

To deal with content in content, *you* (a `markdown-rs` contributor) add
info on events.
Events are a flat list, but they can be connected to each other with a
[`Link`][crate::event::Link].
Links must occur on [`Enter`][Kind::Enter] events only, which are void
(they are followed by their corresponding [`Exit`][Kind::Exit] event).

Links will then be passed through a tokenizer for the corresponding content
type by `subtokenize`.
The subevents they result in are split up into slots for each linked event
and replace those links.

Subevents are not immediately subtokenized as markdown prevents us from
doing so due to definitions, which can occur after references, and thus the
whole document needs to be parsed up to the level of definitions, before
any level that can include references can be parsed.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-05", modified => "2025-03-05"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    link/2,
    link_to/3,
    subtokenize/3,
    divide_events/5
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Link two [`Event`][]s.

Arbitrary (void) events can be linked together.
This optimizes for the common case where the event at `index` is connected
to the previous void event.
""".
-spec link(Events, Index) -> Events when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), Index :: markdown_types:usize().
link(Events = #markdown_vec{size = EventsLength}, Index) when
    ?is_usize(Index) andalso Index >= 2 andalso Index < EventsLength
->
    link_to(Events, Index - 2, Index).

-doc """
Link two arbitrary [`Event`][]s together.
""".
-spec link_to(Events, Previous, Next) -> Events when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Previous :: markdown_types:usize(),
    Next :: markdown_types:usize().
link_to(Events1 = #markdown_vec{size = Events1Length}, Previous, Next) when
    ?is_usize(Previous) andalso ?is_usize(Next) andalso (Previous + 1) < Events1Length andalso Next < Events1Length
->
    PrevEvent1 = markdown_vec:get(Events1, Previous),
    NextEvent1 = markdown_vec:get(Events1, Next),

    %% BEGIN: assertions
    ExitEvent = markdown_vec:get(Events1, Previous + 1),
    #markdown_event{name = EnterEventName} = PrevEvent1,
    #markdown_event{name = ExitEventName} = ExitEvent,
    #markdown_event{name = NextEventName} = NextEvent1,
    ?assertMatch(#markdown_event{kind = 'enter'}, PrevEvent1),
    ?assert(markdown_event:is_void(EnterEventName), "expected previous event to be void"),
    ?assertMatch(#markdown_event{kind = 'exit'}, ExitEvent),
    ?assertEqual(EnterEventName, ExitEventName),
    ?assertMatch(#markdown_event{kind = 'enter'}, NextEvent1),
    ?assert(markdown_event:is_void(NextEventName), "expected next event to be void"),
    %% Note: the exit of this event may not exist, so don't check for that.
    %% END: assertions

    {some, PrevLink1} = PrevEvent1#markdown_event.link,
    PrevLink2 = PrevLink1#markdown_event_link{next = {some, Next}},
    PrevEvent2 = PrevEvent1#markdown_event{link = {some, PrevLink2}},

    {some, NextLink1} = NextEvent1#markdown_event.link,
    NextLink2 = NextLink1#markdown_event_link{previous = {some, Previous}},
    NextEvent2 = NextEvent1#markdown_event{link = {some, NextLink2}},

    %% BEGIN: assertions
    #markdown_event_link{content = PrevLinkContent} = PrevLink2,
    #markdown_event_link{content = NextLinkContent} = NextLink2,
    ?assertEqual(PrevLinkContent, NextLinkContent),
    %% END: assertions

    Events2 = markdown_vec:set(Events1, Previous, PrevEvent2),
    Events3 = markdown_vec:set(Events2, Next, NextEvent2),

    Events3.

-doc """
Parse linked events.

Supposed to be called repeatedly, returns `true` when done.
""".
-spec subtokenize(Events, ParseState, OptionFilter) -> {Events, Result} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    OptionFilter :: markdown_types:option(Content),
    Content :: markdown_event:content(),
    Result :: {ok, Subresult} | {error, Message},
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
subtokenize(Events1 = #markdown_vec{}, ParseState = #markdown_parse_state{}, OptionFilter) ->
    EditMap1 = markdown_edit_map:new(),
    Subresult1 = #markdown_subresult{
        done = true,
        gfm_footnote_definitions = markdown_vec:new(),
        definitions = markdown_vec:new()
    },
    AccRm = 0,
    AccAdd = 0,
    {{Events2, EditMap2}, Result} = subtokenize__events_loop(
        0, Events1, ParseState, OptionFilter, EditMap1, Subresult1, AccRm, AccAdd
    ),
    case Result of
        {ok, Subresult2} ->
            {_EditMap3, Events3} = markdown_edit_map:consume(EditMap2, Events2),
            {Events3, {ok, Subresult2}};
        {error, Message} ->
            {Events2, {error, Message}}
    end.

-doc """
Divide `child_events` over links in `events`, the first of which is at
`link_index`.
""".
-spec divide_events(EditMap, Events, LinkIndex, ChildEvents, AccBefore) -> {EditMap, ChildEvents, AccRm, AccAdd} when
    EditMap :: markdown_edit_map:t(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    LinkIndex :: markdown_types:usize(),
    ChildEvents :: markdown_vec:t(Event),
    AccBefore :: {AccBeforeRm, AccBeforeAdd},
    AccBeforeRm :: markdown_types:usize(),
    AccBeforeAdd :: markdown_types:usize(),
    AccRm :: markdown_types:usize(),
    AccAdd :: markdown_types:usize().
divide_events(
    EditMap1 = #markdown_edit_map{},
    Events = #markdown_vec{},
    LinkIndex,
    ChildEvents1 = #markdown_vec{},
    {AccBeforeRm, AccBeforeAdd}
) when ?is_usize(LinkIndex) andalso ?is_usize(AccBeforeRm) andalso ?is_usize(AccBeforeAdd) ->
    % Loop through `child_events` to figure out which parts belong where and
    % fix deep links.
    EventsLen = markdown_vec:size(Events),
    ChildEventsLen = markdown_vec:size(ChildEvents1),
    Slices1 = markdown_vec:new(),
    SliceStart = 0,
    OldPrev = none,
    {ChildEvents2, Slices2} = divide_events__child_event_loop(
        0, Events, LinkIndex, ChildEvents1, AccBeforeRm, AccBeforeAdd, Slices1, SliceStart, OldPrev
    ),
    SlicesLen = markdown_vec:size(Slices2),
    SlicesIterator = markdown_vec:iterator(Slices2, reversed),
    %% Finally, inject the subevents.
    {EditMap2, ChildEvents3} = divide_events__slice_loop(SlicesIterator, EditMap1, ChildEvents2, EventsLen),
    {EditMap2, ChildEvents3, AccBeforeRm + (SlicesLen * 2), AccBeforeAdd + ChildEventsLen}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec divide_events__child_event_loop(
    ChildIndex, Events, LinkIndex, ChildEvents, AccBeforeRm, AccBeforeAdd, Slices, SliceStart, OptionOldPrev
) -> {ChildEvents, Slices} when
    ChildIndex :: markdown_types:usize(),
    Event :: markdown_event:t(),
    Events :: markdown_vec:t(Event),
    LinkIndex :: markdown_types:usize(),
    ChildEvents :: markdown_vec:t(Event),
    AccBeforeRm :: markdown_types:usize(),
    AccBeforeAdd :: markdown_types:usize(),
    Slices :: markdown_vec:t(Slice),
    Slice :: {LinkIndex, SliceStart},
    SliceStart :: markdown_types:usize(),
    OptionOldPrev :: markdown_types:option(OldPrev),
    OldPrev :: markdown_types:usize().
divide_events__child_event_loop(
    ChildIndex1,
    Events,
    LinkIndex1,
    ChildEvents1,
    AccBeforeRm,
    AccBeforeAdd,
    Slices1,
    SliceStart1,
    OptionOldPrev1
) when ChildIndex1 >= 0 andalso ChildIndex1 < ?markdown_vec_size(ChildEvents1) ->
    % io:format("\n\n[~w] INSIDE[0] subtokenize:divide_events(child_index=~w)\n~ts\n~ts\n~tp\n\n", [markdown:counter_get(), ChildIndex1, markdown_debug:rust_debug_string(OptionOldPrev1), markdown_debug:rust_debug_string(markdown_vec:get(ChildEvents1, ChildIndex1)), erlang:process_info(self(), current_stacktrace)]),
    % io:format("\n\n[~w] INSIDE[0] subtokenize:divide_events(child_index=~w, link_index=~w, acc_before_rm=~w, acc_before_add=~w, slice_start=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex1, LinkIndex1, AccBeforeRm, AccBeforeAdd, SliceStart1, markdown_debug:rust_debug_string(OptionOldPrev1), markdown_debug:rust_debug_string(markdown_vec:get(ChildEvents1, ChildIndex1))]),
    Current = (markdown_vec:get(ChildEvents1, ChildIndex1))#markdown_event.point,
    End = (markdown_vec:get(Events, LinkIndex1 + 1))#markdown_event.point,
    %% Find the first event that starts after the end we're looking for.
    {Slices2, LinkIndex2, SliceStart2} = divide_events__first_event_after_the_end(
        Events, Current, End, ChildIndex1, {Slices1, LinkIndex1, SliceStart1}
    ),
    % io:format("\n\n[~w] INSIDE[1] subtokenize:divide_events(child_index=~w, link_index=~w, acc_before_rm=~w, acc_before_add=~w, slice_start=~w):find_first_event\n~ts\n~w\n~w\n\n", [markdown:counter_get(), ChildIndex1, LinkIndex2, AccBeforeRm, AccBeforeAdd, SliceStart2, markdown_debug:rust_debug_string(Slices2), SliceStart2, LinkIndex2]),
    %% Fix sublinks.
    % io:format("\n\n[~w] BEFORE[2] subtokenize:divide_events(child_index=~w):fix_sublinks1\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex1, markdown_debug:rust_debug_string(OptionOldPrev1), markdown_debug:rust_debug_string(ChildEvents1)]),
    ChildEvents2 = divide_events__fix_sublinks1(
        ChildIndex1, LinkIndex2, ChildEvents1, AccBeforeRm, AccBeforeAdd, Slices2, OptionOldPrev1
    ),
    % io:format("\n\n[~w] AFTER[2] subtokenize:divide_events(child_index=~w):fix_sublinks1\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex1, markdown_debug:rust_debug_string(OptionOldPrev1), markdown_debug:rust_debug_string(ChildEvents2)]),
    %% If there is a `next` link in the subevents, we have to change
    %% its `previous` index to account for the shifted events.
    %% If it points to a next event, we also change the next event’s
    %% reference back to *this* event.
    {ChildEvents3, OptionOldPrev2} = divide_events__fix_sublinks2(
        ChildIndex1, LinkIndex2, ChildEvents2, AccBeforeRm, AccBeforeAdd, Slices2, OptionOldPrev1
    ),
    ChildIndex2 = ChildIndex1 + 1,
    divide_events__child_event_loop(
        ChildIndex2, Events, LinkIndex2, ChildEvents3, AccBeforeRm, AccBeforeAdd, Slices2, SliceStart2, OptionOldPrev2
    );
divide_events__child_event_loop(
    _ChildIndex, _Events, LinkIndex, ChildEvents1, _AccBeforeRm, _AccBeforeAdd, Slices1, SliceStart, _OptionOldPrev
) ->
    Slices2 =
        case not markdown_vec:is_empty(ChildEvents1) of
            true ->
                markdown_vec:push(Slices1, {LinkIndex, SliceStart});
            false ->
                Slices1
        end,
    {ChildEvents1, Slices2}.

%% @private
-compile({inline, [divide_events__first_event_after_the_end/5]}).
-spec divide_events__first_event_after_the_end(Events, Current, End, ChildIndex, {Slices, LinkIndex, SliceStart}) ->
    {Slices, LinkIndex, SliceStart}
when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Current :: markdown_point:t(),
    End :: markdown_point:t(),
    ChildIndex :: markdown_types:usize(),
    Slices :: markdown_vec:t(Slice),
    Slice :: {LinkIndex, SliceStart},
    LinkIndex :: markdown_types:usize(),
    SliceStart :: markdown_types:usize().
divide_events__first_event_after_the_end(Events, Current, End, ChildIndex, {Slices1, LinkIndex1, SliceStart1}) when
    Current#markdown_point.offset > End#markdown_point.offset orelse
        (Current#markdown_point.offset =:= End#markdown_point.offset andalso
            Current#markdown_point.virtual > End#markdown_point.virtual)
->
    Slices2 = markdown_vec:push(Slices1, {LinkIndex1, SliceStart1}),
    SliceStart2 = ChildIndex,
    #markdown_event{link = {some, #markdown_event_link{next = {some, LinkIndex2}}}} = markdown_vec:get(
        Events, LinkIndex1
    ),
    {Slices2, LinkIndex2, SliceStart2};
divide_events__first_event_after_the_end(
    _Events, _Current, _End, _ChildIndex, State = {_Slices, _LinkIndex, _SliceStart}
) ->
    State.

%% @private
-compile({inline, [divide_events__fix_sublinks1/7]}).
-spec divide_events__fix_sublinks1(
    ChildIndex, LinkIndex, ChildEvents, AccBeforeRm, AccBeforeAdd, Slices, OptionOldPrev
) -> ChildEvents when
    ChildIndex :: markdown_types:usize(),
    LinkIndex :: markdown_types:usize(),
    ChildEvents :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    AccBeforeRm :: markdown_types:usize(),
    AccBeforeAdd :: markdown_types:usize(),
    Slices :: markdown_vec:t(Slice),
    Slice :: {LinkIndex, SliceStart},
    SliceStart :: markdown_types:usize(),
    OptionOldPrev :: markdown_types:option(OldPrev),
    OldPrev :: markdown_types:usize().
divide_events__fix_sublinks1(ChildIndex, LinkIndex, ChildEvents1, AccBeforeRm, AccBeforeAdd, Slices, OptionOldPrev) ->
    case markdown_vec:get(ChildEvents1, ChildIndex) of
        #markdown_event{link = {some, #markdown_event_link{previous = {some, _}}}} ->
            {some, OldPrev} = OptionOldPrev,
            PrevEvent1 = #markdown_event{link = {some, PrevLink1}} = markdown_vec:get(ChildEvents1, OldPrev),
            % io:format("\n\n[~w] BEFORE subtokenize:divide_events(child_index=~w, old_prev=~w):fix_sublinks1\n~ts\n\n", [markdown:counter_get(), ChildIndex, OldPrev, markdown_debug:rust_debug_string(PrevEvent1)]),
            %% The `index` in `events` where the current link is,
            %% minus one to get the previous link,
            %% minus 2 events (the enter and exit) for each removed
            %% link.
            NewLink =
                case markdown_vec:is_empty(Slices) of
                    true ->
                        OldPrev + LinkIndex + 2;
                    false ->
                        OldPrev + LinkIndex - ((markdown_vec:size(Slices) - 1) * 2)
                end,
            PrevLink2 = PrevLink1#markdown_event_link{next = {some, NewLink + AccBeforeAdd - AccBeforeRm}},
            PrevEvent2 = PrevEvent1#markdown_event{link = {some, PrevLink2}},
            % io:format("\n\n[~w] AFTER subtokenize:divide_events(child_index=~w, old_prev=~w):fix_sublinks1\n~ts\n\n", [markdown:counter_get(), ChildIndex, OldPrev, markdown_debug:rust_debug_string(PrevEvent2)]),
            ChildEvents2 = markdown_vec:set(ChildEvents1, OldPrev, PrevEvent2),
            ChildEvents2;
        _ ->
            ChildEvents1
    end.

%% @private
-compile({inline, [divide_events__fix_sublinks2/7]}).
-spec divide_events__fix_sublinks2(
    ChildIndex, LinkIndex, ChildEvents, AccBeforeRm, AccBeforeAdd, Slices, OptionOldPrev
) -> {ChildEvents, OptionOldPrev} when
    ChildIndex :: markdown_types:usize(),
    LinkIndex :: markdown_types:usize(),
    ChildEvents :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    AccBeforeRm :: markdown_types:usize(),
    AccBeforeAdd :: markdown_types:usize(),
    Slices :: markdown_vec:t(Slice),
    Slice :: {LinkIndex, SliceStart},
    SliceStart :: markdown_types:usize(),
    OptionOldPrev :: markdown_types:option(OldPrev),
    OldPrev :: markdown_types:usize().
divide_events__fix_sublinks2(ChildIndex, LinkIndex, ChildEvents1, AccBeforeRm, AccBeforeAdd, Slices, OptionOldPrev1) ->
    % io:format("\n\n[~w] INSIDE[2] subtokenize:divide_events(child_index=~w)\n~ts\n\n", [markdown:counter_get(), ChildIndex, markdown_debug:rust_debug_string(markdown_vec:get(ChildEvents1, ChildIndex))]),
    case markdown_vec:get(ChildEvents1, ChildIndex) of
        #markdown_event{link = {some, #markdown_event_link{next = {some, Next}}}} ->
            NextEvent1 = #markdown_event{link = {some, NextLink1}} = markdown_vec:get(ChildEvents1, Next),
            % io:format("\n\n[~w] BEFORE subtokenize:divide_events(child_index=~w, link_index=~w, acc_before_add=~w, add_before_rm=~w, next=~w):fix_sublinks2\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex, LinkIndex, AccBeforeAdd, AccBeforeRm, Next, markdown_debug:rust_debug_string(OptionOldPrev1), markdown_debug:rust_debug_string(NextEvent1)]),
            OptionOldPrev2 = NextLink1#markdown_event_link.previous,
            % io:format("\n\n[~w] INSIDE[1] subtokenize:divide_events(child_index=~w, next=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex, Next, markdown_debug:rust_debug_string(markdown_vec:get(ChildEvents1, ChildIndex)), markdown_debug:rust_debug_string(OptionOldPrev2)]),
            %% The `index` in `events` where the current link is,
            %% minus 2 events (the enter and exit) for each removed
            %% link.
            DoubleSlicesLen = (markdown_vec:size(Slices) * 2),
            NextLink2 = NextLink1#markdown_event_link{
                previous = markdown_option:map(NextLink1#markdown_event_link.previous, fun(Previous) ->
                    Previous + LinkIndex - DoubleSlicesLen + AccBeforeAdd - AccBeforeRm
                end)
            },
            NextEvent2 = NextEvent1#markdown_event{link = {some, NextLink2}},
            % io:format("\n\n[~w] AFTER subtokenize:divide_events(child_index=~w, link_index=~w, acc_before_add=~w, add_before_rm=~w, next=~w):fix_sublinks2\n~ts\n~ts\n\n", [markdown:counter_get(), ChildIndex, LinkIndex, AccBeforeAdd, AccBeforeRm, Next, markdown_debug:rust_debug_string(OptionOldPrev2), markdown_debug:rust_debug_string(NextEvent2)]),
            ChildEvents2 = markdown_vec:set(ChildEvents1, Next, NextEvent2),
            {ChildEvents2, OptionOldPrev2};
        _ ->
            {ChildEvents1, OptionOldPrev1}
    end.

%% @private
-spec divide_events__slice_loop(SlicesIterator, EditMap, ChildEvents, EventsLen) -> {EditMap, ChildEvents} when
    SlicesIterator :: markdown_vec:iterator(Slice),
    Slice :: {LinkIndex, SliceStart},
    LinkIndex :: markdown_types:usize(),
    SliceStart :: markdown_types:usize(),
    EditMap :: markdown_edit_map:t(),
    ChildEvents :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    EventsLen :: markdown_types:usize().
divide_events__slice_loop(SlicesIterator1, EditMap1 = #markdown_edit_map{}, ChildEvents1 = #markdown_vec{}, EventsLen) ->
    case markdown_vec:next(SlicesIterator1) of
        none ->
            {EditMap1, ChildEvents1};
        {_Index, {LinkIndex, SliceStart}, SlicesIterator2} when LinkIndex < EventsLen ->
            {ChildEvents2, Add} = markdown_vec:split_off(ChildEvents1, SliceStart),
            EditMap2 = markdown_edit_map:add(EditMap1, LinkIndex, 2, Add),
            divide_events__slice_loop(SlicesIterator2, EditMap2, ChildEvents2, EventsLen)
    end.

%% @private
-spec subtokenize__event(Event, Index, Events, ParseState, EditMap, Subresult, AccRm, AccAdd) ->
    {{Events, EditMap}, Result}
when
    Index :: markdown_vec:index(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    EditMap :: markdown_edit_map:t(),
    Subresult :: markdown_subresult:t(),
    AccRm :: markdown_types:usize(),
    AccAdd :: markdown_types:usize(),
    Result :: {ok, {Subresult, AccRm, AccAdd}} | {error, Message},
    Message :: markdown_message:t().
subtokenize__event(Event, Index, Events1, ParseState, EditMap1, Subresult1, AccRm1, AccAdd1) ->
    #markdown_event{link = {some, #markdown_event_link{content = LinkContent}}} = Event,
    %% Index into `events` pointing to a chunk.
    LinkIndex = {some, Index},
    %% Subtokenizer.
    Tokenizer1 =
        #markdown_tokenizer{tokenize_state = TokenizeState1} = markdown_tokenizer:new(
            Event#markdown_event.point, ParseState
        ),
    %% Substate.
    State1 = markdown_state:next(
        case LinkContent of
            content -> content_definition_before;
            string -> string_start;
            _ -> text_start
        end
    ),
    %% Check if this is the first paragraph, after zero or more
    %% definitions (or a blank line), in a list item.
    %% Used for GFM task list items.
    TokenizeState2 =
        case ParseState#markdown_parse_state.options#markdown_parse_options.constructs of
            #markdown_construct_options{gfm_task_list_item = true} when Index > 2 ->
                case markdown_vec:get(Events1, Index - 1) of
                    #markdown_event{kind = 'enter', name = 'paragraph'} ->
                        Before = markdown_util_skip:opt_back(
                            Events1,
                            Index - 2,
                            [blank_line_ending, definition, line_ending, space_or_tab]
                        ),
                        case markdown_vec:get(Events1, Before) of
                            #markdown_event{kind = 'exit', name = 'list_item_prefix'} ->
                                TokenizeState1#markdown_tokenize_state{
                                    document_at_first_paragraph_of_list_item = true
                                };
                            _ ->
                                TokenizeState1
                        end;
                    _ ->
                        TokenizeState1
                end;
            _ ->
                TokenizeState1
        end,
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    %% Loop through links to pass them in order to the subtokenizer.
    % io:format("\n\n[~w] BEFORE subtokenize:link\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events1), markdown_debug:rust_debug_string(Tokenizer2)]),
    {Tokenizer3, State2} = subtokenize__link_while(LinkIndex, Events1, Tokenizer2, State1),
    % io:format("\n\n[~w] AFTER subtokenize:link\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer3)]),
    % io:format("\n\n[~w] BEFORE subtokenize:flush\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State2), markdown_debug:rust_debug_string(Tokenizer3)]),
    case markdown_tokenizer:flush(Tokenizer3, State2, true) of
        {Tokenizer4, {ok, NestedSubresult}} ->
            % io:format("\n\n[~w] AFTER subtokenize:flush\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State2), markdown_debug:rust_debug_string(Tokenizer4)]),
            Subresult2 = markdown_subresult:append(Subresult1, NestedSubresult),
            Subresult3 = Subresult2#markdown_subresult{done = false},
            ChildEvents1 = Tokenizer4#markdown_tokenizer.events,
            % io:format("\n\n[~w] BEFORE subtokenize:divide_events(index=~w, acc_rm=~w, acc_add=~w)\n~ts\n\n", [markdown:counter_get(), Index, AccRm1, AccAdd1, markdown_debug:rust_debug_string(EditMap1)]),
            {EditMap2, _ChildEvents2, AccRm2, AccAdd2} = divide_events(
                EditMap1, Events1, Index, ChildEvents1, {AccRm1, AccAdd1}
            ),
            % io:format("\n\n[~w] AFTER subtokenize:divide_events(index=~w, acc_rm=~w, acc_add=~w)\n~ts\n\n", [markdown:counter_get(), Index, AccRm2, AccAdd2, markdown_debug:rust_debug_string(EditMap2)]),
            % io:format("\n\n[~w] AFTER subtokenize:event\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Event), markdown_debug:rust_debug_string(EditMap2)]),
            {{Events1, EditMap2}, {ok, {Subresult3, AccRm2, AccAdd2}}};
        {_Tokenizer4, {error, Message}} ->
            {{Events1, EditMap1}, {error, Message}}
    end.

%% @private
-spec subtokenize__events_loop(Index, Events, ParseState, OptionFilter, EditMap, Subresult, AccRm, AccAdd) ->
    {{Events, EditMap}, Result}
when
    Index :: markdown_vec:index(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    OptionFilter :: markdown_types:option(Content),
    Content :: markdown_event:content(),
    EditMap :: markdown_edit_map:t(),
    Subresult :: markdown_subresult:t(),
    AccRm :: markdown_types:usize(),
    AccAdd :: markdown_types:usize(),
    Result :: {ok, Subresult} | {error, Message},
    Message :: markdown_message:t().
subtokenize__events_loop(Index, Events1, ParseState, OptionFilter, EditMap1, Subresult1, AccRm1, AccAdd1) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events1)
->
    Event = markdown_vec:get(Events1, Index),
    % io:format("\n\n[~w] BEFORE subtokenize:event(index=~w, acc_rm=~w, acc_add=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), Index, AccRm1, AccAdd1, markdown_debug:rust_debug_string(Event), markdown_debug:rust_debug_string(EditMap1)]),
    {{Events2, EditMap2}, Result} =
        case Event#markdown_event.link of
            {some, Link} ->
                %% BEGIN: assertions
                ?assertEqual(enter, Event#markdown_event.kind),
                %% END: assertions
                %% No need to enter linked events again.
                case Link of
                    #markdown_event_link{content = LinkContent, previous = LinkOptionPrevious} when
                        ?is_option_none(LinkOptionPrevious) andalso
                            (?is_option_none(OptionFilter) orelse element(2, OptionFilter) =:= LinkContent)
                    ->
                        %% BEGIN: assertions
                        ?assertNotEqual(flow, LinkContent, "cannot use flow as subcontent yet"),
                        %% END: assertions
                        subtokenize__event(Event, Index, Events1, ParseState, EditMap1, Subresult1, AccRm1, AccAdd1);
                    _ ->
                        {{Events1, EditMap1}, {ok, {Subresult1, AccRm1, AccAdd1}}}
                end;
            none ->
                {{Events1, EditMap1}, {ok, {Subresult1, AccRm1, AccAdd1}}}
        end,
    case Result of
        {ok, {Subresult2, AccRm2, AccAdd2}} ->
            % io:format("\n\n[~w] AFTER subtokenize:event(index=~w, acc_rm=~w, acc_add=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), Index + 1, AccRm2, AccAdd2, markdown_debug:rust_debug_string(Event), markdown_debug:rust_debug_string(EditMap2)]),
            subtokenize__events_loop(
                Index + 1, Events2, ParseState, OptionFilter, EditMap2, Subresult2, AccRm2, AccAdd2
            );
        {error, _Message} ->
            {{Events2, EditMap2}, Result}
    end;
subtokenize__events_loop(_Index, Events, _ParseState, _OptionFilter, EditMap, Subresult, _AccRm, _AccAdd) ->
    {{Events, EditMap}, {ok, Subresult}}.

%% @private
-spec subtokenize__link_while(OptionIndex, Events, Tokenizer, State) -> {Tokenizer, State} when
    OptionIndex :: markdown_types:option(Index),
    Index :: markdown_types:usize(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Tokenizer :: markdown_tokenizer:t(),
    State :: markdown_state:t().
subtokenize__link_while({some, Index}, Events, Tokenizer1, State1) ->
    EnterEvent = #markdown_event{link = {some, EnterLink}, point = EnterPoint} = markdown_vec:get(Events, Index),
    %% BEGIN: assertions
    ?assertEqual('enter', EnterEvent#markdown_event.kind),
    %% END: assertions
    Tokenizer2 =
        case EnterLink of
            #markdown_event_link{previous = {some, _}} ->
                markdown_tokenizer:define_skip(Tokenizer1, EnterPoint);
            _ ->
                Tokenizer1
        end,
    #markdown_event{point = EndPoint} = markdown_vec:get(Events, Index + 1),
    EnterIndex = markdown_point:to_index(EnterPoint),
    EndIndex = markdown_point:to_index(EndPoint),
    {Tokenizer3, State2} = markdown_tokenizer:push(Tokenizer2, EnterIndex, EndIndex, State1),
    subtokenize__link_while(EnterLink#markdown_event_link.next, Events, Tokenizer3, State2);
subtokenize__link_while(none, _Events, Tokenizer, State) ->
    {Tokenizer, State}.
