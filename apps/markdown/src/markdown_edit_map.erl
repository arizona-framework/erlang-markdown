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
-module(markdown_edit_map).
-moduledoc """
Deal with several changes in events, batching them together.

Preferably, changes should be kept to a minimum.
Sometimes, it’s needed to change the list of events, because parsing can be
messy, and it helps to expose a cleaner interface of events to the compiler
and other users.
It can also help to merge many adjacent similar events.
And, in other cases, it’s needed to parse subcontent: pass some events
through another tokenizer and inject the result.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    add/4,
    add_before/4,
    consume/2,
    new/0
]).

%% Types

-doc "Tracks a bunch of edits.".
-type t() :: #markdown_edit_map{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Create an edit: a remove and/or add at a certain place.
""".
-spec add(EditMap, Index, Remove, Add) -> EditMap when
    EditMap :: markdown_edit_map:t(),
    Index :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    Add :: markdown_vec:t(Event) | [Event],
    Event :: markdown_event:t().
add(EditMap = #markdown_edit_map{}, Index, Remove, Add) when
    ?is_usize(Index) andalso ?is_usize(Remove) andalso (is_record(Add, markdown_vec) orelse is_list(Add))
->
    AddVec =
        case Add of
            #markdown_vec{} ->
                Add;
            _ when is_list(Add) ->
                markdown_vec:from_list(Add)
        end,
    add_impl(EditMap, Index, Remove, AddVec, false).

-doc """
Create an edit: but insert `add` before existing additions.
""".
-spec add_before(EditMap, Index, Remove, Add) -> EditMap when
    EditMap :: markdown_edit_map:t(),
    Index :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    Add :: markdown_vec:t(Event) | [Event],
    Event :: markdown_event:t().
add_before(EditMap = #markdown_edit_map{}, Index, Remove, Add) when
    ?is_usize(Index) andalso ?is_usize(Remove) andalso (is_record(Add, markdown_vec) orelse is_list(Add))
->
    AddVec =
        case Add of
            #markdown_vec{} ->
                Add;
            _ when is_list(Add) ->
                markdown_vec:from_list(Add)
        end,
    add_impl(EditMap, Index, Remove, AddVec, true).

-doc """
Done, change the events.
""".
-spec consume(EditMap, Events) -> {EditMap, Events} when
    EditMap :: markdown_edit_map:t(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t().
consume(EditMap = #markdown_edit_map{map = #markdown_vec{size = 0}}, Events = #markdown_vec{}) ->
    % io:format("\n\n[~w] BEFORE edit_map:consume\n\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(EditMap), markdown_debug:rust_debug_string(Events)]),
    % io:format("\n\n[~w] AFTER edit_map:consume\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events)]),
    {EditMap, Events};
consume(EditMap = #markdown_edit_map{map = Map1}, Events1 = #markdown_vec{}) ->
    % io:format("\n\n[~w] BEFORE edit_map:consume\n\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(EditMap), markdown_debug:rust_debug_string(Events1)]),
    Map2 = markdown_vec:sort_by(Map1, fun(_, {A, _, _}, _, {B, _, _}) ->
        A =< B
    end),
    %% Calculate jumps: where items in the current list move to.
    {Jumps, _AddAcc, _RemoveAcc} = consume__calculate_jumps(Map2),
    % io:format("\n\n[~w] JUMPS edit_map:consume\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Jumps)]),
    % io:format("\n\n[~w] BEFORE edit_map:consume:shift_links\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events1)]),
    Events2 = shift_links(Events1, Jumps),
    % io:format("\n\n[~w] AFTER edit_map:consume:shift_links\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events2)]),
    MapIterator1 = markdown_vec:iterator(Map2, reversed),
    Vecs1 = markdown_vec:new(),
    Events3 = consume__split_events(MapIterator1, Events2, Vecs1),
    % io:format("\n\n[~w] AFTER edit_map:consume\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events3)]),
    {EditMap#markdown_edit_map{map = markdown_vec:new()}, Events3}.

-doc """
Create a new edit map.
""".
-spec new() -> EditMap when EditMap :: t().
new() ->
    #markdown_edit_map{
        map = markdown_vec:new()
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Create an edit.
""".
-spec add_impl(EditMap, At, Remove, AddVec, Before) -> EditMap when
    EditMap :: markdown_edit_map:t(),
    At :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    AddVec :: markdown_vec:t(markdown_event:t()),
    Before :: boolean().
add_impl(EditMap1 = #markdown_edit_map{map = Map1}, At, Remove, AddVec, Before) ->
    case Remove =:= 0 andalso markdown_vec:size(AddVec) =:= 0 of
        true ->
            EditMap1;
        false ->
            MapIterator = markdown_vec:iterator(Map1),
            Map2 = add_impl_loop(MapIterator, Map1, At, Remove, AddVec, Before),
            EditMap2 = EditMap1#markdown_edit_map{map = Map2},
            EditMap2
    end.

%% @private
-spec add_impl_loop(MapIterator, Map, At, Remove, AddVec, Before) -> Map when
    MapIterator :: markdown_vec:iterator(Entry),
    Entry :: {At, Remove, AddVec},
    Map :: markdown_vec:t(Entry),
    At :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    AddVec :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Before :: boolean().
add_impl_loop(MapIterator1, Map1, At, Remove, AddVec1, Before) ->
    case markdown_vec:next(MapIterator1) of
        none ->
            markdown_vec:push(Map1, {At, Remove, AddVec1});
        {Index, {At, MapRemove1, MapAddVec1}, _MapIterator2} ->
            MapRemove2 = MapRemove1 + Remove,
            MapAddVec2 =
                case Before of
                    true ->
                        markdown_vec:append(AddVec1, MapAddVec1);
                    false ->
                        markdown_vec:append(MapAddVec1, AddVec1)
                end,
            Map2 = markdown_vec:set(Map1, Index, {At, MapRemove2, MapAddVec2}),
            Map2;
        {_Index, _, MapIterator2} ->
            add_impl_loop(MapIterator2, Map1, At, Remove, AddVec1, Before)
    end.

%% @private
-spec consume__calculate_jumps(MapVec) -> {Jumps, AddAcc, RemoveAcc} when
    MapVec :: markdown_vec:t(MapEntry),
    MapEntry :: {markdown_types:usize(), markdown_types:usize(), markdown_vec:t(markdown_event:t())},
    Jumps :: markdown_vec:t(Jump),
    Jump :: {markdown_types:usize(), markdown_types:usize(), markdown_types:usize()},
    AddAcc :: markdown_types:usize(),
    RemoveAcc :: markdown_types:usize().
consume__calculate_jumps(MapVec = #markdown_vec{}) ->
    Jumps = markdown_vec:new(),
    consume__calculate_jumps_loop(markdown_vec:iterator(MapVec), Jumps, 0, 0).

%% @private
-spec consume__calculate_jumps_loop(MapIterator, Jumps, AddAcc, RemoveAcc) -> {Jumps, AddAcc, RemoveAcc} when
    MapIterator :: markdown_vec:iterator(MapEntry),
    MapEntry :: {markdown_types:usize(), markdown_types:usize(), markdown_vec:t(markdown_event:t())},
    Jumps :: markdown_vec:t(Jump),
    Jump :: {markdown_types:usize(), markdown_types:usize(), markdown_types:usize()},
    AddAcc :: markdown_types:usize(),
    RemoveAcc :: markdown_types:usize().
consume__calculate_jumps_loop(MapIterator, Jumps, AddAcc, RemoveAcc) ->
    case markdown_vec:next(MapIterator) of
        none ->
            {Jumps, AddAcc, RemoveAcc};
        {_Index, {At, Remove, Add}, NextMapIterator} ->
            NewRemoveAcc = RemoveAcc + Remove,
            NewAddAcc = AddAcc + markdown_vec:size(Add),
            NewJumps = markdown_vec:push(Jumps, {At, NewRemoveAcc, NewAddAcc}),
            consume__calculate_jumps_loop(NextMapIterator, NewJumps, NewAddAcc, NewRemoveAcc)
    end.

%% @private
-spec consume__split_events(MapIterator, InEvents, Vecs) -> OutEvents when
    MapIterator :: markdown_vec:iterator(MapEntry),
    MapEntry :: {At, Removed, Add},
    At :: markdown_types:usize(),
    Removed :: markdown_types:usize(),
    Add :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    InEvents :: markdown_vec:t(Event),
    Vecs :: markdown_vec:t(OutEvents),
    OutEvents :: markdown_vec:t(Event).
consume__split_events(MapIterator1, InEvents1 = #markdown_vec{}, Vecs1 = #markdown_vec{}) ->
    case markdown_vec:next(MapIterator1) of
        none ->
            {OutEvents1, SplitEvents1} = markdown_vec:split_off(InEvents1, 0),
            Vecs2 = markdown_vec:push(Vecs1, SplitEvents1),
            OutEvents2 = markdown_vec:reduce_right(Vecs2, OutEvents1, fun(_Index, Slice, OutEventsAcc) ->
                markdown_vec:append(OutEventsAcc, Slice)
            end),
            OutEvents2;
        {_Index, {At, Remove, AddVec}, MapIterator2} ->
            {InEvents2, SplitEvents1} = markdown_vec:split_off(InEvents1, At + Remove),
            Vecs2 = markdown_vec:push(Vecs1, SplitEvents1),
            {_, SplitEvents2} = markdown_vec:split_off(AddVec, 0),
            Vecs3 = markdown_vec:push(Vecs2, SplitEvents2),
            InEvents3 = markdown_vec:truncate(InEvents2, At),
            consume__split_events(MapIterator2, InEvents3, Vecs3)
    end.

%% @private
-doc """
Shift `previous` and `next` links according to `jumps`.

This fixes links in case there are events removed or added between them.
""".
-spec shift_links(Events, Jumps) -> Events when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Jumps :: markdown_vec:t(Jump),
    Jump :: {markdown_types:usize(), markdown_types:usize(), markdown_types:usize()}.
shift_links(Events1 = #markdown_vec{}, Jumps = #markdown_vec{}) ->
    shift_links_loop(Events1, Jumps, 0, 0, 0, 0).

% /// Shift `previous` and `next` links according to `jumps`.
% ///
% /// This fixes links in case there are events removed or added between them.
% fn shift_links(events: &mut [Event], jumps: &[(usize, usize, usize)]) {
%     let mut jump_index = 0;
%     let mut index = 0;
%     let mut add = 0;
%     let mut rm = 0;

%     while index < events.len() {
%         let rm_curr = rm;

%         while jump_index < jumps.len() && jumps[jump_index].0 <= index {
%             add = jumps[jump_index].2;
%             rm = jumps[jump_index].1;
%             jump_index += 1;
%         }

%         // Ignore items that will be removed.
%         if rm > rm_curr {
%             index += rm - rm_curr;
%         } else {
%             if let Some(link) = &events[index].link {
%                 if let Some(next) = link.next {
%                     events[next].link.as_mut().unwrap().previous = Some(index + add - rm);

%                     while jump_index < jumps.len() && jumps[jump_index].0 <= next {
%                         add = jumps[jump_index].2;
%                         rm = jumps[jump_index].1;
%                         jump_index += 1;
%                     }

%                     events[index].link.as_mut().unwrap().next = Some(next + add - rm);
%                     index = next;
%                     continue;
%                 }
%             }

%             index += 1;
%         }
%     }
% }

%% @private
-spec shift_links_loop(Events, Jumps, JumpIndex, Index, Add, Rm) -> Events when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Jumps :: markdown_vec:t(Jump),
    Jump :: {markdown_types:usize(), markdown_types:usize(), markdown_types:usize()},
    JumpIndex :: non_neg_integer(),
    Index :: non_neg_integer(),
    Add :: markdown_types:usize(),
    Rm :: markdown_types:usize().
shift_links_loop(
    Events1 = #markdown_vec{size = EventsLength}, Jumps = #markdown_vec{}, JumpIndex1, Index, Add1, Rm1
) when Index < EventsLength ->
    % io:format("\n\n[~w] INSIDE[before] edit_map:consume:shift_links\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events1)]),
    % io:format("\n\n[~w] BEFORE edit_map:consume:jump_index\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string({JumpIndex1, Index, Add1, Rm1})]),
    {JumpIndex2, Add2, Rm2} = shift_links_update_jumps(Jumps, JumpIndex1, Index, Add1, Rm1),
    % io:format("\n\n[~w] AFTER edit_map:consume:jump_index\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string({JumpIndex2, Index, Add2, Rm2})]),
    case Rm2 > Rm1 of
        true ->
            % Ignore items that will be removed.
            shift_links_loop(Events1, Jumps, JumpIndex2, Index + (Rm2 - Rm1), Add2, Rm2);
        false ->
            case markdown_vec:get(Events1, Index) of
                #markdown_event{link = {some, #markdown_event_link{next = {some, Next}}}} ->
                    % io:format("\n\n[~w] INSIDE[0] edit_map:consume:shift_links(index=~w, next=~w, jump_index=~w, add=~w, rm=~w)\n\n~ts\n\n", [markdown:counter_get(), Index, Next, JumpIndex2, Add2, Rm2, markdown_debug:rust_debug_string(Events1)]),
                    Events2 = markdown_vec:update(Events1, Next, fun(
                        NextEvent1 = #markdown_event{link = {some, NextLink1}}
                    ) ->
                        NextLink2 = NextLink1#markdown_event_link{previous = {some, Index + (Add2 - Rm2)}},
                        NextEvent2 = NextEvent1#markdown_event{link = {some, NextLink2}},
                        NextEvent2
                    end),
                    % io:format("\n\n[~w] INSIDE[1] edit_map:consume:shift_links(index=~w, next=~w, jump_index=~w, add=~w, rm=~w)\n\n~ts\n\n", [markdown:counter_get(), Index, Next, JumpIndex2, Add2, Rm2, markdown_debug:rust_debug_string(Events2)]),
                    {JumpIndex3, Add3, Rm3} = shift_links_update_jumps(Jumps, JumpIndex2, Next, Add2, Rm2),
                    Events3 = markdown_vec:update(Events2, Index, fun(Event1 = #markdown_event{link = {some, Link1}}) ->
                        Link2 = Link1#markdown_event_link{next = {some, Next + (Add3 - Rm3)}},
                        Event2 = Event1#markdown_event{link = {some, Link2}},
                        Event2
                    end),
                    % io:format("\n\n[~w] INSIDE[2] edit_map:consume:shift_links(index=~w, next=~w, jump_index=~w, add=~w, rm=~w)\n\n~ts\n\n", [markdown:counter_get(), Index, Next, JumpIndex3, Add3, Rm3, markdown_debug:rust_debug_string(Events3)]),
                    % io:format("\n\n[~w] INSIDE[after] edit_map:consume:shift_links\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events3)]),
                    shift_links_loop(Events3, Jumps, JumpIndex3, Next, Add3, Rm3);
                _ ->
                    shift_links_loop(Events1, Jumps, JumpIndex2, Index + 1, Add2, Rm2)
            end
    end;
shift_links_loop(Events, _Jumps, _JumpIndex, _Index, _Add, _Rm) ->
    Events.

%% @private
-spec shift_links_update_jumps(Jumps, JumpIndex, Index, Add, Rm) -> {JumpIndex, Add, Rm} when
    Jumps :: markdown_vec:t(Jump),
    Jump :: {markdown_types:usize(), markdown_types:usize(), markdown_types:usize()},
    JumpIndex :: non_neg_integer(),
    Index :: non_neg_integer(),
    Add :: markdown_types:usize(),
    Rm :: markdown_types:usize().
shift_links_update_jumps(Jumps = #markdown_vec{size = JumpsLength}, JumpIndex, Index, Add1, Rm1) when
    JumpIndex < JumpsLength
->
    case markdown_vec:get(Jumps, JumpIndex) of
        {JumpAt, JumpRm, JumpAdd} when JumpAt =< Index ->
            Add2 = JumpAdd,
            Rm2 = JumpRm,
            shift_links_update_jumps(Jumps, JumpIndex + 1, Index, Add2, Rm2);
        _ ->
            {JumpIndex, Add1, Rm1}
    end;
shift_links_update_jumps(_Jumps = #markdown_vec{}, JumpIndex, _Index, Add, Rm) ->
    {JumpIndex, Add, Rm}.
