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
-module(markdown_position).
-moduledoc """
Location of a node in a source file.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    fmt/1,
    from_exit_event/2,
    new/2,
    new/6,
    to_indices/1
]).

%% Types
-doc """
Location of a node in a source file.
""".
-type t() :: #markdown_position{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec fmt(Position) -> {Format, Data} when Position :: t(), Format :: io:format(), Data :: [term()].
fmt(Position = #markdown_position{}) ->
    Start = Position#markdown_position.start,
    End = Position#markdown_position.'end',
    {"~w:~w-~w:~w (~w-~w)", [
        Start#markdown_point.line,
        Start#markdown_point.column,
        End#markdown_point.line,
        End#markdown_point.column,
        Start#markdown_point.offset,
        End#markdown_point.offset
    ]}.

-doc """
Get a position from an exit event.

Looks backwards for the corresponding `enter` event.
This does not support nested events (such as lists in lists).

## Panics

This function panics if an enter event is given.
When `markdown-rs` is used, this function never panics.
""".
-spec from_exit_event(Events, Index) -> Position when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), Index :: markdown_types:usize(), Position :: t().
from_exit_event(Events = #markdown_vec{size = EventsLength}, Index) when Index >= 0 andalso Index < EventsLength ->
    %% BEGIN: assertions
    #markdown_event{kind = 'exit'} = markdown_vec:get(Events, Index),
    %% END: assertions
    #markdown_event{name = Name, point = End} = markdown_vec:get(Events, Index),
    EnterIndex = from_exit_event__find_enter_event(Events, Index - 1, Name),
    #markdown_event{point = Start} = markdown_vec:get(Events, EnterIndex),
    new(Start, End).

-spec new(Start, End) -> Position when
    Start :: markdown_point:t(), End :: markdown_point:t(), Position :: t().
new(Start = #markdown_point{}, End = #markdown_point{}) ->
    #markdown_position{start = Start, 'end' = End}.

-spec new(StartLine, StartColumn, StartOffset, EndLine, EndColumn, EndOffset) -> Position when
    StartLine :: markdown_point:line(),
    StartColumn :: markdown_point:column(),
    StartOffset :: markdown_point:offset(),
    EndLine :: markdown_point:line(),
    EndColumn :: markdown_point:column(),
    EndOffset :: markdown_point:offset(),
    Position :: t().
new(StartLine, StartColumn, StartOffset, EndLine, EndColumn, EndOffset) when
    ?is_pos_integer(StartLine) andalso ?is_pos_integer(StartColumn) andalso ?is_non_neg_integer(StartOffset) andalso
        ?is_pos_integer(EndLine) andalso ?is_pos_integer(EndColumn) andalso ?is_non_neg_integer(EndOffset)
->
    Start = markdown_point:new(StartLine, StartColumn, StartOffset, 0),
    End = markdown_point:new(EndLine, EndColumn, EndOffset, 0),
    new(Start, End).

-doc """
Turn a position into indices.

Indices are places in `bytes` where this position starts and ends.

> 👉 **Note**: indices cannot represent virtual spaces.
""".
-spec to_indices(Position) -> Indices when Position :: t(), Indices :: markdown_indices:t().
to_indices(#markdown_position{start = #markdown_point{offset = StartIndex}, 'end' = #markdown_point{offset = EndIndex}}) ->
    markdown_indices:new(StartIndex, EndIndex).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec from_exit_event__find_enter_event(Events, Index, Name) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Name :: markdown_event:name().
from_exit_event__find_enter_event(Events = #markdown_vec{size = EventsLength}, Index, Name) when
    Index >= 0 andalso Index < EventsLength
->
    case markdown_vec:get(Events, Index) of
        #markdown_event{kind = enter, name = Name} ->
            Index;
        _ ->
            from_exit_event__find_enter_event(Events, Index - 1, Name)
    end.
