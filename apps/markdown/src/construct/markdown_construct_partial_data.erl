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
-module(markdown_construct_partial_data).
-moduledoc """
Data occurs in the [string][] and [text][] content types.

It can include anything (except for line endings) and stops at certain
characters.

[string]: crate::construct::string
[text]: crate::construct::text
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    at_break/1,
    inside/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At beginning of data.

```markdown
> | abc
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Byte}, tokenize_state = #markdown_tokenize_state{markers = Markers}
    }
) ->
    %% Make sure to eat the first `markers`.
    case binary:match(Markers, <<Byte:8>>) of
        {_Start, _Length} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(data_inside),
            {Tokenizer3, State};
        nomatch ->
            State = markdown_state:retry(data_at_break),
            {Tokenizer1, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(data_at_break),
    {Tokenizer, State}.

-doc """
Before something.

```markdown
> | abc
    ^
```
""".
-spec at_break(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_break(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Byte}, tokenize_state = #markdown_tokenize_state{markers = Markers}
    }
) ->
    case binary:match(Markers, <<Byte:8>>) of
        nomatch ->
            case Byte of
                $\n ->
                    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
                    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
                    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
                    State = markdown_state:next(data_at_break),
                    {Tokenizer4, State};
                _ ->
                    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, data),
                    State = markdown_state:retry(data_inside),
                    {Tokenizer2, State}
            end;
        {_Start, _Length} ->
            State = markdown_state:ok(),
            {Tokenizer1, State}
    end;
at_break(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:ok(),
    {Tokenizer, State}.

-doc """
In data.

```markdown
> | abc
    ^^^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Byte}, tokenize_state = #markdown_tokenize_state{markers = Markers}
    }
) ->
    case Byte =/= $\n andalso binary:match(Markers, <<Byte:8>>) =:= nomatch of
        true ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(data_inside),
            {Tokenizer2, State};
        false ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(data_at_break),
            {Tokenizer2, State}
    end;
inside(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
    State = markdown_state:retry(data_at_break),
    {Tokenizer2, State}.

-doc """
Merge adjacent data events.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{events = Events1, map = EditMap1}) ->
    {Events2, EditMap2} = resolve__merge_data_events(Events1, EditMap1, 0),
    {EditMap3, Events3} = markdown_edit_map:consume(EditMap2, Events2),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events3, map = EditMap3},
    {Tokenizer2, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve__find_farthest_data_exit(Events, ExitIndex) -> ExitIndex when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), ExitIndex :: markdown_types:usize().
resolve__find_farthest_data_exit(Events = #markdown_vec{size = EventsLength}, ExitIndex) when
    ExitIndex + 1 < EventsLength
->
    case markdown_vec:get(Events, ExitIndex + 1) of
        #markdown_event{name = data} ->
            resolve__find_farthest_data_exit(Events, ExitIndex + 2);
        _ ->
            ExitIndex
    end;
resolve__find_farthest_data_exit(_Events, ExitIndex) ->
    ExitIndex.

%% @private
-spec resolve__merge_data_events(Events, EditMap, Index) -> {Events, EditMap} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    EditMap :: markdown_edit_map:t(),
    Index :: markdown_types:usize().
resolve__merge_data_events(
    Events1 = #markdown_vec{size = EventsLength}, EditMap1 = #markdown_edit_map{}, Index1
) when Index1 < EventsLength ->
    case markdown_vec:get(Events1, Index1) of
        #markdown_event{kind = enter, name = data} ->
            %% Move to exit.
            {Events2, EditMap2, Index2} = resolve__move_to_exit(Events1, EditMap1, Index1),
            Index3 = Index2 + 1,
            resolve__merge_data_events(Events2, EditMap2, Index3);
        _ ->
            Index2 = Index1 + 1,
            resolve__merge_data_events(Events1, EditMap1, Index2)
    end;
resolve__merge_data_events(Events = #markdown_vec{}, EditMap = #markdown_edit_map{}, _EnterIndex) ->
    {Events, EditMap}.

%% @private
-spec resolve__move_to_exit(Events, EditMap, Index) -> {Events, EditMap, Index} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    EditMap :: markdown_edit_map:t(),
    Index :: markdown_types:usize().
resolve__move_to_exit(Events1, EditMap1, Index1) ->
    Index2 = Index1 + 1,
    ExitIndex1 = Index2,
    %% Find the farthest `data` event exit event.
    ExitIndex2 = resolve__find_farthest_data_exit(Events1, ExitIndex1),
    case ExitIndex2 > Index2 of
        true ->
            EditMap2 = markdown_edit_map:add(
                EditMap1, Index2, ExitIndex2 - Index2, markdown_vec:new()
            ),
            %% Change positional info.
            Events2 = markdown_vec:update(Events1, Index2, fun(Event) ->
                Event#markdown_event{point = (markdown_vec:get(Events1, ExitIndex2))#markdown_event.point}
            end),
            %% Move to the end.
            Index3 = ExitIndex2,
            {Events2, EditMap2, Index3};
        false ->
            {Events1, EditMap1, Index2}
    end.
