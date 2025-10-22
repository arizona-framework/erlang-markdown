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
-module(markdown_mdx_collect).
-moduledoc """
Collect info for MDX.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-10", modified => "2025-10-10"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    collect/5
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec collect(Events, Bytes, From, Names, Stop) -> Result when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    From :: non_neg_integer(),
    Names :: [EventName],
    EventName :: markdown_event:name(),
    Stop :: [EventName],
    Result :: markdown_mdx_collect_result:t().
collect(Events, Bytes, From, Names, Stop) ->
    Result = markdown_mdx_collect_result:new(),
    NamesMap = #{EventName => [] || EventName <- Names},
    StopMap = #{EventName => [] || EventName <- Stop},
    collect_loop(Events, Bytes, From, NamesMap, StopMap, Result).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec collect_loop(Events, Bytes, Index, NamesMap, StopMap, Result) -> Result when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    Index :: non_neg_integer(),
    NamesMap :: #{EventName => []},
    EventName :: markdown_event:name(),
    StopMap :: #{EventName => []},
    Result :: markdown_mdx_collect_result:t().
collect_loop(Events, Bytes, Index, NamesMap, StopMap, Result1) when Index < ?markdown_vec_size(Events) ->
    Event = markdown_vec:get(Events, Index),
    case Event of
        #markdown_event{kind = enter, name = EventName} when is_map_key(EventName, NamesMap) ->
            %% Include virtual spaces, and assume void.
            NextEvent = markdown_vec:get(Events, Index + 1),
            Slice = markdown_slice:from_position(
                Bytes, markdown_position:new(Event#markdown_event.point, NextEvent#markdown_event.point)
            ),
            Value = markdown_slice:serialize(Slice),
            Stop = markdown_stop:new(
                byte_size(Result1#markdown_mdx_collect_result.value), Event#markdown_event.point#markdown_point.offset
            ),
            Result2 = markdown_mdx_collect_result:push(Result1, Value, Stop),
            collect_loop(Events, Bytes, Index + 1, NamesMap, StopMap, Result2);
        #markdown_event{kind = exit, name = EventName} when is_map_key(EventName, StopMap) ->
            Result1;
        #markdown_event{} ->
            collect_loop(Events, Bytes, Index + 1, NamesMap, StopMap, Result1)
    end;
collect_loop(_Events, _Bytes, _Index, _NamesMap, _StopMap, Result) ->
    Result.
