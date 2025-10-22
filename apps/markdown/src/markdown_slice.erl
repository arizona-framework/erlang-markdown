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
-module(markdown_slice).
-moduledoc """
Bytes belonging to a range.

Includes info on virtual spaces before and after the bytes.
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
    as_binary/1,
    as_string/1,
    % fmt/1,
    from_indices/2,
    from_indices/3,
    from_position/2,
    serialize/1,
    size/1
]).

%% Types
-doc """
Bytes belonging to a range.

Includes info on virtual spaces before and after the bytes.
""".
-type t() :: #markdown_slice{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Turn the slice into a unicode binary.

> 👉 **Note**: cannot represent virtual spaces.
""".
-spec as_binary(Slice) -> UnicodeBinary when Slice :: t(), UnicodeBinary :: unicode:unicode_binary().
as_binary(#markdown_slice{bytes = Bytes}) ->
    markdown_types:unicode_binary(Bytes).

-doc """
Turn the slice into a unicode string.

> 👉 **Note**: cannot represent virtual spaces.
""".
-spec as_string(Slice) -> UnicodeString when Slice :: t(), UnicodeString :: string().
as_string(#markdown_slice{bytes = Bytes}) ->
    markdown_types:unicode_string(Bytes).

% -spec fmt(Position) -> {Format, Data} when Position :: t(), Format :: io:format(), Data :: [term()].
% fmt(Position = #markdown_position{}) ->
%     Start = Position#markdown_position.start,
%     End = Position#markdown_position.'end',
%     {"~w:~w-~w:~w (~w-~w)", [
%         Start#markdown_point.line,
%         Start#markdown_point.column,
%         End#markdown_point.line,
%         End#markdown_point.column,
%         Start#markdown_point.offset,
%         End#markdown_point.offset
%     ]}.

-spec from_indices(Bytes, Indices) -> Slice when
    Bytes :: binary(), Indices :: markdown_indices:t(), Slice :: t().
from_indices(Bytes, #markdown_indices{start = Start, 'end' = End}) when is_binary(Bytes) ->
    from_indices(Bytes, Start, End).

-doc """
Get a slice for two indices.

> 👉 **Note**: indices cannot represent virtual spaces.
""".
-spec from_indices(Bytes, Start, End) -> Slice when
    Bytes :: binary(), Start :: markdown_types:usize(), End :: markdown_types:usize(), Slice :: t().
from_indices(Bytes, Start, End) when
    is_binary(Bytes) andalso ?is_usize(Start) andalso ?is_usize(End) andalso End >= Start
->
    #markdown_slice{
        bytes = binary:part(Bytes, Start, End - Start),
        before = 0,
        'after' = 0
    }.

-doc """
Get a slice for a position.
""".
-spec from_position(Bytes, Position) -> Slice when Bytes :: binary(), Position :: markdown_position:t(), Slice :: t().
from_position(Bytes, #markdown_position{start = Start, 'end' = End}) when is_binary(Bytes) ->
    Before1 = Start#markdown_point.virtual,
    After1 = End#markdown_point.virtual,
    StartIndex1 = Start#markdown_point.offset,
    EndIndex1 = End#markdown_point.offset,

    %% If we have virtual spaces before, it means we are past the actual
    %% character at that index, and those virtual spaces.
    {Before2, StartIndex2} =
        case Before1 > 0 of
            true -> {?TAB_SIZE - Before1, StartIndex1 + 1};
            false -> {Before1, StartIndex1}
        end,

    %% If we have virtual spaces after, it means that character is included,
    %% and one less virtual space.
    {After2, EndIndex2} =
        case After1 > 0 of
            true -> {After1 - 1, EndIndex1 + 1};
            false -> {After1, EndIndex1}
        end,

    #markdown_slice{
        bytes = binary:part(Bytes, StartIndex2, EndIndex2 - StartIndex2),
        before = Before2,
        'after' = After2
    }.

-doc """
Turn the slice into chardata.

Supports virtual spaces.
""".
-spec serialize(Slice) -> CharData when Slice :: t(), CharData :: unicode:unicode_binary().
serialize(#markdown_slice{bytes = Bytes, before = Before, 'after' = After}) ->
    Prefix = binary:copy(<<" ">>, Before),
    Suffix = binary:copy(<<" ">>, After),
    <<Prefix/bytes, Bytes/bytes, Suffix/bytes>>.

-doc """
Get the size of this slice, including virtual spaces.
""".
-spec size(Slice) -> Size when Slice :: t(), Size :: markdown_types:usize().
size(#markdown_slice{bytes = Bytes, before = Before, 'after' = After}) ->
    byte_size(Bytes) + Before + After.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
