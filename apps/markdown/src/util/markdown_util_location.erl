%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_util_location).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/1,
    to_point/2
]).

%% Types
-type t() :: #markdown_util_location{}.

-export_type([
    t/0
]).

%% Macros

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Get an index for the given `Bytes`.

Port of <https://github.com/vfile/vfile-location/blob/main/index.js>
""".
-spec new(Bytes) -> Location when Bytes :: binary(), Location :: t().
new(Bytes) when is_binary(Bytes) ->
    new(Bytes, [], 0).

-doc """
Get the line and column-based `Point` for `Offset` in the bound indices.

Returns `none` when given out of bounds input.

Port of <https://github.com/vfile/vfile-location/blob/main/index.js>
""".
-spec to_point(Location, Offset) -> markdown_types:option(Point) when
    Location :: t(), Offset :: markdown_unist_point:offset(), Point :: markdown_unist_point:t().
to_point(Location = #markdown_util_location{}, Offset) when ?is_non_neg_integer(Offset) ->
    Indices = Location#markdown_util_location.indices,
    Size = array:size(Indices),
    case Size of
        0 ->
            none;
        _ ->
            case array:get(Size - 1, Indices) of
                End when Offset < End ->
                    {Index, Previous} =
                        case to_point_index(Indices, 0, Size, Offset) of
                            Idx when Idx > 0 ->
                                {Idx, array:get(Idx - 1, Indices)};
                            Idx = 0 ->
                                {Idx, 0}
                        end,
                    Point = markdown_unist_point:new(Index + 1, Offset + 1 - Previous, Offset),
                    {some, Point};
                _ ->
                    none
            end
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec new(Bytes, Indices, Index) -> Location when
    Bytes :: binary(), Indices :: [Index], Index :: non_neg_integer(), Location :: t().
new(<<$\r, $\n, Rest/bytes>>, Indices1, Index1) ->
    Index2 = Index1 + 2,
    Indices2 = [Index2 | Indices1],
    new(Rest, Indices2, Index2);
new(<<$\r, Rest/bytes>>, Indices1, Index1) ->
    Index2 = Index1 + 1,
    Indices2 = [Index2 | Indices1],
    new(Rest, Indices2, Index2);
new(<<$\n, Rest/bytes>>, Indices1, Index1) ->
    Index2 = Index1 + 1,
    Indices2 = [Index2 | Indices1],
    new(Rest, Indices2, Index2);
new(<<_, Rest/bytes>>, Indices, Index1) ->
    Index2 = Index1 + 1,
    new(Rest, Indices, Index2);
new(<<>>, Indices1, Index1) ->
    Index2 = Index1 + 1,
    Indices2 = [Index2 | Indices1],
    Indices = array:fix(array:from_list(lists:reverse(Indices2))),
    #markdown_util_location{indices = Indices}.

% TODO: binary search here over a `tuple()`
%% @private
-spec to_point_index(Indices, Index, Size, Offset) -> Index when
    Indices :: array:array(markdown_unist_point:offset()),
    Index :: non_neg_integer(),
    Size :: non_neg_integer(),
    Offset :: markdown_unist_point:offset().
to_point_index(_Indices, Size, Size, _Offset) ->
    Size;
to_point_index(Indices, Index, Size, Offset) ->
    case array:get(Index, Indices) of
        IndexOffset when IndexOffset > Offset ->
            Index;
        _ ->
            to_point_index(Indices, Index + 1, Size, Offset)
    end.
