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
-module(markdown_util_skip).
-moduledoc """
Move across lists of events.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    names/1,
    opt/3,
    opt_back/3,
    to/3,
    to_back/3
]).

%% Types
-type name_list() :: list(markdown_event:name()).
-type name_set() :: {set, sets:set(markdown_event:name())}.
-type names() :: name_list() | name_set().

-export_type([
    name_list/0,
    name_set/0,
    names/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec names(NameList | NameSet) -> NameSet when NameList :: name_list(), NameSet :: name_set().
names(NameList) when is_list(NameList) ->
    {set, sets:from_list(NameList, [{version, 2}])};
names(NameSet = {set, _}) ->
    NameSet.

-doc """
Skip from `Index`, optionally past `Names`.
""".
-spec opt(Events, Index, Names) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: names().
opt(Events, Index, Names) ->
    skip_opt_impl(Events, Index, names(Names), true).

-doc """
Skip from `Index`, optionally past `Names`, backwards.
""".
-spec opt_back(Events, Index, Names) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: names().
opt_back(Events, Index, Names) ->
    skip_opt_impl(Events, Index, names(Names), false).

-doc """
Skip from `Index` forwards to `Names`.
""".
-spec to(Events, Index, Names) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: names().
to(Events, Index, Names) ->
    to_impl(Events, Index, names(Names), true).

-doc """
Skip from `Index` backwards to `Names`.
""".
-spec to_back(Events, Index, Names) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: names().
to_back(Events, Index, Names) ->
    to_impl(Events, Index, names(Names), false).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Skip to something.
""".
-spec to_impl(Events, Index, Names, Forward) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: name_set(),
    Forward :: boolean().
to_impl(Events, Index, Names = {set, NameSet}, Forward) when Index >= 0 andalso Index < ?markdown_vec_size(Events) ->
    #markdown_event{name = CurrentName} = markdown_vec:get(Events, Index),
    case sets:is_element(CurrentName, NameSet) of
        true ->
            Index;
        false when Forward =:= true ->
            to_impl(Events, Index + 1, Names, Forward);
        false ->
            to_impl(Events, Index - 1, Names, Forward)
    end;
to_impl(_Events, Index, _Names, _Forward) ->
    Index.

%% @private
-doc """
Skip past things.
""".
-spec skip_opt_impl(Events, Index, Names, Forward) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: name_set(),
    Forward :: boolean().
skip_opt_impl(Events, Index, {set, Names}, Forward) ->
    Balance = 0,
    Open =
        case Forward of
            true ->
                'enter';
            false ->
                'exit'
        end,
    % io:format(user, "skip_opt_impl_event_loop(Events, Index=~0tp, Names=~0tp, Forward=~0tp, Open=~0tp, Balance=~0tp)\n", [Index, Names, Forward, Open, Balance]),
    skip_opt_impl_event_loop(Events, Index, Names, Forward, Open, Balance).

%% @private
-spec skip_opt_impl_event_loop(Events, Index, Names, Forward, Open, Balance) -> Index when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_types:usize(),
    Names :: sets:set(markdown_event:name()),
    Forward :: boolean(),
    Open :: markdown_event:kind(),
    Balance :: integer().
skip_opt_impl_event_loop(Events, Index1, Names, Forward, Open, Balance1) when Index1 < ?markdown_vec_size(Events) ->
    #markdown_event{name = CurrentName, kind = CurrentKind} = markdown_vec:get(Events, Index1),
    % io:format(user, "[el] CurrentName=~0tp, CurrentKind=~0tp\n", [CurrentName, CurrentKind]),
    case (not sets:is_element(CurrentName, Names)) orelse CurrentKind =/= Open of
        true ->
            Index1;
        false ->
            Index2 =
                case Forward of
                    true ->
                        Index1 + 1;
                    false ->
                        Index1 - 1
                end,
            Balance2 = Balance1 + 1,
            % io:format(user, "skip_opt_impl_balance_loop(Events, CurrentName=~0tp, Index2=~0tp, Forward=~0tp, Open=~0tp, Balance2=~0tp)\n", [CurrentName, Index2, Forward, Open, Balance2]),
            {Index3, Balance3} = skip_opt_impl_balance_loop(Events, CurrentName, Index2, Forward, Open, Balance2),
            skip_opt_impl_event_loop(Events, Index3, Names, Forward, Open, Balance3)
    end;
skip_opt_impl_event_loop(_Events, Index, _Names, _Forward, _Open, _Balance) ->
    Index.

%% @private
-spec skip_opt_impl_balance_loop(Events, PinnedName, Index, Forward, Open, Balance) -> {Index, Balance} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    PinnedName :: markdown_event:name(),
    Index :: markdown_types:usize(),
    Forward :: boolean(),
    Open :: markdown_event:kind(),
    Balance :: integer().
skip_opt_impl_balance_loop(Events, PinnedName, Index1, Forward, Open, Balance1) ->
    #markdown_event{name = CurrentName, kind = CurrentKind} = markdown_vec:get(Events, Index1),
    % io:format(user, "[bl] PinnedName=~0tp, Index1=~0tp, Forward=~0tp, Open=~0tp, Balance1=~0tp, CurrentName=~0tp, CurrentKind=~0tp\n", [PinnedName, Index1, Forward, Open, Balance1, CurrentName, CurrentKind]),
    Balance2 =
        case CurrentKind =:= Open of
            true -> Balance1 + 1;
            false -> Balance1 - 1
        end,
    Index2 =
        case Forward of
            true -> Index1 + 1;
            false when Index1 > 0 -> Index1 - 1;
            false -> Index1
        end,
    case CurrentName =:= PinnedName andalso Balance2 =:= 0 of
        true ->
            {Index2, Balance2};
        false ->
            skip_opt_impl_balance_loop(Events, PinnedName, Index2, Forward, Open, Balance2)
    end.
