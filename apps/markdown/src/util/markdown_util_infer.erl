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
-module(markdown_util_infer).
-moduledoc """
Infer things from events.

Used to share between `to_html` and `to_mdast`.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    gfm_table_align/2,
    list_loose/3,
    list_item_loose/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Figure out the alignment of a GFM table.
""".
-spec gfm_table_align(Events, Index) -> Align when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_vec:index(),
    Align :: markdown_vec:t(AlignKind),
    AlignKind :: markdown_align_kind:t().
gfm_table_align(Events = #markdown_vec{}, Index) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events)
->
    %% BEGIN: assertions
    ?assert((markdown_vec:get(Events, Index))#markdown_event.name =:= gfm_table, "expected table"),
    %% END: assertions
    gfm_table_align_loop(Events, Index, false, markdown_vec:new()).

-doc """
Figure out if a list is spread or not.

When `IncludeItems = true` is passed, infers whether the list as a whole
is "loose".
""".
-spec list_loose(Events, Index, IncludeItems) -> boolean() when
    Events :: markdown_vec:t(markdown_event:t()), Index :: markdown_vec:index(), IncludeItems :: boolean().
list_loose(Events = #markdown_vec{}, Index, IncludeItems) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events) andalso is_boolean(IncludeItems)
->
    %% BEGIN: assertions
    #markdown_event{name = Name} = markdown_vec:get(Events, Index),
    ?assert((Name =:= list_ordered) orelse (Name =:= list_unordered), "expected list"),
    %% END: assertions
    list_loose_loop(Events, Index, 0, Name, IncludeItems).

-doc """
Figure out if an item is spread or not.
""".
%% @doc Figure out if an item is spread or not.
-spec list_item_loose(Events, Index) -> boolean() when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), Index :: markdown_vec:index().
list_item_loose(Events = #markdown_vec{}, Index) when Index >= 0 andalso Index < ?markdown_vec_size(Events) ->
    %% BEGIN: assertions
    ?assertMatch(#markdown_event{name = 'list_item'}, markdown_vec:get(Events, Index), "expected list item"),
    %% END: assertions
    list_item_loose_loop(Events, Index, 0).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec check_at_prefix(Events, Before) -> boolean() when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), Before :: markdown_vec:index().
check_at_prefix(Events, Before) ->
    BeforeEvent = markdown_vec:get(Events, Before),

    case BeforeEvent#markdown_event.name of
        space_or_tab ->
            check_at_prefix(Events, Before - 2);
        list_item_prefix ->
            true;
        _ ->
            false
    end.

%% @private
-spec check_empty_item_or_quote(Events, Before) -> empty_list_item | empty_block_quote | none when
    Events :: markdown_vec:t(markdown_event:t()), Before :: markdown_vec:index().
check_empty_item_or_quote(Events, Before) ->
    BeforeEvent = markdown_vec:get(Events, Before),

    case BeforeEvent#markdown_event.name of
        list_item ->
            Before2 = Before - 1,
            Before2Event = markdown_vec:get(Events, Before2),

            case Before2Event#markdown_event.name of
                space_or_tab ->
                    check_empty_item_or_quote_after_space(Events, Before2 - 2);
                _ ->
                    check_empty_item_or_quote_direct(Events, Before2)
            end;
        _ ->
            none
    end.

%% @private
-spec check_empty_item_or_quote_after_space(Events, Before) -> empty_list_item | empty_block_quote | none when
    Events :: markdown_vec:t(markdown_event:t()), Before :: markdown_vec:index().
check_empty_item_or_quote_after_space(Events, Before) ->
    BeforeEvent = markdown_vec:get(Events, Before),

    case BeforeEvent#markdown_event.name of
        block_quote ->
            Before2 = Before - 1,
            Before2Event = markdown_vec:get(Events, Before2),

            case Before2Event#markdown_event.name of
                block_quote_prefix ->
                    empty_block_quote;
                _ ->
                    none
            end;
        list_item_prefix ->
            empty_list_item;
        _ ->
            none
    end.

%% @private
-spec check_empty_item_or_quote_direct(Events, Before) -> empty_list_item | empty_block_quote | none when
    Events :: markdown_vec:t(markdown_event:t()), Before :: markdown_vec:index().
check_empty_item_or_quote_direct(Events, Before) ->
    BeforeEvent = markdown_vec:get(Events, Before),

    case BeforeEvent#markdown_event.name of
        block_quote ->
            Before2 = Before - 1,
            Before2Event = markdown_vec:get(Events, Before2),

            case Before2Event#markdown_event.name of
                block_quote_prefix ->
                    empty_block_quote;
                _ ->
                    none
            end;
        list_item_prefix ->
            empty_list_item;
        _ ->
            none
    end.

%% @private
-spec gfm_table_align_loop(Events, Index, InDelimiterRow, Align) -> Align when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_vec:index(),
    InDelimiterRow :: boolean(),
    Align :: markdown_vec:t(AlignKind),
    AlignKind :: markdown_align_kind:t().
gfm_table_align_loop(Events, Index, InDelimiterRow, Align1) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events)
->
    Event = markdown_vec:get(Events, Index),
    case InDelimiterRow of
        false ->
            case Event of
                #markdown_event{kind = enter, name = gfm_table_delimiter_cell_value} ->
                    %% Start of alignment value: set a new column.
                    NextEvent = markdown_vec:get(Events, Index + 1),
                    AlignKind =
                        case NextEvent of
                            #markdown_event{name = gfm_table_delimiter_marker} ->
                                markdown_align_kind:left();
                            _ ->
                                markdown_align_kind:none()
                        end,
                    Align2 = markdown_vec:push(Align1, AlignKind),
                    gfm_table_align_loop(Events, Index + 1, InDelimiterRow, Align2);
                #markdown_event{kind = exit, name = gfm_table_delimiter_cell_value} ->
                    PrevEvent = markdown_vec:get(Events, Index - 1),
                    case PrevEvent of
                        #markdown_event{name = gfm_table_delimiter_marker} ->
                            %% End of alignment value: change the column.
                            AlignKind =
                                case markdown_vec:last(Align1) of
                                    left ->
                                        markdown_align_kind:center();
                                    _ ->
                                        markdown_align_kind:right()
                                end,
                            Align2 = markdown_vec:set_last(Align1, AlignKind),
                            gfm_table_align_loop(Events, Index + 1, InDelimiterRow, Align2);
                        _ ->
                            gfm_table_align_loop(Events, Index + 1, InDelimiterRow, Align1)
                    end;
                #markdown_event{kind = exit, name = gfm_table_delimiter_row} ->
                    Align1;
                _ ->
                    gfm_table_align_loop(Events, Index + 1, InDelimiterRow, Align1)
            end;
        true ->
            case Event of
                #markdown_event{kind = enter, name = gfm_table_delimiter_row} ->
                    gfm_table_align_loop(Events, Index + 1, true, Align1);
                _ ->
                    gfm_table_align_loop(Events, Index + 1, InDelimiterRow, Align1)
            end
    end.

%% @private
-spec list_item_loose_loop(Events, Index, Balance) -> boolean() when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), Index :: markdown_vec:index(), Balance :: integer().
list_item_loose_loop(Events, Index, Balance) ->
    case Index < markdown_vec:size(Events) of
        false ->
            false;
        true ->
            Event = markdown_vec:get(Events, Index),

            NewBalance =
                case Event#markdown_event.kind of
                    enter ->
                        Balance + 1;
                    exit ->
                        Balance - 1
                end,

            case {NewBalance, Event#markdown_event.name} of
                {1, blank_line_ending} ->
                    %% Blank line directly after a prefix:
                    %%
                    %% ```markdown
                    %% > | -␊
                    %%      ^
                    %%   |   a
                    %% ```
                    AtPrefix = check_at_prefix(Events, Index - 2),

                    case AtPrefix of
                        true ->
                            list_item_loose_loop(Events, Index + 1, NewBalance);
                        false ->
                            true
                    end;
                {0, list_item} ->
                    %% Done.
                    false;
                _ ->
                    list_item_loose_loop(Events, Index + 1, NewBalance)
            end
    end.

%% @private
-spec list_loose_loop(Events, Index, Balance, Name, IncludeItems) -> boolean() when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Index :: markdown_vec:index(),
    Balance :: integer(),
    Name :: atom(),
    IncludeItems :: boolean().
list_loose_loop(Events, Index, Balance, Name, IncludeItems) ->
    case Index < markdown_vec:size(Events) of
        false ->
            false;
        true ->
            Event = markdown_vec:get(Events, Index),

            case Event#markdown_event.kind of
                enter ->
                    NewBalance = Balance + 1,

                    case IncludeItems andalso NewBalance =:= 2 andalso Event#markdown_event.name =:= list_item of
                        true ->
                            case list_item_loose(Events, Index) of
                                true ->
                                    true;
                                false ->
                                    list_loose_loop(Events, Index + 1, NewBalance, Name, IncludeItems)
                            end;
                        false ->
                            list_loose_loop(Events, Index + 1, NewBalance, Name, IncludeItems)
                    end;
                exit ->
                    NewBalance = Balance - 1,

                    case {NewBalance, Event#markdown_event.name} of
                        {1, blank_line_ending} ->
                            %% Blank line directly after item, which is just a prefix.
                            %%
                            %% ```markdown
                            %% > | -␊
                            %%      ^
                            %%   | - a
                            %% ```

                            %% Blank line at block quote prefix:
                            %%
                            %% ```markdown
                            %% > | * >␊
                            %%        ^
                            %%   | * a
                            %% ```

                            Result = check_empty_item_or_quote(Events, Index - 2),

                            case Result of
                                empty_list_item ->
                                    list_loose_loop(Events, Index + 1, NewBalance, Name, IncludeItems);
                                empty_block_quote ->
                                    list_loose_loop(Events, Index + 1, NewBalance, Name, IncludeItems);
                                none ->
                                    true
                            end;
                        {0, EventName} when EventName =:= Name ->
                            %% Done.
                            false;
                        _ ->
                            list_loose_loop(Events, Index + 1, NewBalance, Name, IncludeItems)
                    end
            end
    end.
