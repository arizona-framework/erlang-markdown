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
-module(markdown_vec).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    append/2,
    contains/2,
    first/1,
    first_option/1,
    flatten/1,
    from/1,
    from_iterator/1,
    from_list/1,
    get/2,
    get_option/2,
    insert/3,
    is_empty/1,
    iterator/1,
    iterator/2,
    last/1,
    last_option/1,
    map/2,
    new/0,
    next/1,
    pop/1,
    pop_option/1,
    push/2,
    reduce/3,
    reduce_right/3,
    reduce_right_while/3,
    reduce_while/3,
    remove/2,
    remove_first/2,
    remove_last/2,
    reverse/1,
    set/3,
    size/1,
    sort/1,
    sort_by/2,
    sort_by_key/2,
    split_off/2,
    swap_remove/2,
    swap/3,
    take/2,
    to_list/1,
    truncate/2,
    update/3,
    update_last/2
]).

%% Types
-type index() :: non_neg_integer().
-type item() :: dynamic().
-type iterator() :: iterator(item()).
-opaque iterator(Item) :: none | {{ordered, index()} | {reversed, index()} | {custom, [{index(), Item}]}, t(Item)}.
-type iterator_order() :: iterator_order(item()).
-type iterator_order(Item) :: ordered | reversed | key_func(Item) | order_func(Item).
-type key_func() :: key_func(item()).
-type key_func(Item) ::
    fun((Index :: index(), Item) -> Key :: dynamic()).
-type order_func() :: order_func(item()).
-type order_func(Item) ::
    fun((AIndex :: index(), AItem :: Item, BIndex :: index(), BItem :: Item) -> boolean()).
-type reduce_func() :: reduce_func(item()).
-type reduce_func(Item) :: reduce_func(Item, dynamic()).
-type reduce_func(Item, Acc) :: reduce_func(Item, Acc, Acc).
-type reduce_func(Item, AccIn, AccOut) ::
    fun((index(), Item, AccIn) -> AccOut).
-type reduce_while_func() :: reduce_while_func(item()).
-type reduce_while_func(Item) :: reduce_while_func(Item, dynamic()).
-type reduce_while_func(Item, Acc) :: reduce_while_func(Item, Acc, Acc).
-type reduce_while_func(Item, AccIn, AccOut) ::
    fun((index(), Item, AccIn) -> {cont, AccOut} | {halt, AccOut}).
-type t() :: t(item()).
-type t(Item) :: #markdown_vec{items :: array:array(none | {some, Item})}.

-export_type([
    index/0,
    item/0,
    iterator/0,
    iterator/1,
    iterator_order/0,
    iterator_order/1,
    key_func/0,
    key_func/1,
    order_func/0,
    order_func/1,
    reduce_func/0,
    reduce_func/1,
    reduce_func/2,
    reduce_func/3,
    reduce_while_func/0,
    reduce_while_func/1,
    reduce_while_func/2,
    reduce_while_func/3,
    t/0,
    t/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec append(VecA, VecB | ListB) -> VecC when
    VecA :: t(Item), VecB :: t(Item), ListB :: [Item], VecC :: t(Item), Item :: item().
append(#markdown_vec{items = ItemsA, size = SizeA}, #markdown_vec{items = ItemsB, size = SizeB}) ->
    SizeC = SizeA + SizeB,
    {SizeC, ItemsC} = array:foldl(fun collect_append_array/3, {SizeA, array:resize(SizeC, ItemsA)}, ItemsB),
    VecC = #markdown_vec{items = ItemsC, size = SizeC},
    VecC;
append(#markdown_vec{items = ItemsA, size = SizeA}, ListB) when is_list(ListB) andalso length(ListB) >= 0 ->
    SizeB = length(ListB),
    SizeC = SizeA + SizeB,
    {SizeC, ItemsC} = lists:foldl(fun collect_append_list/2, {SizeA, array:resize(SizeC, ItemsA)}, ListB),
    VecC = #markdown_vec{items = ItemsC, size = SizeC},
    VecC.

-spec contains(Vec, Item) -> Found when Vec :: t(Item), Item :: item(), Found :: boolean().
contains(#markdown_vec{size = 0}, _Item) ->
    false;
contains(Vec = #markdown_vec{}, Item) ->
    {Item, Found} = reduce_while(Vec, {Item, false}, fun collect_contains/3),
    Found.

-spec first(Vec) -> Item when Vec :: t(Item), Item :: item().
first(#markdown_vec{items = Items, size = Size}) when Size > 0 ->
    {some, Item} = array:get(0, Items),
    Item.

-spec first_option(Vec) -> OptionItem when Vec :: t(Item), Item :: item(), OptionItem :: markdown_types:option(Item).
first_option(#markdown_vec{size = 0}) ->
    none;
first_option(Vec = #markdown_vec{}) ->
    {some, first(Vec)}.

-spec flatten(Vec) -> FlattenedVec when
    Vec :: t(Item | NestedVec), NestedVec :: t(), Item :: item(), FlattenedVec :: t(Item).
flatten(Vec = #markdown_vec{size = 0}) ->
    Vec;
flatten(Vec = #markdown_vec{}) ->
    reduce(Vec, new(), fun collect_flatten/3).

-spec from(Vec | List | Iterator) -> Vec when
    Vec :: t(Item), List :: [Item], Iterator :: iterator(Item), Item :: item().
from(Vec = #markdown_vec{}) ->
    Vec;
from(List) when is_list(List) andalso length(List) >= 0 ->
    from_list(List);
from(Iterator = {{_, _}, #markdown_vec{}}) ->
    from_iterator(Iterator).

-spec from_iterator(Iterator) -> Vec when Iterator :: iterator(Item), Item :: item(), Vec :: t(Item).
from_iterator(Iterator = {{_, _}, #markdown_vec{}}) ->
    foldl_iterator(Iterator, fun collect_vec/3, new()).

-spec from_list(List) -> Vec when List :: [Item], Item :: item(), Vec :: t(Item).
from_list([]) ->
    new();
from_list(List) when is_list(List) andalso length(List) > 0 ->
    Size = length(List),
    Items1 = array_with_capacity(Size),
    {Size, Items2} = lists:foldl(fun from_list/2, {0, Items1}, List),
    #markdown_vec{items = Items2, size = Size}.

-spec get(Vec, Index) -> Item when Vec :: t(Item), Item :: item(), Index :: index().
get(#markdown_vec{items = Items, size = Size}, Index) when Index >= 0 andalso Index < Size ->
    {some, Item} = array:get(Index, Items),
    Item.

-spec get_option(Vec, Index) -> OptionItem when
    Vec :: t(Item), Item :: item(), Index :: index(), OptionItem :: markdown_types:option(Item).
get_option(Vec = #markdown_vec{size = Size}, Index) when Index >= 0 andalso Index < Size ->
    {some, get(Vec, Index)};
get_option(#markdown_vec{}, Index) when Index >= 0 ->
    none.

-spec insert(Vec, Index, Item) -> Vec when Vec :: t(Item), Item :: item(), Index :: index().
insert(Vec1 = #markdown_vec{items = Items1, size = Size1}, Index, Item) when Index >= 0 andalso Index < Size1 ->
    Size2 = Size1 + 1,
    Items2 = array:resize(Size2, Items1),
    Items3 = array:set(Index, {some, Item}, Items2),
    {Index, Items4} = reduce_right_while(Vec1, {Index, Items3}, fun reduce_while_insert/3),
    Vec2 = Vec1#markdown_vec{items = Items4, size = Size2},
    Vec2;
insert(#markdown_vec{size = 0}, 0, Item) ->
    Items1 = array_with_capacity(1),
    Items2 = array:set(0, {some, Item}, Items1),
    #markdown_vec{items = Items2, size = 1}.

-spec is_empty(Vec) -> IsEmpty when Vec :: t(), IsEmpty :: boolean().
is_empty(#markdown_vec{size = 0}) -> true;
is_empty(#markdown_vec{size = Size}) when Size > 0 -> false.

-spec iterator(Vec) -> Iterator when Vec :: t(Item), Item :: item(), Iterator :: iterator(Item).
iterator(Vec = #markdown_vec{}) ->
    iterator(Vec, ordered).

-spec iterator(Vec, Order) -> Iterator when
    Vec :: t(Item), Item :: item(), Order :: iterator_order(Item), Iterator :: iterator(Item).
iterator(Vec = #markdown_vec{}, ordered) ->
    {{ordered, 0}, Vec};
iterator(Vec = #markdown_vec{size = Size}, reversed) ->
    {{reversed, Size}, Vec};
iterator(Vec = #markdown_vec{}, KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = key_func_to_order_func(KeyFun),
    iterator(Vec, OrderFun);
iterator(Vec = #markdown_vec{}, OrderFun) when is_function(OrderFun, 4) ->
    IteratorInternal = iterator_internal(Vec, OrderFun),
    {{custom, IteratorInternal}, Vec}.

-spec last(Vec) -> Item when Vec :: t(Item), Item :: item().
last(#markdown_vec{items = Items, size = Size}) when Size > 0 ->
    {some, Item} = array:get(Size - 1, Items),
    Item.

-spec last_option(Vec) -> OptionItem when Vec :: t(Item), Item :: item(), OptionItem :: markdown_types:option(Item).
last_option(#markdown_vec{size = 0}) ->
    none;
last_option(Vec = #markdown_vec{}) ->
    {some, last(Vec)}.

-spec map(VecIn, MapFun) -> VecOut when
    VecIn :: t(ItemIn),
    ItemIn :: item(),
    MapFun :: fun((index(), ItemIn) -> ItemOut),
    ItemOut :: item(),
    VecOut :: t(ItemOut).
map(VecIn = #markdown_vec{items = ItemsIn}, MapFun) when is_function(MapFun, 2) ->
    Function =
        fun(Index, {some, ItemIn}) ->
            ItemOut = MapFun(Index, ItemIn),
            {some, ItemOut}
        end,
    ItemsOut = array:map(Function, ItemsIn),
    VecOut = VecIn#markdown_vec{items = ItemsOut},
    VecOut.

-spec new() -> Vec :: t().
new() ->
    #markdown_vec{
        items = array_with_capacity(0),
        size = 0
    }.

-spec next(Iterator) -> none | {Index, Item, NextIterator} when
    Index :: index(),
    Item :: item(),
    Iterator :: iterator(Item),
    NextIterator :: iterator(Item).
next({{ordered, Index}, Vec = #markdown_vec{items = Items, size = Size}}) when Index >= 0 andalso Index < Size ->
    {some, Item} = array:get(Index, Items),
    NextIterator =
        case Index + 1 of
            Size ->
                none;
            NextIndex ->
                {{ordered, NextIndex}, Vec}
        end,
    {Index, Item, NextIterator};
next({{ordered, Size}, _Vec = #markdown_vec{size = Size}}) ->
    none;
next({{reversed, 0}, _Vec = #markdown_vec{}}) ->
    none;
next({{reversed, Index}, Vec = #markdown_vec{items = Items, size = Size}}) when Index > 0 andalso Index =< Size ->
    NextIndex = Index - 1,
    {some, Item} = array:get(NextIndex, Items),
    NextIterator =
        case NextIndex of
            0 ->
                none;
            _ ->
                {{reversed, NextIndex}, Vec}
        end,
    {NextIndex, Item, NextIterator};
next({{custom, [{Index, Item} | Custom]}, Vec = #markdown_vec{}}) ->
    NextIterator =
        case length(Custom) > 0 of
            true ->
                {{custom, Custom}, Vec};
            false ->
                none
        end,
    {Index, Item, NextIterator};
next({{custom, []}, #markdown_vec{}}) ->
    none;
next(none) ->
    none.

-spec pop(Vec) -> {Vec, Item} when Vec :: t(Item), Item :: item().
pop(Vec1 = #markdown_vec{items = Items1, size = Size1}) when Size1 > 0 ->
    Size2 = Size1 - 1,
    {some, Item} = array:get(Size2, Items1),
    Items2 = array:reset(Size2, Items1),
    Items3 = array:resize(Size2, Items2),
    Items4 = shrink(Items3),
    Vec2 = Vec1#markdown_vec{items = Items4, size = Size2},
    {Vec2, Item}.

-spec pop_option(Vec) -> {Vec, OptionItem} when
    Vec :: t(Item), Item :: item(), OptionItem :: markdown_types:option(Item).
pop_option(Vec = #markdown_vec{size = 0}) ->
    {Vec, none};
pop_option(Vec1 = #markdown_vec{}) ->
    {Vec2, Item} = pop(Vec1),
    {Vec2, {some, Item}}.

-spec push(Vec, Item) -> Vec when Vec :: t(Item), Item :: item().
push(Vec1 = #markdown_vec{items = Items1, size = Size1}, Item) ->
    Size2 = Size1 + 1,
    Items2 = array:set(Size1, {some, Item}, array:resize(Size2, Items1)),
    Vec2 = Vec1#markdown_vec{items = Items2, size = Size2},
    Vec2.

-spec set(Vec, Index, Item) -> Vec when Vec :: t(Item), Item :: item(), Index :: index().
set(Vec1 = #markdown_vec{items = Items1, size = Size}, Index, Item) when Index >= 0 andalso Index < Size ->
    Items2 = array:set(Index, {some, Item}, Items1),
    Vec2 = Vec1#markdown_vec{items = Items2},
    Vec2.

-spec reduce(Vec | Iterator, AccIn, Function) -> AccOut when
    Vec :: t(Item),
    Item :: item(),
    Iterator :: iterator(Item),
    AccIn :: dynamic(),
    Function :: reduce_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce(Vec = #markdown_vec{}, AccIn, Function) when is_function(Function, 3) ->
    Iterator = iterator(Vec),
    reduce_iterator(Iterator, AccIn, Function);
reduce(Iterator = {{_, _}, #markdown_vec{}}, AccIn, Function) when is_function(Function, 3) ->
    reduce_iterator(Iterator, AccIn, Function).

-spec reduce_right(Vec | Iterator, AccIn, Function) -> AccOut when
    Vec :: t(Item),
    Item :: item(),
    Iterator :: iterator(Item),
    AccIn :: dynamic(),
    Function :: reduce_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_right(Vec = #markdown_vec{}, AccIn, Function) when is_function(Function, 3) ->
    Iterator = iterator(Vec, reversed),
    reduce_iterator(Iterator, AccIn, Function);
reduce_right(Iterator = {{_, _}, #markdown_vec{}}, AccIn, Function) when is_function(Function, 3) ->
    reduce_right_iterator(Iterator, AccIn, Function, []).

-spec reduce_right_while(Vec | Iterator, AccIn, Function) -> AccOut when
    Vec :: t(Item),
    Item :: item(),
    Iterator :: iterator(Item),
    AccIn :: dynamic(),
    Function :: reduce_while_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_right_while(Vec = #markdown_vec{}, AccIn, Function) when is_function(Function, 3) ->
    Iterator = iterator(Vec, reversed),
    reduce_while_iterator(Iterator, AccIn, Function);
reduce_right_while(Iterator = {{_, _}, #markdown_vec{}}, AccIn, Function) when is_function(Function, 3) ->
    reduce_right_while_iterator(Iterator, AccIn, Function, []).

-spec reduce_while(Vec | Iterator, AccIn, Function) -> AccOut when
    Vec :: t(Item),
    Item :: item(),
    Iterator :: iterator(Item),
    AccIn :: dynamic(),
    Function :: reduce_while_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_while(Vec = #markdown_vec{}, AccIn, Function) when is_function(Function, 3) ->
    Iterator = iterator(Vec),
    reduce_while_iterator(Iterator, AccIn, Function);
reduce_while(Iterator = {{_, _}, #markdown_vec{}}, AccIn, Function) when is_function(Function, 3) ->
    reduce_while_iterator(Iterator, AccIn, Function).

-spec remove(Vec, Index) -> Vec when Vec :: t(Item), Item :: item(), Index :: index().
remove(Vec1 = #markdown_vec{size = Size}, Index) when Index >= 0 andalso Index < Size ->
    {Vec2, _Item} = take(Vec1, Index),
    Vec2.

-spec remove_first(Vec, Item) -> Vec when Vec :: t(Item), Item :: item().
remove_first(Vec = #markdown_vec{size = 0}, _Item) ->
    Vec;
remove_first(Vec1 = #markdown_vec{}, Item) ->
    case reduce_while(Vec1, {Item, Vec1}, fun collect_remove_first/3) of
        {Item, Vec1} ->
            %% Not found.
            Vec1;
        {Item, Vec2} ->
            %% Found and removed rightmost Item.
            Vec2
    end.

-spec remove_last(Vec, Item) -> Vec when Vec :: t(Item), Item :: item().
remove_last(Vec = #markdown_vec{size = 0}, _Item) ->
    Vec;
remove_last(Vec1 = #markdown_vec{}, Item) ->
    case reduce_right_while(Vec1, {Item, Vec1}, fun collect_remove_first/3) of
        {Item, Vec1} ->
            %% Not found.
            Vec1;
        {Item, Vec2} ->
            %% Found and removed rightmost Item.
            Vec2
    end.

-spec reverse(Vec) -> Vec when Vec :: t(Item), Item :: item().
reverse(Vec = #markdown_vec{}) ->
    reduce(Vec, Vec, fun collect_reverse/3).

-spec size(Vec) -> Size when Vec :: t(), Size :: non_neg_integer().
size(#markdown_vec{size = Size}) ->
    Size.

-spec sort(Vec) -> SortedVec when
    Vec :: t(Item), Item :: item(), SortedVec :: t(Item).
sort(Vec = #markdown_vec{}) ->
    List = to_list(Vec),
    SortedList = lists:sort(List),
    from_list(SortedList).

-spec sort_by(Vec, OrderFun) -> SortedVec when
    Vec :: t(Item), Item :: item(), OrderFun :: order_func(Item), SortedVec :: t(Item).
sort_by(Vec = #markdown_vec{}, OrderFun) when is_function(OrderFun, 4) ->
    InternalIterator = iterator_internal(Vec, OrderFun),
    Size = length(InternalIterator),
    Items1 = array_with_capacity(Size),
    {Size, Items2} = lists:foldl(fun from_list_internal_iterator/2, {0, Items1}, InternalIterator),
    #markdown_vec{items = Items2, size = Size}.

-spec sort_by_key(Vec, KeyFun) -> SortedVec when
    Vec :: t(Item), Item :: item(), KeyFun :: key_func(Item), SortedVec :: t(Item).
sort_by_key(Vec = #markdown_vec{}, KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = key_func_to_order_func(KeyFun),
    sort_by(Vec, OrderFun).

-spec split_off(Vec, Index) -> {KeepVec, DropVec} when
    Vec :: t(Item), Item :: item(), Index :: index(), KeepVec :: Vec, DropVec :: Vec.
split_off(Vec = #markdown_vec{}, 0) ->
    {new(), Vec};
split_off(#markdown_vec{items = Items, size = Size}, Index) when Index > 0 andalso Index < Size ->
    KeepSize = Index,
    DropSize = Size - Index,
    KeepItems1 = array_with_capacity(KeepSize),
    DropItems1 = array_with_capacity(DropSize),
    KeepItems2 = split_off_keep(Items, 0, KeepSize, KeepItems1),
    DropItems2 = split_off_drop(Items, 0, KeepSize, DropSize, DropItems1),
    KeepVec = #markdown_vec{items = KeepItems2, size = KeepSize},
    DropVec = #markdown_vec{items = DropItems2, size = DropSize},
    {KeepVec, DropVec};
split_off(Vec = #markdown_vec{size = Size}, Size) ->
    {Vec, new()}.

-spec swap(Vec, AIndex, BIndex) -> Vec when Vec :: t(Item), Item :: item(), AIndex :: index(), BIndex :: index().
swap(Vec1 = #markdown_vec{items = Items1, size = Size}, AIndex, BIndex) when
    AIndex >= 0 andalso BIndex >= 0 andalso AIndex < Size andalso BIndex < Size
->
    AItem = array:get(AIndex, Items1),
    BItem = array:get(BIndex, Items1),
    Items2 = array:set(BIndex, AItem, Items1),
    Items3 = array:set(AIndex, BItem, Items2),
    Vec2 = Vec1#markdown_vec{items = Items3},
    Vec2.

-spec swap_remove(Vec, Index) -> {Vec, Item} when Vec :: t(Item), Item :: item(), Index :: index().
swap_remove(Vec1 = #markdown_vec{items = Items1, size = Size1}, Index) when
    Size1 > 1 andalso Index >= 0 andalso Index + 1 < Size1
->
    Size2 = Size1 - 1,
    {some, Item} = array:get(Index, Items1),
    {some, LastItem} = array:get(Size2, Items1),
    Items2 = array:set(Index, {some, LastItem}, Items1),
    Items3 = array:reset(Size2, Items2),
    Items4 = array:resize(Size2, Items3),
    Items5 = shrink(Items4),
    Vec2 = Vec1#markdown_vec{items = Items5, size = Size2},
    {Vec2, Item};
swap_remove(Vec1 = #markdown_vec{items = Items1, size = Size1}, Index) when Size1 > 1 andalso Index + 1 =:= Size1 ->
    Size2 = Size1 - 1,
    {some, Item} = array:get(Index, Items1),
    Items2 = array:reset(Size2, Items1),
    Items3 = array:resize(Size2, Items2),
    Items4 = shrink(Items3),
    Vec2 = Vec1#markdown_vec{items = Items4, size = Size2},
    {Vec2, Item};
swap_remove(#markdown_vec{items = Items, size = 1}, 0) ->
    {some, Item} = array:get(0, Items),
    {new(), Item}.

-spec take(Vec, Index) -> {Vec, Item} when Vec :: t(Item), Item :: item(), Index :: index().
take(Vec1 = #markdown_vec{items = Items1, size = Size1}, Index) when Index >= 0 andalso Index < Size1 ->
    {some, Item} = array:get(Index, Items1),
    Items2 = array:reset(Index, Items1),
    Size2 = Size1 - 1,
    {halt, Items3} = array:foldr(fun repair/3, {cont, Items2, Size2}, Items2),
    Items4 = shrink(Items3),
    Vec2 = Vec1#markdown_vec{items = Items4, size = Size2},
    {Vec2, Item}.

-spec to_list(Vec) -> List when Vec :: t(Item), Item :: item(), List :: [Item].
to_list(#markdown_vec{size = 0}) ->
    [];
to_list(#markdown_vec{items = Items}) ->
    array:foldr(fun collect_items_array/3, [], Items);
to_list(Iterator = {{_, _}, #markdown_vec{}}) ->
    lists:reverse(foldl_iterator(Iterator, fun collect_items_iterator/3, [])).

-spec truncate(Vec, Size) -> Vec when Vec :: t(Item), Item :: item(), Size :: non_neg_integer().
truncate(Vec = #markdown_vec{size = Size}, Size) ->
    Vec;
truncate(#markdown_vec{size = OldSize}, 0) when OldSize > 0 ->
    new();
truncate(#markdown_vec{items = Items, size = OldSize}, NewSize) when NewSize < OldSize ->
    NewItems1 = array_with_capacity(NewSize),
    NewItems2 = split_off_keep(Items, 0, NewSize, NewItems1),
    NewVec = #markdown_vec{items = NewItems2, size = NewSize},
    NewVec.

-spec update(Vec, Index, UpdateFun) -> Vec when
    Vec :: t(Item), Item :: item(), Index :: index(), UpdateFun :: fun((Item) -> Item).
update(Vec1 = #markdown_vec{items = Items1, size = Size}, Index, UpdateFun) when
    Index >= 0 andalso Index < Size andalso is_function(UpdateFun, 1)
->
    {some, Item1} = array:get(Index, Items1),
    Item2 = UpdateFun(Item1),
    Items2 = array:set(Index, {some, Item2}, Items1),
    Vec2 = Vec1#markdown_vec{items = Items2},
    Vec2.

-spec update_last(Vec, UpdateFun) -> Vec when
    Vec :: t(Item), Item :: item(), UpdateFun :: fun((Item) -> Item).
update_last(Vec1 = #markdown_vec{size = Size}, UpdateFun) when Size > 0 andalso is_function(UpdateFun, 1) ->
    update(Vec1, Size - 1, UpdateFun).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [array_with_capacity/1]}).
-spec array_with_capacity(Capacity) -> Array when
    Capacity :: non_neg_integer(), Array :: array:array(Entry), Entry :: none | {some, Item}, Item :: item().
array_with_capacity(Capacity) ->
    markdown_types:dynamic_cast(array:new(Capacity, [{default, none}, fixed])).

%% @private
-spec collect_append_array(Index, Entry, {Size, Items}) -> {Size, Items} when
    Index :: index(),
    Entry :: none | {some, Item},
    Item :: item(),
    Items :: array:array(Entry),
    Size :: non_neg_integer().
collect_append_array(_Index, {some, Item}, {Size, Items}) ->
    {Size + 1, array:set(Size, {some, Item}, Items)}.

%% @private
-spec collect_append_list(Item, {Size, Items}) -> {Size, Items} when
    Item :: item(), Size :: non_neg_integer(), Items :: array:array(Entry), Entry :: none | {some, Item}.
collect_append_list(Item, {Size, Items}) ->
    {Size + 1, array:set(Size, {some, Item}, Items)}.

%% @private
-spec collect_contains(Index, Item, {Target, Found}) -> {cont | halt, {Target, Found}} when
    Index :: index(), Item :: item(), Target :: item(), Found :: boolean().
collect_contains(_Index, Target, {Target, false}) ->
    {halt, {Target, true}};
collect_contains(_Index, _Item, Acc = {_Target, false}) ->
    {cont, Acc}.

%% @private
-spec collect_flatten(Index, Vec, VecAcc) -> VecAcc when
    Index :: index(), Vec :: t(Item | NestedVec), NestedVec :: t(), VecAcc :: t(Item), Item :: item().
collect_flatten(_Index, Vec = #markdown_vec{}, VecAcc = #markdown_vec{}) ->
    reduce(Vec, VecAcc, fun collect_flatten/3);
collect_flatten(_Index, Item, VecAcc = #markdown_vec{}) ->
    push(VecAcc, Item).

%% @private
-spec collect_items_array(Index, Entry, Items) -> Items when
    Index :: index(), Entry :: none | {some, Item}, Item :: item(), Items :: [Item].
collect_items_array(_Index, {some, Item}, Items) ->
    [Item | Items].

%% @private
-spec collect_items_iterator(Index, Item, Items) -> Items when
    Index :: index(), Item :: item(), Items :: [Item].
collect_items_iterator(_Index, Item, Items) ->
    [Item | Items].

%% @private
-spec collect_remove_first(Index, Item, {Target, VecAcc}) -> {cont | halt, {Target, VecAcc}} when
    Index :: index(), Item :: item(), Target :: item(), VecAcc :: t(Item | Target).
collect_remove_first(Index, Target, {Target, VecAcc1 = #markdown_vec{}}) ->
    {VecAcc2, Target} = take(VecAcc1, Index),
    {halt, {Target, VecAcc2}};
collect_remove_first(_Index, _Item, Acc = {_Target, _VecAcc = #markdown_vec{}}) ->
    {cont, Acc}.

%% @private
-spec collect_reverse(Index, Item, VecAcc) -> VecAcc when
    Index :: index(), Item :: item(), VecAcc :: t(Item).
collect_reverse(Index, Item, VecAcc = #markdown_vec{size = Size}) ->
    set(VecAcc, Size - Index - 1, Item).

%% @private
-spec collect_vec(Index, Item, VecAcc) -> VecAcc when
    Index :: index(), Item :: item(), VecAcc :: t(Item).
collect_vec(_Index, Item, VecAcc = #markdown_vec{}) ->
    push(VecAcc, Item).

%% @private
-spec from_list(Item, {Index, Items}) -> {Index, Items} when
    Item :: item(), Index :: index(), Items :: array:array(none | {some, Item}).
from_list(Item, {Index, Items1}) ->
    Items2 = array:set(Index, {some, Item}, Items1),
    {Index + 1, Items2}.

%% @private
-spec from_list_internal_iterator({OldIndex, Item}, {Index, Items}) -> {Index, Items} when
    OldIndex :: index(), Item :: item(), Index :: index(), Items :: array:array(none | {some, Item}).
from_list_internal_iterator({_OldIndex, Item}, {Index, Items1}) ->
    Items2 = array:set(Index, {some, Item}, Items1),
    {Index + 1, Items2}.

%% @private
-spec foldl_iterator(Iterator, Function, AccIn) -> AccOut when
    Index :: index(),
    Item :: item(),
    Iterator :: iterator(Item),
    Function :: fun((Index, Item, AccIn) -> AccOut),
    AccIn :: dynamic(),
    AccOut :: dynamic().
foldl_iterator(Iterator, Function, Acc1) ->
    case next(Iterator) of
        none ->
            Acc1;
        {Index, Item, NextIterator} ->
            Acc2 = Function(Index, Item, Acc1),
            foldl_iterator(NextIterator, Function, Acc2)
    end.

%% @private
-spec iterator_internal(Vec, OrderFun) -> InternalIterator when
    Vec :: t(Item), Item :: item(), OrderFun :: order_func(Item), InternalIterator :: [{index(), Item}].
iterator_internal(_Vec = #markdown_vec{items = Items}, OrderFun) when is_function(OrderFun, 4) ->
    Sorted = lists:sort(
        fun({AIndex, {some, AItem}}, {BIndex, {some, BItem}}) ->
            OrderFun(AIndex, AItem, BIndex, BItem)
        end,
        array:to_orddict(Items)
    ),
    [{Index, Item} || {Index, {some, Item}} <- Sorted].

%% @private
-spec key_func_to_order_func(KeyFun) -> OrderFun when
    KeyFun :: key_func(Item), OrderFun :: order_func(Item), Item :: item().
key_func_to_order_func(KeyFun) when is_function(KeyFun, 2) ->
    OrderFun = fun(AIndex, AItem, BIndex, BItem) ->
        AKey = KeyFun(AIndex, AItem),
        BKey = KeyFun(BIndex, BItem),
        AKey =< BKey
    end,
    OrderFun.

%% @private
-spec reduce_iterator(Iterator, AccIn, Function) -> AccOut when
    Iterator :: iterator(Item),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_iterator(Iterator, AccIn, Function) ->
    case next(Iterator) of
        none ->
            AccIn;
        {Index, Item, NextIterator} ->
            AccOut = Function(Index, Item, AccIn),
            reduce_iterator(NextIterator, AccOut, Function)
    end.

%% @private
-spec reduce_right_iterator(Entries, AccIn, Function) -> AccOut when
    Entries :: [{Index, Item}],
    Index :: index(),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_right_iterator([{Index, Item} | Entries], AccIn, Function) ->
    AccOut = Function(Index, Item, AccIn),
    reduce_right_iterator(Entries, AccOut, Function);
reduce_right_iterator([], AccIn, _Function) ->
    AccIn.

%% @private
-spec reduce_right_iterator(Iterator, AccIn, Function, Entries) -> AccOut when
    Iterator :: iterator(Item),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_func(Item, AccIn, AccOut),
    AccOut :: dynamic(),
    Entries :: [{Index, Item}],
    Index :: index().
reduce_right_iterator(Iterator, AccIn, Function, Entries) ->
    case next(Iterator) of
        none ->
            reduce_right_iterator(Entries, AccIn, Function);
        {Index, Item, NextIterator} ->
            reduce_right_iterator(NextIterator, AccIn, Function, [{Index, Item} | Entries])
    end.

%% @private
-spec reduce_right_while_iterator(Entries, AccIn, Function) -> AccOut when
    Entries :: [{Index, Item}],
    Index :: index(),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_while_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_right_while_iterator([{Index, Item} | Entries], AccIn, Function) ->
    case Function(Index, Item, AccIn) of
        {cont, AccOut} ->
            reduce_right_while_iterator(Entries, AccOut, Function);
        {halt, AccOut} ->
            AccOut
    end;
reduce_right_while_iterator([], AccIn, _Function) ->
    AccIn.

%% @private
-spec reduce_right_while_iterator(Iterator, AccIn, Function, Entries) -> AccOut when
    Iterator :: iterator(Item),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_while_func(Item, AccIn, AccOut),
    AccOut :: dynamic(),
    Entries :: [{Index, Item}],
    Index :: index().
reduce_right_while_iterator(Iterator, AccIn, Function, Entries) ->
    case next(Iterator) of
        none ->
            reduce_right_while_iterator(Entries, AccIn, Function);
        {Index, Item, NextIterator} ->
            reduce_right_while_iterator(NextIterator, AccIn, Function, [{Index, Item} | Entries])
    end.

%% @private
-spec reduce_while_insert(OldIndex, Item, {Index, ItemsAcc}) -> {cont | halt, {Index, ItemsAcc}} when
    OldIndex :: index(), Item :: item(), Index :: index(), ItemsAcc :: array:array(Entry), Entry :: none | {some, Item}.
reduce_while_insert(OldIndex, Item, {Index, ItemsAcc}) when Index =< OldIndex ->
    NewIndex = OldIndex + 1,
    {cont, {Index, array:set(NewIndex, {some, Item}, ItemsAcc)}};
reduce_while_insert(_OldIndex, _Item, {Index, ItemsAcc}) ->
    {halt, {Index, ItemsAcc}}.

%% @private
-spec reduce_while_iterator(Iterator, AccIn, Function) -> AccOut when
    Iterator :: iterator(Item),
    Item :: item(),
    AccIn :: dynamic(),
    Function :: reduce_while_func(Item, AccIn, AccOut),
    AccOut :: dynamic().
reduce_while_iterator(Iterator, AccIn, Function) ->
    case next(Iterator) of
        none ->
            AccIn;
        {Index, Item, NextIterator} ->
            case Function(Index, Item, AccIn) of
                {cont, AccOut} ->
                    reduce_while_iterator(NextIterator, AccOut, Function);
                {halt, AccOut} ->
                    AccOut
            end
    end.

%% @private
-spec repair(Index, none | {some, Item}, Acc) -> Acc when
    Index :: index(),
    Item :: item(),
    Acc :: {cont, Items, NewSize} | {halt, Items},
    Items :: array:array(none | {some, Item}),
    NewSize :: non_neg_integer().
repair(_Index, none, {cont, Items1, NewSize}) ->
    Items2 = array:set(NewSize, none, Items1),
    Items3 = array:resize(NewSize, Items2),
    {halt, Items3};
repair(Index, {some, Item}, {cont, Items1, NewSize}) ->
    Items2 = array:set(Index - 1, {some, Item}, Items1),
    {cont, Items2, NewSize};
repair(_Index, {some, _Item}, Acc = {halt, _Items}) ->
    Acc.

%% @private
-spec shrink(Items) -> Items when
    Items :: array:array(none | {some, Item}),
    Item :: item().
shrink(Items) ->
    %% TODO: make this less expensive, maybe upstream fix
    List = array:to_list(Items),
    markdown_types:dynamic_cast(array:fix(array:from_list(List, none))).

%% @private
-spec split_off_keep(Items, Index, KeepSize, KeepItems) -> KeepItems when
    Items :: array:array(Entry),
    Entry :: none | {some, Item},
    Item :: item(),
    Index :: index(),
    KeepSize :: non_neg_integer(),
    KeepItems :: Items.
split_off_keep(Items, Index, KeepSize, KeepItems1) when Index < KeepSize ->
    KeepItems2 = array:set(Index, array:get(Index, Items), KeepItems1),
    split_off_keep(Items, Index + 1, KeepSize, KeepItems2);
split_off_keep(_Items, KeepSize, KeepSize, KeepItems) ->
    KeepItems.

%% @private
-spec split_off_drop(Items, Index, KeepSize, DropSize, DropItems) -> DropItems when
    Items :: array:array(Entry),
    Entry :: none | {some, Item},
    Item :: item(),
    Index :: index(),
    KeepSize :: non_neg_integer(),
    DropSize :: non_neg_integer(),
    DropItems :: Items.
split_off_drop(Items, Index, KeepSize, DropSize, DropItems1) when Index < DropSize ->
    DropItems2 = array:set(Index, array:get(Index + KeepSize, Items), DropItems1),
    split_off_drop(Items, Index + 1, KeepSize, DropSize, DropItems2);
split_off_drop(_Items, DropSize, _KeepSize, DropSize, DropItems) ->
    DropItems.
