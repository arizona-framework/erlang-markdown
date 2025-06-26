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
-module(markdown_construct_list_item).
-moduledoc """
List item occurs in the [document][] content type.

## Grammar

List item forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
; Restriction: if there is no space after the marker, the start must be followed by an `eol`.
; Restriction: if the first line after the marker is not blank and starts with `5(space_or_tab)`,
; only the first `space_or_tab` is part of the start.
list_item_start ::= '*' | '+' | '-' | 1*9(ascii_decimal) ('.' | ')') [1*4 space_or_tab]

; Restriction: blank line allowed, except when this is the first continuation after a blank start.
; Restriction: if not blank, the line must be indented, exactly `n` times.
list_item_cont ::= [n(space_or_tab)]
```

Further lines that are not prefixed with `list_item_cont` cause the list
item to be exited, except when those lines are lazy continuation or blank.
Like so many things in markdown, list items too are complex.
See [*§ Phase 1: block structure* in `CommonMark`][commonmark_block] for
more on parsing details.

As list item is a container, it takes several bytes from the start of the
line, while the rest of the line includes more containers or flow.

## HTML

List item relates to the `<li>`, `<ol>`, and `<ul>` elements in HTML.
See [*§ 4.4.8 The `li` element*][html_li],
[*§ 4.4.5 The `ol` element*][html_ol], and
[*§ 4.4.7 The `ul` element*][html_ul] in the HTML spec for more info.

## Recommendation

Use a single space after a marker.
Never use lazy continuation.

## Tokens

*   [`ListItem`][Name::ListItem]
*   [`ListItemMarker`][Name::ListItemMarker]
*   [`ListItemPrefix`][Name::ListItemPrefix]
*   [`ListItemValue`][Name::ListItemValue]
*   [`ListOrdered`][Name::ListOrdered]
*   [`ListUnordered`][Name::ListUnordered]

## References

*   [`list.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/list.js)
*   [*§ 5.2 List items* in `CommonMark`](https://spec.commonmark.org/0.31/#list-items)
*   [*§ 5.3 Lists* in `CommonMark`](https://spec.commonmark.org/0.31/#lists)

[document]: crate::construct::document
[html_li]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-li-element
[html_ol]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-ol-element
[html_ul]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-ul-element
[commonmark_block]: https://spec.commonmark.org/0.31/#phase-1-block-structure
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    before/1,
    before_unordered/1,
    before_ordered/1,
    value/1,
    marker/1,
    marker_after/1,
    marker_after_filled/1,
    whitespace/1,
    whitespace_after/1,
    prefix_other/1,
    'after'/1,
    cont_start/1,
    cont_blank/1,
    cont_filled/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of list item.

```markdown
> | * a
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{list_item = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, list_item),
    case Current of
        {some, C} when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(list_item_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true -> ?TAB_SIZE - 1;
                    false -> infinity
                end,
            {Tokenizer4, StateName} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxIndent
            ),
            State = markdown_state:retry(StateName),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(list_item_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After optional whitespace, at list item prefix.

```markdown
> | * a
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}, interrupt = Interrupt}) ->
    case Current of
        %% Unordered.
        C when C =:= $* orelse C =:= $- ->
            Tokenizer2 = markdown_tokenizer:check(
                Tokenizer1,
                markdown_state:nok(),
                markdown_state:next(list_item_before_unordered)
            ),
            State = markdown_state:retry(thematic_break_start),
            {Tokenizer2, State};
        $+ ->
            State = markdown_state:retry(list_item_before_unordered),
            {Tokenizer1, State};
        %% Ordered.
        C when C =:= $1 orelse (C >= $0 andalso C =< $9 andalso not Interrupt) ->
            State = markdown_state:retry(list_item_before_ordered),
            {Tokenizer1, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end;
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
At unordered list item marker.

The line is not a thematic break.

```markdown
> | * a
    ^
```
""".
-spec before_unordered(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_unordered(Tokenizer) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer, list_item_prefix),
    State = markdown_state:retry(list_item_marker),
    {Tokenizer2, State}.

-doc """
At ordered list item value.

```markdown
> | * a
    ^
```
""".
-spec before_ordered(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_ordered(Tokenizer) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer, list_item_prefix),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, list_item_value),
    State = markdown_state:retry(list_item_value),
    {Tokenizer3, State}.

-doc """
In ordered list item value.

```markdown
> | 1. a
    ^
```
""".
-spec value(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
value(
    Tokenizer = #markdown_tokenizer{
        current = {some, Current}, interrupt = Interrupt, tokenize_state = #markdown_tokenize_state{size = Size}
    }
) when
    (Current =:= $. orelse Current =:= $)), (not Interrupt orelse Size < 2)
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer, list_item_value),
    State = markdown_state:retry(list_item_marker),
    {Tokenizer2, State};
value(
    Tokenizer = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState = #markdown_tokenize_state{size = Size}
    }
) when
    Current >= $0, Current =< $9, Size + 1 < ?LIST_ITEM_VALUE_SIZE_MAX
->
    TokenizeState2 = TokenizeState#markdown_tokenize_state{size = Size + 1},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(list_item_value),
    {Tokenizer3, State};
value(Tokenizer = #markdown_tokenizer{tokenize_state = TokenizeState}) ->
    TokenizeState2 = TokenizeState#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
At list item marker.

```markdown
> | * a
    ^
> | 1. b
     ^
```
""".
-spec marker(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
marker(Tokenizer) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer, list_item_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, list_item_marker),
    State = markdown_state:next(list_item_marker_after),
    {Tokenizer4, State}.

-doc """
After list item marker.

```markdown
> | * a
     ^
> | 1. b
      ^
```
""".
-spec marker_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
marker_after(Tokenizer = #markdown_tokenizer{tokenize_state = TokenizeState}) ->
    TokenizeState2 = TokenizeState#markdown_tokenize_state{size = 1},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:check(
        Tokenizer2,
        markdown_state:next(list_item_after),
        markdown_state:next(list_item_marker_after_filled)
    ),
    State = markdown_state:retry(blank_line_start),
    {Tokenizer3, State}.

-doc """
After list item marker.

The marker is not followed by a blank line.

```markdown
> | * a
     ^
```
""".
-spec marker_after_filled(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
marker_after_filled(Tokenizer) ->
    TokenizeState = (Tokenizer#markdown_tokenizer.tokenize_state)#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer#markdown_tokenizer{tokenize_state = TokenizeState},

    %% Attempt to parse up to the largest allowed indent, `nok` if there is more whitespace.
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(list_item_after),
        markdown_state:next(list_item_prefix_other)
    ),
    State = markdown_state:retry(list_item_whitespace),
    {Tokenizer3, State}.

-doc """
After marker, at whitespace.

```markdown
> | * a
     ^
```
""".
-spec whitespace(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
whitespace(Tokenizer) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer,
        markdown_state:next(list_item_whitespace_after),
        markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer2, 1, ?TAB_SIZE
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State}.

-doc """
After acceptable whitespace.

```markdown
> | * a
     ^
```
""".
-spec whitespace_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
whitespace_after(Tokenizer = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t; Current =:= $\s ->
    State = markdown_state:nok(),
    {Tokenizer, State};
whitespace_after(Tokenizer) ->
    State = markdown_state:ok(),
    {Tokenizer, State}.

-doc """
After marker, followed by no indent or more indent that needed.

```markdown
> | * a
     ^
```
""".
-spec prefix_other(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
prefix_other(Tokenizer = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t; Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer, space_or_tab),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, space_or_tab),
    State = markdown_state:next(list_item_after),
    {Tokenizer4, State};
prefix_other(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After list item prefix.

```markdown
> | * a
      ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size}}) ->
    Blank = Size =:= 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    case Blank andalso Tokenizer2#markdown_tokenizer.interrupt of
        true ->
            State = markdown_state:nok(),
            {Tokenizer2, State};
        false ->
            Events = Tokenizer2#markdown_tokenizer.events,
            Start = markdown_util_skip:to_back(Events, markdown_vec:size(Events) - 1, [list_item]),
            StartEvent = markdown_vec:get(Events, Start),
            Prefix1 = markdown_slice:size(
                markdown_slice:from_position(
                    markdown_tokenizer:bytes(Tokenizer2),
                    markdown_position:new(StartEvent#markdown_event.point, Tokenizer2#markdown_tokenizer.point)
                )
            ),
            Prefix2 =
                case Blank of
                    true -> Prefix1 + 1;
                    false -> Prefix1
                end,
            DocContinued = TokenizeState2#markdown_tokenize_state.document_continued,
            DocStack1 = TokenizeState2#markdown_tokenize_state.document_container_stack,
            DocStack2 = markdown_vec:update(DocStack1, DocContinued, fun(Container) ->
                Container#markdown_container{
                    blank_initial = Blank,
                    size = Prefix2
                }
            end),
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{document_container_stack = DocStack2},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, list_item_prefix),
            Tokenizer5 = markdown_tokenizer:register_resolver_before(Tokenizer4, list_item),
            State = markdown_state:ok(),
            {Tokenizer5, State}
    end.

-doc """
Start of list item continuation.

```markdown
  | * a
> |   b
    ^
```
""".
-spec cont_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_start(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:check(
        Tokenizer1,
        markdown_state:next(list_item_cont_blank),
        markdown_state:next(list_item_cont_filled)
    ),
    State = markdown_state:retry(blank_line_start),
    {Tokenizer2, State}.

-doc """
Start of blank list item continuation.

```markdown
  | * a
> |
    ^
  |   b
```
""".
-spec cont_blank(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_blank(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = #markdown_tokenize_state{
            document_continued = DocumentContinued, document_container_stack = Stack
        }
    }
) ->
    Container = markdown_vec:get(Stack, DocumentContinued),
    Size = Container#markdown_container.size,

    case Container#markdown_container.blank_initial of
        true ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        false when Current =:= {some, $\t} orelse Current =:= {some, $\s} ->
            %% Consume, optionally, at most `size`.
            {Tokenizer2, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer1, 0, Size
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer2, State};
        false ->
            State = markdown_state:ok(),
            {Tokenizer1, State}
    end.

-doc """
Start of non-blank list item continuation.

```markdown
  | * a
> |   b
    ^
```
""".
-spec cont_filled(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
cont_filled(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued, document_container_stack = Stack1
            }
    }
) ->
    Container1 = markdown_vec:get(Stack1, DocumentContinued),
    Size = Container1#markdown_container.size,

    Container2 = Container1#markdown_container{blank_initial = false},
    Stack2 = markdown_vec:set(Stack1, DocumentContinued, Container2),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = Stack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    case Current of
        {some, $\t} ->
            %% Consume exactly `size`.
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer2, Size, Size
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        {some, $\s} ->
            %% Consume exactly `size`.
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer2, Size, Size
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.

-doc """
Find adjacent list items with the same marker.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(
    Tokenizer1 = #markdown_tokenizer{events = Events1, parse_state = #markdown_parse_state{bytes = Bytes}, map = Map1}
) ->
    ListsWip1 = markdown_vec:new(),
    Lists1 = markdown_vec:new(),
    % io:format("\n\n[~w:~w] BEFORE list_item:resolve_loop\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Lists1)]),
    Lists2 = resolve_loop(Events1, Bytes, 0, 0, ListsWip1, Lists1),
    % io:format("\n\n[~w:~w] AFTER list_item:resolve_loop\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Lists2)]),

    %% Inject events
    Map2 = markdown_vec:reduce(
        Lists2,
        Map1,
        fun(_, ListItem, MapAcc1) ->
            StartEvent = markdown_vec:get(Events1, ListItem#markdown_list_item.start),
            EndEvent = markdown_vec:get(Events1, ListItem#markdown_list_item.'end'),
            Name =
                case ListItem#markdown_list_item.marker of
                    $. -> list_ordered;
                    $) -> list_ordered;
                    _ -> list_unordered
                end,

            ListStart = StartEvent#markdown_event{name = Name},
            ListEnd = EndEvent#markdown_event{name = Name},

            MapAcc2 = markdown_edit_map:add(MapAcc1, ListItem#markdown_list_item.start, 0, [ListStart]),
            MapAcc3 = markdown_edit_map:add(MapAcc2, ListItem#markdown_list_item.'end' + 1, 0, [ListEnd]),

            MapAcc3
        end
    ),

    %% Consume map
    {Map3, Events2} = markdown_edit_map:consume(Map2, Events1),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events2, map = Map3},

    {Tokenizer2, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(Events, Bytes, Index, Balance, ListsWip, Lists) -> Lists when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    Index :: non_neg_integer(),
    Balance :: non_neg_integer(),
    ListsWip :: markdown_vec:t(ListItem),
    ListItem :: markdown_list_item:t(),
    Lists :: markdown_vec:t(ListItem).
resolve_loop(Events, Bytes, Index1, Balance1, ListsWip1, Lists1) when
    Index1 >= 0 andalso Index1 < ?markdown_vec_size(Events)
->
    case markdown_vec:get(Events, Index1) of
        #markdown_event{name = list_item, kind = 'enter'} ->
            End = markdown_util_skip:opt(Events, Index1, [list_item]) - 1,
            MarkerIndex = markdown_util_skip:to(Events, Index1, [list_item_marker]),
            MarkerEvent = markdown_vec:get(Events, MarkerIndex),
            Marker = binary:at(Bytes, MarkerEvent#markdown_event.point#markdown_point.offset),
            Current = #markdown_list_item{marker = Marker, balance = Balance1, start = Index1, 'end' = End},
            % io:format("\n\n[~w:~w] BEFORE list_item:resolve_loop:enter:check_for_matches\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(ListsWip1), markdown_debug:rust_debug_string(Lists1)]),
            {ListsWip2, Lists2, Matched} = resolve_loop__check_for_matches(
                Events, markdown_vec:size(ListsWip1), Current, ListsWip1, Lists1
            ),
            % io:format("\n\n[~w:~w] AFTER list_item:resolve_loop:enter:check_for_matches\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(ListsWip2), markdown_debug:rust_debug_string(Lists2)]),
            % io:format("\n\n[~w:~w] BEFORE list_item:resolve_loop:enter:matched = ~w\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), Matched, markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(ListsWip2), markdown_debug:rust_debug_string(Lists2)]),
            {ListsWip3, Lists3} =
                case Matched of
                    true ->
                        {ListsWip2, Lists2};
                    false ->
                        resolve_loop__handle_new_item(markdown_vec:size(ListsWip2), Current, ListsWip2, Lists2, none)
                end,
            % io:format("\n\n[~w:~w] AFTER list_item:resolve_loop:enter:matched = ~w\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), Matched, markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(ListsWip3), markdown_debug:rust_debug_string(Lists3)]),
            Balance2 = Balance1 + 1,
            Index2 = Index1 + 1,
            resolve_loop(Events, Bytes, Index2, Balance2, ListsWip3, Lists3);
        #markdown_event{name = list_item, kind = 'exit'} ->
            Balance2 = Balance1 - 1,
            Index2 = Index1 + 1,
            resolve_loop(Events, Bytes, Index2, Balance2, ListsWip1, Lists1);
        _ ->
            Index2 = Index1 + 1,
            resolve_loop(Events, Bytes, Index2, Balance1, ListsWip1, Lists1)
    end;
resolve_loop(_Events, _Bytes, _Index, _Balance, ListsWip, Lists) ->
    % io:format("\n\n[~w] INSPECT list_item:resolve_loop:append\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(ListsWip), markdown_debug:rust_debug_string(Lists)]),
    markdown_vec:append(Lists, ListsWip).

%% @private
-spec resolve_loop__check_for_matches(Events, ListIndex, Current, ListsWip, Lists) -> {ListsWip, Lists, Matched} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ListIndex :: markdown_vec:index(),
    Current :: ListItem,
    ListItem :: markdown_list_item:t(),
    ListsWip :: markdown_vec:t(ListItem),
    Lists :: markdown_vec:t(ListItem),
    Matched :: boolean().
resolve_loop__check_for_matches(Events, ListIndex1, Current, ListsWip1, Lists1) when ListIndex1 > 0 ->
    ListIndex2 = ListIndex1 - 1,
    Previous1 = markdown_vec:get(ListsWip1, ListIndex2),
    Before = markdown_util_skip:opt(Events, Previous1#markdown_list_item.'end' + 1, [
        space_or_tab, line_ending, blank_line_ending, block_quote_prefix
    ]),
    case
        Previous1#markdown_list_item.marker =:= Current#markdown_list_item.marker andalso
            Previous1#markdown_list_item.balance =:= Current#markdown_list_item.balance andalso
            Before =:= Current#markdown_list_item.start
    of
        true ->
            Previous2 = Previous1#markdown_list_item{'end' = Current#markdown_list_item.'end'},
            ListsWip2 = markdown_vec:set(ListsWip1, ListIndex2, Previous2),
            {ListsWip3, ListsAdd} = markdown_vec:split_off(ListsWip2, ListIndex2 + 1),
            Lists2 = markdown_vec:append(Lists1, ListsAdd),
            Matched = true,
            {ListsWip3, Lists2, Matched};
        false ->
            resolve_loop__check_for_matches(Events, ListIndex2, Current, ListsWip1, Lists1)
    end;
resolve_loop__check_for_matches(_Events, _ListIndex, _Current, ListsWip, Lists) ->
    {ListsWip, Lists, false}.

%% @private
-compile({inline, [resolve_loop__add_new_item/4]}).
-spec resolve_loop__add_new_item(Current, ListsWip, Lists, OptionExit) -> {ListsWip, Lists} when
    Current :: ListItem,
    ListItem :: markdown_list_item:t(),
    ListsWip :: markdown_vec:t(ListItem),
    Lists :: markdown_vec:t(ListItem),
    OptionExit :: markdown_option:t(Exit),
    Exit :: markdown_types:usize().
resolve_loop__add_new_item(Current, ListsWip1, Lists1, OptionExit) ->
    {ListsWip2, Lists2} =
        case OptionExit of
            none ->
                {ListsWip1, Lists1};
            {some, Exit} ->
                % io:format("\n\n[~w:~w] BEFORE list_item:resolve_loop:enter:exit = ~w\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), Exit, markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(ListsWip1), markdown_debug:rust_debug_string(Lists1)]),
                LW1 = ListsWip1,
                {LW2, LAdd} = markdown_vec:split_off(LW1, Exit),
                L1 = Lists1,
                L2 = markdown_vec:append(L1, LAdd),
                % io:format("\n\n[~w:~w] BEFORE list_item:resolve_loop:enter:exit = ~w\ncurrent = ~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), Exit, markdown_debug:rust_debug_string(Current), markdown_debug:rust_debug_string(LW2), markdown_debug:rust_debug_string(L2)]),
                {LW2, L2}
        end,
    ListsWip3 = markdown_vec:push(ListsWip2, Current),
    {ListsWip3, Lists2}.

%% @private
-spec resolve_loop__handle_new_item(Index, Current, ListsWip, Lists, OptionExit) -> {ListsWip, Lists} when
    Index :: integer(),
    Current :: ListItem,
    ListItem :: markdown_list_item:t(),
    ListsWip :: markdown_vec:t(ListItem),
    Lists :: markdown_vec:t(ListItem),
    OptionExit :: markdown_option:t(Exit),
    Exit :: markdown_types:usize().
resolve_loop__handle_new_item(Index1, Current, ListsWip1, Lists1, OptionExit1) when Index1 > 0 ->
    Index2 = Index1 - 1,
    %% If the current (new) item starts after where this
    %% item on the stack ends, we can remove it from the
    %% stack.
    case Current#markdown_list_item.start > (markdown_vec:get(ListsWip1, Index2))#markdown_list_item.'end' of
        true ->
            OptionExit2 = {some, Index2},
            resolve_loop__handle_new_item(Index2, Current, ListsWip1, Lists1, OptionExit2);
        false ->
            resolve_loop__add_new_item(Current, ListsWip1, Lists1, OptionExit1)
    end;
resolve_loop__handle_new_item(_Index, Current, ListsWip1, Lists1, OptionExit) ->
    resolve_loop__add_new_item(Current, ListsWip1, Lists1, OptionExit).
