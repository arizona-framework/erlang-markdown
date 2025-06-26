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
-module(markdown_construct_document).
-moduledoc """
How to handle [`markdown_state:ok()`][] or [`markdown_state:nok()`][].
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    before_frontmatter/1,
    container_existing_before/1,
    container_existing_after/1,
    container_new_before/1,
    container_new_before_not_block_quote/1,
    container_new_before_not_list/1,
    container_new_before_not_footnote_definition/1,
    container_new_after/1,
    containers_after/1,
    flow_inside/1,
    flow_end/1
]).

%% Types
-doc """
Phases where we can exit containers.
""".
-type phase() ::
    %% After parsing a line of lazy flow which resulted in something that
    %% exits containers before the line.
    %%
    %% ```markdown
    %%   | * a
    %% > | ```js
    %%          ^
    %%   | b
    %%   | ```
    %% ```
    'after'
    %% When a new container replaces an existing container.
    %%
    %% ```markdown
    %%   | * a
    %% > | > b
    %%     ^
    %% ```
    | 'prefix'
    %% After everything.
    %%
    %% ```markdown
    %% > | * a
    %%        ^
    %% ```
    | 'eof'.

-export_type([
    phase/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of document, at an optional BOM.

```markdown
> | a
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{parse_state = ParseState, point = Point, tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        document_child = {some, markdown_tokenizer:new(Point, ParseState)}
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{
        tokenize_state = TokenizeState2
    },
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(document_before_frontmatter), markdown_state:next(document_before_frontmatter)
    ),
    State = markdown_state:retry(bom_start),
    {Tokenizer3, State}.

-doc """
At optional frontmatter.

```markdown
> | ---
    ^
  | title: Venus
  | ---
```
""".
-spec before_frontmatter(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_frontmatter(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(document_container_new_before),
        markdown_state:next(document_container_new_before)
    ),
    State = markdown_state:retry(frontmatter_start),
    {Tokenizer2, State}.

-doc """
At optional existing containers.

```markdown
  | * a
> | > b
    ^
```
""".
-spec container_existing_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_existing_before(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = #markdown_tokenize_state{
            document_continued = DocumentContinued, document_container_stack = DocumentContainerStack
        }
    }
) ->
    ContainerStackSize = markdown_vec:size(DocumentContainerStack),
    case DocumentContinued < ContainerStackSize of
        true ->
            Container = markdown_vec:get(DocumentContainerStack, DocumentContinued),
            Name =
                case Container#markdown_container.kind of
                    block_quote -> block_quote_cont_start;
                    gfm_footnote_definition -> gfm_footnote_definition_cont_start;
                    list_item -> list_item_cont_start
                end,
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(document_container_existing_after),
                markdown_state:next(document_container_new_before)
            ),
            State = markdown_state:retry(Name),
            {Tokenizer2, State};
        false ->
            %% Otherwise, check new containers.
            State = markdown_state:retry(document_container_new_before),
            {Tokenizer1, State}
    end.

-doc """
After continued existing container.

```markdown
  | * a
> |   b
      ^
```
""".
-spec container_existing_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_existing_after(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{document_continued = DocumentContinued1}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_continued = DocumentContinued1 + 1},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(document_container_existing_before),
    {Tokenizer2, State}.

-doc """
At new containers.

```markdown
> | * a
    ^
> | > b
    ^
```
""".
-spec container_new_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_before(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = #markdown_tokenize_state{
            document_child = OptionDocumentChild,
            document_continued = DocumentContinued,
            document_container_stack = DocumentContainerStack
        }
    }
) ->
    %% If we have completely continued, restore the flow’s past `interrupt` status.
    case DocumentContinued =:= markdown_vec:size(DocumentContainerStack) of
        true ->
            {some, #markdown_tokenizer{concrete = ChildConcrete, interrupt = ChildInterrupt}} = OptionDocumentChild,
            Tokenizer2 = Tokenizer1#markdown_tokenizer{interrupt = ChildInterrupt},
            %% …and if we're in a concrete construct, new containers can't "pierce" into them.
            case ChildConcrete of
                true ->
                    State = markdown_state:retry(document_containers_after),
                    {Tokenizer2, State};
                false ->
                    container_new_before__continue(Tokenizer2)
            end;
        false ->
            container_new_before__continue(Tokenizer1)
    end.

%% @private
-spec container_new_before__continue(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_before__continue(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued, document_container_stack = DocumentContainerStack1
            }
    }
) ->
    %% Check for a new container.
    %% Block quote?
    %% Add a new container at the end of the stack.
    Container = markdown_container:block_quote(false, 0),
    Tail = markdown_vec:size(DocumentContainerStack1),
    DocumentContainerStack2 = markdown_vec:push(DocumentContainerStack1, Container),
    %% Swap the existing container with the new one.
    DocumentContainerStack3 = markdown_vec:swap(DocumentContainerStack2, DocumentContinued, Tail),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = DocumentContainerStack3},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(document_container_new_after),
        markdown_state:next(document_container_new_before_not_block_quote)
    ),
    State = markdown_state:retry(block_quote_start),
    {Tokenizer3, State}.

-doc """
At new container, but not a block quote.

```markdown
> | * a
    ^
```
""".
-spec container_new_before_not_block_quote(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_before_not_block_quote(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued, document_container_stack = DocumentContainerStack1
            }
    }
) ->
    %% List item?
    %% We replace the empty block quote container for this new list item one.
    Container = markdown_container:list_item(false, 0),
    DocumentContainerStack2 = markdown_vec:set(DocumentContainerStack1, DocumentContinued, Container),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = DocumentContainerStack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(document_container_new_after),
        markdown_state:next(document_container_new_before_not_list)
    ),
    State = markdown_state:retry(list_item_start),
    {Tokenizer3, State}.

-doc """
At new container, but not a block quote or list item.

```markdown
> | a
    ^
```
""".
-spec container_new_before_not_list(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_before_not_list(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued, document_container_stack = DocumentContainerStack1
            }
    }
) ->
    %% Footnote definition?
    %% We replace the empty list item container for this new footnote definition one.
    Container = markdown_container:gfm_footnote_definition(false, 0),
    DocumentContainerStack2 = markdown_vec:set(DocumentContainerStack1, DocumentContinued, Container),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = DocumentContainerStack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(document_container_new_after),
        markdown_state:next(document_container_new_before_not_gfm_footnote_definition)
    ),
    State = markdown_state:retry(gfm_footnote_definition_start),
    {Tokenizer3, State}.

-doc """
At new container, but not a block quote, list item, or footnote definition.

```markdown
> | a
   ^
```
""".
-spec container_new_before_not_footnote_definition(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_before_not_footnote_definition(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued, document_container_stack = DocumentContainerStack1
            }
    }
) ->
    %% It wasn't a new block quote, list item, or footnote definition.
    %% Swap the new container (in the middle) with the existing one (at the end).
    %% Drop what was in the middle.
    {DocumentContainerStack2, _} = markdown_vec:swap_remove(DocumentContainerStack1, DocumentContinued),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = DocumentContainerStack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(document_containers_after),
    {Tokenizer2, State}.

-doc """
After new container.

```markdown
> | * a
      ^
> | > b
      ^
```
""".
-spec container_new_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
container_new_after(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued1, document_container_stack = Stack1
            }
    }
) ->
    %% It was a new block quote, list item, or footnote definition.
    %% Swap the new container (in the middle) with the existing one (at the end).
    %% Take the new container.
    {Stack2, Container} = markdown_vec:swap_remove(Stack1, DocumentContinued1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = Stack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    %% If we did not continue all existing containers, and there is a new one,
    %% close the flow and those containers.
    case container_new_after__maybe_exit_containers(Tokenizer2) of
        {
            Tokenizer3 = #markdown_tokenizer{
                tokenize_state =
                    TokenizeState3 = #markdown_tokenize_state{
                        document_child = {some, DocumentChild3},
                        document_container_stack = Stack3,
                        document_continued = DocumentContinued3
                    }
            },
            ok
        } ->
            %% We are "piercing" into the flow with a new container.
            DocumentChild4 = DocumentChild3#markdown_tokenizer{pierce = true},
            TokenizeState4 = TokenizeState3#markdown_tokenize_state{document_child = {some, DocumentChild4}},

            %% Push the container to the stack and increment continued count
            Stack4 = markdown_vec:push(Stack3, Container),
            TokenizeState5 = TokenizeState4#markdown_tokenize_state{
                document_container_stack = Stack4,
                document_continued = DocumentContinued3 + 1
            },
            Tokenizer4 = Tokenizer3#markdown_tokenizer{
                tokenize_state = TokenizeState5,
                interrupt = false
            },

            State = markdown_state:retry(document_container_new_before),
            {Tokenizer4, State};
        {Tokenizer3, {error, Message}} ->
            State = markdown_state:error(Message),
            {Tokenizer3, State}
    end.

-doc """
After containers, at flow.

```markdown
> | * a
    ^
> | > b
    ^
```
""".
-spec containers_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
containers_after(
    Tokenizer1 = #markdown_tokenizer{
        point = Point,
        current = Current,
        events = Events1,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_child = {some, Child1},
                document_continued = DocumentContinued,
                document_container_stack = DocumentContainerStack,
                document_data_index = OptionPreviousIndex
            }
    }
) ->
    Child2 = Child1#markdown_tokenizer{
        lazy = DocumentContinued =/= markdown_vec:size(DocumentContainerStack)
    },
    Child3 = markdown_tokenizer:define_skip(Child2, Point),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_child = {some, Child3}},
    case Current of
        none ->
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:retry(document_flow_end),
            {Tokenizer2, State};
        _ ->
            CurrentIndex = markdown_vec:size(Events1),
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{document_data_index = {some, CurrentIndex}},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState3},
            Tokenizer3 =
                case OptionPreviousIndex of
                    {some, PreviousIndex} ->
                        EventUpdateFun = fun(Event1 = #markdown_event{link = {some, Link1}}) ->
                            Link2 = Link1#markdown_event_link{next = {some, CurrentIndex}},
                            Event2 = Event1#markdown_event{link = {some, Link2}},
                            Event2
                        end,
                        Events2 = markdown_vec:update(Events1, PreviousIndex, EventUpdateFun),
                        Tokenizer2#markdown_tokenizer{events = Events2};
                    _ ->
                        Tokenizer2
                end,
            Link = markdown_event_link:new(OptionPreviousIndex, none, flow),
            % markdown:display("\n\n~ts:~ts/1:\n\nLink =\n", [?MODULE, ?FUNCTION_NAME], Link),
            Tokenizer4 = markdown_tokenizer:enter_link(Tokenizer3, data, Link),
            State = markdown_state:retry(document_flow_inside),
            {Tokenizer4, State}
    end.

-doc """
In flow.

```markdown
> | * ab
      ^
```
""".
-spec flow_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
flow_inside(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(document_flow_end),
            {Tokenizer2, State};
        %% Note: EOL is part of data.
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, data),
            State = markdown_state:next(document_flow_end),
            {Tokenizer3, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(document_flow_inside),
            {Tokenizer2, State}
    end.

-doc """
After flow (after eol or at eof).

```markdown
    | * a
> | > b
    ^  ^
```
""".
-spec flow_end(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
flow_end(
    Tokenizer1 = #markdown_tokenizer{
        point = Point,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_child = {some, Child1 = #markdown_tokenizer{point = ChildPoint}},
                document_exits = DocumentExits1,
                document_child_state = OptionChildState
            }
    }
) ->
    {TokenizeState2, State1} =
        case OptionChildState of
            {some, ChildState} ->
                {TokenizeState1#markdown_tokenize_state{document_child_state = none}, ChildState};
            none ->
                {TokenizeState1, markdown_state:next(flow_start)}
        end,
    DocumentExits2 = markdown_vec:push(DocumentExits1, none),
    {Child2, State2} = markdown_tokenizer:push(
        Child1, markdown_point:to_index(ChildPoint), markdown_point:to_index(Point), State1
    ),
    TokenizeState3 = TokenizeState2#markdown_tokenize_state{
        document_child = {some, Child2}, document_child_state = {some, State2}, document_exits = DocumentExits2
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState3},
    flow_end__continue(Tokenizer2).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-compile({inline, [container_new_after__maybe_exit_containers/1]}).
-spec container_new_after__maybe_exit_containers(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(), Result :: ok | {error, Message}, Message :: markdown_message:t().
container_new_after__maybe_exit_containers(
    Tokenizer = #markdown_tokenizer{
        tokenize_state = #markdown_tokenize_state{
            document_container_stack = Stack, document_continued = DocumentContinued
        }
    }
) when ?markdown_vec_size(Stack) =/= DocumentContinued ->
    exit_containers(Tokenizer, prefix);
container_new_after__maybe_exit_containers(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, ok}.

%% @private
-doc """
Close containers (and flow if needed).
""".
-spec exit_containers(Tokenizer, Phase) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Phase :: phase(),
    Result :: ok | {error, Message},
    Message :: markdown_message:t().
exit_containers(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_continued = DocumentContinued,
                document_container_stack = DocumentContainerStack1,
                document_child = {some, Child1},
                document_child_state = OptionDocumentChildState
            }
    },
    Phase
) ->
    {DocumentContainerStack2, StackClose} = markdown_vec:split_off(DocumentContainerStack1, DocumentContinued),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_container_stack = DocumentContainerStack2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    %% Flush if needed.
    case Phase =/= 'after' of
        true ->
            State =
                case OptionDocumentChildState of
                    {some, DocumentChildState} ->
                        DocumentChildState;
                    none ->
                        markdown_state:next(flow_start)
                end,
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{document_child_state = none},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
            case markdown_tokenizer:flush(Child1, State, false) of
                {Child2, {ok, _Subresult}} ->
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{document_child = {some, Child2}},
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
                    exit_containers__continue(Tokenizer4, Phase, StackClose);
                {Child2, {error, Message}} ->
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{document_child = {some, Child2}},
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
                    {Tokenizer4, {error, Message}}
            end;
        false ->
            exit_containers__continue(Tokenizer2, Phase, StackClose)
    end.

%% @private
-spec exit_containers__continue(Tokenizer, Phase, StackClose) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Phase :: phase(),
    StackClose :: markdown_vec:t(markdown_container:t()),
    Result :: ok.
exit_containers__continue(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{document_child = {some, Child1}}
    },
    _Phase,
    _StackClose = #markdown_vec{size = 0}
) ->
    Child2 = Child1#markdown_tokenizer{interrupt = false},
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_child = {some, Child2}},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, ok};
exit_containers__continue(
    Tokenizer1 = #markdown_tokenizer{tokenize_state = #markdown_tokenize_state{document_exits = DocumentExits1}},
    Phase,
    StackClose = #markdown_vec{}
) ->
    ExitIndex =
        markdown_vec:size(DocumentExits1) -
            (case Phase of
                'after' -> 2;
                _ -> 1
            end),
    Exits1 = markdown_vec:new(),
    {Tokenizer2, Exits2} = exit_containers__stack_close(
        Tokenizer1, markdown_vec:iterator(StackClose, reversed), Exits1
    ),
    #markdown_tokenizer{
        tokenize_state =
            TokenizeState2 = #markdown_tokenize_state{document_child = {some, Child1}, document_exits = DocumentExits2}
    } = Tokenizer2,
    DocumentExits3 = markdown_vec:set(DocumentExits2, ExitIndex, {some, Exits2}),
    Child2 = Child1#markdown_tokenizer{interrupt = false},
    TokenizeState3 = TokenizeState2#markdown_tokenize_state{
        document_child = {some, Child2}, document_exits = DocumentExits3
    },
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
    {Tokenizer3, ok}.

%% @private
-spec exit_containers__stack_close(Tokenizer, StackCloseIterator, Exits) -> {Tokenizer, Exits} when
    Tokenizer :: markdown_tokenizer:t(),
    StackCloseIterator :: markdown_vec:iterator(markdown_container:t()),
    Exits :: markdown_vec:t(markdown_event:t()).
exit_containers__stack_close(
    Tokenizer1 = #markdown_tokenizer{point = Point, stack = Stack1}, StackCloseIterator1, Exits1
) ->
    case markdown_vec:next(StackCloseIterator1) of
        none ->
            {Tokenizer1, Exits1};
        {_, #markdown_container{kind = ContainerKind}, StackCloseIterator2} ->
            EventName =
                case ContainerKind of
                    block_quote -> block_quote;
                    gfm_footnote_definition -> gfm_footnote_definition;
                    list_item -> list_item
                end,
            Exits2 = markdown_vec:push(Exits1, markdown_event:new('exit', EventName, Point, none)),
            Stack2 = markdown_vec:remove_last(Stack1, EventName),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{stack = Stack2},
            exit_containers__stack_close(Tokenizer2, StackCloseIterator2, Exits2)
    end.

%% @private
-spec flow_end__continue(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
flow_end__continue(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_child =
                    {some, Child1 = #markdown_tokenizer{events = ChildEvents1, lazy = ChildLazy1, stack = ChildStack1}},
                document_container_stack = DocumentContainerStack1,
                document_continued = DocumentContinued1,
                document_lazy_accepting_before = DocumentLazyAcceptingBefore
            }
    }
) ->
    %% If we're in a lazy line, and the previous (lazy or not) line is something
    %% that can be lazy, and this line is that too, allow it.
    %%
    %% Accept:
    %%
    %% ```markdown
    %%   | * a
    %% > | b
    %%     ^
    %%   | ```
    %% ```
    %%
    %% Do not accept:
    %%
    %% ```markdown
    %%   | * # a
    %% > | b
    %%     ^
    %%   | ```
    %% ```
    %%
    %% Do not accept:
    %%
    %% ```markdown
    %%   | * a
    %% > | # b
    %%     ^
    %%   | ```
    %% ```

    DocumentLazyContinuationCurrent1 = false,

    %% Use two algo's: one for when we're suspended or in multiline things
    %% like definitions, another for when we fed the line ending and closed.
    ChildStackReduceWhile = fun(_Index, Name, false) ->
        case Name =:= content orelse Name =:= gfm_table_head of
            true ->
                {halt, true};
            false ->
                {cont, false}
        end
    end,
    DocumentLazyContinuationCurrent2 =
        markdown_vec:reduce_right_while(ChildStack1, DocumentLazyContinuationCurrent1, ChildStackReduceWhile),

    %% …another because we parse each "rest" line as a paragraph, and we passed
    %% a EOL already.
    ChildEventsSize1 = markdown_vec:size(ChildEvents1),
    DocumentLazyContinuationCurrent3 =
        case not DocumentLazyContinuationCurrent2 andalso not markdown_vec:is_empty(ChildEvents1) of
            true ->
                Before = markdown_util_skip:opt_back(
                    ChildEvents1, ChildEventsSize1 - 1, [line_ending]
                ),
                #markdown_event{name = ChildEventName} = markdown_vec:get(ChildEvents1, Before),
                (ChildEventName =:= content) orelse (ChildEventName =:= heading_setext_underline);
            false ->
                DocumentLazyContinuationCurrent2
        end,

    %% Reset "piercing".
    Child2 = Child1#markdown_tokenizer{pierce = false},
    DocumentContinued2 =
        case ChildLazy1 andalso DocumentLazyAcceptingBefore andalso DocumentLazyContinuationCurrent3 of
            true ->
                markdown_vec:size(DocumentContainerStack1);
            false ->
                DocumentContinued1
        end,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        document_child = {some, Child2},
        document_continued = DocumentContinued2
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},

    {Tokenizer3, Result} =
        case DocumentContinued2 =/= markdown_vec:size(DocumentContainerStack1) of
            true ->
                exit_containers(Tokenizer2, 'after');
            false ->
                {Tokenizer2, ok}
        end,

    case Result of
        ok ->
            case Tokenizer3 of
                #markdown_tokenizer{current = none, tokenize_state = TokenizeState3} ->
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{document_continued = 0},
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState4},
                    case exit_containers(Tokenizer4, 'eof') of
                        {Tokenizer5, ok} ->
                            Tokenizer6 = resolve(Tokenizer5),
                            {Tokenizer6, markdown_state:ok()};
                        {Tokenizer5, {error, ExitContainersEofMessage}} ->
                            {Tokenizer5, {error, ExitContainersEofMessage}}
                    end;
                #markdown_tokenizer{tokenize_state = TokenizeState3} ->
                    %% Containers would only be interrupting if we’ve continued.
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{
                        document_continued = 0, document_lazy_accepting_before = DocumentLazyContinuationCurrent3
                    },
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{interrupt = false, tokenize_state = TokenizeState4},
                    {Tokenizer4, markdown_state:retry(document_container_existing_before)}
            end;
        {error, ExitContainersAfterMessage} ->
            {Tokenizer3, {error, ExitContainersAfterMessage}}
    end.

%% @private
-doc """
Inject everything together.
""".
-spec resolve(Tokenizer) -> Tokenizer when Tokenizer :: markdown_tokenizer:t().
resolve(Tokenizer1 = #markdown_tokenizer{}) ->
    %% First, add the container exits into `child`.
    ChildIndex1 = 0,
    Line1 = 0,
    {
        Tokenizer2 = #markdown_tokenizer{
            events = Events1,
            map = EditMap1,
            tokenize_state =
                TokenizeState1 = #markdown_tokenize_state{
                    document_child = {some, Child1 = #markdown_tokenizer{events = ChildEvents1}}
                }
        },
        _ChildIndex2,
        Line2
    } = resolve__container_exits(Tokenizer1, ChildIndex1, Line1),
    FlowNames = markdown_util_skip:names([data]),
    FlowIndex1 = markdown_util_skip:to(Events1, 0, FlowNames),
    FlowIndex2 = resolve__flow_index(Events1, FlowIndex1, FlowNames),
    %% Now, add all child events into our parent document tokenizer.
    {EditMap2, ChildEvents2, _AccRm, _AccAdd} = markdown_subtokenizer:divide_events(
        EditMap1, Events1, FlowIndex2, ChildEvents1, {0, 0}
    ),
    %% Replace the flow data with actual events.
    {EditMap3, Events2} = markdown_edit_map:consume(EditMap2, Events1),
    Child2 = Child1#markdown_tokenizer{events = ChildEvents2},
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_child = {some, Child2}},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{
        events = Events2,
        map = EditMap3,
        tokenize_state = TokenizeState2
    },
    %% Now, add some final container exits due to the EOF.
    %% We can’t inject them into the child earlier, as they are “outside” its
    %% linked data.
    Tokenizer4 = resolve__final_container_exits(Tokenizer3, Line2),
    %% Add the resolvers from child.
    Tokenizer5 = resolve__add_resolvers_from_child(Tokenizer4),
    Tokenizer5.

%% @private
-spec resolve__add_resolvers_from_child(Tokenizer) -> Tokenizer when Tokenizer :: markdown_tokenizer:t().
resolve__add_resolvers_from_child(
    Tokenizer1 = #markdown_tokenizer{
        resolvers = Resolvers1,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                definitions = Definitions1,
                document_child =
                    {some,
                        Child1 = #markdown_tokenizer{
                            resolvers = ChildResolvers1,
                            tokenize_state =
                                ChildTokenizeState1 = #markdown_tokenize_state{definitions = ChildDefinitions1}
                        }}
            }
    }
) ->
    {ChildResolvers2, Resolvers2} = markdown_vec:split_off(ChildResolvers1, 0),
    Resolvers3 = markdown_vec:append(Resolvers1, Resolvers2),
    {ChildDefinitions2, Definitions2} = markdown_vec:split_off(ChildDefinitions1, 0),
    Definitions3 = markdown_vec:append(Definitions1, Definitions2),
    ChildTokenizeState2 = ChildTokenizeState1#markdown_tokenize_state{definitions = ChildDefinitions2},
    Child2 = Child1#markdown_tokenizer{resolvers = ChildResolvers2, tokenize_state = ChildTokenizeState2},
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        definitions = Definitions3, document_child = {some, Child2}
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{resolvers = Resolvers3, tokenize_state = TokenizeState2},
    Tokenizer2.

%% @private
-spec resolve__container_exits(Tokenizer, ChildIndex, Line) -> {Tokenizer, ChildIndex, Line} when
    Tokenizer :: markdown_tokenizer:t(), ChildIndex :: non_neg_integer(), Line :: non_neg_integer().
resolve__container_exits(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_child =
                    {some,
                        Child1 = #markdown_tokenizer{
                            events = ChildEvents1 = #markdown_vec{size = ChildEventsSize1}, map = ChildEditMap1
                        }},
                document_exits = DocumentExits1
            }
    },
    ChildIndex1,
    Line1
) when ChildIndex1 >= 0 andalso ChildIndex1 < ChildEventsSize1 ->
    ChildEvent1 = markdown_vec:get(ChildEvents1, ChildIndex1),
    case ChildEvent1 of
        #markdown_event{kind = 'exit', name = ChildEventName1} when
            ChildEventName1 =:= line_ending orelse ChildEventName1 =:= blank_line_ending
        ->
            %% Inject before `Enter:LineEnding`.
            InjectIndex1 = ChildIndex1 - 1,
            #markdown_event{point = ChildPoint1} = markdown_vec:get(ChildEvents1, InjectIndex1),
            {ChildIndex2, InjectIndex2, ChildPoint2} = resolve__inject_before_line_ending(
                ChildEvents1, ChildIndex1, InjectIndex1, ChildPoint1
            ),
            case Line1 < markdown_vec:size(DocumentExits1) of
                true ->
                    case markdown_vec:get(DocumentExits1, Line1) of
                        {some, Exits1} ->
                            DocumentExits2 = markdown_vec:set(DocumentExits1, Line1, none),
                            ExitMapFun = fun(_ExitIndex, Exit1) ->
                                Exit2 = Exit1#markdown_event{point = ChildPoint2},
                                Exit2
                            end,
                            Exits2 = markdown_vec:map(Exits1, ExitMapFun),
                            ChildEditMap2 = markdown_edit_map:add(ChildEditMap1, InjectIndex2, 0, Exits2),
                            Child2 = Child1#markdown_tokenizer{map = ChildEditMap2},
                            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                                document_child = {some, Child2}, document_exits = DocumentExits2
                            },
                            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                            ChildIndex3 = ChildIndex2 + 1,
                            Line2 = Line1 + 1,
                            resolve__container_exits(Tokenizer2, ChildIndex3, Line2);
                        none ->
                            ChildIndex3 = ChildIndex2 + 1,
                            Line2 = Line1 + 1,
                            resolve__container_exits(Tokenizer1, ChildIndex3, Line2)
                    end;
                false ->
                    ChildIndex3 = ChildIndex2 + 1,
                    Line2 = Line1 + 1,
                    resolve__container_exits(Tokenizer1, ChildIndex3, Line2)
            end;
        _ ->
            ChildIndex2 = ChildIndex1 + 1,
            resolve__container_exits(Tokenizer1, ChildIndex2, Line1)
    end;
resolve__container_exits(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_child = {some, Child1 = #markdown_tokenizer{events = ChildEvents1, map = ChildEditMap1}}
            }
    },
    ChildIndex,
    Line
) ->
    {ChildEditMap2, ChildEvents2} = markdown_edit_map:consume(ChildEditMap1, ChildEvents1),
    Child2 = Child1#markdown_tokenizer{events = ChildEvents2, map = ChildEditMap2},
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_child = {some, Child2}},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, ChildIndex, Line}.

%% @private
-spec resolve__final_container_exits(Tokenizer, Line) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(), Line :: non_neg_integer().
resolve__final_container_exits(
    Tokenizer1 = #markdown_tokenizer{
        events = Events1,
        point = Point,
        tokenize_state =
            TokenizeState1 = #markdown_tokenize_state{
                document_exits = DocumentExits1 = #markdown_vec{size = DocumentExitsSize}
            }
    },
    Line
) when Line < DocumentExitsSize ->
    case markdown_vec:get(DocumentExits1, Line) of
        {some, Exits1} ->
            DocumentExits2 = markdown_vec:set(DocumentExits1, Line, none),
            ExitMapFun = fun(_ExitIndex, Exit1) ->
                Exit2 = Exit1#markdown_event{point = Point},
                Exit2
            end,
            Exits2 = markdown_vec:map(Exits1, ExitMapFun),
            Events2 = markdown_vec:append(Events1, Exits2),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{document_exits = DocumentExits2},
            Tokenizer1#markdown_tokenizer{events = Events2, tokenize_state = TokenizeState2};
        none ->
            Tokenizer1
    end;
resolve__final_container_exits(Tokenizer = #markdown_tokenizer{}, _Line) ->
    Tokenizer.

%% @private
-spec resolve__flow_index(Events, FlowIndex, FlowNames) -> FlowIndex when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    FlowIndex :: non_neg_integer(),
    FlowNames :: markdown_util_skip:names().
resolve__flow_index(Events = #markdown_vec{size = EventsSize}, FlowIndex1, FlowNames) when FlowIndex1 < EventsSize ->
    case markdown_vec:get(Events, FlowIndex1) of
        #markdown_event{link = none} ->
            FlowIndex2 = markdown_util_skip:to(Events, FlowIndex1 + 1, FlowNames),
            resolve__flow_index(Events, FlowIndex2, FlowNames);
        #markdown_event{link = {some, #markdown_event_link{content = Content}}} when Content =/= flow ->
            FlowIndex2 = markdown_util_skip:to(Events, FlowIndex1 + 1, FlowNames),
            resolve__flow_index(Events, FlowIndex2, FlowNames);
        _ ->
            FlowIndex1
    end;
resolve__flow_index(_Events, FlowIndex, _FlowNames) ->
    FlowIndex.

%% @private
-spec resolve__inject_before_line_ending(Events, ChildIndex, InjectIndex, ChildPoint) ->
    {ChildIndex, InjectIndex, ChildPoint}
when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ChildIndex :: non_neg_integer(),
    InjectIndex :: non_neg_integer(),
    ChildPoint :: markdown_point:t().
resolve__inject_before_line_ending(
    Events = #markdown_vec{size = EventsSize}, ChildIndex1, InjectIndex1, ChildPoint1
) when ChildIndex1 + 1 < EventsSize ->
    case markdown_vec:get(Events, ChildIndex1 + 1) of
        #markdown_event{kind = 'exit', point = ChildPoint2} ->
            ChildIndex2 = ChildIndex1 + 1,
            %% Inject after `Exit:*`.
            InjectIndex2 = InjectIndex1 + 1,
            resolve__inject_before_line_ending(Events, ChildIndex2, InjectIndex2, ChildPoint2);
        _ ->
            {ChildIndex1, InjectIndex1, ChildPoint1}
    end;
resolve__inject_before_line_ending(_Events, ChildIndex, InjectIndex, ChildPoint) ->
    {ChildIndex, InjectIndex, ChildPoint}.
