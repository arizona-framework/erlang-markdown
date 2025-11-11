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
-module(markdown_mdast_compile_context).
-moduledoc """
Context used to compile markdown.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-06-26", modified => "2025-06-26"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    buffer/1,
    new/2,
    on_mismatch_error/3,
    resume/1,
    serialize_abbreviated_tag/1,
    tail/1,
    tail_mut/3,
    tail_penultimate_mut/3,
    tail_pop/1,
    tail_push/2,
    tail_push_again/1
]).

%% Types
-type mdast_node() :: markdown_mdast_node:t().
-type node_mut_func() :: node_mut_func(dynamic(), dynamic()).
-type node_mut_func(AccIn, AccOut) :: fun((Node :: mdast_node(), AccIn) -> {NewNode :: mdast_node(), AccOut}).
-type t() :: #markdown_mdast_compile_context{}.

-export_type([
    mdast_node/0,
    node_mut_func/0,
    node_mut_func/2,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Push a buffer.
""".
-spec buffer(CompileContext) -> CompileContext when CompileContext :: t().
buffer(CompileContext1 = #markdown_mdast_compile_context{trees = Trees1}) ->
    Trees2 = markdown_vec:push(Trees1, {
        markdown_mdast_node:paragraph(#markdown_mdast_paragraph{
            children = markdown_vec:new(),
            position = none
        }),
        markdown_vec:new(),
        markdown_vec:new()
    }),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    CompileContext2.

-doc """
Create a new compile context.
""".
-spec new(Events, Bytes) -> CompileContext when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    CompileContext :: t().
new(Events = #markdown_vec{}, Bytes) when is_binary(Bytes) ->
    EventsIsEmpty = markdown_vec:is_empty(Events),
    StartPoint =
        case EventsIsEmpty of
            true ->
                markdown_unist_point:new(1, 1, 0);
            false ->
                markdown_point:to_unist((markdown_vec:first(Events))#markdown_event.point)
        end,
    EndPoint =
        case EventsIsEmpty of
            true ->
                markdown_unist_point:new(1, 1, 0);
            false ->
                markdown_point:to_unist((markdown_vec:last(Events))#markdown_event.point)
        end,
    RootPosition = markdown_unist_position:new(StartPoint, EndPoint),
    Tree = markdown_mdast_node:root(#markdown_mdast_root{
        children = markdown_vec:new(),
        position = {some, RootPosition}
    }),
    #markdown_mdast_compile_context{
        events = Events,
        bytes = Bytes,
        character_reference_marker = 0,
        gfm_table_inside = false,
        hard_break_after = false,
        heading_setext_text_after = false,
        jsx_tag_stack = markdown_vec:new(),
        jsx_tag = none,
        media_reference_stack = markdown_vec:new(),
        raw_flow_fence_seen = false,
        trees = markdown_vec:from_list([
            {Tree, markdown_vec:new(), markdown_vec:new()}
        ]),
        index = 0
    }.

-doc """
Handle a mismatch.

Mismatches can occur with MDX JSX tags.
""".
-spec on_mismatch_error(CompileContext, OptionLeft, Right) -> {error, Message} when
    CompileContext :: t(),
    OptionLeft :: markdown_option:t(Left),
    Left :: markdown_event:t(),
    Right :: markdown_event:t(),
    Message :: markdown_message:t().
on_mismatch_error(
    #markdown_mdast_compile_context{events = Events, jsx_tag = JsxTag, jsx_tag_stack = JsxTagStack}, OptionLeft, Right
) ->
    case {OptionLeft, Right#markdown_event.name} of
        {_, RightName} when RightName =:= mdx_jsx_flow_tag orelse RightName =:= mdx_jsx_text_tag ->
            Tag = markdown_vec:last(JsxTagStack),
            Point =
                case OptionLeft of
                    none ->
                        (markdown_vec:last(Events))#markdown_event.point;
                    {some, Left} ->
                        Left#markdown_event.point
                end,
            OptionPlace = {some, markdown_place:point(markdown_point:to_unist(Point))},
            Reason = ?'format!'("Expected a closing tag for `~ts` (~w:~w)~ts", [
                serialize_abbreviated_tag(Tag),
                Tag#markdown_mdast_jsx_tag.start#markdown_unist_point.line,
                Tag#markdown_mdast_jsx_tag.start#markdown_unist_point.column,
                case OptionLeft of
                    none ->
                        <<>>;
                    {some, #markdown_event{name = LeftName}} ->
                        ?'format!'(" before the end of `~ts`", [LeftName])
                end
            ]),
            RuleId = <<"end-tag-mismatch">>,
            Source = <<"erlang-markdown">>,
            Message = markdown_message:new(OptionPlace, Reason, RuleId, Source),
            {error, Message};
        {{some, #markdown_event{name = LeftName}}, RightName} when
            LeftName =:= mdx_jsx_flow_tag orelse LeftName =:= mdx_jsx_text_tag
        ->
            Tag = JsxTag,
            OptionPlace = {some, markdown_place:point(Tag#markdown_mdast_jsx_tag.start)},
            Reason = ?'format!'(
                "Expected the closing tag `~ts` either before the start of `~ts` (~w:~w), or another opening tag after that start",
                [
                    serialize_abbreviated_tag(Tag),
                    RightName,
                    Right#markdown_event.point#markdown_point.line,
                    Right#markdown_event.point#markdown_point.column
                ]
            ),
            RuleId = <<"end-tag-mismatch">>,
            Source = <<"erlang-markdown">>,
            Message = markdown_message:new(OptionPlace, Reason, RuleId, Source),
            {error, Message};
        {{some, #markdown_event{name = LeftName}}, RightName} ->
            ?'unreachable!'("mismatched (non-jsx): ~0tp / ~0tp", [LeftName, RightName]);
        {none, RightName} ->
            ?'unreachable!'("mismatched (non-jsx): document / ~0tp", [RightName])
    end.

-doc """
Pop a buffer, returning its value.
""".
-spec resume(CompileContext) -> {CompileContext, Node} when CompileContext :: t(), Node :: markdown_mdast_node:t().
resume(CompileContext1 = #markdown_mdast_compile_context{trees = Trees1}) ->
    Trees1PopResult = markdown_vec:pop_option(Trees1),
    %% BEGIN: assertions
    ?assertMatch({_, {some, _}}, Trees1PopResult, "unreachable: Cannot resume w/o buffer"),
    %% END: assertions
    {Trees2, {some, {Node, StackA, StackB}}} = Trees1PopResult,
    %% BEGIN: assertions
    ?assertEqual(0, markdown_vec:size(StackA), "expected stack (nodes in tree) to be drained"),
    ?assertEqual(0, markdown_vec:size(StackB), "expected stack (opening events) to be drained"),
    %% END: assertions
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    {CompileContext2, Node}.

-doc """
Format a JSX tag, ignoring its attributes.
""".
-spec serialize_abbreviated_tag(Tag) -> Serialized when
    Tag :: markdown_mdast_jsx_tag:t(), Serialized :: unicode:unicode_binary().
serialize_abbreviated_tag(Tag = #markdown_mdast_jsx_tag{}) ->
    ?'format!'(
        "<~ts~ts>",
        [
            case Tag#markdown_mdast_jsx_tag.close of
                true ->
                    <<"/">>;
                false ->
                    <<>>
            end,
            case Tag#markdown_mdast_jsx_tag.name of
                {some, Name} ->
                    Name;
                none ->
                    <<>>
            end
        ]
    ).

-spec tail(CompileContext) -> Node when CompileContext :: t(), Node :: markdown_mdast_node:t().
tail(#markdown_mdast_compile_context{trees = Trees}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree, Stack, _EventStack} = markdown_vec:last(Trees),
    NodeMutFunc = fun(Node, none) ->
        {Node, {some, Node}}
    end,
    {Tree, {some, Node}} = delve_mut(0, Stack, Tree, none, NodeMutFunc),
    Node.

-spec tail_mut(CompileContext, AccIn, NodeMutFunc) -> {CompileContext, AccOut} when
    CompileContext :: t(), AccIn :: dynamic(), NodeMutFunc :: node_mut_func(AccIn, AccOut), AccOut :: dynamic().
tail_mut(CompileContext1 = #markdown_mdast_compile_context{trees = Trees1}, AccIn, NodeMutFunc) when
    is_function(NodeMutFunc, 2)
->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees1), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree1, StackA, StackB} = markdown_vec:last(Trees1),
    {Tree2, AccOut} = delve_mut(0, StackA, Tree1, AccIn, NodeMutFunc),
    Trees2 = markdown_vec:set_last(Trees1, {Tree2, StackA, StackB}),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    {CompileContext2, AccOut}.

-spec tail_penultimate_mut(CompileContext, AccIn, NodeMutFunc) -> {CompileContext, AccOut} when
    CompileContext :: t(), AccIn :: dynamic(), NodeMutFunc :: node_mut_func(AccIn, AccOut), AccOut :: dynamic().
tail_penultimate_mut(CompileContext1 = #markdown_mdast_compile_context{trees = Trees1}, AccIn, NodeMutFunc) when
    is_function(NodeMutFunc, 2)
->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees1), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree1, StackA, StackB} = markdown_vec:last(Trees1),
    {StackAWithoutLast, _} = markdown_vec:pop(StackA),
    {Tree2, AccOut} = delve_mut(0, StackAWithoutLast, Tree1, AccIn, NodeMutFunc),
    Trees2 = markdown_vec:set_last(Trees1, {Tree2, StackA, StackB}),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    {CompileContext2, AccOut}.

-spec tail_pop(CompileContext) -> {ok, CompileContext} | {error, Message} when
    CompileContext :: t(), Message :: markdown_message:t().
tail_pop(CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index, trees = Trees1}) ->
    Event = markdown_vec:get(Events, Index),
    End = markdown_point:to_unist(Event#markdown_event.point),
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees1), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree1, Stack1, EventStack1} = markdown_vec:last(Trees1),
    NodeMutFunc = fun(Node1, ok) ->
        %% BEGIN: assertions
        begin
            ?assertMatch({some, _}, markdown_mdast_node:position(Node1), "Cannot pop manually added node")
        end,
        %% END: assertions
        {some, Position1} = markdown_mdast_node:position(Node1),
        Position2 = Position1#markdown_unist_position{'end' = End},
        Node2 = markdown_mdast_node:position_set(Node1, {some, Position2}),
        {Node2, ok}
    end,
    {Tree2, ok} = delve_mut(0, Stack1, Tree1, ok, NodeMutFunc),
    {Stack2, _} = markdown_vec:pop(Stack1),
    {EventStack2, LeftIndex} = markdown_vec:pop(EventStack1),
    Trees2 = markdown_vec:set_last(Trees1, {Tree2, Stack2, EventStack2}),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    Left = markdown_vec:get(Events, LeftIndex),
    case Event#markdown_event.name =/= Left#markdown_event.name of
        false ->
            {ok, CompileContext2};
        true ->
            on_mismatch_error(CompileContext2, {some, Event}, Left)
    end.

-spec tail_push(CompileContext, Child) -> CompileContext when CompileContext :: t(), Child :: markdown_mdast_node:t().
tail_push(
    CompileContext1 = #markdown_mdast_compile_context{events = Events, index = Index, trees = Trees1},
    Child1 = #markdown_mdast_node{}
) ->
    Child2 =
        case markdown_mdast_node:position(Child1) of
            none ->
                CurrentEvent = markdown_vec:get(Events, Index),
                markdown_mdast_node:position_set(Child1, {some, position_from_event(CurrentEvent)});
            {some, _} ->
                Child1
        end,
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees1), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree1, Stack1, EventStack1} = markdown_vec:last(Trees1),
    NodeMutFunc = fun(Node1, Acc1) ->
        %% BEGIN: assertions
        begin
            ?assertMatch({some, _}, markdown_mdast_node:children(Node1), "Cannot push to non-parent")
        end,
        %% END: assertions
        ChildrenMutFunc = fun(Children1, AccStack1) ->
            ChildIndex = markdown_vec:size(Children1),
            Children2 = markdown_vec:push(Children1, Child2),
            AccStack2 = markdown_vec:push(AccStack1, ChildIndex),
            {Children2, AccStack2}
        end,
        {Node2, Acc2} = markdown_mdast_node:children_mut(Node1, Acc1, ChildrenMutFunc),
        {Node2, Acc2}
    end,
    {Tree2, Stack2} = delve_mut(0, Stack1, Tree1, Stack1, NodeMutFunc),
    EventStack2 = markdown_vec:push(EventStack1, Index),
    Trees2 = markdown_vec:set_last(Trees1, {Tree2, Stack2, EventStack2}),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    CompileContext2.

-spec tail_push_again(CompileContext) -> CompileContext when CompileContext :: t().
tail_push_again(CompileContext1 = #markdown_mdast_compile_context{index = Index, trees = Trees1}) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Trees1), "Cannot get tail w/o tree"),
    %% END: assertions
    {Tree1, Stack1, EventStack1} = markdown_vec:last(Trees1),
    NodeMutFunc = fun(Node, AccStack1) ->
        %% BEGIN: assertions
        begin
            ?assertMatch({some, _}, markdown_mdast_node:children(Node), "Cannot push to non-parent")
        end,
        %% END: assertions
        {some, Children} = markdown_mdast_node:children(Node),
        ChildIndex = markdown_vec:size(Children) - 1,
        AccStack2 = markdown_vec:push(AccStack1, ChildIndex),
        {Node, AccStack2}
    end,
    {Tree2, Stack2} = delve_mut(0, Stack1, Tree1, Stack1, NodeMutFunc),
    EventStack2 = markdown_vec:push(EventStack1, Index),
    Trees2 = markdown_vec:set_last(Trees1, {Tree2, Stack2, EventStack2}),
    CompileContext2 = CompileContext1#markdown_mdast_compile_context{trees = Trees2},
    CompileContext2.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec delve_mut(StackIndex, Stack, Node, AccIn, NodeMutFunc) -> {Node, AccOut} when
    StackIndex :: markdown_vec:index(),
    Stack :: markdown_vec:t(markdown_types:usize()),
    Node :: markdown_mdast_node:t(),
    AccIn :: dynamic(),
    NodeMutFunc :: node_mut_func(AccIn, AccOut),
    AccOut :: dynamic().
delve_mut(StackIndex, Stack, Node1, AccIn, NodeMutFunc) when
    StackIndex >= 0 andalso StackIndex < ?markdown_vec_size(Stack) andalso is_function(NodeMutFunc, 2)
->
    Index = markdown_vec:get(Stack, StackIndex),
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_mdast_node:children(Node1), "Cannot delve into non-parent"),
    %% END: assertions
    ChildrenMutFunc = fun(Children1, Acc1) ->
        Child1 = markdown_vec:get(Children1, Index),
        {Child2, Acc2} = delve_mut(StackIndex + 1, Stack, Child1, Acc1, NodeMutFunc),
        Children2 = markdown_vec:set(Children1, Index, Child2),
        {Children2, Acc2}
    end,
    {Node2, AccOut} = markdown_mdast_node:children_mut(Node1, AccIn, ChildrenMutFunc),
    {Node2, AccOut};
delve_mut(_StackIndex, _Stack, Node1, AccIn, NodeMutFunc) when is_function(NodeMutFunc, 2) ->
    {Node2, AccOut} = NodeMutFunc(Node1, AccIn),
    {Node2, AccOut}.

%% @private
-doc """
Create a position from an event.
""".
-spec position_from_event(Event) -> Position when Event :: markdown_event:t(), Position :: markdown_unist_position:t().
position_from_event(#markdown_event{point = Point}) ->
    End = markdown_point:to_unist(Point),
    Position = markdown_unist_position:new(End, End),
    Position.
