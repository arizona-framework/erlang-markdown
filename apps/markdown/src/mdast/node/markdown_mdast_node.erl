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
-module(markdown_mdast_node).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% New API
-export([
    blockquote/1,
    break/1,
    code/1,
    definition/1,
    delete/1,
    emphasis/1,
    footnote_definition/1,
    footnote_reference/1,
    heading/1,
    html/1,
    image/1,
    image_reference/1,
    inline_code/1,
    inline_math/1,
    link/1,
    link_reference/1,
    list/1,
    list_item/1,
    math/1,
    mdx_flow_expression/1,
    mdx_jsx_flow_element/1,
    mdx_jsx_text_element/1,
    mdx_text_expression/1,
    mdxjs_esm/1,
    paragraph/1,
    root/1,
    strong/1,
    table/1,
    table_cell/1,
    table_row/1,
    text/1,
    thematic_break/1,
    toml/1,
    yaml/1
]).
%% Instance API
-export([
    children/1,
    children_mut/3,
    position/1,
    position_set/2,
    to_string/1
]).

%% Types
-type children() :: markdown_vec:t(t()).
-type children_mut_func() :: children_mut_func(dynamic(), dynamic()).
-type children_mut_func(AccIn, AccOut) :: fun((Children :: children(), AccIn) -> {NewChildren :: children(), AccOut}).
-type inner() ::
    %% Document:
    %%% Root.
    markdown_mdast_root:t()
    %% Container:
    %%% Block quote.
    | markdown_mdast_blockquote:t()
    %%% Footnote definition.
    | markdown_mdast_footnote_definition:t()
    %%% MDX: JSX element (container).
    | markdown_mdast_mdx_jsx_flow_element:t()
    %%% List.
    | markdown_mdast_list:t()
    %% Frontmatter:
    %%% MDX.js ESM.
    | markdown_mdast_mdxjs_esm:t()
    %%% Toml.
    | markdown_mdast_toml:t()
    %%% Yaml.
    | markdown_mdast_yaml:t()
    %% Phrasing:
    %%% Break.
    | markdown_mdast_break:t()
    %%% Code (phrasing).
    | markdown_mdast_inline_code:t()
    %%% Math (phrasing).
    | markdown_mdast_inline_math:t()
    %%% Delete.
    | markdown_mdast_delete:t()
    %%% Emphasis.
    | markdown_mdast_emphasis:t()
    %% MDX: expression (text).
    | markdown_mdast_mdx_text_expression:t()
    %%% Footnote reference.
    | markdown_mdast_footnote_reference:t()
    %%% Html (phrasing).
    | markdown_mdast_html:t()
    %%% Image.
    | markdown_mdast_image:t()
    %%% Image reference.
    | markdown_mdast_image_reference:t()
    %% MDX: JSX element (text).
    | markdown_mdast_mdx_jsx_text_element:t()
    %%% Link.
    | markdown_mdast_link:t()
    %%% Link reference.
    | markdown_mdast_link_reference:t()
    %%% Strong
    | markdown_mdast_strong:t()
    %%% Text.
    | markdown_mdast_text:t()
    %% Flow:
    %%% Code (flow).
    | markdown_mdast_code:t()
    %%% Math (flow).
    | markdown_mdast_math:t()
    %% MDX: expression (flow).
    | markdown_mdast_mdx_flow_expression:t()
    %%% Heading.
    | markdown_mdast_heading:t()
    %%% Html (flow).
    %% Html(Html),
    %%% Table.
    | markdown_mdast_table:t()
    %%% Thematic break.
    | markdown_mdast_thematic_break:t()
    %% Table content.
    %%% Table row.
    | markdown_mdast_table_row:t()
    %% Row content.
    %%% Table cell.
    | markdown_mdast_table_cell:t()
    %% List content.
    %%% List item.
    | markdown_mdast_list_item:t()
    %% Content.
    %%% Definition.
    | markdown_mdast_definition:t()
    %%% Paragraph.
    | markdown_mdast_paragraph:t().
-type t() :: #markdown_mdast_node{}.

-export_type([
    children/0,
    children_mut_func/0,
    children_mut_func/2,
    inner/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec blockquote(Inner) -> Node when Inner :: markdown_mdast_blockquote:t(), Node :: t().
blockquote(Inner = #markdown_mdast_blockquote{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec break(Inner) -> Node when Inner :: markdown_mdast_break:t(), Node :: t().
break(Inner = #markdown_mdast_break{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec code(Inner) -> Node when Inner :: markdown_mdast_code:t(), Node :: t().
code(Inner = #markdown_mdast_code{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec definition(Inner) -> Node when Inner :: markdown_mdast_definition:t(), Node :: t().
definition(Inner = #markdown_mdast_definition{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec delete(Inner) -> Node when Inner :: markdown_mdast_delete:t(), Node :: t().
delete(Inner = #markdown_mdast_delete{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec emphasis(Inner) -> Node when Inner :: markdown_mdast_emphasis:t(), Node :: t().
emphasis(Inner = #markdown_mdast_emphasis{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec footnote_definition(Inner) -> Node when Inner :: markdown_mdast_footnote_definition:t(), Node :: t().
footnote_definition(Inner = #markdown_mdast_footnote_definition{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec footnote_reference(Inner) -> Node when Inner :: markdown_mdast_footnote_reference:t(), Node :: t().
footnote_reference(Inner = #markdown_mdast_footnote_reference{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec heading(Inner) -> Node when Inner :: markdown_mdast_heading:t(), Node :: t().
heading(Inner = #markdown_mdast_heading{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec html(Inner) -> Node when Inner :: markdown_mdast_html:t(), Node :: t().
html(Inner = #markdown_mdast_html{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec image(Inner) -> Node when Inner :: markdown_mdast_image:t(), Node :: t().
image(Inner = #markdown_mdast_image{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec image_reference(Inner) -> Node when Inner :: markdown_mdast_image_reference:t(), Node :: t().
image_reference(Inner = #markdown_mdast_image_reference{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec inline_code(Inner) -> Node when Inner :: markdown_mdast_inline_code:t(), Node :: t().
inline_code(Inner = #markdown_mdast_inline_code{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec inline_math(Inner) -> Node when Inner :: markdown_mdast_inline_math:t(), Node :: t().
inline_math(Inner = #markdown_mdast_inline_math{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec link(Inner) -> Node when Inner :: markdown_mdast_link:t(), Node :: t().
link(Inner = #markdown_mdast_link{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec link_reference(Inner) -> Node when Inner :: markdown_mdast_link_reference:t(), Node :: t().
link_reference(Inner = #markdown_mdast_link_reference{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec list(Inner) -> Node when Inner :: markdown_mdast_list:t(), Node :: t().
list(Inner = #markdown_mdast_list{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec list_item(Inner) -> Node when Inner :: markdown_mdast_list_item:t(), Node :: t().
list_item(Inner = #markdown_mdast_list_item{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec math(Inner) -> Node when Inner :: markdown_mdast_math:t(), Node :: t().
math(Inner = #markdown_mdast_math{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec mdx_flow_expression(Inner) -> Node when Inner :: markdown_mdast_mdx_flow_expression:t(), Node :: t().
mdx_flow_expression(Inner = #markdown_mdast_mdx_flow_expression{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec mdx_jsx_flow_element(Inner) -> Node when Inner :: markdown_mdast_mdx_jsx_flow_element:t(), Node :: t().
mdx_jsx_flow_element(Inner = #markdown_mdast_mdx_jsx_flow_element{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec mdx_jsx_text_element(Inner) -> Node when Inner :: markdown_mdast_mdx_jsx_text_element:t(), Node :: t().
mdx_jsx_text_element(Inner = #markdown_mdast_mdx_jsx_text_element{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec mdx_text_expression(Inner) -> Node when Inner :: markdown_mdast_mdx_text_expression:t(), Node :: t().
mdx_text_expression(Inner = #markdown_mdast_mdx_text_expression{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec mdxjs_esm(Inner) -> Node when Inner :: markdown_mdast_mdxjs_esm:t(), Node :: t().
mdxjs_esm(Inner = #markdown_mdast_mdxjs_esm{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec paragraph(Inner) -> Node when Inner :: markdown_mdast_paragraph:t(), Node :: t().
paragraph(Inner = #markdown_mdast_paragraph{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec root(Inner) -> Node when Inner :: markdown_mdast_root:t(), Node :: t().
root(Inner = #markdown_mdast_root{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec strong(Inner) -> Node when Inner :: markdown_mdast_strong:t(), Node :: t().
strong(Inner = #markdown_mdast_strong{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec table(Inner) -> Node when Inner :: markdown_mdast_table:t(), Node :: t().
table(Inner = #markdown_mdast_table{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec table_cell(Inner) -> Node when Inner :: markdown_mdast_table_cell:t(), Node :: t().
table_cell(Inner = #markdown_mdast_table_cell{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec table_row(Inner) -> Node when Inner :: markdown_mdast_table_row:t(), Node :: t().
table_row(Inner = #markdown_mdast_table_row{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec text(Inner) -> Node when Inner :: markdown_mdast_text:t(), Node :: t().
text(Inner = #markdown_mdast_text{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec thematic_break(Inner) -> Node when Inner :: markdown_mdast_thematic_break:t(), Node :: t().
thematic_break(Inner = #markdown_mdast_thematic_break{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec toml(Inner) -> Node when Inner :: markdown_mdast_toml:t(), Node :: t().
toml(Inner = #markdown_mdast_toml{}) ->
    #markdown_mdast_node{inner = Inner}.

-spec yaml(Inner) -> Node when Inner :: markdown_mdast_yaml:t(), Node :: t().
yaml(Inner = #markdown_mdast_yaml{}) ->
    #markdown_mdast_node{inner = Inner}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-doc """
Get children of a node.
""".
-spec children(Node) -> OptionChildren when
    Node :: t(), OptionChildren :: markdown_option:t(Children), Children :: children().
children(Node = #markdown_mdast_node{}) ->
    case Node#markdown_mdast_node.inner of
        %% Parent.
        #markdown_mdast_root{children = Children} ->
            {some, Children};
        #markdown_mdast_paragraph{children = Children} ->
            {some, Children};
        #markdown_mdast_heading{children = Children} ->
            {some, Children};
        #markdown_mdast_blockquote{children = Children} ->
            {some, Children};
        #markdown_mdast_list{children = Children} ->
            {some, Children};
        #markdown_mdast_list_item{children = Children} ->
            {some, Children};
        #markdown_mdast_emphasis{children = Children} ->
            {some, Children};
        #markdown_mdast_strong{children = Children} ->
            {some, Children};
        #markdown_mdast_link{children = Children} ->
            {some, Children};
        #markdown_mdast_link_reference{children = Children} ->
            {some, Children};
        #markdown_mdast_footnote_definition{children = Children} ->
            {some, Children};
        #markdown_mdast_table{children = Children} ->
            {some, Children};
        #markdown_mdast_table_row{children = Children} ->
            {some, Children};
        #markdown_mdast_table_cell{children = Children} ->
            {some, Children};
        #markdown_mdast_delete{children = Children} ->
            {some, Children};
        #markdown_mdast_mdx_jsx_flow_element{children = Children} ->
            {some, Children};
        #markdown_mdast_mdx_jsx_text_element{children = Children} ->
            {some, Children};
        %% Non-parent.
        _ ->
            none
    end.

-doc """
Mutate children of a node.
""".
-spec children_mut(Node, AccIn, ChildrenMutFunc) -> {Node, AccOut} when
    Node :: t(),
    AccIn :: dynamic(),
    ChildrenMutFunc :: children_mut_func(AccIn, AccOut),
    AccOut :: dynamic().
children_mut(Node1 = #markdown_mdast_node{inner = Inner1}, AccIn, ChildrenMutFunc) when
    is_function(ChildrenMutFunc, 2)
->
    case Inner1 of
        %% Parent.
        #markdown_mdast_root{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_root{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_paragraph{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_paragraph{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_heading{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_heading{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_blockquote{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_blockquote{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_list{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_list{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_list_item{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_list_item{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_emphasis{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_emphasis{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_strong{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_strong{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_link{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_link{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_link_reference{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_link_reference{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_footnote_definition{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_footnote_definition{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_table{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_table{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_table_row{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_table_row{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_table_cell{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_table_cell{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_delete{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_delete{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_mdx_jsx_flow_element{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_mdx_jsx_flow_element{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        #markdown_mdast_mdx_jsx_text_element{children = Children1} ->
            {Children2, AccOut} = ChildrenMutFunc(Children1, AccIn),
            Inner2 = Inner1#markdown_mdast_mdx_jsx_text_element{children = Children2},
            Node2 = Node1#markdown_mdast_node{inner = Inner2},
            {Node2, AccOut};
        %% Non-parent.
        _ ->
            {Node1, AccIn}
    end.

-doc """
Get position of a node.
""".
-spec position(Node) -> OptionPosition when
    Node :: t(), OptionPosition :: markdown_option:t(Position), Position :: markdown_position:t().
position(Node = #markdown_mdast_node{}) ->
    case Node#markdown_mdast_node.inner of
        #markdown_mdast_root{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_blockquote{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_footnote_definition{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_mdx_jsx_flow_element{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_list{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_mdxjs_esm{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_toml{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_yaml{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_break{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_inline_code{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_inline_math{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_delete{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_emphasis{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_mdx_text_expression{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_footnote_reference{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_html{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_image{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_image_reference{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_mdx_jsx_text_element{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_link{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_link_reference{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_strong{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_text{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_code{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_math{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_mdx_flow_expression{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_heading{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_table{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_thematic_break{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_table_row{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_table_cell{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_list_item{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_definition{position = OptionPosition} ->
            OptionPosition;
        #markdown_mdast_paragraph{position = OptionPosition} ->
            OptionPosition
    end.

-doc """
Set position of a node.
""".
-spec position_set(Node, OptionPosition) -> Node when
    Node :: t(), OptionPosition :: markdown_option:t(Position), Position :: markdown_position:t().
position_set(Node = #markdown_mdast_node{}, OptionPosition) when
    ?is_option_record(OptionPosition, markdown_unist_position)
->
    NewInner =
        case Node#markdown_mdast_node.inner of
            Inner = #markdown_mdast_root{} ->
                Inner#markdown_mdast_root{position = OptionPosition};
            Inner = #markdown_mdast_blockquote{} ->
                Inner#markdown_mdast_blockquote{position = OptionPosition};
            Inner = #markdown_mdast_footnote_definition{} ->
                Inner#markdown_mdast_footnote_definition{position = OptionPosition};
            Inner = #markdown_mdast_mdx_jsx_flow_element{} ->
                Inner#markdown_mdast_mdx_jsx_flow_element{position = OptionPosition};
            Inner = #markdown_mdast_list{} ->
                Inner#markdown_mdast_list{position = OptionPosition};
            Inner = #markdown_mdast_mdxjs_esm{} ->
                Inner#markdown_mdast_mdxjs_esm{position = OptionPosition};
            Inner = #markdown_mdast_toml{} ->
                Inner#markdown_mdast_toml{position = OptionPosition};
            Inner = #markdown_mdast_yaml{} ->
                Inner#markdown_mdast_yaml{position = OptionPosition};
            Inner = #markdown_mdast_break{} ->
                Inner#markdown_mdast_break{position = OptionPosition};
            Inner = #markdown_mdast_inline_code{} ->
                Inner#markdown_mdast_inline_code{position = OptionPosition};
            Inner = #markdown_mdast_inline_math{} ->
                Inner#markdown_mdast_inline_math{position = OptionPosition};
            Inner = #markdown_mdast_delete{} ->
                Inner#markdown_mdast_delete{position = OptionPosition};
            Inner = #markdown_mdast_emphasis{} ->
                Inner#markdown_mdast_emphasis{position = OptionPosition};
            Inner = #markdown_mdast_mdx_text_expression{} ->
                Inner#markdown_mdast_mdx_text_expression{position = OptionPosition};
            Inner = #markdown_mdast_footnote_reference{} ->
                Inner#markdown_mdast_footnote_reference{position = OptionPosition};
            Inner = #markdown_mdast_html{} ->
                Inner#markdown_mdast_html{position = OptionPosition};
            Inner = #markdown_mdast_image{} ->
                Inner#markdown_mdast_image{position = OptionPosition};
            Inner = #markdown_mdast_image_reference{} ->
                Inner#markdown_mdast_image_reference{position = OptionPosition};
            Inner = #markdown_mdast_mdx_jsx_text_element{} ->
                Inner#markdown_mdast_mdx_jsx_text_element{position = OptionPosition};
            Inner = #markdown_mdast_link{} ->
                Inner#markdown_mdast_link{position = OptionPosition};
            Inner = #markdown_mdast_link_reference{} ->
                Inner#markdown_mdast_link_reference{position = OptionPosition};
            Inner = #markdown_mdast_strong{} ->
                Inner#markdown_mdast_strong{position = OptionPosition};
            Inner = #markdown_mdast_text{} ->
                Inner#markdown_mdast_text{position = OptionPosition};
            Inner = #markdown_mdast_code{} ->
                Inner#markdown_mdast_code{position = OptionPosition};
            Inner = #markdown_mdast_math{} ->
                Inner#markdown_mdast_math{position = OptionPosition};
            Inner = #markdown_mdast_mdx_flow_expression{} ->
                Inner#markdown_mdast_mdx_flow_expression{position = OptionPosition};
            Inner = #markdown_mdast_heading{} ->
                Inner#markdown_mdast_heading{position = OptionPosition};
            Inner = #markdown_mdast_table{} ->
                Inner#markdown_mdast_table{position = OptionPosition};
            Inner = #markdown_mdast_thematic_break{} ->
                Inner#markdown_mdast_thematic_break{position = OptionPosition};
            Inner = #markdown_mdast_table_row{} ->
                Inner#markdown_mdast_table_row{position = OptionPosition};
            Inner = #markdown_mdast_table_cell{} ->
                Inner#markdown_mdast_table_cell{position = OptionPosition};
            Inner = #markdown_mdast_list_item{} ->
                Inner#markdown_mdast_list_item{position = OptionPosition};
            Inner = #markdown_mdast_definition{} ->
                Inner#markdown_mdast_definition{position = OptionPosition};
            Inner = #markdown_mdast_paragraph{} ->
                Inner#markdown_mdast_paragraph{position = OptionPosition}
        end,
    NewNode = Node#markdown_mdast_node{inner = NewInner},
    NewNode.

-spec to_string(Node) -> String when Node :: t(), String :: binary().
to_string(Node = #markdown_mdast_node{}) ->
    case Node#markdown_mdast_node.inner of
        %% Parents.
        #markdown_mdast_root{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_blockquote{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_footnote_definition{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_mdx_jsx_flow_element{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_list{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_delete{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_emphasis{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_mdx_jsx_text_element{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_link{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_link_reference{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_strong{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_heading{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_table{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_table_row{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_table_cell{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_list_item{children = Children} ->
            children_to_string(Children);
        #markdown_mdast_paragraph{children = Children} ->
            children_to_string(Children);
        %% Literals.
        #markdown_mdast_mdxjs_esm{value = Value} ->
            Value;
        #markdown_mdast_toml{value = Value} ->
            Value;
        #markdown_mdast_yaml{value = Value} ->
            Value;
        #markdown_mdast_inline_code{value = Value} ->
            Value;
        #markdown_mdast_inline_math{value = Value} ->
            Value;
        #markdown_mdast_mdx_text_expression{value = Value} ->
            Value;
        #markdown_mdast_html{value = Value} ->
            Value;
        #markdown_mdast_text{value = Value} ->
            Value;
        #markdown_mdast_code{value = Value} ->
            Value;
        #markdown_mdast_math{value = Value} ->
            Value;
        #markdown_mdast_mdx_flow_expression{value = Value} ->
            Value;
        %% Voids.
        #markdown_mdast_break{} ->
            <<>>;
        #markdown_mdast_footnote_reference{} ->
            <<>>;
        #markdown_mdast_image{} ->
            <<>>;
        #markdown_mdast_image_reference{} ->
            <<>>;
        #markdown_mdast_thematic_break{} ->
            <<>>;
        #markdown_mdast_definition{} ->
            <<>>
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec children_to_string(Children) -> String when Children :: children(), String :: binary().
children_to_string(Children) ->
    String = markdown_vec:reduce(Children, <<>>, fun children_to_string/3),
    String.

%% @private
-spec children_to_string(Index, Node, StringIn) -> StringOut when
    Index :: markdown_vec:index(), Node :: t(), StringIn :: binary(), StringOut :: binary().
children_to_string(_Index, Node, StringIn) ->
    StringOut = <<StringIn/bytes, (to_string(Node))/bytes>>,
    StringOut.
