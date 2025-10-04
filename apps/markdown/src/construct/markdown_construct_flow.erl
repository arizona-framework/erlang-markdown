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
-module(markdown_construct_flow).
-moduledoc """
The flow content type.

**Flow** represents the sections, such as headings and code, which are
parsed per line.
An example is HTML, which has a certain starting condition (such as
`<script>` on its own line), then continues for a while, until an end
condition is found (such as `</style>`).
If that line with an end condition is never found, that flow goes until
the end.

The constructs found in flow are:

*   [Blank line][crate::construct::blank_line]
*   [Code (indented)][crate::construct::code_indented]
*   [Heading (atx)][crate::construct::heading_atx]
*   [Heading (setext)][crate::construct::heading_setext]
*   [HTML (flow)][crate::construct::html_flow]
*   [MDX esm][crate::construct::mdx_esm]
*   [MDX expression (flow)][crate::construct::mdx_expression_flow]
*   [MDX JSX (flow)][crate::construct::mdx_jsx_flow]
*   [Raw (flow)][crate::construct::raw_flow] (code (fenced), math (flow))
*   [Thematic break][crate::construct::thematic_break]
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    blank_line_before/1,
    before_code_indented/1,
    before_raw/1,
    before_html/1,
    before_mdx_jsx/1,
    before_heading_atx/1,
    before_heading_setext/1,
    before_thematic_break/1,
    before_attribute_list/1,
    before_mdx_expression/1,
    before_gfm_table/1,
    before_content/1,
    blank_line_after/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of flow.

```markdown
> | ## alpha
    ^
> |     bravo
    ^
> | ***
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, $#}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(heading_atx_start),
    {Tokenizer2, State};
start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $$ orelse Current =:= $` orelse Current =:= $~
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(raw_flow_start),
    {Tokenizer2, State};
%% Note: `-` is also used in setext heading underline so it's not
%% included here.
start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $* orelse Current =:= $_ ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(thematic_break_start),
    {Tokenizer2, State};
start(Tokenizer1 = #markdown_tokenizer{current = {some, $<}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_mdx_jsx)
    ),
    State = markdown_state:retry(html_flow_start),
    {Tokenizer2, State};
start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $e orelse Current =:= $i ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(mdx_esm_start),
    {Tokenizer2, State};
start(Tokenizer1 = #markdown_tokenizer{current = {some, ${}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(mdx_expression_flow_start),
    {Tokenizer2, State};
%% Actual parsing: blank line? Indented code? Indented anything?
%% Tables, setext heading underlines, definitions, and Contents are
%% particularly weird.
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(flow_blank_line_before),
    {Tokenizer, State}.

-doc """
At blank line.

```markdown
> | ␠␠␊
    ^
```
""".
-spec blank_line_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
blank_line_before(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_blank_line_after),
        markdown_state:next(flow_before_code_indented)
    ),
    State = markdown_state:retry(blank_line_start),
    {Tokenizer2, State}.

-doc """
At code (indented).

```markdown
> | ␠␠␠␠a
    ^
```
""".
-spec before_code_indented(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_code_indented(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_raw)
    ),
    State = markdown_state:retry(code_indented_start),
    {Tokenizer2, State}.

-doc """
At raw.

````markdown
> | ```
    ^
````
""".
-spec before_raw(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_raw(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_html)
    ),
    State = markdown_state:retry(raw_flow_start),
    {Tokenizer2, State}.

-doc """
At html (flow).

```markdown
> | <a>
    ^
```
""".
-spec before_html(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_html(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_mdx_jsx)
    ),
    State = markdown_state:retry(html_flow_start),
    {Tokenizer2, State}.

-doc """
At mdx jsx (flow).

```markdown
> | <A />
    ^
```
""".
-spec before_mdx_jsx(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_mdx_jsx(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_heading_atx)
    ),
    State = markdown_state:retry(mdx_jsx_flow_start),
    {Tokenizer2, State}.

-doc """
At heading (atx).

```markdown
> | # a
    ^
```
""".
-spec before_heading_atx(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_heading_atx(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_heading_setext)
    ),
    State = markdown_state:retry(heading_atx_start),
    {Tokenizer2, State}.

-doc """
At heading (setext).

```markdown
  | a
> | =
    ^
```
""".
-spec before_heading_setext(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_heading_setext(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_thematic_break)
    ),
    State = markdown_state:retry(heading_setext_start),
    {Tokenizer2, State}.

-doc """
At thematic break.

```markdown
> | ***
    ^
```
""".
-spec before_thematic_break(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_thematic_break(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_mdx_expression)
    ),
    State = markdown_state:retry(thematic_break_start),
    {Tokenizer2, State}.

-doc """
At attribute list (flow).

```markdown
> | {.red}
    ^
```
""".
-spec before_attribute_list(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_attribute_list(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_mdx_expression)
    ),
    State = markdown_state:retry(attribute_list_flow_start),
    {Tokenizer2, State}.

-doc """
At MDX expression (flow).

```markdown
> | {Math.PI}
    ^
```
""".
-spec before_mdx_expression(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_mdx_expression(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_gfm_table)
    ),
    State = markdown_state:retry(mdx_expression_flow_start),
    {Tokenizer2, State}.

-doc """
At GFM table.

```markdown
> | | a |
    ^
```
""".
-spec before_gfm_table(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_gfm_table(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:next(flow_before_content)
    ),
    State = markdown_state:retry(gfm_table_start),
    {Tokenizer2, State}.

-doc """
At content.

```markdown
> | a
    ^
```
""".
-spec before_content(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_content(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(flow_after),
        markdown_state:nok()
    ),
    State = markdown_state:retry(content_chunk_start),
    {Tokenizer2, State}.

-doc """
After blank line.

```markdown
> | ␠␠␊
      ^
```
""".
-spec blank_line_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
blank_line_after(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    State = markdown_state:ok(),
    {Tokenizer1, State};
blank_line_after(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, blank_line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, blank_line_ending),
    %% Feel free to interrupt.
    Tokenizer5 = Tokenizer4#markdown_tokenizer{interrupt = false},
    State = markdown_state:next(flow_start),
    {Tokenizer5, State};
blank_line_after(_Tokenizer) ->
    error(unreachable, ["expected eol/eof"]).

-doc """
After flow.

```markdown
> | # a␊
       ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:ok(),
    {Tokenizer, State};
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    % io:format("\n\n[~w] BEFORE flow:after\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer1)]),
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(flow_start),
    % io:format("\n\n[~w] AFTER flow:after\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer4)]),
    {Tokenizer4, State};
'after'(_Tokenizer) ->
    erlang:error(expected_eol_eof).
