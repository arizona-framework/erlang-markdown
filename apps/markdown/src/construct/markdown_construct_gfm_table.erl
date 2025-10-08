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
-module(markdown_construct_gfm_table).
-moduledoc """
GFM: table occurs in the [flow][] content type.

## Grammar

Tables form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
gfm_table ::= gfm_table_head 0*(eol gfm_table_body_row)

; Restriction: both rows must have the same number of cells.
gfm_table_head ::= gfm_table_row eol gfm_table_delimiter_row

gfm_table_row ::= ['|'] gfm_table_cell 0*('|' gfm_table_cell) ['|'] *space_or_tab
gfm_table_cell ::= *space_or_tab gfm_table_text *space_or_tab
gfm_table_text ::= 0*(line - '\\' - '|' | '\\' ['\\' | '|'])

gfm_table_delimiter_row ::= ['|'] gfm_table_delimiter_cell 0*('|' gfm_table_delimiter_cell) ['|'] *space_or_tab
gfm_table_delimiter_cell ::= *space_or_tab gfm_table_delimiter_value *space_or_tab
gfm_table_delimiter_value ::= [':'] 1*'-' [':']
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

The above grammar shows that basically anything can be a cell or a row.
The main thing that makes something a row, is that it occurs directly before
or after a delimiter row, or after another row.

It is not required for a table to have a body: it can end right after the
delimiter row.

Each column can be marked with an alignment.
The alignment marker is a colon (`:`) used before and/or after delimiter row
filler.
To illustrate:

```markdown
| none | left | right | center |
| ---- | :--- | ----: | :----: |
```

The number of cells in the delimiter row, is the number of columns of the
table.
Only the head row is required to have the same number of cells.
Body rows are not required to have a certain number of cells.
For body rows that have less cells than the number of columns of the table,
empty cells are injected.
When a row has more cells than the number of columns of the table, the
superfluous cells are dropped.
To illustrate:

```markdown
| a | b |
| - | - |
| c |
| d | e | f |
```

Yields:

```html
<table>
<thead>
<tr>
<th>a</th>
<th>b</th>
</tr>
</thead>
<tbody>
<tr>
<td>c</td>
<td></td>
</tr>
<tr>
<td>d</td>
<td>e</td>
</tr>
</tbody>
</table>
```

Each cell’s text is interpreted as the [text][] content type.
That means that it can include constructs such as [attention][attention].

The grammar for cells prohibits the use of `|` in them.
To use pipes in cells, encode them as a character reference or character
escape: `&vert;` (or `&VerticalLine;`, `&verbar;`, `&#124;`, `&#x7c;`) or
`\|`.

Escapes will typically work, but they are not supported in
[code (text)][raw_text] (and the math (text) extension).
To work around this, GitHub came up with a rather weird “trick”.
When inside a table cell *and* inside code, escaped pipes *are* decoded.
To illustrate:

```markdown
| Name | Character |
| - | - |
| Left curly brace | `{` |
| Pipe | `\|` |
| Right curly brace | `}` |
```

Yields:

```html
<table>
<thead>
<tr>
<th>Name</th>
<th>Character</th>
</tr>
</thead>
<tbody>
<tr>
<td>Left curly brace</td>
<td><code>{</code></td>
</tr>
<tr>
<td>Pipe</td>
<td><code>|</code></td>
</tr>
<tr>
<td>Right curly brace</td>
<td><code>}</code></td>
</tr>
</tbody>
</table>
```

> 👉 **Note**: no other character can be escaped like this.
> Escaping pipes in code does not work when not inside a table, either.

## HTML

GFM tables relate to several HTML elements: `<table>`, `<tbody>`, `<td>`,
`<th>`, `<thead>`, and `<tr>`.
See
[*§ 4.9.1 The `table` element*][html_table],
[*§ 4.9.5 The `tbody` element*][html_tbody],
[*§ 4.9.9 The `td` element*][html_td],
[*§ 4.9.10 The `th` element*][html_th],
[*§ 4.9.6 The `thead` element*][html_thead], and
[*§ 4.9.8 The `tr` element*][html_tr]
in the HTML spec for more info.

If the alignment of a column is left, right, or center, a deprecated
`align` attribute is added to each `<th>` and `<td>` element belonging to
that column.
That attribute is interpreted by browsers as if a CSS `text-align` property
was included, with its value set to that same keyword.

## Recommendation

When authoring markdown with GFM tables, it’s recommended to *always* put
pipes around cells.
Without them, it can be hard to infer whether the table will work, how many
columns there are, and which column you are currently editing.

It is recommended to not use many columns, as it results in very long lines,
making it hard to infer which column you are currently editing.

For larger tables, particularly when cells vary in size, it is recommended
*not* to manually “pad” cell text.
While it can look better, it results in a lot of time spent realigning
everything when a new, longer cell is added or the longest cell removed, as
every row then must be changed.
Other than costing time, it also causes large diffs in Git.

To illustrate, when authoring large tables, it is discouraged to pad cells
like this:

```markdown
| Alpha bravo charlie |              delta |
| ------------------- | -----------------: |
| Echo                | Foxtrot golf hotel |
```

Instead, use single spaces (and single filler dashes):

```markdown
| Alpha bravo charlie | delta |
| - | -: |
| Echo | Foxtrot golf hotel |
```

## Bugs

GitHub’s own algorithm to parse tables contains a bug.
This bug is not present in this project.
The issue relating to tables is:

*   [GFM tables: escaped escapes are incorrectly treated as escapes](https://github.com/github/cmark-gfm/issues/277)

## Tokens

*   [`GfmTable`][Name::GfmTable]
*   [`GfmTableBody`][Name::GfmTableBody]
*   [`GfmTableCell`][Name::GfmTableCell]
*   [`GfmTableCellDivider`][Name::GfmTableCellDivider]
*   [`GfmTableCellText`][Name::GfmTableCellText]
*   [`GfmTableDelimiterCell`][Name::GfmTableDelimiterCell]
*   [`GfmTableDelimiterCellValue`][Name::GfmTableDelimiterCellValue]
*   [`GfmTableDelimiterFiller`][Name::GfmTableDelimiterFiller]
*   [`GfmTableDelimiterMarker`][Name::GfmTableDelimiterMarker]
*   [`GfmTableDelimiterRow`][Name::GfmTableDelimiterRow]
*   [`GfmTableHead`][Name::GfmTableHead]
*   [`GfmTableRow`][Name::GfmTableRow]
*   [`LineEnding`][Name::LineEnding]

## References

*   [`micromark-extension-gfm-table`](https://github.com/micromark/micromark-extension-gfm-table)
*   [*§ 4.10 Tables (extension)* in `GFM`](https://github.github.com/gfm/#tables-extension-)

[flow]: crate::construct::flow
[text]: crate::construct::text
[attention]: crate::construct::attention
[raw_text]: crate::construct::raw_text
[html_table]: https://html.spec.whatwg.org/multipage/tables.html#the-table-element
[html_tbody]: https://html.spec.whatwg.org/multipage/tables.html#the-tbody-element
[html_td]: https://html.spec.whatwg.org/multipage/tables.html#the-td-element
[html_th]: https://html.spec.whatwg.org/multipage/tables.html#the-th-element
[html_thead]: https://html.spec.whatwg.org/multipage/tables.html#the-thead-element
[html_tr]: https://html.spec.whatwg.org/multipage/tables.html#the-tr-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    start/1,
    head_row_before/1,
    head_row_start/1,
    head_row_break/1,
    head_row_data/1,
    head_row_escape/1,
    head_delimiter_start/1,
    head_delimiter_before/1,
    head_delimiter_cell_before/1,
    head_delimiter_value_before/1,
    head_delimiter_left_alignment_after/1,
    head_delimiter_filler/1,
    head_delimiter_right_alignment_after/1,
    head_delimiter_cell_after/1,
    head_delimiter_nok/1,
    body_row_start/1,
    body_row_break/1,
    body_row_data/1,
    body_row_escape/1,
    resolve/1
]).

%% Internal Types
-record(cell, {
    %% ```markdown
    %% > | | aa | bb | cc |
    %%          ^-- exit
    %%           ^^^^-- this cell
    %% ```
    exit = 0 :: non_neg_integer(),
    %% ```markdown
    %% > | | aa | bb | cc |
    %%           ^-- enter
    %%           ^^^^-- this cell
    %% ```
    enter = 0 :: non_neg_integer(),
    %% ```markdown
    %% > | | aa | bb | cc |
    %%            ^-- enter
    %%             ^-- exit
    %%           ^^^^-- this cell
    %% ```
    data_enter = 0 :: non_neg_integer(),
    data_exit = 0 :: non_neg_integer()
}).
-record(resolve, {
    index :: non_neg_integer(),
    in_first_cell_awaiting_pipe :: boolean(),
    in_row :: boolean(),
    in_delimiter_row :: boolean(),
    last_cell :: cell(),
    cell :: cell(),
    after_head_awaiting_first_body_row :: boolean(),
    last_table_end :: non_neg_integer(),
    last_table_has_body :: boolean()
}).

-type cell() :: #cell{}.
-type resolve() :: #resolve{}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of a GFM table.

If there is a valid table row or table head before, then we try to parse
another row.
Otherwise, we try to parse a head.

```markdown
> | | a |
    ^
  | | - |
> | | b |
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer = #markdown_tokenizer{
        pierce = false,
        events = Events = #markdown_vec{size = EventsSize},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{gfm_table = true}}
        }
    }
) when EventsSize > 0 ->
    SkipIndex = markdown_util_skip:opt_back(Events, EventsSize - 1, [line_ending, space_or_tab]),
    case markdown_vec:get(Events, SkipIndex) of
        #markdown_event{name = Name} when Name =:= gfm_table_head orelse Name =:= gfm_table_row ->
            State = markdown_state:retry(gfm_table_body_row_start),
            {Tokenizer, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_row_before),
            {Tokenizer, State}
    end;
start(
    Tokenizer = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{gfm_table = true}}
        }
    }
) ->
    State = markdown_state:retry(gfm_table_head_row_before),
    {Tokenizer, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
Before table head row.

```markdown
> | | a |
    ^
  | | - |
  | | b |
```
""".
-spec head_row_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_row_before(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_table_head),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_table_row),
    case Tokenizer3 of
        #markdown_tokenizer{current = {some, Current}} when Current =:= $\t orelse Current =:= $\s ->
            MaxIndent =
                case Tokenizer3#markdown_tokenizer.parse_state of
                    #markdown_parse_state{
                        options = #markdown_parse_options{
                            constructs = #markdown_construct_options{code_indented = true}
                        }
                    } ->
                        ?TAB_SIZE - 1;
                    _ ->
                        infinity
                end,
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(gfm_table_head_row_start), markdown_state:nok()
            ),
            {Tokenizer5, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer4, 0, MaxIndent
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer5, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_row_start),
            {Tokenizer3, State}
    end.

-doc """
Before table head row, after whitespace.

```markdown
> | | a |
    ^
  | | - |
  | | b |
```
""".
-spec head_row_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_row_start(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        %% 4+ spaces.
        {some, C} when C =:= $\t orelse C =:= $\s ->
            State = markdown_state:nok(),
            {Tokenizer1, State};
        {some, $|} ->
            State = markdown_state:retry(gfm_table_head_row_break),
            {Tokenizer1, State};
        _ ->
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = true},
            %% Count the first character, that isn't a pipe, double.
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{
                size_b = TokenizeState2#markdown_tokenize_state.size_b + 1
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState3},
            State = markdown_state:retry(gfm_table_head_row_break),
            {Tokenizer2, State}
    end.

-doc """
At break in table head row.

```markdown
> | | a |
    ^
      ^
        ^
  | | - |
  | | b |
```
""".
-spec head_row_break(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_row_break(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1}) ->
    case Current of
        none ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                seen = false,
                size = 0,
                size_b = 0
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        {some, $\n} ->
            %% If anything other than one pipe (ignoring whitespace) was used, it's fine.
            case TokenizeState1#markdown_tokenize_state.size_b > 1 of
                true ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = 0},
                    %% Feel free to interrupt:
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{
                        tokenize_state = TokenizeState2,
                        interrupt = true
                    },
                    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, gfm_table_row),
                    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, line_ending),
                    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
                    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, line_ending),
                    State = markdown_state:next(gfm_table_head_delimiter_start),
                    {Tokenizer6, State};
                false ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        seen = false,
                        size = 0,
                        size_b = 0
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:nok(),
                    {Tokenizer2, State}
            end;
        {some, C} when C =:= $\t orelse C =:= $\s ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(gfm_table_head_row_break), markdown_state:nok()
            ),
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        _ ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                size_b = TokenizeState1#markdown_tokenize_state.size_b + 1
            },

            %% Whether a delimiter was seen.
            {TokenizeState3, Tokenizer2} =
                case TokenizeState2#markdown_tokenize_state.seen of
                    true ->
                        %% Header cell count.
                        TS = TokenizeState2#markdown_tokenize_state{
                            seen = false,
                            size = TokenizeState2#markdown_tokenize_state.size + 1
                        },
                        {TS, Tokenizer1#markdown_tokenizer{tokenize_state = TS}};
                    false ->
                        {TokenizeState2, Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2}}
                end,

            case Tokenizer2#markdown_tokenizer.current of
                {some, $|} ->
                    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_table_cell_divider),
                    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
                    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_table_cell_divider),
                    %% Whether a delimiter was seen.
                    TokenizeState4 = TokenizeState3#markdown_tokenize_state{seen = true},
                    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState4},
                    State = markdown_state:next(gfm_table_head_row_break),
                    {Tokenizer6, State};
                _ ->
                    %% Anything else is cell data.
                    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, data),
                    State = markdown_state:retry(gfm_table_head_row_data),
                    {Tokenizer3, State}
            end
    end.

-doc """
In table head row data.

```markdown
> | | a |
      ^
  | | - |
  | | b |
```
""".
-spec head_row_data(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_row_data(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(gfm_table_head_row_break),
            {Tokenizer2, State};
        {some, C} when C =:= $\t orelse C =:= $\n orelse C =:= $\s orelse C =:= $| ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            State = markdown_state:retry(gfm_table_head_row_break),
            {Tokenizer2, State};
        _ ->
            Name =
                case Tokenizer1#markdown_tokenizer.current of
                    {some, $\\} ->
                        gfm_table_head_row_escape;
                    _ ->
                        gfm_table_head_row_data
                end,
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(Name),
            {Tokenizer2, State}
    end.

-doc """
In table head row escape.

```markdown
> | | a\-b |
        ^
  | | ---- |
  | | c    |
```
""".
-spec head_row_escape(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_row_escape(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when C =:= $\\ orelse C =:= $| ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(gfm_table_head_row_data),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_row_data),
            {Tokenizer1, State}
    end.

-doc """
Before delimiter row.

```markdown
  | | a |
> | | - |
    ^
  | | b |
```
""".
-spec head_delimiter_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_start(Tokenizer1 = #markdown_tokenizer{lazy = Lazy, pierce = Pierce, current = Current}) ->
    %% Reset `interrupt`.
    Tokenizer2 = Tokenizer1#markdown_tokenizer{interrupt = false},

    case Lazy orelse Pierce of
        true ->
            TokenizeState1 = Tokenizer2#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer3, State};
        false ->
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_table_delimiter_row),
            %% Track if we've seen a `:` or `|`.
            TokenizeState1 = Tokenizer3#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = false},
            Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState2},

            case Current of
                {some, C} when C =:= $\t orelse C =:= $\s ->
                    Tokenizer5 = markdown_tokenizer:attempt(
                        Tokenizer4,
                        markdown_state:next(gfm_table_head_delimiter_before),
                        markdown_state:next(gfm_table_head_delimiter_nok)
                    ),
                    MaxIndent =
                        case Tokenizer4#markdown_tokenizer.parse_state of
                            #markdown_parse_state{
                                options = #markdown_parse_options{
                                    constructs = #markdown_construct_options{code_indented = true}
                                }
                            } ->
                                ?TAB_SIZE - 1;
                            _ ->
                                infinity
                        end,
                    {Tokenizer6, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                        Tokenizer5, 0, MaxIndent
                    ),
                    State3 = markdown_state:retry(SpaceOrTabState),
                    {Tokenizer6, State3};
                _ ->
                    State = markdown_state:retry(gfm_table_head_delimiter_before),
                    {Tokenizer4, State}
            end
    end.

-doc """
Before delimiter row, after optional whitespace.

Reused when a `|` is found later, to parse another cell.

```markdown
  | | a |
> | | - |
    ^
  | | b |
```
""".
-spec head_delimiter_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when C =:= $- orelse C =:= $: ->
            State = markdown_state:retry(gfm_table_head_delimiter_value_before),
            {Tokenizer1, State};
        {some, $|} ->
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = true},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            %% If we start with a pipe, we open a cell marker.
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_table_cell_divider),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_table_cell_divider),
            State = markdown_state:next(gfm_table_head_delimiter_cell_before),
            {Tokenizer5, State};
        %% More whitespace / empty row not allowed at start.
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_nok),
            {Tokenizer1, State}
    end.

-doc """
After `|`, before delimiter cell.

```markdown
  | | a |
> | | - |
     ^
```
""".
-spec head_delimiter_cell_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_cell_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when C =:= $\t orelse C =:= $\s ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(gfm_table_head_delimiter_value_before), markdown_state:nok()
            ),
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_value_before),
            {Tokenizer1, State}
    end.

-doc """
Before delimiter cell value.

```markdown
  | | a |
> | | - |
      ^
```
""".
-spec head_delimiter_value_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_value_before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:retry(gfm_table_head_delimiter_cell_after),
            {Tokenizer1, State};
        {some, $\n} ->
            State = markdown_state:retry(gfm_table_head_delimiter_cell_after),
            {Tokenizer1, State};
        {some, $:} ->
            %% Align: left.
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                size_b = TokenizeState1#markdown_tokenize_state.size_b + 1,
                seen = true
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_table_delimiter_marker),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_table_delimiter_marker),
            State = markdown_state:next(gfm_table_head_delimiter_left_alignment_after),
            {Tokenizer5, State};
        {some, $-} ->
            %% Align: none.
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                size_b = TokenizeState1#markdown_tokenize_state.size_b + 1
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:retry(gfm_table_head_delimiter_left_alignment_after),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_nok),
            {Tokenizer1, State}
    end.

-doc """
After delimiter cell left alignment marker.

```markdown
  | | a  |
> | | :- |
       ^
```
""".
-spec head_delimiter_left_alignment_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_left_alignment_after(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_table_delimiter_filler),
            State = markdown_state:retry(gfm_table_head_delimiter_filler),
            {Tokenizer2, State};
        %% Anything else is not ok after the left-align colon.
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_nok),
            {Tokenizer1, State}
    end.

-doc """
In delimiter cell filler.

```markdown
  | | a |
> | | - |
      ^
```
""".
-spec head_delimiter_filler(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_filler(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, $-} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(gfm_table_head_delimiter_filler),
            {Tokenizer2, State};
        {some, $:} ->
            %% Align is `center` if it was `left`, `right` otherwise.
            TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{seen = true},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, gfm_table_delimiter_filler),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, gfm_table_delimiter_marker),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, gfm_table_delimiter_marker),
            State = markdown_state:next(gfm_table_head_delimiter_right_alignment_after),
            {Tokenizer6, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, gfm_table_delimiter_filler),
            State = markdown_state:retry(gfm_table_head_delimiter_right_alignment_after),
            {Tokenizer2, State}
    end.

-doc """
After delimiter cell right alignment marker.

```markdown
  | |  a |
> | | -: |
        ^
```
""".
-spec head_delimiter_right_alignment_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_right_alignment_after(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        {some, C} when C =:= $\t orelse C =:= $\s ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(gfm_table_head_delimiter_cell_after), markdown_state:nok()
            ),
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_cell_after),
            {Tokenizer1, State}
    end.

-doc """
After delimiter cell.

```markdown
  | |  a |
> | | -: |
         ^
```
""".
-spec head_delimiter_cell_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_cell_after(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState}) ->
    case Current of
        none ->
            %% Exit when:
            %% * there was no `:` or `|` at all (it's a thematic break or setext
            %%   underline instead)
            %% * the header cell count is not the delimiter cell count
            case
                not TokenizeState#markdown_tokenize_state.seen orelse
                    TokenizeState#markdown_tokenize_state.size =/= TokenizeState#markdown_tokenize_state.size_b
            of
                true ->
                    State = markdown_state:retry(gfm_table_head_delimiter_nok),
                    {Tokenizer1, State};
                false ->
                    %% Reset.
                    TokenizeState1 = TokenizeState#markdown_tokenize_state{
                        seen = false,
                        size = 0,
                        size_b = 0
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState1},
                    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, gfm_table_delimiter_row),
                    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_table_head),
                    Tokenizer5 = markdown_tokenizer:register_resolver(Tokenizer4, gfm_table),
                    State = markdown_state:ok(),
                    {Tokenizer5, State}
            end;
        {some, $\n} ->
            %% Exit when:
            %% * there was no `:` or `|` at all (it's a thematic break or setext
            %%   underline instead)
            %% * the header cell count is not the delimiter cell count
            case
                not TokenizeState#markdown_tokenize_state.seen orelse
                    TokenizeState#markdown_tokenize_state.size =/= TokenizeState#markdown_tokenize_state.size_b
            of
                true ->
                    State = markdown_state:retry(gfm_table_head_delimiter_nok),
                    {Tokenizer1, State};
                false ->
                    %% Reset.
                    TokenizeState1 = TokenizeState#markdown_tokenize_state{
                        seen = false,
                        size = 0,
                        size_b = 0
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState1},
                    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, gfm_table_delimiter_row),
                    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_table_head),
                    Tokenizer5 = markdown_tokenizer:register_resolver(Tokenizer4, gfm_table),
                    State = markdown_state:ok(),
                    {Tokenizer5, State}
            end;
        {some, $|} ->
            State = markdown_state:retry(gfm_table_head_delimiter_before),
            {Tokenizer1, State};
        _ ->
            State = markdown_state:retry(gfm_table_head_delimiter_nok),
            {Tokenizer1, State}
    end.

-doc """
In delimiter row, at a disallowed byte.

```markdown
  | | a |
> | | x |
      ^
```
""".
-spec head_delimiter_nok(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
head_delimiter_nok(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    %% Reset.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        seen = false,
        size = 0,
        size_b = 0
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
Before table body row.

```markdown
  | | a |
  | | - |
> | | b |
    ^
```
""".
-spec body_row_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
body_row_start(Tokenizer1 = #markdown_tokenizer{lazy = true}) ->
    State = markdown_state:nok(),
    {Tokenizer1, State};
body_row_start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_table_row),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(gfm_table_body_row_break), markdown_state:nok()
    ),
    %% We're parsing a body row.
    %% If we're here, we already attempted blank lines and indented
    %% code.
    %% So parse as much whitespace as needed:
    {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer3, 0, markdown_types:usize_max()
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer4, State};
body_row_start(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_table_row),
    State = markdown_state:retry(gfm_table_body_row_break),
    {Tokenizer2, State}.

-doc """
At break in table body row.

```markdown
  | | a |
  | | - |
> | | b |
    ^
      ^
        ^
```
""".
-spec body_row_break(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
body_row_break(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    ?is_option_none(Current) orelse ?is_option_some(Current, $\n)
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, gfm_table_row),
    State = markdown_state:ok(),
    {Tokenizer2, State};
body_row_break(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(gfm_table_body_row_break), markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
body_row_break(Tokenizer1 = #markdown_tokenizer{current = {some, $|}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_table_cell_divider),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_table_cell_divider),
    State = markdown_state:next(gfm_table_body_row_break),
    {Tokenizer4, State};
body_row_break(Tokenizer1 = #markdown_tokenizer{}) ->
    %% Anything else is cell content.
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, data),
    State = markdown_state:retry(gfm_table_body_row_data),
    {Tokenizer2, State}.

-doc """
In table body row data.

```markdown
  | | a |
  | | - |
> | | b |
      ^
```
""".
-spec body_row_data(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
body_row_data(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
    State = markdown_state:retry(gfm_table_body_row_break),
    {Tokenizer2, State};
body_row_data(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\n orelse Current =:= $\s orelse Current =:= $|
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
    State = markdown_state:retry(gfm_table_body_row_break),
    {Tokenizer2, State};
body_row_data(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) ->
    Name =
        case Current of
            $\\ -> gfm_table_body_row_escape;
            _ -> gfm_table_body_row_data
        end,
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(Name),
    {Tokenizer2, State}.

-doc """
In table body row escape.

```markdown
  | | a    |
  | | ---- |
> | | b\-c |
        ^
```
""".
-spec body_row_escape(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
body_row_escape(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\\ orelse Current =:= $|
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(gfm_table_body_row_data),
    {Tokenizer2, State};
body_row_escape(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:retry(gfm_table_body_row_data),
    {Tokenizer1, State}.

-doc """
Resolve GFM table.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{}) ->
    State1 = #resolve{
        index = 0,
        in_first_cell_awaiting_pipe = true,
        in_row = false,
        in_delimiter_row = false,
        last_cell = #cell{exit = 0, enter = 0, data_enter = 0, data_exit = 0},
        cell = #cell{exit = 0, enter = 0, data_enter = 0, data_exit = 0},
        after_head_awaiting_first_body_row = false,
        last_table_end = 0,
        last_table_has_body = false
    },
    {
        Tokenizer2,
        #resolve{last_table_end = LastTableEnd, last_table_has_body = LastTableHasBody}
    } = resolve_loop(Tokenizer1, State1),
    Tokenizer3 =
        case LastTableEnd of
            0 ->
                Tokenizer2;
            _ ->
                resolve__flush_table_end(Tokenizer2, LastTableEnd, LastTableHasBody)
        end,
    Tokenizer4 = markdown_tokenizer:map_consume(Tokenizer3),
    {Tokenizer4, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(Tokenizer, State) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: resolve().
resolve_loop(Tokenizer1 = #markdown_tokenizer{events = Events}, State1 = #resolve{index = Index}) when
    Index >= 0 andalso Index < ?markdown_vec_size(Events)
->
    Event = markdown_vec:get(Events, Index),
    {Tokenizer2, State2} =
        case Event of
            %% Enter events.
            #markdown_event{kind = enter} ->
                case Event#markdown_event.name of
                    %% Start of head.
                    gfm_table_head ->
                        resolve__enter_gfm_table_head(Tokenizer1, State1, Event);
                    Name when Name =:= gfm_table_row orelse Name =:= gfm_table_delimiter_row ->
                        resolve__enter_gfm_table_row(Tokenizer1, State1, Event);
                    %% Cell data.
                    Name when
                        State1#resolve.in_row =:= true andalso
                            (Name =:= data orelse Name =:= gfm_table_delimiter_marker orelse
                                Name =:= gfm_table_delimiter_filler)
                    ->
                        resolve__enter_data(Tokenizer1, State1, Event);
                    gfm_table_cell_divider ->
                        resolve__enter_gfm_table_cell_divider(Tokenizer1, State1, Event);
                    _ ->
                        {Tokenizer1, State1}
                end;
            %% Exit events.
            #markdown_event{kind = exit} ->
                case Event#markdown_event.name of
                    gfm_table_head ->
                        resolve__exit_gfm_table_head(Tokenizer1, State1, Event);
                    Name when Name =:= gfm_table_row orelse Name =:= gfm_table_delimiter_row ->
                        resolve__exit_gfm_table_row(Tokenizer1, State1, Event);
                    Name when
                        State1#resolve.in_row =:= true andalso
                            (Name =:= data orelse Name =:= gfm_table_delimiter_marker orelse
                                Name =:= gfm_table_delimiter_filler)
                    ->
                        resolve__exit_data(Tokenizer1, State1, Event);
                    _ ->
                        {Tokenizer1, State1}
                end
        end,
    State3 = State2#resolve{index = State2#resolve.index + 1},
    resolve_loop(Tokenizer2, State3);
resolve_loop(Tokenizer1 = #markdown_tokenizer{}, State1 = #resolve{}) ->
    {Tokenizer1, State1}.

% -spec cell_to_string(cell()) -> dynamic().
% cell_to_string(#cell{exit = Exit, enter = Enter, data_enter = DataEnter, data_exit = DataExit}) ->
%     markdown_debug:rust_debug_string({Exit, Enter, DataEnter, DataExit}).

%% @private
-spec resolve__enter_data(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__enter_data(
    Tokenizer1 = #markdown_tokenizer{},
    State1 = #resolve{index = Index, in_delimiter_row = InDelimiterRow, last_cell = LastCell1, cell = Cell1},
    _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__enter_data\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{in_first_cell_awaiting_pipe = false},
    case Cell1#cell.data_enter of
        0 ->
            %% First value in cell.
            {Tokenizer3, State4} =
                case LastCell1#cell.enter of
                    0 ->
                        {Tokenizer1, State2};
                    _ ->
                        % io:format("\n\n[~w:~w] BEFORE resolve__enter_data:resolve__flush_cell\ncell = ~ts\nlast_cell = ~ts\nin_delimiter_row = ~ts\n\n", [markdown:counter_get(), markdown:stack_push(), cell_to_string(Cell1), cell_to_string(LastCell1), markdown_debug:rust_debug_string(InDelimiterRow)]),
                        Tokenizer2 = resolve__flush_cell(Tokenizer1, LastCell1, InDelimiterRow, none),
                        State3 = State2#resolve{
                            last_cell = #cell{exit = 0, enter = 0, data_enter = 0, data_exit = 0},
                            cell = Cell1#cell{exit = Cell1#cell.enter}
                        },
                        % io:format("\n\n[~w:~w] AFTER resolve__enter_data:resolve__flush_cell\ncell = ~ts\nlast_cell = ~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), cell_to_string(State3#resolve.cell), cell_to_string(State3#resolve.last_cell)]),
                        {Tokenizer2, State3}
                end,
            Cell2 = State4#resolve.cell,
            Cell3 = Cell2#cell{data_enter = Index},
            State5 = State4#resolve{cell = Cell3},
            % io:format("\n\n[~w:~w] AFTER resolve__enter_data\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer3)]),
            {Tokenizer3, State5};
        _ ->
            % io:format("\n\n[~w:~w] AFTER resolve__enter_data\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer1)]),
            {Tokenizer1, State2}
    end.

%% @private
-spec resolve__enter_gfm_table_cell_divider(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__enter_gfm_table_cell_divider(
    Tokenizer1 = #markdown_tokenizer{},
    State1 = #resolve{in_first_cell_awaiting_pipe = true},
    _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__enter_gfm_table_cell_divider\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{in_first_cell_awaiting_pipe = false},
    % io:format("\n\n[~w:~w] AFTER resolve__enter_gfm_table_cell_divider\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer1)]),
    {Tokenizer1, State2};
resolve__enter_gfm_table_cell_divider(
    Tokenizer1 = #markdown_tokenizer{},
    State1 = #resolve{index = Index, last_cell = LastCell1, cell = Cell1},
    _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__enter_gfm_table_cell_divider\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    {Tokenizer3, State3} =
        case LastCell1#cell.enter of
            0 ->
                {Tokenizer1, State1};
            _ ->
                Cell2 = Cell1#cell{exit = Cell1#cell.enter},
                State2 = State1#resolve{cell = Cell2},
                Tokenizer2 = resolve__flush_cell(Tokenizer1, LastCell1, State1#resolve.in_delimiter_row, none),
                {Tokenizer2, State2}
        end,
    Cell3 = State3#resolve.cell,
    LastCell2 = Cell3,
    Cell4 = Cell3#cell{exit = LastCell2#cell.enter, enter = Index, data_enter = 0, data_exit = 0},
    State4 = State3#resolve{last_cell = LastCell2, cell = Cell4},
    % io:format("\n\n[~w:~w] AFTER resolve__enter_gfm_table_cell_divider\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer3)]),
    {Tokenizer3, State4}.

%% @private
-spec resolve__enter_gfm_table_head(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__enter_gfm_table_head(
    Tokenizer1 = #markdown_tokenizer{},
    State1 = #resolve{index = Index, last_table_end = LastTableEnd1, last_table_has_body = LastTableHasBody1},
    Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__enter_gfm_table_head\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{after_head_awaiting_first_body_row = false},
    {Tokenizer2, State3} =
        case LastTableEnd1 of
            0 ->
                {Tokenizer1, State2};
            _ ->
                %% Inject previous (body end and) table end.
                Tokenizer1_1 = Tokenizer1,
                Tokenizer1_2 = resolve__flush_table_end(Tokenizer1_1, LastTableEnd1, LastTableHasBody1),
                State2_1 = State2,
                State2_2 = State2_1#resolve{last_table_end = 0, last_table_has_body = false},
                {Tokenizer1_2, State2_2}
        end,
    %% Inject table start.
    EnterEvent = markdown_event:gfm_table(enter, Event#markdown_event.point, none),
    Tokenizer3 = markdown_tokenizer:map_add(Tokenizer2, Index, 0, [EnterEvent]),
    % io:format("\n\n[~w:~w] AFTER resolve__enter_gfm_table_head\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer3)]),
    {Tokenizer3, State3}.

%% @private
-spec resolve__enter_gfm_table_row(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__enter_gfm_table_row(
    Tokenizer1 = #markdown_tokenizer{},
    State1 = #resolve{index = Index, after_head_awaiting_first_body_row = AfterHeadAwaitingFirstBodyRow1},
    Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__enter_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{
        in_delimiter_row = Event#markdown_event.name =:= gfm_table_delimiter_row,
        in_first_cell_awaiting_pipe = true,
        in_row = true,
        last_cell = #cell{exit = 0, enter = 0, data_enter = 0, data_exit = 0},
        cell = #cell{exit = 0, enter = Index + 1, data_enter = 0, data_exit = 0}
    },
    {Tokenizer2, State3} =
        case AfterHeadAwaitingFirstBodyRow1 of
            false ->
                {Tokenizer1, State2};
            true ->
                %% Inject table body start.
                State2_1 = State2,
                State2_2 = State2_1#resolve{after_head_awaiting_first_body_row = false, last_table_has_body = true},
                EnterEvent = markdown_event:gfm_table_body(enter, Event#markdown_event.point, none),
                Tokenizer1_1 = Tokenizer1,
                Tokenizer1_2 = markdown_tokenizer:map_add(Tokenizer1_1, Index, 0, [EnterEvent]),
                {Tokenizer1_2, State2_2}
        end,
    % io:format("\n\n[~w:~w] AFTER resolve__enter_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer2)]),
    {Tokenizer2, State3}.

%% @private
-spec resolve__exit_data(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__exit_data(
    Tokenizer1 = #markdown_tokenizer{}, State1 = #resolve{index = Index, cell = Cell1}, _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__exit_data\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    Cell2 = Cell1#cell{data_exit = Index},
    State2 = State1#resolve{cell = Cell2},
    % io:format("\n\n[~w:~w] AFTER resolve__exit_data\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer1)]),
    {Tokenizer1, State2}.

%% @private
-spec resolve__exit_gfm_table_head(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__exit_gfm_table_head(
    Tokenizer1 = #markdown_tokenizer{}, State1 = #resolve{index = Index}, _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__exit_gfm_table_head\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{after_head_awaiting_first_body_row = true, last_table_end = Index},
    % io:format("\n\n[~w:~w] AFTER resolve__exit_gfm_table_head\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer1)]),
    {Tokenizer1, State2}.

%% @private
-spec resolve__exit_gfm_table_row(Tokenizer, State, Event) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: resolve(), Event :: markdown_event:t().
resolve__exit_gfm_table_row(
    Tokenizer1 = #markdown_tokenizer{}, State1 = #resolve{index = Index}, _Event = #markdown_event{}
) ->
    % io:format("\n\n[~w:~w] BEFORE resolve__exit_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Tokenizer1)]),
    State2 = State1#resolve{in_row = false, last_table_end = Index},
    case State2 of
        #resolve{last_cell = LastCell1 = #cell{enter = LastCellEnter}} when LastCellEnter =/= 0 ->
            Cell1 = State2#resolve.cell,
            Cell2 = Cell1#cell{exit = Cell1#cell.enter},
            State3 = State2#resolve{cell = Cell2},
            Tokenizer2 = resolve__flush_cell(Tokenizer1, LastCell1, State2#resolve.in_delimiter_row, {some, Index}),
            % io:format("\n\n[~w:~w] AFTER resolve__exit_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer2)]),
            {Tokenizer2, State3};
        #resolve{cell = Cell1 = #cell{enter = CellEnter}} when CellEnter =/= 0 ->
            Tokenizer2 = resolve__flush_cell(Tokenizer1, Cell1, State2#resolve.in_delimiter_row, {some, Index}),
            % io:format("\n\n[~w:~w] AFTER resolve__exit_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer2)]),
            {Tokenizer2, State2};
        _ ->
            % io:format("\n\n[~w:~w] AFTER resolve__exit_gfm_table_row\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Tokenizer1)]),
            {Tokenizer1, State2}
    end.

%% @private
-doc """
Generate a cell.
""".
-spec resolve__flush_cell(Tokenizer, Range, InDelimiterRow, OptionRowEnd) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    Range :: cell(),
    InDelimiterRow :: boolean(),
    OptionRowEnd :: markdown_option:t(RowEnd),
    RowEnd :: markdown_types:usize().
resolve__flush_cell(Tokenizer1 = #markdown_tokenizer{}, Range = #cell{}, InDelimiterRow, OptionRowEnd) ->
    {MakeGroupEvent, MakeValueEvent} =
        case InDelimiterRow of
            true ->
                {fun markdown_event:gfm_table_delimiter_cell/3, fun markdown_event:gfm_table_delimiter_cell_value/3};
            false ->
                {fun markdown_event:gfm_table_cell/3, fun markdown_event:gfm_table_cell_text/3}
        end,
    %% Insert an exit for the previous cell, if there is one.
    %%
    %% ```markdown
    %% > | | aa | bb | cc |
    %%          ^-- exit
    %%           ^^^^-- this cell
    %% ```
    Tokenizer2 =
        case Range#cell.exit of
            0 ->
                Tokenizer1;
            RangeExit ->
                RangeExitEvent = markdown_vec:get(Tokenizer1#markdown_tokenizer.events, RangeExit),
                ExitEvent = MakeGroupEvent(exit, RangeExitEvent#markdown_event.point, none),
                markdown_tokenizer:map_add(Tokenizer1, RangeExit, 0, [ExitEvent])
        end,
    %% Insert enter of this cell.
    %%
    %% ```markdown
    %% > | | aa | bb | cc |
    %%           ^-- enter
    %%           ^^^^-- this cell
    %% ```
    RangeEnter = Range#cell.enter,
    RangeEnterEvent = markdown_vec:get(Tokenizer2#markdown_tokenizer.events, RangeEnter),
    EnterEvent = MakeGroupEvent(enter, RangeEnterEvent#markdown_event.point, none),
    Tokenizer3 = markdown_tokenizer:map_add(Tokenizer2, RangeEnter, 0, [EnterEvent]),
    %% Insert text start at first data start and end at last data end, and
    %% remove events between.
    %%
    %% ```markdown
    %% > | | aa | bb | cc |
    %%            ^-- enter
    %%             ^-- exit
    %%           ^^^^-- this cell
    %% ```
    Tokenizer7 =
        case Range#cell.data_enter of
            0 ->
                Tokenizer3;
            RangeDataEnter ->
                RangeDataEnterEvent1 = markdown_vec:get(Tokenizer3#markdown_tokenizer.events, RangeDataEnter),
                DataEnterEvent = MakeValueEvent(enter, RangeDataEnterEvent1#markdown_event.point, none),
                Tokenizer4 = markdown_tokenizer:map_add(Tokenizer3, RangeDataEnter, 0, [DataEnterEvent]),
                RangeDataExit = Range#cell.data_exit,
                %% BEGIN: assertions
                ?assert(RangeDataExit =/= 0, "expected RangeDataExit to be non-zero"),
                %% END: assertions
                Tokenizer6 =
                    case InDelimiterRow of
                        false ->
                            Events4 = Tokenizer4#markdown_tokenizer.events,
                            RangeDataEnterEvent2 = markdown_vec:get(Events4, RangeDataEnter),
                            RangeDataEnterEvent3 = RangeDataEnterEvent2#markdown_event{
                                link = {some, markdown_event_link:new(none, none, text)}
                            },
                            Events5 = markdown_vec:set(Events4, RangeDataEnter, RangeDataEnterEvent3),
                            Tokenizer5 = Tokenizer4#markdown_tokenizer{events = Events5},
                            case RangeDataExit > RangeDataEnter + 1 of
                                false ->
                                    Tokenizer5;
                                true ->
                                    %% To do: positional info of the remaining `data` nodes likely have
                                    %% to be fixed.
                                    A = RangeDataEnter + 1,
                                    B = RangeDataExit - RangeDataEnter - 1,
                                    markdown_tokenizer:map_add(Tokenizer5, A, B, [])
                            end;
                        true ->
                            Tokenizer4
                    end,
                RangeDataExitEvent = markdown_vec:get(Tokenizer6#markdown_tokenizer.events, RangeDataExit),
                DataExitEvent = MakeValueEvent(exit, RangeDataExitEvent#markdown_event.point, none),
                markdown_tokenizer:map_add(Tokenizer6, RangeDataExit + 1, 0, [DataExitEvent])
        end,
    %% Insert an exit for the last cell, if at the row end.
    %%
    %% ```markdown
    %% > | | aa | bb | cc |
    %%                    ^-- exit
    %%               ^^^^^^-- this cell (the last one contains two “between” parts)
    %% ```
    case OptionRowEnd of
        none ->
            Tokenizer7;
        {some, RowEnd} ->
            RowEndEvent = markdown_vec:get(Tokenizer7#markdown_tokenizer.events, RowEnd),
            RowEndExitEvent = MakeGroupEvent(exit, RowEndEvent#markdown_event.point, none),
            markdown_tokenizer:map_add(Tokenizer7, RowEnd, 0, [RowEndExitEvent])
    end.

%% @private
-doc """
Generate table end (and table body end).
""".
-spec resolve__flush_table_end(Tokenizer, Index, Body) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(), Index :: markdown_types:usize(), Body :: boolean().
resolve__flush_table_end(Tokenizer1 = #markdown_tokenizer{events = Events}, Index, Body) ->
    Event = markdown_vec:get(Events, Index),
    Point = Event#markdown_event.point,
    Exits1 = markdown_vec:new(),
    Exits2 =
        case Body of
            true ->
                TableBodyExitEvent = markdown_event:gfm_table_body(exit, Point, none),
                markdown_vec:push(Exits1, TableBodyExitEvent);
            false ->
                Exits1
        end,
    TableExitEvent = markdown_event:gfm_table(exit, Point, none),
    Exits3 = markdown_vec:push(Exits2, TableExitEvent),
    Tokenizer2 = markdown_tokenizer:map_add(Tokenizer1, Index + 1, 0, Exits3),
    Tokenizer2.
