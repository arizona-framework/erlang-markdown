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
-include_lib("markdown/include/markdown_vec.hrl").

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
    head_delimiter_nok/1
]).

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
            {Tokenizer2, SpaceOrTabState} = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(gfm_table_head_delimiter_value_before), markdown_state:nok()
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer2, State};
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
