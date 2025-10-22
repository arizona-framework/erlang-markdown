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
-module(markdown_construct_gfm_task_list_item_check).
-moduledoc """
GFM: Task list item check occurs in the [text][] content type.

## Grammar

Checks form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
gfm_task_list_item_check ::= '[' (0x09 | ' ' | 'X' | 'x') ']'
```

The check is only allowed at the start of the first paragraph, optionally
following zero or more definitions or a blank line, in a list item.
The check must be followed by whitespace, which is in turn followed by
non-whitespace.

## HTML

Checks relate to the `<input>` element, in the checkbox state
(`type=checkbox`), in HTML.
See [*§ 4.10.5.1.15 Checkbox state (`type=checkbox`)*][html-input-checkbox]
in the HTML spec for more info.

## Recommendation

It is recommended to use lowercase `x` (instead of uppercase `X`), because
in markdown, it is more common to use lowercase in places where casing does
not matter.
It is also recommended to use a space (instead of a tab), as there is no
benefit of using tabs in this case.

## Tokens

*   [`GfmTaskListItemCheck`][Name::GfmTaskListItemCheck]
*   [`GfmTaskListItemMarker`][Name::GfmTaskListItemMarker]
*   [`GfmTaskListItemValueChecked`][Name::GfmTaskListItemValueChecked]
*   [`GfmTaskListItemValueUnchecked`][Name::GfmTaskListItemValueUnchecked]

## References

*   [`micromark-extension-gfm-task-list-item`](https://github.com/micromark/micromark-extension-gfm-task-list-item)
*   [*§ 5.3 Task list items (extension)* in `GFM`](https://github.github.com/gfm/#task-list-items-extension-)

[text]: crate::construct::text
[html-input-checkbox]: https://html.spec.whatwg.org/multipage/input.html#checkbox-state-(type=checkbox)
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    inside/1,
    close/1,
    'after'/1,
    after_space_or_tab/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At start of task list item check.

```markdown
> | * [x] y.
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $[},
        previous = none,
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{gfm_task_list_item = true}}
        },
        tokenize_state = #markdown_tokenize_state{document_at_first_paragraph_of_list_item = true}
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_task_list_item_check),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, gfm_task_list_item_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_task_list_item_marker),
    State = markdown_state:next(gfm_task_list_item_check_inside),
    {Tokenizer5, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In task list item check.

```markdown
> | * [x] y.
       ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $\t orelse Current =:= $\n orelse Current =:= $\s
->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_task_list_item_value_unchecked),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_task_list_item_value_unchecked),
    State = markdown_state:next(gfm_task_list_item_check_close),
    {Tokenizer4, State};
inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $X orelse Current =:= $x ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_task_list_item_value_checked),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_task_list_item_value_checked),
    State = markdown_state:next(gfm_task_list_item_check_close),
    {Tokenizer4, State};
inside(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
At close of task list item check.

```markdown
> | * [x] y.
        ^
```
""".
-spec close(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
close(Tokenizer1 = #markdown_tokenizer{current = {some, $]}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_task_list_item_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_task_list_item_marker),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_task_list_item_check),
    State = markdown_state:next(gfm_task_list_item_check_after),
    {Tokenizer5, State};
close(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After task list item check.

```markdown
> | * [x] y.
         ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    State = markdown_state:ok(),
    {Tokenizer1, State};
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:check(Tokenizer1, markdown_state:ok(), markdown_state:nok()),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(gfm_task_list_item_check_after_space_or_tab),
        markdown_state:nok()
    ),
    {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer3),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer4, State};
'after'(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After whitespace, after task list item check.

```markdown
> | * [x] y.
          ^
```
""".
-spec after_space_or_tab(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_space_or_tab(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:nok(),
    {Tokenizer, State};
after_space_or_tab(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:ok(),
    {Tokenizer, State}.
