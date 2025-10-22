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
-module(markdown_construct_frontmatter).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    start/1,
    open_sequence/1,
    open_after/1,
    close_start/1,
    close_sequence/1,
    close_after/1,
    content_start/1,
    content_inside/1,
    content_end/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of frontmatter.

```markdown
> | ---
    ^
  | title: "Venus"
  | ---
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{frontmatter = true}}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $+ orelse Current =:= $- ->
    %% Indent is not allowed.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Current},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, frontmatter),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, frontmatter_fence),
    Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, frontmatter_sequence),
    State = markdown_state:retry(frontmatter_open_sequence),
    {Tokenizer5, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In open sequence.

```markdown
> | ---
    ^
  | title: "Venus"
  | ---
```
""".
-spec open_sequence(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open_sequence(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size1}
    }
) when Current =:= Marker ->
    Size2 = Size1 + 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(frontmatter_open_sequence),
    {Tokenizer3, State};
open_sequence(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = ?FRONTMATTER_SEQUENCE_SIZE}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, frontmatter_sequence),
    case Current of
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(frontmatter_open_after), markdown_state:nok()
            ),
            {Tokenizer5, State} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer4),
            {Tokenizer5, markdown_state:retry(State)};
        _ ->
            State = markdown_state:retry(frontmatter_open_after),
            {Tokenizer3, State}
    end;
open_sequence(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0, size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After open sequence.

```markdown
> | ---
       ^
  | title: "Venus"
  | ---
```
""".
-spec open_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open_after(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter_fence),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, line_ending),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, line_ending),
    Tokenizer6 = markdown_tokenizer:attempt(
        Tokenizer5, markdown_state:next(frontmatter_after), markdown_state:next(frontmatter_content_start)
    ),
    State = markdown_state:next(frontmatter_close_start),
    {Tokenizer6, State};
open_after(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
Start of close sequence.

```markdown
  | ---
  | title: "Venus"
> | ---
    ^
```
""".
-spec close_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
close_start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = #markdown_tokenize_state{marker = Marker}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, frontmatter_fence),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, frontmatter_sequence),
    State = markdown_state:retry(frontmatter_close_sequence),
    {Tokenizer3, State};
close_start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In close sequence.

```markdown
  | ---
  | title: "Venus"
> | ---
    ^
```
""".
-spec close_sequence(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
close_sequence(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size1}
    }
) when Current =:= Marker ->
    Size2 = Size1 + 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(frontmatter_close_sequence),
    {Tokenizer3, State};
close_sequence(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = ?FRONTMATTER_SEQUENCE_SIZE}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, frontmatter_sequence),
    case Current of
        {some, Char} when Char =:= $\t orelse Char =:= $\s ->
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(frontmatter_close_after), markdown_state:nok()
            ),
            {Tokenizer5, State} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer4),
            {Tokenizer5, markdown_state:retry(State)};
        _ ->
            State = markdown_state:retry(frontmatter_close_after),
            {Tokenizer3, State}
    end;
close_sequence(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After close sequence.

```markdown
  | ---
  | title: "Venus"
> | ---
       ^
```
""".
-spec close_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
close_after(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter_fence),
            State = markdown_state:ok(),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter_fence),
            State = markdown_state:ok(),
            {Tokenizer2, State};
        {some, _} ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
Start of content chunk.

```markdown
  | ---
> | title: "Venus"
    ^
  | ---
```
""".
-spec content_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_start(Tokenizer = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            State = markdown_state:retry(frontmatter_content_end),
            {Tokenizer, State};
        {some, $\n} ->
            State = markdown_state:retry(frontmatter_content_end),
            {Tokenizer, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer, frontmatter_chunk),
            State = markdown_state:retry(frontmatter_content_inside),
            {Tokenizer2, State}
    end.

-doc """
In content chunk.

```markdown
  | ---
> | title: "Venus"
    ^
  | ---
```
""".
-spec content_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_inside(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter_chunk),
            State = markdown_state:retry(frontmatter_content_end),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter_chunk),
            State = markdown_state:retry(frontmatter_content_end),
            {Tokenizer2, State};
        {some, _} ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(frontmatter_content_inside),
            {Tokenizer2, State}
    end.

-doc """
End of content chunk.

```markdown
  | ---
> | title: "Venus"
                  ^
  | ---
```
""".
-spec content_end(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_end(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) ->
    case Current of
        none ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State};
        {some, $\n} ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
            Tokenizer5 = markdown_tokenizer:attempt(
                Tokenizer4, markdown_state:next(frontmatter_after), markdown_state:next(frontmatter_content_start)
            ),
            State = markdown_state:next(frontmatter_close_start),
            {Tokenizer5, State}
    end.

-doc """
After frontmatter.

```markdown
  | ---
  | title: "Venus"
> | ---
       ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = OptionCurrent}) ->
    %% BEGIN: assertions
    ?assert(
        ?is_option_none(OptionCurrent) orelse ?is_option_some(OptionCurrent, $\n),
        "expected eol/eof after closing fence"
    ),
    %% END: assertions
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, frontmatter),
    State = markdown_state:ok(),
    {Tokenizer2, State}.
