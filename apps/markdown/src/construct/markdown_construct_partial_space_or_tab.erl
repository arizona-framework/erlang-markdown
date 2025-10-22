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
-module(markdown_construct_partial_space_or_tab).
-moduledoc """
Space or tab occurs in tons of places.

## Grammar

Space or tab forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
space_or_tab ::= 1*('\t' | ' ')
```

## References

*   [`micromark-factory-space/index.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-factory-space/dev/index.js)
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    space_or_tab/1,
    space_or_tab_min_max/3,
    space_or_tab_with_options/2,
    start/1,
    inside/1,
    'after'/1
]).

%% Types
-type options() :: #markdown_space_or_tab_options{}.

-export_type([
    options/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
One or more `space_or_tab`.

```bnf
space_or_tab ::= 1*( ' ' '\t' )
```
""".
-spec space_or_tab(Tokenizer) -> {Tokenizer, StateName} when
    Tokenizer :: markdown_tokenizer:t(), StateName :: markdown_state:name().
space_or_tab(Tokenizer) ->
    space_or_tab_min_max(Tokenizer, 1, infinity).

-doc """
Between `x` and `y` `space_or_tab`.

```bnf
space_or_tab_min_max ::= x*y( ' ' '\t' )
```
""".
-spec space_or_tab_min_max(Tokenizer, Min, Max) -> {Tokenizer, StateName} when
    Tokenizer :: markdown_tokenizer:t(),
    Min :: markdown_types:usize(),
    Max :: markdown_types:usize() | infinity,
    StateName :: markdown_state:name().
space_or_tab_min_max(Tokenizer, Min, Max) ->
    space_or_tab_with_options(
        Tokenizer,
        #markdown_space_or_tab_options{
            kind = space_or_tab,
            min = Min,
            max = Max,
            content = none,
            connect = false
        }
    ).

-doc """
`space_or_tab`, with the given options.
""".
-spec space_or_tab_with_options(Tokenizer, Options) -> {Tokenizer, StateName} when
    Tokenizer :: markdown_tokenizer:t(),
    Options :: options(),
    StateName :: markdown_state:name().
space_or_tab_with_options(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}, Options) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        space_or_tab_connect = Options#markdown_space_or_tab_options.connect,
        space_or_tab_content = Options#markdown_space_or_tab_options.content,
        space_or_tab_min = Options#markdown_space_or_tab_options.min,
        space_or_tab_max = Options#markdown_space_or_tab_options.max,
        space_or_tab_token = Options#markdown_space_or_tab_options.kind
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, space_or_tab_start}.

-doc """
Start of `space_or_tab`.

```markdown
> | a␠␠b
        ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = #markdown_tokenize_state{
            space_or_tab_content = OptionSpaceOrTabContent,
            space_or_tab_max = SpaceOrTabMax,
            space_or_tab_token = SpaceOrTabToken
        }
    }
) when
    (SpaceOrTabMax > 0 orelse SpaceOrTabMax =:= infinity) andalso
        (Current =:= {some, $\t} orelse Current =:= {some, $\s})
->
    Tokenizer2 =
        #markdown_tokenizer{
            events = Events1,
            tokenize_state = TokenizeState2 = #markdown_tokenize_state{space_or_tab_connect = SpaceOrTabConnect}
        } =
        case OptionSpaceOrTabContent of
            {some, SpaceOrTabContent} ->
                Link = markdown_event_link:new(none, none, SpaceOrTabContent),
                % markdown:display("\n\n~ts:~ts/1:\n\nLink =\n", [?MODULE, ?FUNCTION_NAME], Link),
                markdown_tokenizer:enter_link(Tokenizer1, SpaceOrTabToken, Link);
            none ->
                markdown_tokenizer:enter(Tokenizer1, SpaceOrTabToken)
        end,
    Tokenizer3 =
        case SpaceOrTabConnect of
            true ->
                EventsIndex = markdown_vec:size(Events1) - 1,
                Events2 = markdown_subtokenizer:link(Events1, EventsIndex),
                Tokenizer2#markdown_tokenizer{events = Events2};
            false ->
                TokenizeState3 = TokenizeState2#markdown_tokenize_state{space_or_tab_connect = true},
                Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3}
        end,
    State = markdown_state:retry(space_or_tab_inside),
    {Tokenizer3, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(space_or_tab_after),
    {Tokenizer, State}.

-doc """
In `space_or_tab`.

```markdown
> | a␠␠b
      ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = #markdown_tokenize_state{space_or_tab_size = Size, space_or_tab_max = Max}
    }
) when (Current =:= $\t orelse Current =:= $\s) andalso (Max =:= infinity orelse Size < Max) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    TokenizeState2 = Tokenizer2#markdown_tokenizer.tokenize_state#markdown_tokenize_state{space_or_tab_size = Size + 1},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(space_or_tab_inside),
    {Tokenizer3, State};
inside(Tokenizer1 = #markdown_tokenizer{}) ->
    Token = Tokenizer1#markdown_tokenizer.tokenize_state#markdown_tokenize_state.space_or_tab_token,
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, Token),
    State = markdown_state:retry(space_or_tab_after),
    {Tokenizer2, State}.

-doc """
After `space_or_tab`.

```markdown
> | a␠␠b
       ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = #markdown_tokenize_state{space_or_tab_size = Size, space_or_tab_min = Min}
    }
) ->
    State =
        case Size >= Min of
            true ->
                markdown_state:ok();
            false ->
                markdown_state:nok()
        end,
    TokenizeState2 = Tokenizer1#markdown_tokenizer.tokenize_state#markdown_tokenize_state{
        space_or_tab_connect = false,
        space_or_tab_content = none,
        space_or_tab_size = 0,
        space_or_tab_max = 0,
        space_or_tab_min = 0,
        space_or_tab_token = space_or_tab
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, State}.
