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
-module(markdown_construct_partial_attribute_list).
-moduledoc """

""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    start/1,
    before/1,
    inside/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of an attribute list.

```markdown
> | a {.red}
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, C},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = Token1},
        events = Events1
    }
) when C =:= ${ ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token1),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, attribute_list_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, attribute_list_marker),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = markdown_vec:size(Events1) - 1},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(attribute_list_before),
    {Tokenizer6, State}.

-doc """
Before data.

```markdown
> | a {.red}
       ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(attribute_list_eol_after),
    {Tokenizer4, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, C}, tokenize_state = TokenizeState1}) when C =:= $} ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, attribute_list_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, attribute_list_marker),
    Tokenizer6 = markdown_tokenizer:exit(
        Tokenizer5, Tokenizer5#markdown_tokenizer.tokenize_state#markdown_tokenize_state.token_1
    ),
    State = markdown_state:ok(),
    {Tokenizer6, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, _}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, attribute_list_data),
    State = markdown_state:next(attribute_list_inside),
    {Tokenizer2, State}.

-doc """
In data.

```markdown
> | a {.red}
       ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{size = Size}}) when
    ?is_option_none(Current) orelse ?is_option_some(Current, $\n) orelse
        (?is_option_some(Current, $}) andalso Size =:= 0)
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, attribute_list_data),
    State = markdown_state:retry(attribute_list_before),
    {Tokenizer2, State};
inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{size = Size}}) when
    ?is_option_none(Current) orelse ?is_option_some(Current, $\n) orelse
        (?is_option_some(Current, $}) andalso Size =:= 0)
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(attribute_list_inside),
    {Tokenizer2, State}.
