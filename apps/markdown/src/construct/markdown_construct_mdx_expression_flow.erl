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
-module(markdown_construct_mdx_expression_flow).
-moduledoc """
MDX expression (flow) occurs in the [flow][] content type.

## Grammar

MDX expression (flow) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_expression_flow ::= mdx_expression *space_or_tab

; See the `partial_mdx_expression` construct for the BNF of that part.
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

See [`mdx_expression`][mdx_expression] for more info.

## Tokens

*   [`MdxFlowExpression`][Name::MdxFlowExpression]
*   [`SpaceOrTab`][Name::SpaceOrTab]
*   see [`mdx_expression`][mdx_expression] for more

## Recommendation

See [`mdx_expression`][mdx_expression] for recommendations.

## References

*   [`syntax.js` in `micromark-extension-mdx-expression`](https://github.com/micromark/micromark-extension-mdx-expression/blob/main/packages/micromark-extension-mdx-expression/dev/lib/syntax.js)
*   [`mdxjs.com`](https://mdxjs.com)

[flow]: crate::construct::flow
[mdx_expression]: crate::construct::partial_mdx_expression
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
    before/1,
    'after'/1,
    'end'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of an MDX expression (flow).

```markdown
> | {Math.PI}
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{mdx_expression_flow = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    TokenizeState2 = (Tokenizer1#markdown_tokenizer.tokenize_state)#markdown_tokenize_state{
        token_1 = mdx_flow_expression
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    case Current of
        C when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(mdx_expression_flow_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true ->
                        ?TAB_SIZE - 1;
                    false ->
                        infinity
                end,
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxIndent
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(mdx_expression_flow_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After optional whitespace, before expression.

```markdown
> | {Math.PI}
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= ${ ->
    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = true},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(mdx_expression_flow_after),
        markdown_state:nok()
    ),
    State = markdown_state:retry(mdx_expression_start),
    {Tokenizer3, State};
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After expression.

```markdown
> | {Math.PI}
             ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    OkState = markdown_state:next(mdx_expression_flow_end),
    NokState = markdown_state:nok(),
    Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, OkState, NokState),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
'after'(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:retry(mdx_expression_flow_end),
    {Tokenizer1, State}.

-doc """
After expression, after optional whitespace.

```markdown
> | {Math.PI}␠␊
              ^
```
""".
-spec 'end'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'end'(Tokenizer1 = #markdown_tokenizer{current = Current}) when Current =:= none orelse Current =:= {some, $\n} ->
    Tokenizer2 = reset(Tokenizer1),
    State = markdown_state:ok(),
    {Tokenizer2, State};
'end'(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $<},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{mdx_jsx_flow = true}}
        },
        tokenize_state = TokenizeState1
    }
) ->
    %% Tag.
    %% We can't just say: fine.
    %% Lines of blocks have to be parsed until an eol/eof.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = mdx_jsx_flow_tag},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    OkState = markdown_state:next(mdx_jsx_flow_after),
    NokState = markdown_state:next(mdx_jsx_flow_nok),
    Tokenizer3 = markdown_tokenizer:attempt(Tokenizer2, OkState, NokState),
    State = markdown_state:retry(mdx_jsx_start),
    {Tokenizer3, State};
'end'(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = reset(Tokenizer1),
    State = markdown_state:nok(),
    {Tokenizer2, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Reset state.
""".
-spec reset(Tokenizer) -> Tokenizer when Tokenizer :: markdown_tokenizer:t().
reset(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = false, tokenize_state = TokenizeState2},
    Tokenizer2.
