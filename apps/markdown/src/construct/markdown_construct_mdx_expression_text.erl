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
-module(markdown_construct_mdx_expression_text).
-moduledoc """
MDX expression (text) occurs in the [text][] content type.

## Grammar

MDX expression (text) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_expression_text ::= mdx_expression

; See the `partial_mdx_expression` construct for the BNF of that part.
```

See [`mdx_expression`][mdx_expression] for more info.

## Tokens

*   [`MdxTextExpression`][Name::MdxTextExpression]
*   see [`mdx_expression`][mdx_expression] for more

## Recommendation

See [`mdx_expression`][mdx_expression] for recommendations.

## References

*   [`syntax.js` in `micromark-extension-mdx-expression`](https://github.com/micromark/micromark-extension-mdx-expression/blob/main/packages/micromark-extension-mdx-expression/dev/lib/syntax.js)
*   [`mdxjs.com`](https://mdxjs.com)

[text]: crate::construct::text
[mdx_expression]: crate::construct::partial_mdx_expression
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of an MDX expression (text).

```markdown
> | a {Math.PI} c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, C},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{mdx_expression_text = true}}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when C =:= ${ ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = mdx_text_expression},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(mdx_expression_text_after), markdown_state:nok()
    ),
    State2 = markdown_state:retry(mdx_expression_start),
    {Tokenizer3, State2};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After expression.

```markdown
> | a {Math.PI} c
               ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:ok(),
    {Tokenizer2, State}.
