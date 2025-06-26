%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  08 Apr 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_construct_mdx_jsx_text).
-moduledoc """
MDX JSX (text) occurs in the [text][] content type.

## Grammar

MDX JSX (text) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_jsx_text ::= mdx_jsx

; See the `partial_mdx_jsx` construct for the BNF of that part.
```

See [`mdx_jsx`][mdx_jsx] for more info.

## Tokens

*   [`MdxJsxTextTag`][Name::MdxJsxTextTag]
*   see [`mdx_jsx`][mdx_jsx] for more

## Recommendation

See [`mdx_jsx`][mdx_jsx] for recommendations.

## References

*   [`jsx-text.js` in `micromark-extension-mdx-jsx`](https://github.com/micromark/micromark-extension-mdx-jsx/blob/main/dev/lib/jsx-text.js)
*   [`mdxjs.com`](https://mdxjs.com)

[text]: crate::construct::text
[mdx_jsx]: crate::construct::partial_mdx_jsx
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    'after'/1,
    nok/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of MDX: JSX (text).

```markdown
> | a <B /> c
     ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $<},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{mdx_jsx_text = true}}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = mdx_jsx_text_tag},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(mdx_jsx_text_after),
        markdown_state:next(mdx_jsx_text_nok)
    ),
    State = markdown_state:retry(mdx_jsx_start),
    {Tokenizer3, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After an MDX JSX (text) tag.

```markdown
> | a <b> c
        ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:ok(),
    {Tokenizer2, State}.

-doc """
At something that wasn't an MDX JSX (text) tag.

```markdown
> | a < b
     ^
```
""".
-spec nok(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
nok(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = data},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.
