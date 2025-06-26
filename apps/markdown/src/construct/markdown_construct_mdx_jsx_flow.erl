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
-module(markdown_construct_mdx_jsx_flow).
-moduledoc """
MDX JSX (flow) occurs in the [flow][] content type.

## Grammar

MDX JSX (flow) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_jsx_flow ::= mdx_jsx *space_or_tab [mdx_jsx *space_or_tab]

; See the `partial_mdx_jsx` construct for the BNF of that part.
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).
It is allowed to use multiple tags after each other, optionally with only
whitespace between them.

See [`mdx_jsx`][mdx_jsx] for more info.

## Tokens

*   [`MdxJsxFlowTag`][Name::MdxJsxFlowTag]
*   [`SpaceOrTab`][Name::SpaceOrTab]
*   see [`mdx_jsx`][mdx_jsx] for more

## Recommendation

See [`mdx_jsx`][mdx_jsx] for recommendations.

## References

*   [`jsx-flow.js` in `micromark-extension-mdx-jsx`](https://github.com/micromark/micromark-extension-mdx-jsx/blob/main/dev/lib/jsx-flow.js)
*   [`mdxjs.com`](https://mdxjs.com)

[flow]: crate::construct::flow
[mdx_jsx]: crate::construct::partial_mdx_jsx
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of MDX: JSX (flow).

```markdown
> | <A />
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{mdx_jsx_flow = true, code_indented = CodeIndented}
            }
        }
    }
) ->
    TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{token_1 = mdx_jsx_flow_tag},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2, concrete = true},
    case Current of
        C when C =:= $\t orelse C =:= $\s ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2,
                markdown_state:next(mdx_jsx_flow_before),
                markdown_state:nok()
            ),
            MaxIndent =
                case CodeIndented of
                    true -> ?TAB_SIZE - 1;
                    false -> infinity
                end,
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxIndent
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(mdx_jsx_flow_before),
            {Tokenizer2, State}
    end;
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
