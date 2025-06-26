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
-module(markdown_construct_mdx_esm).
-moduledoc """
MDX ESM occurs in the [flow][] content type.

## Grammar

MDX expression (flow) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_esm ::= word *line *(eol *line)

word ::= 'e' 'x' 'p' 'o' 'r' 't' | 'i' 'm' 'p' 'o' 'r' 't'
```

This construct must be followed by a blank line or eof (end of file).
It can include blank lines if [`MdxEsmParse`][crate::MdxEsmParse] passed in
[`ParseOptions`][parse_options] allows it.

## Tokens

*   [`LineEnding`][Name::LineEnding]
*   [`MdxEsm`][Name::MdxEsm]
*   [`MdxEsmData`][Name::MdxEsmData]

## References

*   [`syntax.js` in `micromark-extension-mdxjs-esm`](https://github.com/micromark/micromark-extension-mdxjs-esm/blob/main/dev/lib/syntax.js)
*   [`mdxjs.com`](https://mdxjs.com)

[flow]: crate::construct::flow
[parse_options]: crate::ParseOptions
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of MDX ESM.

```markdown
> | import a from 'b'
    ^
\```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        interrupt = false,
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{mdx_esm = true}, mdx_esm_parse = {some, _}
            }
        },
        point = #markdown_point{column = 1},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) when Current =:= $e orelse Current =:= $i ->
    %% If it's turned on.
    %% If there is a gnostic parser.
    %% When not interrupting.
    %% Only at the start of a line, not at whitespace or in a container.

    %% Place where keyword starts.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        start = Tokenizer1#markdown_tokenizer.point#markdown_point.offset
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_esm),
    Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, mdx_esm_data),
    Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
    State = markdown_state:next(mdx_esm_word),
    {Tokenizer5, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
