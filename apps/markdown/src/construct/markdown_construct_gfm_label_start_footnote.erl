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
-module(markdown_construct_gfm_label_start_footnote).
-moduledoc """
Label start (footnote) occurs in the [text][] content type.

## Grammar

Label start (footnote) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
gfm_label_start_footnote ::= '[' '^'
```

## HTML

Label start (footnote) does not, on its own, relate to anything in HTML.
When matched with a [label end][label_end], they together relate to `<sup>`
and `<a>` elements in HTML.
See [*Â§ 4.5.19 The `sub` and `sup` elements*][html_sup] and
[*Â§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.
Without an end, the characters (`[^`) are output.

## Tokens

*   [`GfmFootnoteCallLabel`][Name::GfmFootnoteCallLabel]
*   [`GfmFootnoteCallMarker`][Name::GfmFootnoteCallMarker]
*   [`LabelMarker`][Name::LabelMarker]

## References

*   [`micromark-extension-gfm-footnote`](https://github.com/micromark/micromark-extension-gfm-footnote)

> ð **Note**: Footnotes are not specified in GFM yet.
> See [`github/cmark-gfm#270`](https://github.com/github/cmark-gfm/issues/270)
> for the related issue.

[text]: crate::construct::text
[label_end]: crate::construct::label_end
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
[html_sup]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-sub-and-sup-elements
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    open/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of label (footnote) start.

```markdown
> | a [^b] c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $[},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{gfm_label_start_footnote = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_footnote_call_label),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, label_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, label_marker),
    State = markdown_state:next(gfm_label_start_footnote_open),
    {Tokenizer5, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `[`, at `^`.

```markdown
> | a [^b] c
       ^
```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = {some, $^}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, gfm_footnote_call_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, gfm_footnote_call_marker),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, gfm_footnote_call_label),
    Events5Length = markdown_vec:size(Tokenizer5#markdown_tokenizer.events),
    LabelStart = markdown_label_start:new(
        gfm_footnote, markdown_indices:new(Events5Length - 6, Events5Length - 1), false
    ),
    TokenizeState5 = Tokenizer4#markdown_tokenizer.tokenize_state,
    LabelStarts5 = TokenizeState5#markdown_tokenize_state.label_starts,
    LabelStarts6 = markdown_vec:push(LabelStarts5, LabelStart),
    TokenizeState6 = TokenizeState5#markdown_tokenize_state{label_starts = LabelStarts6},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState6},
    Tokenizer7 = markdown_tokenizer:register_resolver_before(Tokenizer6, label),
    State = markdown_state:ok(),
    {Tokenizer7, State};
open(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
