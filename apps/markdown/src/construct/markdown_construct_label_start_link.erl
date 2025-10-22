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
-module(markdown_construct_label_start_link).
-moduledoc """
Label start (link) occurs in the [text][] content type.

## Grammar

Label start (link) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
label_start_link ::= '['
```

## HTML

Label start (link) does not, on its own, relate to anything in HTML.
When matched with a [label end][label_end], they together relate to the
`<a>` element in HTML.
See [*§ 4.5.1 The `a` element*][html_a] in the HTML spec for more info.
Without an end, the character (`[`) is output.

## Tokens

*   [`LabelLink`][Name::LabelLink]
*   [`LabelMarker`][Name::LabelMarker]

## References

*   [`label-start-link.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/label-start-link.js)
*   [*§ 6.3 Links* in `CommonMark`](https://spec.commonmark.org/0.31/#links)

[text]: crate::construct::text
[label_end]: crate::construct::label_end
[html_a]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-a-element
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of label (link) start.

```markdown
> | a [b] c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{label_start_link = true}}
        },
        current = {some, $[},
        events = Events,
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{label_starts = LabelStarts1}
    }
) ->
    Start = markdown_vec:size(Events),
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, label_link),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, label_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, label_marker),
    Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, label_link),
    LabelStart = markdown_label_start:new(
        link, markdown_indices:new(Start, markdown_vec:size(Tokenizer6#markdown_tokenizer.events) - 1), false
    ),
    LabelStarts2 = markdown_vec:push(LabelStarts1, LabelStart),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{label_starts = LabelStarts2},
    Tokenizer7 = Tokenizer6#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer8 = markdown_tokenizer:register_resolver_before(Tokenizer7, label),
    State = markdown_state:ok(),
    {Tokenizer8, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
