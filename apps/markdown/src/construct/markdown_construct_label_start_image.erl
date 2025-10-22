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
-module(markdown_construct_label_start_image).
-moduledoc """
Label start (image) occurs in the [text][] content type.

## Grammar

Label start (image) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
label_start_image ::= '!' '['
```

## HTML

Label start (image) does not, on its own, relate to anything in HTML.
When matched with a [label end][label_end], they together relate to the
`<img>` element in HTML.
See [*§ 4.8.3 The `img` element*][html_img] in the HTML spec for more info.
Without an end, the characters (`![`) are output.

## Tokens

*   [`LabelImage`][Name::LabelImage]
*   [`LabelImageMarker`][Name::LabelImageMarker]
*   [`LabelMarker`][Name::LabelMarker]

## References

*   [`label-start-image.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/label-start-image.js)
*   [*§ 6.4 Images* in `CommonMark`](https://spec.commonmark.org/0.31/#images)

[text]: crate::construct::text
[label_end]: crate::construct::label_end
[html_img]: https://html.spec.whatwg.org/multipage/embedded-content.html#the-img-element
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    open/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of label (image) start.

```markdown
> | a ![b] c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $!},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{label_start_image = true}}
        }
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, label_image),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, label_image_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, label_image_marker),
    State = markdown_state:next(label_start_image_open),
    {Tokenizer5, State};
start(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `!`, at `[`.

```markdown
> | a ![b] c
       ^
```
""".
-spec open(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
open(Tokenizer1 = #markdown_tokenizer{current = {some, $[}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, label_marker),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, label_marker),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, label_image),
    State = markdown_state:next(label_start_image_after),
    {Tokenizer5, State};
open(Tokenizer) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
After `![`.

```markdown
> | a ![b] c
        ^
```

This is needed in because, when GFM footnotes are enabled, images never
form when started with a `^`.
Instead, links form:

```markdown
![^a](b)

![^a][b]

[b]: c
```

```html
<p>!<a href=\"b\">^a</a></p>
<p>!<a href=\"c\">^a</a></p>
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $^},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{constructs = #markdown_construct_options{gfm_label_start_footnote = true}}
        }
    }
) ->
    State = markdown_state:nok(),
    {Tokenizer1, State};
'after'(
    Tokenizer1 = #markdown_tokenizer{
        events = Events1, tokenize_state = TokenizeState1 = #markdown_tokenize_state{label_starts = LabelStarts1}
    }
) ->
    EventsSize = markdown_vec:size(Events1),
    LabelStart = markdown_label_start:new(image, markdown_indices:new(EventsSize - 6, EventsSize - 1), false),
    LabelStarts2 = markdown_vec:push(LabelStarts1, LabelStart),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{label_starts = LabelStarts2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:register_resolver_before(Tokenizer2, label),
    State = markdown_state:ok(),
    {Tokenizer3, State}.
