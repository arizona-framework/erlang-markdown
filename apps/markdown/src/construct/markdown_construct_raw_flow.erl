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
-module(markdown_construct_raw_flow).
-moduledoc """
Raw (flow) occurs in the [flow][] content type.
It forms code (fenced) and math (flow).

## Grammar

Code (fenced) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
raw_flow ::= fence_open *( eol *byte ) [ eol fence_close ]

; Restriction: math (flow) does not support the `info` part.
fence_open ::= sequence [*space_or_tab info [1*space_or_tab meta]] *space_or_tab
; Restriction: the number of markers in the closing fence sequence must be
; equal to or greater than the number of markers in the opening fence
; sequence.
; Restriction: the marker in the closing fence sequence must match the
; marker in the opening fence sequence
fence_close ::= sequence *space_or_tab
sequence ::= 3*'`' | 3*'~' | 2*'$'
; Restriction: the marker cannot occur in `info` if it is the `$` or `` ` `` character.
info ::= 1*text
; Restriction: the marker cannot occur in `meta` if it is the `$` or `` ` `` character.
meta ::= 1*text *(*space_or_tab 1*text)
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

The above grammar does not show how indentation (with `space_or_tab`) of
each line is handled.
To parse raw (flow), let `x` be the number of `space_or_tab` characters
before the opening fence sequence.
Each line of text is then allowed (not required) to be indented with up
to `x` spaces or tabs, which are then ignored as an indent instead of being
considered as part of the content.
This indent does not affect the closing fence.
It can be indented up to a separate 3 spaces or tabs.
A bigger indent makes it part of the content instead of a fence.

The `info` and `meta` parts are interpreted as the [string][] content type.
That means that [character escapes][character_escape] and
[character references][character_reference] are allowed.
Math (flow) does not support `info`.

The optional `meta` part is ignored: it is not used when parsing or
rendering.

The optional `info` part is used and is expected to specify the programming
language that the content is in.
Which value it holds depends on what your syntax highlighter supports, if
one is used.

In markdown, it is also possible to use [raw (text)][raw_text] in the
[text][] content type.
It is also possible to create code with the
[code (indented)][code_indented] construct.

## HTML

Code (fenced) relates to both the `<pre>` and the `<code>` elements in
HTML.
See [*§ 4.4.3 The `pre` element*][html_pre] and the [*§ 4.5.15 The `code`
element*][html_code] in the HTML spec for more info.

Math (flow) does not relate to HTML elements.
`MathML`, which is sort of like SVG but for math, exists but it doesn’t work
well and isn’t widely supported.
Instead, it is recommended to use client side JavaScript with something like
`KaTeX` or `MathJax` to process the math
For that, the math is compiled as a `<pre>`, and a `<code>` element with two
classes: `language-math` and `math-display`.
Client side JavaScript can look for these classes to process them further.

The `info` is, when rendering to HTML, typically exposed as a class.
This behavior stems from the HTML spec ([*§ 4.5.15 The `code`
element*][html_code]).
For example:

```markdown
~~~css
* { color: tomato }
~~~
```

Yields:

```html
<pre><code class="language-css">* { color: tomato }
</code></pre>
```

## Recommendation

It is recommended to use code (fenced) instead of code (indented).
Code (fenced) is more explicit, similar to code (text), and has support
for specifying the programming language.

When authoring markdown with math, keep in mind that math doesn’t work in
most places.
Notably, GitHub currently has a really weird crappy client-side regex-based
thing.
But on your own (math-heavy?) site it can be great!
You can use code (fenced) with an info string of `math` to improve this, as
that works in many places.

## Tokens

*   [`CodeFenced`][Name::CodeFenced]
*   [`CodeFencedFence`][Name::CodeFencedFence]
*   [`CodeFencedFenceInfo`][Name::CodeFencedFenceInfo]
*   [`CodeFencedFenceMeta`][Name::CodeFencedFenceMeta]
*   [`CodeFencedFenceSequence`][Name::CodeFencedFenceSequence]
*   [`CodeFlowChunk`][Name::CodeFlowChunk]
*   [`LineEnding`][Name::LineEnding]
*   [`MathFlow`][Name::MathFlow]
*   [`MathFlowFence`][Name::MathFlowFence]
*   [`MathFlowFenceMeta`][Name::MathFlowFenceMeta]
*   [`MathFlowFenceSequence`][Name::MathFlowFenceSequence]
*   [`MathFlowChunk`][Name::MathFlowChunk]
*   [`SpaceOrTab`][Name::SpaceOrTab]

## References

*   [`code-fenced.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/code-fenced.js)
*   [`micromark-extension-math`](https://github.com/micromark/micromark-extension-math)
*   [*§ 4.5 Fenced code blocks* in `CommonMark`](https://spec.commonmark.org/0.31/#fenced-code-blocks)

> 👉 **Note**: math is not specified anywhere.

[flow]: crate::construct::flow
[string]: crate::construct::string
[text]: crate::construct::text
[character_escape]: crate::construct::character_escape
[character_reference]: crate::construct::character_reference
[code_indented]: crate::construct::code_indented
[raw_text]: crate::construct::raw_text
[html_code]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-code-element
[html_pre]: https://html.spec.whatwg.org/multipage/grouping-content.html#the-pre-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    start/1,
    before_sequence_open/1,
    sequence_open/1,
    info_before/1,
    info/1,
    meta_before/1,
    meta/1,
    at_non_lazy_break/1,
    close_start/1,
    before_sequence_close/1,
    sequence_close/1,
    sequence_close_after/1,
    content_before/1,
    content_start/1,
    before_content_chunk/1,
    content_chunk/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of raw.

```markdown
> | ~~~js
    ^
  | console.log(1)
  | ~~~
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{
                    code_fenced = CodeFenced, math_flow = MathFlow, code_indented = CodeIndented
                }
            }
        }
    }
) when
    (CodeFenced orelse MathFlow) andalso (Current =:= $\t orelse Current =:= $\s)
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(raw_flow_before_sequence_open),
        markdown_state:nok()
    ),
    MaxIndent =
        case CodeIndented of
            true -> ?TAB_SIZE - 1;
            false -> infinity
        end,
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
        Tokenizer2, 0, MaxIndent
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{code_fenced = CodeFenced, math_flow = MathFlow}
            }
        }
    }
) when
    (CodeFenced orelse MathFlow) andalso (Current =:= $$ orelse Current =:= $` orelse Current =:= $~)
->
    State = markdown_state:retry(raw_flow_before_sequence_open),
    {Tokenizer1, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In opening fence, after prefix, at sequence.

```markdown
> | ~~~js
    ^
  | console.log(1)
  | ~~~
```
""".
-spec before_sequence_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_sequence_open(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        events = Events,
        parse_state = #markdown_parse_state{bytes = Bytes, options = #markdown_parse_options{constructs = Constructs}},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{}
    }
) ->
    Prefix =
        case markdown_vec:last_option(Events) of
            {some, #markdown_event{name = space_or_tab}} ->
                Position = markdown_position:from_exit_event(Events, markdown_vec:size(Events) - 1),
                Slice = markdown_slice:from_position(Bytes, Position),
                markdown_slice:size(Slice);
            _ ->
                0
        end,

    %% Code (fenced).
    IsCodeFenced =
        Constructs#markdown_construct_options.code_fenced =:= true andalso
            (Current =:= {some, $`} orelse Current =:= {some, $~}),
    %% Math (flow).
    IsMathFlow = Constructs#markdown_construct_options.math_flow =:= true andalso Current =:= {some, $$},

    case IsCodeFenced orelse IsMathFlow of
        true ->
            {some, Marker} = Current,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                marker = Marker,
                size_c = Prefix
            },

            {Token1, Token2, Token3, Token4, Token5, Token6} =
                case Marker of
                    $$ ->
                        %% Math (flow) does not support an `info` part: everything after the
                        %% opening sequence is the `meta` part.
                        {math_flow, math_flow_fence, math_flow_fence_sequence,
                            TokenizeState2#markdown_tokenize_state.token_4, math_flow_fence_meta, math_flow_chunk};
                    _ ->
                        {code_fenced, code_fenced_fence, code_fenced_fence_sequence, code_fenced_fence_info,
                            code_fenced_fence_meta, code_flow_chunk}
                end,

            TokenizeState3 = TokenizeState2#markdown_tokenize_state{
                token_1 = Token1,
                token_2 = Token2,
                token_3 = Token3,
                token_4 = Token4,
                token_5 = Token5,
                token_6 = Token6
            },

            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState3},
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, Token1),
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, Token2),
            Tokenizer5 = markdown_tokenizer:enter(Tokenizer4, Token3),
            State = markdown_state:retry(raw_flow_sequence_open),
            {Tokenizer5, State};
        false ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
In opening fence sequence.

```markdown
> | ~~~js
     ^
  | console.log(1)
  | ~~~
```
""".
-spec sequence_open(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence_open(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size = Size}
    }
) ->
    case Current of
        {some, Current1} when Current1 =:= Marker ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size + 1},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
            State = markdown_state:next(raw_flow_sequence_open),
            {Tokenizer3, State};
        _ ->
            MinSize =
                case Marker of
                    $$ -> ?MATH_FLOW_SEQUENCE_SIZE_MIN;
                    _ -> ?CODE_FENCED_SEQUENCE_SIZE_MIN
                end,
            case Size < MinSize of
                true ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        marker = 0,
                        size_c = 0,
                        size = 0,
                        token_1 = data,
                        token_2 = data,
                        token_3 = data,
                        token_4 = data,
                        token_5 = data,
                        token_6 = data
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:nok(),
                    {Tokenizer2, State};
                false ->
                    %% Math (flow) does not support an `info` part: everything after the
                    %% opening sequence is the `meta` part.
                    Next =
                        case Marker of
                            $$ -> raw_flow_meta_before;
                            _ -> raw_flow_info_before
                        end,

                    case Current of
                        {some, C} when (C =:= $\t orelse C =:= $\s) ->
                            Tokenizer2 = markdown_tokenizer:exit(
                                Tokenizer1, TokenizeState1#markdown_tokenize_state.token_3
                            ),
                            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(
                                Tokenizer2
                            ),
                            Tokenizer4 = markdown_tokenizer:attempt(
                                Tokenizer3, markdown_state:next(Next), markdown_state:nok()
                            ),
                            {Tokenizer4, markdown_state:retry(SpaceOrTabState)};
                        _ ->
                            Tokenizer2 = markdown_tokenizer:exit(
                                Tokenizer1, TokenizeState1#markdown_tokenize_state.token_3
                            ),
                            State = markdown_state:retry(Next),
                            {Tokenizer2, State}
                    end
            end
    end.

-doc """
In opening fence, after the sequence (and optional whitespace), before info.

```markdown
> | ~~~js
       ^
  | console.log(1)
  | ~~~
```
""".
-spec info_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
info_before(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{}}
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, TokenizeState#markdown_tokenize_state.token_2),
            %% Do not form containers.
            Tokenizer3 = Tokenizer2#markdown_tokenizer{concrete = true},
            Tokenizer4 = markdown_tokenizer:check(
                Tokenizer3, markdown_state:next(raw_flow_at_non_lazy_break), markdown_state:next(raw_flow_after)
            ),
            State = markdown_state:retry(non_lazy_continuation_start),
            {Tokenizer4, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_4),
            Link = markdown_event_link:new(none, none, string),
            Tokenizer3 = markdown_tokenizer:enter_link(Tokenizer2, data, Link),
            State = markdown_state:retry(raw_flow_info),
            {Tokenizer3, State}
    end.

-doc """
In info.

```markdown
> | ~~~js
       ^
  | console.log(1)
  | ~~~
```
""".
-spec info(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
info(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker}
    }
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState1#markdown_tokenize_state.token_4),
            State = markdown_state:retry(raw_flow_info_before),
            {Tokenizer3, State};
        {some, C} when (C =:= $\t orelse C =:= $\s) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState1#markdown_tokenize_state.token_4),
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(raw_flow_meta_before), markdown_state:nok()
            ),
            {Tokenizer5, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer4),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer5, State};
        {some, Byte} ->
            %% This looks like code (text) / math (text).
            %% Note: no reason to check for `~`, because 3 of them can't be
            %% used as strikethrough in text.
            case Marker =:= Byte andalso (Byte =:= $$ orelse Byte =:= $`) of
                true ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        marker = 0,
                        size_c = 0,
                        size = 0,
                        token_1 = data,
                        token_2 = data,
                        token_3 = data,
                        token_4 = data,
                        token_5 = data,
                        token_6 = data
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = false, tokenize_state = TokenizeState2},
                    State = markdown_state:nok(),
                    {Tokenizer2, State};
                false ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(raw_flow_info),
                    {Tokenizer2, State}
            end
    end.

-doc """
In opening fence, after info and whitespace, before meta.

```markdown
> | ~~~js eval
          ^
  | console.log(1)
  | ~~~
```
""".
-spec meta_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
meta_before(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{}}
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            State = markdown_state:retry(raw_flow_info_before),
            {Tokenizer1, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_5),
            Link = markdown_event_link:new(none, none, string),
            Tokenizer3 = markdown_tokenizer:enter_link(Tokenizer2, data, Link),
            State = markdown_state:retry(raw_flow_meta),
            {Tokenizer3, State}
    end.

-doc """
In meta.

```markdown
> | ~~~js eval
          ^
  | console.log(1)
  | ~~~
```
""".
-spec meta(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
meta(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker}
    }
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState1#markdown_tokenize_state.token_5),
            State = markdown_state:retry(raw_flow_info_before),
            {Tokenizer3, State};
        {some, Byte} ->
            %% This looks like code (text) / math (text).
            %% Note: no reason to check for `~`, because 3 of them can't be
            %% used as strikethrough in text.
            case Marker =:= Byte andalso (Byte =:= $$ orelse Byte =:= $`) of
                true ->
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        marker = 0,
                        size_c = 0,
                        size = 0,
                        token_1 = data,
                        token_2 = data,
                        token_3 = data,
                        token_4 = data,
                        token_5 = data,
                        token_6 = data
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = false, tokenize_state = TokenizeState2},
                    State = markdown_state:nok(),
                    {Tokenizer2, State};
                false ->
                    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
                    State = markdown_state:next(raw_flow_meta),
                    {Tokenizer2, State}
            end
    end.

-doc """
At eol/eof in raw, before a non-lazy closing fence or content.

```markdown
> | ~~~js
         ^
> | console.log(1)
                  ^
  | ~~~
```
""".
-spec at_non_lazy_break(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_non_lazy_break(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(raw_flow_after), markdown_state:next(raw_flow_content_before)
    ),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, line_ending),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, line_ending),
    State = markdown_state:next(raw_flow_close_start),
    {Tokenizer5, State}.

-doc """
Before closing fence, at optional whitespace.

```markdown
  | ~~~js
  | console.log(1)
> | ~~~
    ^
```
""".
-spec close_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
close_start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = TokenizeState = #markdown_tokenize_state{},
        parse_state = #markdown_parse_state{options = #markdown_parse_options{constructs = Constructs}}
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_2),

    case Current of
        {some, C} when (C =:= $\t orelse C =:= $\s) ->
            Tokenizer3 = markdown_tokenizer:attempt(
                Tokenizer2, markdown_state:next(raw_flow_before_sequence_close), markdown_state:nok()
            ),
            MaxSize =
                case Constructs#markdown_construct_options.code_indented of
                    true -> ?TAB_SIZE - 1;
                    false -> infinity
                end,
            {Tokenizer4, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer3, 0, MaxSize
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer4, State};
        _ ->
            State = markdown_state:retry(raw_flow_before_sequence_close),
            {Tokenizer2, State}
    end.

-doc """
In closing fence, after optional whitespace, at sequence.

```markdown
  | ~~~js
  | console.log(1)
> | ~~~
    ^
```
""".
-spec before_sequence_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_sequence_close(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState = #markdown_tokenize_state{marker = Marker}
    }
) when Current =:= Marker ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_3),
    State = markdown_state:retry(raw_flow_sequence_close),
    {Tokenizer2, State};
before_sequence_close(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.

-doc """
In closing fence sequence.

```markdown
  | ~~~js
  | console.log(1)
> | ~~~
    ^
```
""".
-spec sequence_close(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence_close(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{marker = Marker, size_b = SizeB}
    }
) when Current =:= Marker ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = SizeB + 1},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(raw_flow_sequence_close),
    {Tokenizer3, State};
sequence_close(
    Tokenizer1 = #markdown_tokenizer{
        current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size_b = SizeB, size = Size}
    }
) when SizeB >= Size ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:exit(Tokenizer2, TokenizeState1#markdown_tokenize_state.token_3),
    case Current of
        {some, C} when (C =:= $\t orelse C =:= $\s) ->
            Tokenizer4 = markdown_tokenizer:attempt(
                Tokenizer3, markdown_state:next(raw_flow_after_sequence_close), markdown_state:nok()
            ),
            {Tokenizer5, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer4),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer5, State};
        _ ->
            State = markdown_state:retry(raw_flow_after_sequence_close),
            {Tokenizer3, State}
    end;
sequence_close(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_b = 0},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:nok(),
    {Tokenizer2, State}.

-doc """
After closing fence sequence, after optional whitespace.

```markdown
  | ~~~js
  | console.log(1)
> | ~~~
       ^
```
""".
-spec sequence_close_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
sequence_close_after(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{}}
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, TokenizeState#markdown_tokenize_state.token_2),
            State = markdown_state:ok(),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:nok(),
            {Tokenizer1, State}
    end.

-doc """
Before raw content, not a closing fence, at eol.

```markdown
  | ~~~js
> | console.log(1)
                  ^
  | ~~~
```
""".
-spec content_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_before(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(raw_flow_content_start),
    {Tokenizer4, State}.

-doc """
Before raw content, not a closing fence.

```markdown
  | ~~~js
> | console.log(1)
    ^
  | ~~~
```
""".
-spec content_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_start(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{size_c = SizeC}}
) ->
    case Current of
        {some, C} when (C =:= $\t orelse C =:= $\s) ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1, markdown_state:next(raw_flow_before_content_chunk), markdown_state:nok()
            ),
            {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_min_max(
                Tokenizer2, 0, SizeC
            ),
            State = markdown_state:retry(SpaceOrTabState),
            {Tokenizer3, State};
        _ ->
            State = markdown_state:retry(raw_flow_before_content_chunk),
            {Tokenizer1, State}
    end.

-doc """
Before raw content, after optional prefix.

```markdown
  | ~~~js
> | console.log(1)
    ^
  | ~~~
```
""".
-spec before_content_chunk(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_content_chunk(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{}}
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:check(
                Tokenizer1, markdown_state:next(raw_flow_at_non_lazy_break), markdown_state:next(raw_flow_after)
            ),
            State = markdown_state:retry(non_lazy_continuation_start),
            {Tokenizer2, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, TokenizeState#markdown_tokenize_state.token_6),
            State = markdown_state:retry(raw_flow_content_chunk),
            {Tokenizer2, State}
    end.

-doc """
In raw content.

```markdown
  | ~~~js
> | console.log(1)
    ^^^^^^^^^^^^^^
  | ~~~
```
""".
-spec content_chunk(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
content_chunk(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState = #markdown_tokenize_state{}}
) ->
    case Current of
        _ when ?is_option_none(Current) orelse ?is_option_some(Current, $\n) ->
            Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, TokenizeState#markdown_tokenize_state.token_6),
            State = markdown_state:retry(raw_flow_before_content_chunk),
            {Tokenizer2, State};
        _ ->
            Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
            State = markdown_state:next(raw_flow_content_chunk),
            {Tokenizer2, State}
    end.

-doc """
After raw.

```markdown
  | ~~~js
  | console.log(1)
> | ~~~
       ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, TokenizeState1#markdown_tokenize_state.token_1),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        marker = 0,
        size_c = 0,
        size = 0,
        token_1 = data,
        token_2 = data,
        token_3 = data,
        token_4 = data,
        token_5 = data,
        token_6 = data
    },
    %% Feel free to interrupt.
    %% No longer concrete.
    Tokenizer3 = Tokenizer2#markdown_tokenizer{
        tokenize_state = TokenizeState2,
        interrupt = false,
        concrete = false
    },
    State = markdown_state:ok(),
    {Tokenizer3, State}.
