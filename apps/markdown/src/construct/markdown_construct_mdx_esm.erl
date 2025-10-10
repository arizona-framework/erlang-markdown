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
-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    start/1,
    word/1,
    inside/1,
    line_start/1,
    continuation_start/1,
    blank_line_before/1,
    at_end/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of MDX ESM.

```markdown
> | import a from 'b'
    ^
```
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

-doc """
In keyword.

```markdown
> | import a from 'b'
    ^^^^^^
```
""".
-spec word(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
word(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current >= $a andalso Current =< $z ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(mdx_esm_word),
    {Tokenizer2, State};
word(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        parse_state = #markdown_parse_state{bytes = Bytes},
        point = #markdown_point{offset = Index},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{start = Start}
    }
) ->
    Slice = markdown_slice:from_indices(Bytes, Start, Index),
    SliceBytes = markdown_slice:as_binary(Slice),
    case (SliceBytes =:= <<"export">> orelse SliceBytes =:= <<"import">>) andalso Current =:= {some, $\s} of
        true ->
            Tokenizer2 = Tokenizer1#markdown_tokenizer{concrete = true},
            EventsLen = markdown_vec:size(Tokenizer2#markdown_tokenizer.events),
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = EventsLen - 1},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            State = markdown_state:next(mdx_esm_inside),
            {Tokenizer4, State};
        false ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = 0},
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            State = markdown_state:nok(),
            {Tokenizer2, State}
    end.

-doc """
In data.

```markdown
> | import a from 'b'
          ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = Current}) when Current =:= none orelse Current =:= {some, $\n} ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_esm_data),
    State = markdown_state:retry(mdx_esm_line_start),
    {Tokenizer2, State};
inside(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(mdx_esm_inside),
    {Tokenizer2, State}.

-doc """
At start of line.

```markdown
  | import a from 'b'
> | export {a}
    ^
```
""".
-spec line_start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
line_start(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    State = markdown_state:retry(mdx_esm_at_end),
    {Tokenizer1, State};
line_start(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    OkState = markdown_state:next(mdx_esm_at_end),
    NokState = markdown_state:next(mdx_esm_continuation_start),
    Tokenizer2 = markdown_tokenizer:check(Tokenizer1, OkState, NokState),
    State = markdown_state:retry(mdx_esm_blank_line_before),
    {Tokenizer2, State};
line_start(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_esm_data),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(mdx_esm_inside),
    {Tokenizer3, State}.

-doc """
At start of line that continues.

```markdown
  | import a from 'b'
> | export {a}
    ^
```
""".
-spec continuation_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
continuation_start(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(mdx_esm_line_start),
    {Tokenizer4, State}.

-doc """
At start of a potentially blank line.

```markdown
  | import a from 'b'
> | export {a}
    ^
```
""".
-spec blank_line_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
blank_line_before(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(blank_line_start),
    {Tokenizer4, State}.

-doc """
At end of line (blank or eof).

```markdown
> | import a from 'b'
                     ^
```
""".
-spec at_end(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_end(Tokenizer1 = #markdown_tokenizer{}) ->
    {Tokenizer2, Result} = parse_esm(Tokenizer1),
    case Result of
        ok ->
            Tokenizer3 = Tokenizer2#markdown_tokenizer{concrete = false},
            Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, mdx_esm),
            State = markdown_state:ok(),
            {Tokenizer4, State};
        _ ->
            {Tokenizer2, Result}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Parse ESM with a given function.
""".
-spec parse_esm(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
parse_esm(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        events = Events,
        parse_state = #markdown_parse_state{
            bytes = Bytes, location = OptionLocation, options = #markdown_parse_options{mdx_esm_parse = {some, Parse}}
        },
        point = Point,
        tokenize_state = #markdown_tokenize_state{start = Start}
    }
) when is_function(Parse, 1) ->
    %% Collect the body of the ESM and positional info for each run of it.
    CollectResult = markdown_mdx_collect:collect(Events, Bytes, Start, [mdx_esm_data, line_ending], []),
    CollectValue = CollectResult#markdown_mdx_collect_result.value,
    CollectStops = CollectResult#markdown_mdx_collect_result.stops,
    %% Parse and handle what was signaled back.
    case Parse(CollectValue) of
        #markdown_mdx_signal{inner = ok} ->
            State = markdown_state:ok(),
            {Tokenizer1, State};
        #markdown_mdx_signal{inner = {error, Message, Relative, Source, RuleId}} ->
            %% BEGIN: assertions
            ?assertMatch({some, _}, OptionLocation, "expected location index if aware mdx is on"),
            %% END: assertions
            {some, Location} = OptionLocation,
            OptionRelativePoint = markdown_location:relative_to_point(Location, CollectStops, Relative),
            %% BEGIN: assertions
            ?assertMatch({some, _}, OptionRelativePoint, "expected non-empty string"),
            %% END: assertions
            {some, RelativePoint} = OptionRelativePoint,
            Place = markdown_place:point(RelativePoint),
            State = markdown_state:error(markdown_message:new({some, Place}, Message, Source, RuleId)),
            {Tokenizer1, State};
        #markdown_mdx_signal{inner = {eof, Message, Source, RuleId}} ->
            case Current of
                none ->
                    Place = markdown_place:point(markdown_point:to_unist(Point)),
                    State = markdown_state:error(markdown_message:new({some, Place}, Message, Source, RuleId)),
                    {Tokenizer1, State};
                {some, _} ->
                    TokenizeState1 = Tokenizer1#markdown_tokenizer.tokenize_state,
                    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                        mdx_last_parse_error = {some, {Message, Source, RuleId}}
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
                    State = markdown_state:retry(mdx_esm_continuation_start),
                    {Tokenizer2, State}
            end
    end.
