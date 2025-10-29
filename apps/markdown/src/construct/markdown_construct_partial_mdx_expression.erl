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
-module(markdown_construct_partial_mdx_expression).
-moduledoc """
MDX expression occurs in [MDX expression (flow)][mdx_expression_flow] and
[MDX expression (text)][mdx_expression_text].

## Grammar

MDX expression forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
mdx_expression ::= '{' *(expression_text | expression) '}'
expression_text ::= char - '{' - '}'
```

## Tokens

*   [`LineEnding`][Name::LineEnding]
*   [`MdxExpressionMarker`][Name::MdxExpressionMarker]
*   [`MdxExpressionData`][Name::MdxExpressionData]

## Recommendation

When authoring markdown with JavaScript, keep in mind that MDX is a
whitespace sensitive and line-based language, while JavaScript is
insensitive to whitespace.
This affects how markdown and JavaScript interleave with eachother in MDX.
For more info on how it works, see [§ Interleaving][interleaving] on the
MDX site.

## Errors

### Unexpected end of file in expression, expected a corresponding closing brace for `{`

This error occurs if a `{` was seen without a `}`.
For example:

```markdown
a { b
```

### Unexpected lazy line in expression in container, expected line to be prefixed with `>` when in a block quote, whitespace when in a list, etc

This error occurs if a a lazy line (of a container) is found in an expression.
For example:

```markdown
> {a +
b}
```

## References

*   [`micromark-factory-mdx-expression`](https://github.com/micromark/micromark-extension-mdx-expression/blob/main/packages/micromark-factory-mdx-expression/dev/index.js)
*   [`mdxjs.com`](https://mdxjs.com)

[mdx_expression_flow]: crate::construct::mdx_expression_flow
[mdx_expression_text]: crate::construct::mdx_expression_text
[interleaving]: https://mdxjs.com/docs/what-is-mdx/#interleaving
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-28", modified => "2025-10-28"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    start/1,
    before/1,
    inside/1,
    eol_after/1,
    prefix/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of an MDX expression.
```markdown
> | a {Math.PI} c
      ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, ${}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{token_1 = Token1}
    }
) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, Token1),
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_expression_marker),
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, mdx_expression_marker),
    #markdown_tokenizer{events = Events2} = Tokenizer5,
    Start = markdown_vec:size(Events2) - 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{start = Start},
    Tokenizer6 = Tokenizer5#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:next(mdx_expression_before),
    {Tokenizer6, State}.

-doc """
Before data.
```markdown
> | a {Math.PI} c
       ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(
    Tokenizer1 = #markdown_tokenizer{
        current = none,
        point = Point,
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{mdx_last_parse_error = MdxLastParseError1}
    }
) ->
    {Problem, MdxLastParseError2} =
        case MdxLastParseError1 of
            {some, Error} ->
                {Error, none};
            none ->
                {
                    {<<"Unexpected end of file in expression, expected a corresponding closing brace for `{`">>,
                        <<"erlang-markdown">>, <<"unexpected-eof">>},
                    none
                }
        end,
    {Reason, Source, RuleId} = Problem,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{mdx_last_parse_error = MdxLastParseError2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:error(
        markdown_message:new(
            {some, markdown_place:point(markdown_point:to_unist(Point))},
            Reason,
            RuleId,
            Source
        )
    ),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(mdx_expression_eol_after),
    {Tokenizer4, State};
before(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{mdx_expression_parse = OptionMdxExpressionParse}
        },
        tokenize_state = #markdown_tokenize_state{size = 0, token_1 = Token1}
    }
) when Current =:= $} ->
    {Tokenizer2, State1} =
        case OptionMdxExpressionParse of
            {some, MdxExpressionParse} ->
                parse_expression(Tokenizer1, MdxExpressionParse);
            none ->
                State = markdown_state:ok(),
                {Tokenizer1, State}
        end,
    case State1 of
        ok ->
            TokenizeState2 = Tokenizer2#markdown_tokenizer.tokenize_state,
            TokenizeState3 = TokenizeState2#markdown_tokenize_state{start = 0},
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3},
            Tokenizer4 = markdown_tokenizer:enter(Tokenizer3, mdx_expression_marker),
            Tokenizer5 = markdown_tokenizer:consume(Tokenizer4),
            Tokenizer6 = markdown_tokenizer:exit(Tokenizer5, mdx_expression_marker),
            Tokenizer7 = markdown_tokenizer:exit(Tokenizer6, Token1),
            {Tokenizer7, State1};
        _ ->
            {Tokenizer2, State1}
    end;
before(Tokenizer1 = #markdown_tokenizer{current = {some, _}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, mdx_expression_data),
    State = markdown_state:retry(mdx_expression_inside),
    {Tokenizer2, State}.

-doc """
In data.
```markdown
> | a {Math.PI} c
       ^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = #markdown_tokenize_state{size = Size}}) when
    (?is_option_none(Current) orelse ?is_option_some(Current, $\n)) orelse
        (?is_option_some(Current, $}) andalso Size =:= 0)
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, mdx_expression_data),
    State = markdown_state:retry(mdx_expression_before),
    {Tokenizer2, State};
inside(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{mdx_expression_parse = MdxExpressionParse}
        },
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{size = Size1}
    }
) ->
    %% Don't count if gnostic.
    Tokenizer2 =
        case Current of
            ${ when ?is_option_none(MdxExpressionParse) ->
                TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size1 + 1},
                Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2};
            $} ->
                TokenizeState2 = TokenizeState1#markdown_tokenize_state{size = Size1 - 1},
                Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2};
            _ ->
                Tokenizer1
        end,
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(mdx_expression_inside),
    {Tokenizer3, State}.

-doc """
After eol.
```markdown
  | a {b +
> | c} d
    ^
```
""".
-spec eol_after(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
eol_after(
    Tokenizer1 = #markdown_tokenizer{
        lazy = true, point = Point, tokenize_state = #markdown_tokenize_state{token_1 = Token1, token_2 = Token2}
    }
) when Token1 =:= mdx_flow_expression orelse Token2 =:= mdx_jsx_flow_tag ->
    %% Lazy continuation in a flow expression (or flow tag) is a syntax error.
    State = markdown_state:error(
        markdown_message:new(
            {some, markdown_place:point(markdown_point:to_unist(Point))},
            <<"Unexpected lazy line in expression in container, expected line to be prefixed with `>` when in a block quote, whitespace when in a list, etc">>,
            <<"unexpected-lazy">>,
            <<"erlang-markdown">>
        )
    ),
    {Tokenizer1, State};
eol_after(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    %% Idea: investigate if we'd need to use more complex stripping.
    %% Take this example:
    %%
    %% ```markdown
    %% >  aaa <b c={`
    %% >      d
    %% >  `} /> eee
    %% ```
    %%
    %% Currently, the "paragraph" starts at `> | aaa`, so for the next line
    %% here we split it into `>␠|␠␠|␠␠␠d` (prefix, this indent here,
    %% expression data).
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_prefix),
    State = markdown_state:retry(mdx_expression_prefix),
    {Tokenizer2, State};
eol_after(Tokenizer1 = #markdown_tokenizer{}) ->
    State = markdown_state:retry(mdx_expression_before),
    {Tokenizer1, State}.

-doc """
""".
-spec prefix(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
prefix(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current}, tokenize_state = TokenizeState1 = #markdown_tokenize_state{size_c = SizeC1}
    }
) when (Current =:= $\t orelse Current =:= $\s) andalso SizeC1 < 2 ->
    %% Tab-size to eat has to be the same as what we serialize as.
    %% While in some places in markdown that's 4, in JS it's more common as 2.
    %% Which is what's also in `mdast-util-mdx-jsx`:
    %% <https://github.com/syntax-tree/mdast-util-mdx-jsx/blob/40b951b/lib/index.js#L52>
    %% <https://github.com/micromark/micromark-extension-mdx-expression/blob/7c305ff/packages/micromark-factory-mdx-expression/dev/index.js#L37>
    SizeC2 = SizeC1 + 1,
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_c = SizeC2},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    State = markdown_state:next(mdx_expression_prefix),
    {Tokenizer3, State};
prefix(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, line_prefix),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{size_c = 0},
    Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(mdx_expression_before),
    {Tokenizer3, State}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Parse an expression with a given function.
""".
-spec parse_expression(Tokenizer, Parse) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), Parse :: markdown_mdx:expression_parse(), State :: markdown_state:t().
parse_expression(
    Tokenizer1 = #markdown_tokenizer{
        events = Events,
        parse_state = #markdown_parse_state{bytes = Bytes, location = Location},
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{start = Start, token_1 = Token1}
    },
    Parse
) when is_function(Parse, 2) ->
    %% Collect the body of the expression and positional info for each run of it.
    Result = markdown_mdx_collect:collect(Events, Bytes, Start, [mdx_expression_data, line_ending], []),
    %% Turn the name of the expression into a kind.
    Kind =
        case Token1 of
            mdx_flow_expression ->
                expression;
            mdx_text_expression ->
                expression;
            mdx_jsx_tag_attribute_expression ->
                attribute_expression;
            mdx_jsx_tag_attribute_value_expression ->
                attribute_value_expression;
            _ ->
                ?'unreachable!'("cannot handle unknown expression name: ~0tp", [Token1])
        end,
    %% Parse and handle what was signaled back.
    #markdown_mdx_collect_result{value = Value, stops = Stops} = Result,
    case Parse(Value, Kind) of
        #markdown_mdx_signal{inner = ok} ->
            State = markdown_state:ok(),
            {Tokenizer1, State};
        #markdown_mdx_signal{inner = {error, Reason, Relative, Source, RuleId}} ->
            Point =
                case Location of
                    {some, Loc} ->
                        case markdown_location:relative_to_point(Loc, markdown_vec:to_list(Stops), Relative) of
                            {some, P} ->
                                P;
                            none ->
                                markdown_point:to_unist(Tokenizer1#markdown_tokenizer.point)
                        end;
                    none ->
                        markdown_point:to_unist(Tokenizer1#markdown_tokenizer.point)
                end,
            State = markdown_state:error(
                markdown_message:new(
                    {some, markdown_place:point(Point)},
                    Reason,
                    RuleId,
                    Source
                )
            ),
            {Tokenizer1, State};
        #markdown_mdx_signal{inner = {eof, Reason, Source, RuleId}} ->
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                mdx_last_parse_error = {some, {Reason, Source, RuleId}}
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
            Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, mdx_expression_data),
            Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
            State = markdown_state:next(mdx_expression_inside),
            {Tokenizer4, State}
    end.
