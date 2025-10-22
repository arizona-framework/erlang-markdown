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
-module(markdown_construct_content).
-moduledoc """
Content occurs in the [flow][] content type.

Content contains zero or more [definition][definition]s, followed by zero
or one [paragraph][].

The constructs found in flow are:

*   [Definition][crate::construct::definition]
*   [Paragraph][crate::construct::paragraph]

## Tokens

*   [`Content`][Name::Content]

> 👉 **Note**: while parsing, [`Content`][Name::Content]
> is used, which is later compiled away.

## References

*   [`content.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/content.js)

[flow]: crate::construct::flow
[definition]: crate::construct::definition
[paragraph]: crate::construct::paragraph
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    chunk_start/1,
    chunk_inside/1,
    definition_before/1,
    definition_after/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Before a content chunk.

```markdown
> | abc
    ^
```
""".
-spec chunk_start(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
chunk_start(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =/= none andalso Current =/= {some, $\n}
->
    Link = markdown_event_link:new(none, none, content),
    Tokenizer2 = markdown_tokenizer:enter_link(Tokenizer1, content, Link),
    State = markdown_state:retry(content_chunk_inside),
    {Tokenizer2, State}.

-doc """
In a content chunk.

```markdown
> | abc
    ^^^
```
""".
-spec chunk_inside(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
chunk_inside(Tokenizer1 = #markdown_tokenizer{current = Current}) when
    Current =:= none orelse Current =:= {some, $\n}
->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, content),
    Tokenizer3 = markdown_tokenizer:register_resolver_before(Tokenizer2, content),
    %% You'd be interrupting.
    Tokenizer4 = Tokenizer3#markdown_tokenizer{interrupt = true},
    State = markdown_state:ok(),
    {Tokenizer4, State};
chunk_inside(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    State = markdown_state:next(content_chunk_inside),
    {Tokenizer2, State}.

-doc """
Before a definition.

```markdown
> | [a]: b
    ^
```
""".
-spec definition_before(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
definition_before(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(content_definition_after),
        markdown_state:next(paragraph_start)
    ),
    State = markdown_state:retry(definition_start),
    {Tokenizer2, State}.

-doc """
After a definition.

```markdown
> | [a]: b
          ^
  | c
```
""".
-spec definition_after(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
definition_after(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    State = markdown_state:ok(),
    {Tokenizer1, State};
definition_after(Tokenizer1 = #markdown_tokenizer{current = {some, $\n}}) ->
    Tokenizer2 = markdown_tokenizer:enter(Tokenizer1, line_ending),
    Tokenizer3 = markdown_tokenizer:consume(Tokenizer2),
    Tokenizer4 = markdown_tokenizer:exit(Tokenizer3, line_ending),
    State = markdown_state:next(content_definition_before),
    {Tokenizer4, State}.

-doc """
Merge `Content` chunks, which currently span a single line, into actual
`Content`s that span multiple lines.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{events = Events1, map = Map1, parse_state = ParseState}) ->
    % io:format("\n\n[~w] BEFORE content:resolve_loop\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events1), markdown_debug:rust_debug_string(Map1)]),
    {Events2, Map2, _Index} = resolve_loop(Events1, Map1, 0),
    % io:format("\n\n[~w] AFTER content:resolve_loop\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events2), markdown_debug:rust_debug_string(Map2)]),
    % io:format("\n\n[~w] BEFORE content:resolve_consume\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events2), markdown_debug:rust_debug_string(Map2)]),
    {Map3, Events3} = markdown_edit_map:consume(Map2, Events2),
    % io:format("\n\n[~w] AFTER content:resolve_consume\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events3), markdown_debug:rust_debug_string(Map3)]),
    % io:format("\n\n[~w:~w] BEFORE content:resolve_subtokenize\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Events3)]),
    {Events4, Result} = markdown_subtokenizer:subtokenize(Events3, ParseState, {some, content}),
    % io:format("\n\n[~w:~w] AFTER content:resolve_subtokenize\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Events4)]),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events4, map = Map3},
    case Result of
        {ok, Subresult} ->
            {Tokenizer2, {ok, {some, Subresult}}};
        {error, Message} ->
            {Tokenizer2, {error, Message}}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec resolve_loop(Events, Map, Index) -> {Events, Map, Index} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Map :: markdown_edit_map:t(),
    Index :: non_neg_integer().
resolve_loop(Events, Map, Index) when Index >= ?markdown_vec_size(Events) ->
    {Events, Map, Index};
resolve_loop(Events1, Map1, Index1) ->
    case markdown_vec:get(Events1, Index1) of
        #markdown_event{kind = 'enter', name = 'content'} ->
            ExitIndex1 = Index1 + 1,
            {Events2, Map2, ExitIndex2} = resolve_content_loop(Events1, Map1, ExitIndex1),
            Index2 = ExitIndex2 + 1,
            resolve_loop(Events2, Map2, Index2);
        _ ->
            Index2 = Index1 + 1,
            resolve_loop(Events1, Map1, Index2)
    end.

%% @private
-spec resolve_content_loop(Events, Map, ExitIndex) -> {Events, Map, ExitIndex} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Map :: markdown_edit_map:t(),
    ExitIndex :: non_neg_integer().
resolve_content_loop(Events1, Map1, ExitIndex1) ->
    EnterIndex1 = ExitIndex1 + 1,
    case
        EnterIndex1 =:= markdown_vec:size(Events1) orelse
            (markdown_vec:get(Events1, EnterIndex1))#markdown_event.name =/= line_ending
    of
        true ->
            {Events1, Map1, ExitIndex1};
        false ->
            %% Skip past line ending.
            EnterIndex2 = EnterIndex1 + 2,
            %% Skip past prefix.
            EnterIndex3 = skip_prefixes(Events1, EnterIndex2),
            case
                EnterIndex3 =:= markdown_vec:size(Events1) orelse
                    (markdown_vec:get(Events1, EnterIndex3))#markdown_event.name =/= content
            of
                true ->
                    {Events1, Map1, ExitIndex1};
                false ->
                    %% Set Exit:Content point to Exit:LineEnding.
                    #markdown_event{point = LineEndingPoint} = markdown_vec:get(Events1, ExitIndex1 + 2),
                    Events2 = markdown_vec:update(Events1, ExitIndex1, fun(ExitEvent) ->
                        ExitEvent#markdown_event{point = LineEndingPoint}
                    end),
                    %% Remove Enter:LineEnding, Exit:LineEnding.
                    Map2 = markdown_edit_map:add(Map1, ExitIndex1 + 1, 2, markdown_vec:new()),
                    %% Link Enter:Content to Enter:Content on this line and vice versa.
                    Events3 = markdown_vec:update(
                        Events2,
                        ExitIndex1 - 1,
                        fun(PrevEnterEvent1 = #markdown_event{link = {some, PrevEnterEventLink1}}) ->
                            PrevEnterEventLink2 = PrevEnterEventLink1#markdown_event_link{next = {some, EnterIndex3}},
                            PrevEnterEvent2 = PrevEnterEvent1#markdown_event{link = {some, PrevEnterEventLink2}},
                            PrevEnterEvent2
                        end
                    ),
                    Events4 = markdown_vec:update(
                        Events3,
                        EnterIndex3,
                        fun(EnterEvent1 = #markdown_event{link = {some, EnterEventLink1}}) ->
                            EnterEventLink2 = EnterEventLink1#markdown_event_link{previous = {some, ExitIndex1 - 1}},
                            EnterEvent2 = EnterEvent1#markdown_event{link = {some, EnterEventLink2}},
                            EnterEvent2
                        end
                    ),
                    %% Potential next start.
                    ExitIndex2 = EnterIndex3 + 1,
                    resolve_content_loop(Events4, Map2, ExitIndex2)
            end
    end.

%% @private
-spec skip_prefixes(Events, EnterIndex) -> EnterIndex when
    Events :: markdown_vec:t(Event), Event :: markdown_event:t(), EnterIndex :: non_neg_integer().
skip_prefixes(Events, EnterIndex) when EnterIndex >= ?markdown_vec_size(Events) ->
    EnterIndex;
skip_prefixes(Events, EnterIndex) ->
    case markdown_vec:get(Events, EnterIndex) of
        #markdown_event{name = Name} when
            Name =/= space_or_tab andalso Name =/= block_quote_prefix andalso Name =/= block_quote_marker
        ->
            EnterIndex;
        _ ->
            skip_prefixes(Events, EnterIndex + 1)
    end.
