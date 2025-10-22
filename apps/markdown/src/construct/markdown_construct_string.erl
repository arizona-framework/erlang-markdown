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
-module(markdown_construct_string).
-moduledoc """
The string content type.

**String** is a limited [text][] like content type which only allows
character escapes and character references.
It exists in things such as identifiers (media references, definitions),
titles, URLs, code (fenced) info and meta parts.

The constructs found in string are:

*   [Character escape][crate::construct::character_escape]
*   [Character reference][crate::construct::character_reference]

[text]: crate::construct::text
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
    start/1,
    before/1,
    before_data/1,
    resolve/1
]).

%% Macros
-define(MARKERS, <<"&\\">>).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of string.

````markdown
> | ```js
       ^
````
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{markers = ?MARKERS},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:retry(string_before),
    {Tokenizer2, State}.

-doc """
Before string.

````markdown
> | ```js
       ^
````
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = Current}) ->
    case Current of
        none ->
            Tokenizer2 = markdown_tokenizer:register_resolver(Tokenizer1, data),
            Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, string),
            State = markdown_state:ok(),
            {Tokenizer3, State};
        {some, $&} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(string_before),
                markdown_state:next(string_before_data)
            ),
            State = markdown_state:retry(character_reference_start),
            {Tokenizer2, State};
        {some, $\\} ->
            Tokenizer2 = markdown_tokenizer:attempt(
                Tokenizer1,
                markdown_state:next(string_before),
                markdown_state:next(string_before_data)
            ),
            State = markdown_state:retry(character_escape_start),
            {Tokenizer2, State};
        _ ->
            State = markdown_state:retry(string_before_data),
            {Tokenizer1, State}
    end.

-doc """
At data.

````markdown
> | ```js
       ^
````
""".
-spec before_data(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_data(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_tokenizer:attempt(Tokenizer1, markdown_state:next(string_before), markdown_state:nok()),
    State = markdown_state:retry(data_start),
    {Tokenizer2, State}.

-doc """
Resolve whitespace in string.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1 = #markdown_tokenizer{}) ->
    Tokenizer2 = markdown_construct_partial_whitespace:resolve_whitespace(Tokenizer1, false, false),
    {Tokenizer2, {ok, none}}.
