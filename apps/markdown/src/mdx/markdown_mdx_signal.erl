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
-module(markdown_mdx_signal).
-include_lib("markdown/include/markdown_util.hrl").
-moduledoc """
Signal used as feedback when parsing MDX ESM/expressions.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-10-10", modified => "2025-10-10"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdx.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    eof/3,
    error/4,
    ok/0
]).

%% Types
-doc """
An error at the end of the (partial?) expression.

`erlang-markdown` will either crash with error message `String` if it
doesn’t have any more text, or it will try again later when more text
is available.

## Examples

```erlang ignore
markdown_mdx_signal:eof(<<"Unexpected end of file in string literal">>, <<"source">>, <<"rule_id">>)
```
""".
-type eof() :: {eof, unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()}.
-doc """
A syntax error.

`erlang-markdown` will crash with error message `String`, and convert the
`usize` (byte offset into `&str` passed to `MdxExpressionParse` or
`MdxEsmParse`) to where it happened in the whole document.

## Examples

```erlang ignore
markdown_mdx_signal:error(<<"Unexpected `\"`, expected identifier">>, 1, <<"source">>, <<"rule_id">>)
```
""".
-type error() ::
    {error, unicode:unicode_binary(), non_neg_integer(), unicode:unicode_binary(), unicode:unicode_binary()}.
-type inner() :: eof() | error() | ok().
-doc """
Done, successfully.

`erlang-markdown` knows that this is the end of a valid expression/esm and
continues with markdown.

## Examples

```erlang ignore
markdown_mdx_signal:ok()
```
""".
-type ok() :: ok.
-doc """
Signal used as feedback when parsing MDX ESM/expressions.
""".
-type t() :: #markdown_mdx_signal{}.

-export_type([
    eof/0,
    error/0,
    inner/0,
    ok/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
An error at the end of the (partial?) expression.

`erlang-markdown` will either crash with error message `String` if it
doesn’t have any more text, or it will try again later when more text
is available.

## Examples

```erlang ignore
markdown_mdx_signal:eof(<<"Unexpected end of file in string literal">>, <<"source">>, <<"rule_id">>)
```
""".
-spec eof(Message, Source, RuleId) -> Signal when
    Message :: unicode:unicode_binary(),
    Source :: unicode:unicode_binary(),
    RuleId :: unicode:unicode_binary(),
    Signal :: t().
eof(Message, Source, RuleId) when is_binary(Message) andalso is_binary(Source) andalso is_binary(RuleId) ->
    #markdown_mdx_signal{
        inner = {eof, Message, Source, RuleId}
    }.

-doc """
A syntax error.

`erlang-markdown` will crash with error message `String`, and convert the
`usize` (byte offset into `&str` passed to `MdxExpressionParse` or
`MdxEsmParse`) to where it happened in the whole document.

## Examples

```erlang ignore
markdown_mdx_signal:error(<<"Unexpected `\"`, expected identifier">>, 1, <<"source">>, <<"rule_id">>)
```
""".
-spec error(Message, Relative, Source, RuleId) -> Signal when
    Message :: unicode:unicode_binary(),
    Relative :: non_neg_integer(),
    Source :: unicode:unicode_binary(),
    RuleId :: unicode:unicode_binary(),
    Signal :: t().
error(Message, Relative, Source, RuleId) when
    is_binary(Message) andalso ?is_non_neg_integer(Relative) andalso is_binary(Source) andalso is_binary(RuleId)
->
    #markdown_mdx_signal{
        inner = {error, Message, Relative, Source, RuleId}
    }.

-doc """
Done, successfully.

`erlang-markdown` knows that this is the end of a valid expression/esm and
continues with markdown.

## Examples

```erlang ignore
markdown_mdx_signal:ok()
```
""".
-spec ok() -> Signal when Signal :: t().
ok() ->
    #markdown_mdx_signal{
        inner = ok
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
