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
-module(markdown_test_utils_swc).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-09-27", modified => "2025-09-27"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    parse_esm/1,
    parse_expression/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse_esm(Value) -> Signal when Value :: unicode:unicode_binary(), Signal :: markdown_mdx:signal().
parse_esm(Value) ->
    % Check if the ESM has balanced braces/brackets/parens
    % If not balanced, return eof to indicate more input is needed
    case is_balanced(Value) of
        true -> markdown_mdx_signal:ok();
        false -> markdown_mdx_signal:eof(<<"Unexpected end of file">>, <<"mdx">>, <<"mdx">>)
    end.

-spec parse_expression(Value, Kind) -> Signal when
    Value :: unicode:unicode_binary(), Kind :: markdown_mdx:expression_kind(), Signal :: markdown_mdx:signal().
parse_expression(_Value, _Kind) ->
    markdown_mdx_signal:ok().

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec is_balanced(Value) -> boolean() when Value :: unicode:unicode_binary().
is_balanced(Value) ->
    % Check if braces, brackets, and parentheses are balanced
    is_balanced(Value, 0, 0, 0, false).

%% @private
-spec is_balanced(Value, BraceCount, BracketCount, ParenCount, InString) -> boolean() when
    Value :: unicode:unicode_binary(),
    BraceCount :: integer(),
    BracketCount :: integer(),
    ParenCount :: integer(),
    InString :: boolean() | quote.
is_balanced(<<>>, BraceCount, BracketCount, ParenCount, _InString) ->
    % All brackets must be balanced (count = 0)
    BraceCount =:= 0 andalso BracketCount =:= 0 andalso ParenCount =:= 0;
% Handle escape sequences in strings
is_balanced(<<"\\", _:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString) when InString =/= false ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, InString);
% Handle double quotes
is_balanced(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle single quotes
is_balanced(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle backticks (template literals)
is_balanced(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle brackets only when not in a string
is_balanced(<<"{", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount + 1, BracketCount, ParenCount, false);
is_balanced(<<"}", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount - 1, BracketCount, ParenCount, false);
is_balanced(<<"[", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount + 1, ParenCount, false);
is_balanced(<<"]", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount - 1, ParenCount, false);
is_balanced(<<"(", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount + 1, false);
is_balanced(<<")", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount - 1, false);
% Skip other characters
is_balanced(<<_:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, InString).
