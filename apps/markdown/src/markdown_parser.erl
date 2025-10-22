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
-module(markdown_parser).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    parse/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Turn a string of markdown into events.

Passes the bytes back so the compiler can access the source.
""".
-spec parse(Value, ParseOptions) -> {ok, {Events, ParseState}} | {error, Message} when
    Value :: unicode:unicode_binary(),
    ParseOptions :: markdown_parse_options:t(),
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    Message :: markdown_message:t().
parse(Value, ParseOptions = #markdown_parse_options{}) when is_binary(Value) ->
    Bytes = Value,
    OptionLocation =
        case ParseOptions of
            #markdown_parse_options{
                mdx_esm_parse = OptionMdxEsmParse, mdx_expression_parse = OptionMdxExpressionParse
            } when ?is_option_some(OptionMdxEsmParse) orelse ?is_option_some(OptionMdxExpressionParse) ->
                {some, markdown_location:new(Bytes)};
            _ ->
                none
        end,
    ParseState1 = markdown_parse_state:new(OptionLocation, ParseOptions, Bytes),
    StartPoint = markdown_point:new(1, 1, 0, 0),
    StartIndex = markdown_index:new(0, 0),
    EndIndex = markdown_index:new(byte_size(Bytes), 0),
    FirstState = markdown_state:next(document_start),
    Tokenizer1 = markdown_tokenizer:new(StartPoint, ParseState1),
    {Tokenizer2, State} = markdown_tokenizer:push(Tokenizer1, StartIndex, EndIndex, FirstState),
    % markdown:display("\n\nBEFORE FLUSH:\n\n", [], Tokenizer2),
    % io:format("\n\n[~w] BEFORE flush\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State), markdown_debug:rust_debug_string(Tokenizer2)]),
    case markdown_tokenizer:flush(Tokenizer2, State, true) of
        {Tokenizer3, {ok, Subresult}} ->
            % io:format("\n\n[~w] AFTER flush\n~ts\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State), markdown_debug:rust_debug_string(Tokenizer3), markdown_debug:rust_debug_string(Subresult)]),
            % markdown:display("\n\nAFTER FLUSH:\n\n", [], Tokenizer3),
            Events3 = Tokenizer3#markdown_tokenizer.events,
            ParseState3 = Tokenizer3#markdown_tokenizer.parse_state,
            case subtokenize_loop(Events3, Subresult, ParseState3) of
                {Events4, {ok, ParseState4}} ->
                    % io:format("\n\n[~w] FINAL\n~ts\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Events4), markdown_debug:rust_debug_string(ParseState4)]),
                    {ok, {Events4, ParseState4}};
                {_Events4, {error, Message}} ->
                    {error, Message}
            end;
        {_Tokenizer3, {error, Message}} ->
            {error, Message}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec subtokenize_loop(Events, Subresult, ParseState) -> {Events, Result} when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Subresult :: markdown_subresult:t(),
    ParseState :: markdown_parse_state:t(),
    Result :: {ok, ParseState} | {error, Message},
    Message :: markdown_message:t().
subtokenize_loop(Events1, Subresult1 = #markdown_subresult{done = Done}, ParseState1 = #markdown_parse_state{}) ->
    ParseState2 = markdown_parse_state:append(ParseState1, Subresult1),
    case Done of
        false ->
            % io:format("\n\n[~w:~w] BEFORE subtokenize\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), markdown_debug:rust_debug_string(Events1)]),
            case markdown_subtokenizer:subtokenize(Events1, ParseState2, none) of
                {Events2, {ok, Subresult2}} ->
                    % io:format("\n\n[~w:~w] AFTER subtokenize\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), markdown_debug:rust_debug_string(Events2)]),
                    subtokenize_loop(Events2, Subresult2, ParseState2);
                {Events2, {error, Message}} ->
                    {Events2, {error, Message}}
            end;
        true ->
            {Events1, {ok, ParseState1}}
    end.
