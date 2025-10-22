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
-module(markdown_html_compile_context).
-moduledoc """
Context used to compile markdown.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-05", modified => "2025-03-05"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_html.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    buffer/1,
    line_ending/1,
    line_ending_if_needed/1,
    new/4,
    push/2,
    resume/1
]).

%% Types
-type t() :: #markdown_html_compile_context{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Push a buffer.
""".
-spec buffer(CompileContext) -> CompileContext when CompileContext :: t().
buffer(CompileContext1 = #markdown_html_compile_context{buffers = Buffers1}) ->
    Buffers2 = markdown_vec:push(Buffers1, <<>>),
    CompileContext2 = CompileContext1#markdown_html_compile_context{buffers = Buffers2},
    CompileContext2.

-doc """
Add a line ending.
""".
-spec line_ending(CompileContext) -> CompileContext when CompileContext :: t().
line_ending(CompileContext = #markdown_html_compile_context{line_ending_default = LineEndingDefault}) ->
    push(CompileContext, markdown_line_ending:as_binary(LineEndingDefault)).

-doc """
Add a line ending if needed (as in, there’s no eol/eof already).
""".
-spec line_ending_if_needed(CompileContext) -> CompileContext when CompileContext :: t().
line_ending_if_needed(CompileContext = #markdown_html_compile_context{buffers = Buffers}) ->
    case markdown_vec:last(Buffers) of
        <<>> ->
            CompileContext;
        LastBuf ->
            case binary:at(LastBuf, byte_size(LastBuf) - 1) of
                $\n ->
                    CompileContext;
                $\r ->
                    CompileContext;
                _ ->
                    line_ending(CompileContext)
            end
    end.

-spec new(Events, Bytes, CompileOptions, LineEnding) -> CompileContext when
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    Bytes :: binary(),
    CompileOptions :: markdown_compile_options:t(),
    LineEnding :: markdown_line_ending:t(),
    CompileContext :: t().
new(Events = #markdown_vec{}, Bytes, CompileOptions = #markdown_compile_options{}, LineEnding) when
    is_binary(Bytes) andalso ?is_markdown_line_ending(LineEnding)
->
    #markdown_html_compile_context{
        events = Events,
        bytes = Bytes,
        heading_atx_rank = none,
        heading_setext_buffer = none,
        raw_flow_seen_data = none,
        raw_flow_fences_count = none,
        raw_text_inside = false,
        character_reference_marker = none,
        list_expect_first_marker = none,
        media_stack = markdown_vec:new(),
        definitions = markdown_vec:new(),
        gfm_footnote_definitions = markdown_vec:new(),
        gfm_footnote_definition_calls = markdown_vec:new(),
        gfm_footnote_definition_stack = markdown_vec:new(),
        gfm_table_in_head = false,
        gfm_table_align = none,
        gfm_table_column = 0,
        tight_stack = markdown_vec:new(),
        slurp_one_line_ending = false,
        image_alt_inside = false,
        encode_html = true,
        line_ending_default = LineEnding,
        buffers = markdown_vec:from_list([<<>>]),
        index = 0,
        options = CompileOptions
    }.

-doc """
Push a str to the last buffer.
""".
-spec push(CompileContext, Value) -> CompileContext when CompileContext :: t(), Value :: binary().
push(CompileContext1 = #markdown_html_compile_context{buffers = Buffers1}, Value) when is_binary(Value) ->
    Buffers2 = markdown_vec:update_last(Buffers1, fun(LastBuf) ->
        <<LastBuf/bytes, Value/bytes>>
    end),
    CompileContext2 = CompileContext1#markdown_html_compile_context{buffers = Buffers2},
    CompileContext2.

-doc """
Pop a buffer, returning its value.
""".
-spec resume(CompileContext) -> {CompileContext, Buffer} when CompileContext :: t(), Buffer :: binary().
resume(CompileContext1 = #markdown_html_compile_context{buffers = Buffers1}) ->
    {Buffers2, Buffer} = markdown_vec:pop(Buffers1),
    CompileContext2 = CompileContext1#markdown_html_compile_context{buffers = Buffers2},
    {CompileContext2, Buffer}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
