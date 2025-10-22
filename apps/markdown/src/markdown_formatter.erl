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
-module(markdown_formatter).
-moduledoc """
Link to another event.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% New API
-export([
    new_io_device/2,
    new_string/1
]).
%% Formatter API
-export([
    do/2,
    erase/2,
    get/3,
    finalize/1,
    format/3,
    put/3,
    shift_left/1,
    shift_right/1,
    write/2,
    write_indent/1
]).

%% Records
-record(markdown_formatter, {
    env = #{} :: env(),
    depth = 0 :: non_neg_integer(),
    indent = 4 :: non_neg_integer(),
    output = [] :: iolist() | io:device()
}).

%% Types
-type env() :: #{
    key() => value()
}.
-type key() :: dynamic().
-type options() :: #{
    env => env(),
    depth => non_neg_integer(),
    indent => non_neg_integer()
}.
-type t() :: #markdown_formatter{}.
-type value() :: dynamic().

-export_type([
    env/0,
    key/0,
    options/0,
    t/0,
    value/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec new_io_device(IoDevice, Options) -> Formatter when
    IoDevice :: io:device(), Options :: options(), Formatter :: t().
new_io_device(IoDevice, Options) when is_map(Options) ->
    Env = maps:get(env, Options, #{}),
    Depth = maps:get(depth, Options, 0),
    Indent = maps:get(indent, Options, 4),
    #markdown_formatter{env = Env, depth = Depth, indent = Indent, output = IoDevice}.

-spec new_string(Options) -> Formatter when Options :: options(), Formatter :: t().
new_string(Options) when is_map(Options) ->
    Env = maps:get(env, Options, #{}),
    Depth = maps:get(depth, Options, 0),
    Indent = maps:get(indent, Options, 4),
    #markdown_formatter{env = Env, depth = Depth, indent = Indent, output = []}.

%%%=============================================================================
%%% Formatter API functions
%%%=============================================================================

-spec do(Formatter, ActionList) -> Formatter when
    Formatter :: t(),
    ActionList :: [Action],
    Action ::
        {erase, Key}
        | {format, Format, Data}
        | {put, Key, Value}
        | shift_left
        | shift_right
        | {write, CharData}
        | write_indent,
    Format :: io:format(),
    Data :: [term()],
    Key :: key(),
    Value :: value(),
    CharData :: unicode:chardata().
do(Formatter1, [{erase, Key} | Actions]) ->
    Formatter2 = erase(Formatter1, Key),
    do(Formatter2, Actions);
do(Formatter1, [{format, Format, Data} | Actions]) ->
    Formatter2 = format(Formatter1, Format, Data),
    do(Formatter2, Actions);
do(Formatter1, [{put, Key, Value} | Actions]) ->
    Formatter2 = put(Formatter1, Key, Value),
    do(Formatter2, Actions);
do(Formatter1, [shift_left | Actions]) ->
    Formatter2 = shift_left(Formatter1),
    do(Formatter2, Actions);
do(Formatter1, [shift_right | Actions]) ->
    Formatter2 = shift_right(Formatter1),
    do(Formatter2, Actions);
do(Formatter1, [{write, CharData} | Actions]) ->
    Formatter2 = write(Formatter1, CharData),
    do(Formatter2, Actions);
do(Formatter1, [write_indent | Actions]) ->
    Formatter2 = write_indent(Formatter1),
    do(Formatter2, Actions);
do(Formatter, []) ->
    Formatter.

-spec erase(Formatter1, Key) -> Formatter2 when
    Formatter1 :: t(), Key :: key(), Formatter2 :: t().
erase(Formatter1 = #markdown_formatter{env = Env1}, Key) ->
    Env2 = maps:remove(Key, Env1),
    Formatter2 = Formatter1#markdown_formatter{env = Env2},
    Formatter2.

-spec get(Formatter, Key, Default) -> Value | Default when
    Formatter :: t(), Key :: key(), Default :: value(), Value :: value().
get(#markdown_formatter{env = Env}, Key, Default) ->
    maps:get(Key, Env, Default).

-spec finalize(Formatter) -> ok | iolist() when Formatter :: t().
finalize(#markdown_formatter{output = Output}) when is_list(Output) ->
    Output;
finalize(Formatter = #markdown_formatter{}) ->
    Formatter = write(Formatter, <<"\n">>),
    ok.

-spec format(Formatter1, Format :: io:format(), Data :: [term()]) -> Formatter2 when
    Formatter1 :: t(), Formatter2 :: t().
format(Formatter1 = #markdown_formatter{output = Output1}, Format, Data) when is_list(Output1) ->
    Output2 = [Output1 | io_lib:format(Format, Data)],
    Formatter2 = Formatter1#markdown_formatter{output = Output2},
    Formatter2;
format(Formatter1 = #markdown_formatter{output = IoDevice}, Format, Data) ->
    ok = io:format(IoDevice, Format, Data),
    Formatter1.

-spec put(Formatter1, Key, Value) -> Formatter2 when
    Formatter1 :: t(), Key :: key(), Value :: value(), Formatter2 :: t().
put(Formatter1 = #markdown_formatter{env = Env1}, Key, Value) ->
    Env2 = maps:put(Key, Value, Env1),
    Formatter2 = Formatter1#markdown_formatter{env = Env2},
    Formatter2.

-spec shift_left(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_left(Formatter1 = #markdown_formatter{depth = Depth1}) ->
    Depth2 = Depth1 - 1,
    Formatter2 = Formatter1#markdown_formatter{depth = Depth2},
    Formatter2.

-spec shift_right(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
shift_right(Formatter1 = #markdown_formatter{depth = Depth1}) ->
    Depth2 = Depth1 + 1,
    Formatter2 = Formatter1#markdown_formatter{depth = Depth2},
    Formatter2.

-spec write(Formatter, CharData) -> Formatter when Formatter :: t(), CharData :: unicode:chardata().
write(Formatter1 = #markdown_formatter{output = Output1}, CharData) when is_list(Output1) ->
    Output2 = [Output1, CharData],
    Formatter2 = Formatter1#markdown_formatter{output = Output2},
    Formatter2;
write(Formatter1 = #markdown_formatter{output = IoDevice}, CharData) ->
    ok = io:put_chars(IoDevice, CharData),
    Formatter1.

-spec write_indent(Formatter1) -> Formatter2 when Formatter1 :: t(), Formatter2 :: t().
write_indent(Formatter1 = #markdown_formatter{depth = Depth, indent = Indent}) ->
    Formatter2 = write(Formatter1, binary:copy(<<" ">>, Indent * Depth)),
    Formatter2.
