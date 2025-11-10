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
-module(markdown_message).
-moduledoc """
Message.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/4,
    format/1
]).

%% Types
-doc """
Message.
""".
-type t() :: #markdown_message{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionPlace, Reason, RuleId, Source) -> Message when
    OptionPlace :: markdown_types:option(Place),
    Place :: markdown_place:t(),
    Reason :: unicode:unicode_binary(),
    RuleId :: unicode:unicode_binary(),
    Source :: unicode:unicode_binary(),
    Message :: t().
new(OptionPlace, Reason, RuleId, Source) when
    ?is_option_record(OptionPlace, markdown_place) andalso is_binary(Reason) andalso is_binary(RuleId) andalso
        is_binary(Source)
->
    #markdown_message{
        place = OptionPlace,
        reason = Reason,
        rule_id = RuleId,
        source = Source
    }.

-doc """
Format a message as a binary string.

The format is: `<line>:<column>: <reason> (<source>:<rule_id>)`

## Examples

```erlang
Message = markdown_message:new(
    {some, markdown_place:new(markdown_unist_point:new(1, 3, 2))},
    <<"Unexpected end of file">>,
    <<"unexpected-eof">>,
    <<"erlang-markdown">>
),
FormattedMessage = markdown_message:format(Message),
%% => <<"1:3: Unexpected end of file (erlang-markdown:unexpected-eof)">>
```
""".
-spec format(Message) -> FormattedMessage when
    Message :: t(),
    FormattedMessage :: unicode:unicode_binary().
format(#markdown_message{
    place = {some, #markdown_place{inner = #markdown_unist_point{line = Line, column = Column}}},
    reason = Reason,
    rule_id = RuleId,
    source = Source
}) ->
    iolist_to_binary(io_lib:format("~w:~w: ~ts (~ts:~ts)", [Line, Column, Reason, Source, RuleId]));
format(#markdown_message{place = none, reason = Reason, rule_id = RuleId, source = Source}) ->
    iolist_to_binary(io_lib:format("~ts (~ts:~ts)", [Reason, Source, RuleId])).
