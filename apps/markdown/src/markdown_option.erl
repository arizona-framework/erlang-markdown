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
-module(markdown_option).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% New API
-export([
    none/0,
    some/1
]).
%% Instance API
-export([
    map/2
]).

%% Types
-type map_func() :: map_func(value(), value()).
-type map_func(ValueIn, ValueOut) ::
    fun((ValueIn) -> ValueOut).
-type t() :: t(value()).
-type t(Value) :: none | {some, Value}.
-type value() :: dynamic().

-export_type([
    map_func/0,
    map_func/2,
    t/0,
    t/1,
    value/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-spec none() -> Option when Option :: t().
none() -> none.

-spec some(Value) -> Option when Option :: t(Value), Value :: value().
some(Value) ->
    {some, Value}.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec map(OptionIn, MapFun) -> OptionOut when
    OptionIn :: t(ValueIn),
    ValueIn :: value(),
    MapFun :: map_func(ValueIn, ValueOut),
    ValueOut :: value(),
    OptionOut :: t(ValueOut).
map(none, MapFun) when is_function(MapFun, 1) ->
    none;
map({some, ValueIn}, MapFun) when is_function(MapFun, 1) ->
    ValueOut = MapFun(ValueIn),
    {some, ValueOut}.
