%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_option).
-moduledoc """

""".
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
