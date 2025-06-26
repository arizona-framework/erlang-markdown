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
-module(markdown_attempt).
-moduledoc """
How to handle [`markdown_state:ok()`][] or [`markdown_state:nok()`][].
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/4
]).
%% Kind API
-export([
    attempt/3,
    check/3
]).

%% Types

-doc "Different kinds of attempts.".
-type kind() ::
    %% Discard that was tokenized when unsuccessful.
    attempt
    %% Discard always.
    | check.

-doc """
How to handle [`markdown_state:ok()`][] or [`markdown_state:nok()`][].
""".
-type t() :: #markdown_attempt{}.

-export_type([
    kind/0,
    t/0
]).

%% Macros
-define(is_kind(X), ((X) =:= attempt orelse (X) =:= check)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Ok, Nok, Kind, OptionProgress) -> Attempt when
    Ok :: markdown_state:t(),
    Nok :: markdown_state:t(),
    Kind :: kind(),
    OptionProgress :: markdown_types:option(Progress),
    Progress :: markdown_progress:t(),
    Attempt :: t().
new(Ok, Nok, Kind, OptionProgress) when ?is_kind(Kind) andalso ?is_option_record(OptionProgress, markdown_progress) ->
    #markdown_attempt{ok = Ok, nok = Nok, kind = Kind, progress = OptionProgress}.

%%%=============================================================================
%%% Kind API functions
%%%=============================================================================

-spec attempt(Ok, Nok, OptionProgress) -> Attempt when
    Ok :: markdown_state:t(),
    Nok :: markdown_state:t(),
    OptionProgress :: markdown_types:option(Progress),
    Progress :: markdown_progress:t(),
    Attempt :: t().
attempt(Ok, Nok, OptionProgress) when ?is_option_record(OptionProgress, markdown_progress) ->
    #markdown_attempt{ok = Ok, nok = Nok, kind = ?FUNCTION_NAME, progress = OptionProgress}.

-spec check(Ok, Nok, OptionProgress) -> Attempt when
    Ok :: markdown_state:t(),
    Nok :: markdown_state:t(),
    OptionProgress :: markdown_types:option(Progress),
    Progress :: markdown_progress:t(),
    Attempt :: t().
check(Ok, Nok, OptionProgress) when ?is_option_record(OptionProgress, markdown_progress) ->
    #markdown_attempt{ok = Ok, nok = Nok, kind = ?FUNCTION_NAME, progress = OptionProgress}.
