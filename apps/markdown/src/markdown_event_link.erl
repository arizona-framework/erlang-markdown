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
-module(markdown_event_link).
-moduledoc """
Link to another event.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    new/3
]).

%% Types
-doc "Link to another event.".
-type t() :: #markdown_event_link{}.

-export_type([
    t/0
]).

%% Macros
-define(is_content(X), ((X) =:= flow orelse (X) =:= content orelse (X) =:= string orelse (X) =:= text)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionPrevious, OptionNext, Content) -> Link when
    OptionPrevious :: markdown_types:option(Previous),
    OptionNext :: markdown_types:option(Next),
    Previous :: markdown_types:usize(),
    Next :: markdown_types:usize(),
    Content :: markdown_event:content(),
    Link :: t().
new(OptionPrevious, OptionNext, Content) when
    ?is_option_usize(OptionPrevious) andalso ?is_option_usize(OptionNext) andalso ?is_content(Content)
->
    #markdown_event_link{
        previous = OptionPrevious,
        next = OptionNext,
        content = Content
    }.
