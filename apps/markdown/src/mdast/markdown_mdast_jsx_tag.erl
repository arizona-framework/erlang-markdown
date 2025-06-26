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
-module(markdown_mdast_jsx_tag).
-moduledoc """
Info on a tag.

JSX tags are parsed on their own.
They’re matched together here.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_mdast.hrl").
-include_lib("markdown/include/markdown_unist.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    new/6
]).

%% Types
-type t() :: #markdown_mdast_jsx_tag{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(OptionName, Attributes, Close, SelfClosing, Start, End) -> JsxTag when
    OptionName :: markdown_option:t(Name),
    Name :: binary(),
    Attributes :: markdown_vec:t(Attribute),
    Attribute :: markdown_mdast_attribute_content:t(),
    Close :: boolean(),
    SelfClosing :: boolean(),
    Start :: markdown_unist_point:t(),
    End :: markdown_unist_point:t(),
    JsxTag :: t().
new(
    OptionName,
    Attributes = #markdown_vec{},
    Close,
    SelfClosing,
    Start = #markdown_unist_point{},
    End = #markdown_unist_point{}
) when ?is_option_binary(OptionName) andalso is_boolean(Close) andalso is_boolean(SelfClosing) ->
    #markdown_mdast_jsx_tag{
        name = OptionName,
        attributes = Attributes,
        close = Close,
        self_closing = SelfClosing,
        start = Start,
        'end' = End
    }.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
