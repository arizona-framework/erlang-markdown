%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
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
    parse_esm/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse_esm(Value) -> Signal when Value :: unicode:unicode_binary(), Signal :: markdown_mdx:signal().
parse_esm(_Value) ->
    markdown_mdx_signal:ok().
