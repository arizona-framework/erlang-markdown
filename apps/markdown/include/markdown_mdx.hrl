%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  10 Oct 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
-ifndef(MARKDOWN_MDX_HRL).
-define(MARKDOWN_MDX_HRL, 1).

%% Collect info for MDX.
-record(markdown_mdx_collect_result, {
    value :: unicode:unicode_binary(),
    stops :: markdown_vec:t(markdown_stop:t())
}).

%% Signal used as feedback when parsing MDX ESM/expressions.
-record(markdown_mdx_signal, {
    inner :: markdown_mdx_signal:inner()
}).

-endif.
