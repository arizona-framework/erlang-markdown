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
%%% @doc
%%%
%%% @end
%%% Created :  10 Oct 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%%% % @oncall whatsapp_clr
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
