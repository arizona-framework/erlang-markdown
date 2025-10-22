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
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%%% % @oncall whatsapp_clr
-ifndef(MARKDOWN_HRL).
-define(MARKDOWN_HRL, 1).

% %% How to handle [`State::Ok`][] or [`State::Nok`][].
% -record(markdown_attempt, {
%     %% Where to go to when successful.
%     ok :: markdown_state:t(),
%     %% Where to go to when unsuccessful.
%     nok :: markdown_state:t(),
%     %% Kind of attempt.
%     kind :: markdown_attempt:kind(),
%     %% If needed, the progress to revert to.
%     %%
%     %% It is not needed to discard an [`AttemptKind::Attempt`] that has a
%     %% `nok` of [`State::Nok`][], because that means it is used in *another*
%     %% attempt, which will receive that `Nok`, and has to handle it.
%     progress: Option<Progress>,
% }).

%% One place in a source file.
%%
%% The interface for the location in the document comes from unist
%% [`Point`](https://github.com/syntax-tree/unist#point).
-record(markdown_point, {
    %% 1-indexed integer representing a line in a source file.
    line :: markdown_point:line(),
    %% 1-indexed integer representing a column in a source file.
    %%
    %% This is increased up to a tab stop for tabs.
    column :: markdown_point:column(),
    %% 0-indexed integer representing a byte in a source file.
    offset :: markdown_point:offset(),
    %% 0-indexed integer representing a virtual byte in a source file.
    virtual :: markdown_point:virtual()
}).

-endif.
