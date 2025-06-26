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
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
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
