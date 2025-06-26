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
-ifndef(MARKDOWN_UNIST_HRL).
-define(MARKDOWN_UNIST_HRL, 1).

% One place in a source file.
-record(markdown_unist_point, {
    % 1-indexed integer representing a line in a source file.
    line :: markdown_unist_point:line(),
    % 1-indexed integer representing a column in a source file.
    column :: markdown_unist_point:column(),
    % 0-indexed integer representing a character in a source file.
    offset :: markdown_unist_point:offset()
}).

% Location of a node in a source file.
-record(markdown_unist_position, {
    % Represents the place of the first character of the parsed source region.
    start :: markdown_unist_point:t(),
    % Represents the place of the first character after the parsed source region, whether it exists or not.
    'end' :: markdown_unist_point:t()
}).

-endif.
