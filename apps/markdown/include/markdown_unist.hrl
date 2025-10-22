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
