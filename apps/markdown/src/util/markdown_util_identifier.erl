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
%%% % @format
-module(markdown_util_identifier).
-moduledoc """
Info on JavaScript identifiers.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    id_start/1,
    id_cont/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec id_start(Char) -> boolean() when Char :: char().
id_start(Char) ->
    case Char of
        $$ -> true;
        $_ -> true;
        _ -> markdown_util_unicode:is_id_start(Char)
    end.

-spec id_cont(Char, Jsx) -> boolean() when Char :: char(), Jsx :: boolean().
id_cont(Char, Jsx) ->
    case Char of
        16#200c -> true;
        16#200d -> true;
        $- when Jsx =:= true -> true;
        _ -> markdown_util_unicode:is_id_continue(Char)
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
