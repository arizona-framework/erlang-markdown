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
-ifndef(MARKDOWN_UTIL_HRL).
-define(MARKDOWN_UTIL_HRL, 1).

-define('format!'(Fmt, Args), markdown_types:unicode_binary(lists:flatten(io_lib:format(Fmt, Args)))).
-define('unreachable!'(Fmt, Args), erlang:error(unreachable, [?'format!'(Fmt, Args)])).
-define('vec!'(List), markdown_vec:from_list(List)).

-define(is_ascii_alphabetic(X),
    (((X) >= $A) andalso ((X) =< $Z) orelse
        ((X) >= $a) andalso ((X) =< $z))
).
-define(is_ascii_alphanumeric(X),
    (((X) >= $0) andalso ((X) =< $9) orelse
        ((X) >= $A) andalso ((X) =< $Z) orelse
        ((X) >= $a) andalso ((X) =< $z))
).
-define(is_ascii_digit(X),
    (((X) >= $0) andalso ((X) =< $9))
).
-define(is_ascii_hexdigit(X),
    (((X) >= $0) andalso ((X) =< $9) orelse
        ((X) >= $A) andalso ((X) =< $F) orelse
        ((X) >= $a) andalso ((X) =< $f))
).
-define(is_ascii_punctuation(X),
    (((X) >= $!) andalso ((X) =< $/) orelse
        ((X) >= $:) andalso ((X) =< $@) orelse
        ((X) >= $[) andalso ((X) =< $`) orelse
        ((X) >= ${) andalso ((X) =< $~))
).
-define(is_ascii_whitespace(X),
    (((X) =:= $\s) orelse ((X) =:= $\t) orelse ((X) =:= $\n) orelse ((X) =:= $\r) orelse ((X) =:= $\f) orelse
        ((X) =:= $\v))
).

-define(is_i64(X), (is_integer((X)) andalso (X) >= -16#8000000000000000 andalso (X) =< 16#7FFFFFFFFFFFFFFF)).
-define(is_u8(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FF)).
-define(is_u32(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFF)).
-define(is_u64(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFFFFFFFFFF)).
-define(is_usize(X), ?is_u64(X)).

-define(is_option_none(X), ((X) =:= none)).
-define(is_option_some(X), (is_tuple((X)) andalso tuple_size((X)) =:= 2 andalso element(1, (X)) =:= some)).
-define(is_option_some(X, Y), (?is_option_some(X) andalso (element(2, (X)) =:= (Y)))).
-define(is_option_binary(X), (?is_option_none(X) orelse (?is_option_some(X) andalso is_binary(element(2, (X)))))).
-define(is_option_boolean(X), (?is_option_none(X) orelse (?is_option_some(X) andalso is_boolean(element(2, (X)))))).
-define(is_option_record(X, T), (?is_option_none(X) orelse (?is_option_some(X) andalso is_record(element(2, (X)), T)))).
-define(is_option_u8(X), (?is_option_none(X) orelse (?is_option_some(X) andalso ?is_u8(element(2, (X)))))).
-define(is_option_usize(X), (?is_option_none(X) orelse (?is_option_some(X) andalso ?is_usize(element(2, (X)))))).
-define(is_non_neg_integer(X), (is_integer(X) andalso X >= 0)).
-define(is_pos_integer(X), (is_integer(X) andalso X >= 1)).

-define(none, none).
-define(some(X), ({some, X})).

-endif.
