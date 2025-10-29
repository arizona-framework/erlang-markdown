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
-module(markdown_util_char).
-moduledoc """
Deal with bytes, chars, and kinds.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_util.hrl").

%% Kind API
-export([
    whitespace/0,
    punctuation/0,
    other/0
]).
%% API
-export([
    before_index/2,
    after_index/2,
    kind_after_index/2,
    classify/1,
    classify_opt/1,
    format_byte/1,
    format_opt/1
]).

%% Types
-doc """
Character kinds.
""".
-type kind() :: whitespace | punctuation | other.

-export_type([
    kind/0
]).

%%%=============================================================================
%%% Kind API functions
%%%=============================================================================

-doc """
Whitespace.

## Example

```markdown
> | **a_b_ c**.
   ^      ^    ^
```
""".
-spec whitespace() -> kind().
whitespace() -> whitespace.

-doc """
Punctuation.

## Example

```markdown
> | **a_b_ c**.
    ^^ ^ ^    ^
```
""".
-spec punctuation() -> kind().
punctuation() -> punctuation.

-doc """
Everything else.

## Example

```markdown
> | **a_b_ c**.
      ^ ^  ^
```
""".
-spec other() -> kind().
other() -> other.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Get a [`char`][] right before `index` in bytes (`&[u8]`).

In most cases, markdown operates on ASCII bytes.
In a few cases, it is unicode aware, so we need to find an actual char.
""".
-spec before_index(Bytes, Index) -> OptionCharacter when
    Bytes :: binary(), Index :: non_neg_integer(), OptionCharacter :: markdown_option:t(Character), Character :: char().
before_index(Bytes, Index) when is_binary(Bytes) andalso ?is_non_neg_integer(Index) ->
    Start =
        if
            Index < 4 -> 0;
            true -> Index - 4
        end,
    Slice = binary:part(Bytes, Start, Index - Start),
    case markdown_types:unicode_string_lossy(Slice) of
        [] -> none;
        Chars -> {some, lists:last(Chars)}
    end.

-doc """
Get a [`char`][] right at `index` in bytes (`&[u8]`).

In most cases, markdown operates on ASCII bytes.
In a few cases, it is unicode aware, so we need to find an actual char.
""".
-spec after_index(Bytes, Index) -> OptionCharacter when
    Bytes :: binary(), Index :: non_neg_integer(), OptionCharacter :: markdown_option:t(Character), Character :: char().
after_index(Bytes, Index) when is_binary(Bytes) andalso ?is_non_neg_integer(Index) ->
    BytesSize = byte_size(Bytes),
    End =
        if
            Index + 4 > BytesSize -> BytesSize;
            true -> Index + 4
        end,
    Slice = binary:part(Bytes, Index, End - Index),
    case markdown_types:unicode_binary_lossy(Slice) of
        <<>> -> none;
        <<FirstChar/utf8, _/bytes>> -> {some, FirstChar}
    end.

-doc """
Classify a char at `index` in bytes (`&[u8]`).
""".
-spec kind_after_index(Bytes, Index) -> kind() when Bytes :: binary(), Index :: non_neg_integer().
kind_after_index(Bytes, Index) when is_binary(Bytes) andalso ?is_non_neg_integer(Index) ->
    BytesSize = byte_size(Bytes),
    if
        Index =:= BytesSize ->
            whitespace;
        true ->
            Byte = binary:at(Bytes, Index),
            case Byte of
                _ when ?is_ascii_whitespace(Byte) ->
                    whitespace;
                _ when ?is_ascii_punctuation(Byte) ->
                    punctuation;
                _ when ?is_ascii_alphanumeric(Byte) ->
                    other;
                _ ->
                    %% Otherwise: seems to be an ASCII control, so it seems to be a
                    %% non-ASCII `char`.
                    classify_opt(after_index(Bytes, Index))
            end
    end.

-doc """
Classify whether a `char` represents whitespace, punctuation, or something
else.

Used for attention (emphasis, strong), whose sequences can open or close
based on the class of surrounding characters.

## References

*   [`micromark-util-classify-character` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-util-classify-character/dev/index.js)
""".
-spec classify(Char) -> kind() when Char :: char().
classify(Char) ->
    maybe
        %% Unicode whitespace.
        {whitespace, false} ?=
            {whitespace, ?is_ascii_whitespace(Char) orelse markdown_util_unicode:is_whitespace(Char)},
        %% Unicode punctuation.
        {punctuation, false} ?=
            {punctuation, ?is_ascii_punctuation(Char) orelse markdown_util_unicode:is_punctuation(Char)},
        %% Everything else.
        other
    else
        {Kind, true} ->
            Kind
    end.

-doc """
Like [`classify`], but supports eof as whitespace.
""".
-spec classify_opt(CharOpt) -> kind() when CharOpt :: {some, char()} | none.
classify_opt(none) ->
    whitespace;
classify_opt({some, Char}) ->
    classify(Char).

-doc """
Format a byte (`u8`).
""".
-spec format_byte(Byte) -> String when Byte :: byte(), String :: unicode:unicode_binary().
format_byte(Byte) when Byte >= 0 andalso Byte =< 255 ->
    Representation = <<"U+00", (binary:encode_hex(<<Byte:8>>, uppercase))/bytes>>,
    Printable =
        case Byte of
            $` ->
                {some, <<"`` ` ``">>};
            _ when Byte >= $! andalso Byte =< $~ ->
                {some, <<"`", Byte/utf8, "`">>};
            _ ->
                none
        end,
    case Printable of
        {some, Char} ->
            <<Char/bytes, " (", Representation/bytes, ")">>;
        none ->
            Representation
    end.

-doc """
Format an optional `char` (`none` means eof).
""".
-spec format_opt(CharOpt) -> String when
    CharOpt :: markdown_option:t(Char), Char :: char(), String :: unicode:unicode_binary().
format_opt(none) -> <<"end of file"/utf8>>;
format_opt({some, Char}) -> <<"character ", Char/utf8>>.
