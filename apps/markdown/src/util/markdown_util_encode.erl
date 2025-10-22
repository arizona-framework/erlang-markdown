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
-module(markdown_util_encode).
-moduledoc """
Encode HTML.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    encode/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Encode dangerous html characters.

This ensures that certain characters which have special meaning in HTML are
dealt with.
Technically, we can skip `>` and `"` in many cases, but `CommonMark`
includes them.

This behavior is not explained in prose in `CommonMark` but can be inferred
from the input/output test cases.

## Examples

```rust ignore
use markdown::util::encode;

assert_eq!(encode("I <3 🦀"), "I &lt;3 🦀");
```

## References

* [`micromark-util-encode` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-encode)
""".
-spec encode(Value, EncodeHtml) -> UnicodeValue when
    Value :: binary(), EncodeHtml :: boolean(), UnicodeValue :: unicode:unicode_binary().
encode(Value, EncodeHtml) when is_binary(Value) andalso is_boolean(EncodeHtml) ->
    %% It’ll grow a bit bigger for each dangerous character.
    encode_loop(Value, 0, 0, Value, EncodeHtml, <<>>).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec encode_loop(Bytes, Start, Index, Value, EncodeHtml, Acc) -> Acc when
    Bytes :: binary(),
    Start :: non_neg_integer(),
    Index :: non_neg_integer(),
    Value :: binary(),
    EncodeHtml :: boolean(),
    Acc :: unicode:unicode_binary().
encode_loop(<<Byte:8, Bytes/bytes>>, Start1, Index1, Value, EncodeHtml, Acc1) ->
    case Byte of
        _ when
            Byte =:= $\x00 orelse
                (EncodeHtml =:= true andalso (Byte =:= $& orelse Byte =:= $" orelse Byte =:= $< orelse Byte =:= $>))
        ->
            Escaped =
                case Byte of
                    $\x00 -> <<"�"/utf8>>;
                    $& -> <<"&amp;">>;
                    $" -> <<"&quot;">>;
                    $< -> <<"&lt;">>;
                    $> -> <<"&gt;">>
                end,
            Acc2 = <<Acc1/bytes, (binary:part(Value, Start1, Index1 - Start1))/bytes, Escaped/bytes>>,
            Start2 = Index1 + 1,
            Index2 = Index1 + 1,
            encode_loop(Bytes, Start2, Index2, Value, EncodeHtml, Acc2);
        _ ->
            Index2 = Index1 + 1,
            encode_loop(Bytes, Start1, Index2, Value, EncodeHtml, Acc1)
    end;
encode_loop(<<>>, Start, _Index, Value, _EncodeHtml, Acc1) ->
    Acc2 = <<Acc1/bytes, (binary:part(Value, Start, byte_size(Value) - Start))/bytes>>,
    Acc2.
