%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_util_character_reference).
-moduledoc """
Helpers for character references.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    decode_named/2,
    decode_numeric/2,
    decode/3,
    parse/1,
    value_max/1,
    value_test/2
]).

%% Types
-type marker() :: $# | $x | $&.

-export_type([
    marker/0
]).

%% Macros
-define(REPLACEMENT_CHARACTER, 16#FFFD).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Decode named character references.

Turn the name coming from a named character reference (without the `&` or
`;`) into a string.
This looks the given string up at `0` in the tuples of
`CHARACTER_REFERENCES` (or `CHARACTER_REFERENCES_HTML_4`)
and then takes the corresponding value from `1`.

The `html5` boolean is used for named character references, and specifier
whether the 2125 names from HTML 5 or the 252 names from HTML 4 are
supported.

The result is `String` instead of `char` because named character references
can expand into multiple characters.

## Examples

```rust ignore
use markdown::util::decode_character_reference::decode_named;

assert_eq!(decode_named("amp", true), "&");
assert_eq!(decode_named("AElig", true), "Æ");
assert_eq!(decode_named("aelig", true), "æ");
```

## References

*   [`wooorm/decode-named-character-reference`](https://github.com/wooorm/decode-named-character-reference)
*   [*§ 2.5 Entity and numeric character references* in `CommonMark`](https://spec.commonmark.org/0.31/#entity-and-numeric-character-references)
""".
-spec decode_named(Value, Html5) -> OptionName when
    Value :: binary(), Html5 :: boolean(), OptionName :: markdown_option:t(Name), Name :: binary().
decode_named(Value, Html5) when is_binary(Value) andalso Html5 =:= true ->
    case ?CHARACTER_REFERENCES of
        #{Value := Name} ->
            {some, Name};
        #{} ->
            none
    end;
decode_named(Value, Html5) when is_binary(Value) andalso Html5 =:= false ->
    case ?CHARACTER_REFERENCES_HTML_4 of
        #{Value := Name} ->
            {some, Name};
        #{} ->
            none
    end.

-doc """
Decode numeric character references.

Turn the number (in string form as either hexadecimal or decimal) coming
from a numeric character reference into a string.
The base of the string form must be passed as the `radix` parameter, as
`10` (decimal) or `16` (hexadecimal).

This returns a `String` form of the associated character or a replacement
character for C0 control characters (except for ASCII whitespace), C1
control characters, lone surrogates, noncharacters, and out of range
characters.

## Examples

```rust ignore
use markdown::util::decode_character_reference::decode_numeric;

assert_eq!(decode_numeric("123", 10), "{");
assert_eq!(decode_numeric("9", 16), "\t");
assert_eq!(decode_numeric("0", 10), "�"); // Not allowed.
```

## Panics

This function panics if a invalid string or an out of bounds valid string
is given.
It is expected that figuring out whether a number is allowed is handled in
the parser.
When `markdown-rs` is used, this function never panics.

## References

*   [`micromark-util-decode-numeric-character-reference` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-decode-numeric-character-reference)
*   [*§ 2.5 Entity and numeric character references* in `CommonMark`](https://spec.commonmark.org/0.31/#entity-and-numeric-character-references)
""".
-spec decode_numeric(Value, Radix) -> Numeric when Value :: binary(), Radix :: pos_integer(), Numeric :: binary().
decode_numeric(Value, Radix) when is_binary(Value) andalso ?is_pos_integer(Radix) ->
    CodePoint = erlang:binary_to_integer(Value, Radix),
    try <<CodePoint/utf8>> of
        <<Char/utf8>> ->
            case decode_numeric_is_invalid(Char) of
                false ->
                    <<Char/utf8>>;
                true ->
                    <<?REPLACEMENT_CHARACTER/utf8>>
            end
    catch
        error:badarg ->
            <<?REPLACEMENT_CHARACTER/utf8>>
    end.

-doc """
Decode a character reference.

This turns the number (in string form as either hexadecimal or decimal) or
name from a character reference into a string.

The marker specifies the format: `#` for hexadecimal, `x` for decimal, and
`&` for named.

The `html5` boolean is used for named character references, and specifier
whether the 2125 names from HTML 5 or the 252 names from HTML 4 are
supported.

## Panics

Panics if `marker` is not `b'&'`, `b'x'`, or `b'#'`.
""".
-spec decode(Value, Marker, Html5) -> OptionDecodedValue when
    Value :: binary(),
    Marker :: marker(),
    Html5 :: boolean(),
    OptionDecodedValue :: markdown_option:t(Name | Numeric),
    Name :: binary(),
    Numeric :: binary().
decode(Value, $#, Html5) when is_binary(Value) andalso is_boolean(Html5) -> {some, decode_numeric(Value, 10)};
decode(Value, $x, Html5) when is_binary(Value) andalso is_boolean(Html5) -> {some, decode_numeric(Value, 16)};
decode(Value, $&, Html5) when is_binary(Value) andalso is_boolean(Html5) -> decode_named(Value, Html5).

-doc """
Decode character references in a string.

> 👉 **Note**: this currently only supports the 252 named character
> references from HTML 4, as it's only used for JSX.
>
> If it's ever needed to support HTML 5 (which is what normal markdown
> uses), a boolean parameter can be added here.
""".
-spec parse(Value) -> Result when Value :: binary(), Result :: binary().
parse(Value) when is_binary(Value) ->
    parse_loop(Value, 0, byte_size(Value), <<>>, 0).

-doc """
Get the maximum size of a value for different kinds of references.

The value is the stuff after the markers, before the `;`.

## Panics

Panics if `marker` is not `b'&'`, `b'x'`, or `b'#'`.
""".
-spec value_max(Marker) -> Length when Marker :: marker(), Length :: markdown_types:usize().
value_max($&) -> ?CHARACTER_REFERENCE_NAMED_SIZE_MAX;
value_max($x) -> ?CHARACTER_REFERENCE_HEXADECIMAL_SIZE_MAX;
value_max($#) -> ?CHARACTER_REFERENCE_DECIMAL_SIZE_MAX.

-doc """
Get a test to check if a byte is allowed as a value for different kinds of
references.

The value is the stuff after the markers, before the `;`.

## Panics

Panics if `marker` is not `b'&'`, `b'x'`, or `b'#'`.
""".
-spec value_test(Marker, Byte) -> IsValid when Marker :: marker(), Byte :: byte(), IsValid :: boolean().
value_test($&, Byte) -> ?is_ascii_alphanumeric(Byte);
value_test($x, Byte) -> ?is_ascii_hexdigit(Byte);
value_test($#, Byte) -> ?is_ascii_digit(Byte).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec decode_numeric_is_invalid(Char) -> IsInvalid when Char :: char(), IsInvalid :: boolean().
decode_numeric_is_invalid(C) when
    (C >= $\0 andalso C =< $\x08) orelse (C =:= $\x0B) orelse (C >= $\x0E andalso C =< $\x1F)
->
    %% C0 except for HT, LF, FF, CR, space.
    true;
decode_numeric_is_invalid(C) when (C >= $\x7F andalso C =< $\x9F) ->
    %% Control character (DEL) of C0, and C1 controls.
    true;
decode_numeric_is_invalid(_) ->
    %% Lone surrogates, noncharacters, and out of range are handled by Erlang.
    false.

%% @private
-spec parse_loop(Value, Index, Len, Result, Start) -> FinalResult when
    Value :: binary(),
    Index :: non_neg_integer(),
    Len :: non_neg_integer(),
    Result :: binary(),
    Start :: non_neg_integer(),
    FinalResult :: binary().
parse_loop(Value, Index, Len, Result, Start) when Index < Len ->
    case binary:at(Value, Index) of
        $& ->
            {Marker, ValueStart} =
                case Index + 1 < Len andalso binary:at(Value, Index + 1) =:= $# of
                    true ->
                        case
                            Index + 2 < Len andalso
                                (binary:at(Value, Index + 2) =:= $x orelse
                                    binary:at(Value, Index + 2) =:= $X)
                        of
                            true -> {$x, Index + 3};
                            false -> {$#, Index + 2}
                        end;
                    false ->
                        {$&, Index + 1}
                end,

            Max = value_max(Marker),
            ValueIndex = find_value_end(Value, ValueStart, Len, Marker, 0, Max),
            ValueEnd = ValueStart + ValueIndex,

            case ValueIndex > 0 andalso ValueEnd < Len andalso binary:at(Value, ValueEnd) =:= $; of
                true ->
                    ValueBinary = binary:part(Value, ValueStart, ValueIndex),
                    case decode(ValueBinary, Marker, false) of
                        {some, Decoded} ->
                            Prefix = binary:part(Value, Start, Index - Start),
                            NewResult = <<Result/binary, Prefix/binary, Decoded/binary>>,
                            NewStart = ValueEnd + 1,
                            parse_loop(Value, NewStart, Len, NewResult, NewStart);
                        none ->
                            parse_loop(Value, Index + 1, Len, Result, Start)
                    end;
                false ->
                    parse_loop(Value, Index + 1, Len, Result, Start)
            end;
        _ ->
            parse_loop(Value, Index + 1, Len, Result, Start)
    end;
parse_loop(Value, _Index, _Len, Result, Start) ->
    Remaining = binary:part(Value, Start, byte_size(Value) - Start),
    <<Result/binary, Remaining/binary>>.

%% @private
-spec find_value_end(Value, ValueStart, Len, Marker, ValueIndex, Max) -> FinalValueIndex when
    Value :: binary(),
    ValueStart :: non_neg_integer(),
    Len :: non_neg_integer(),
    Marker :: marker(),
    ValueIndex :: non_neg_integer(),
    Max :: non_neg_integer(),
    FinalValueIndex :: non_neg_integer().
find_value_end(Value, ValueStart, Len, Marker, ValueIndex, Max) when
    ValueIndex < Max andalso (ValueStart + ValueIndex) < Len
->
    Byte = binary:at(Value, ValueStart + ValueIndex),
    case value_test(Marker, Byte) of
        true ->
            find_value_end(Value, ValueStart, Len, Marker, ValueIndex + 1, Max);
        false ->
            ValueIndex
    end;
find_value_end(_Value, _ValueStart, _Len, _Marker, ValueIndex, _Max) ->
    ValueIndex.
