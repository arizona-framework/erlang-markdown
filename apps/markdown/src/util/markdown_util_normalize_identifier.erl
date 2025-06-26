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
-module(markdown_util_normalize_identifier).
-moduledoc """
Normalize identifiers.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    normalize_identifier/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Normalize an identifier, as found in [references][label_end] and
[definitions][definition], so it can be compared when matching.

This collapsed whitespace found in markdown (`\t`, `\r`, `\n`, and ` `)
into one space, trims it (as in, dropping the first and last space), and
then performs unicode case folding twice: first by lowercasing uppercase
characters, and then uppercasing lowercase characters.

Some characters are considered “uppercase”, such as U+03F4 (`ϴ`), but if
their lowercase counterpart (U+03B8 (`θ`)) is uppercased will result in a
different uppercase character (U+0398 (`Θ`)).
Hence, to get that form, we perform both lower- and uppercase.

Performing these steps in that order works, but the inverse does not work.
To illustrate, say the source markdown containes two identifiers
`SS` (U+0053 U+0053) and `ẞ` (U+1E9E), which would be lowercased to
`ss` (U+0073 U+0073) and `ß` (U+00DF), and those in turn would both
uppercase to `SS` (U+0053 U+0053).
If we’d inverse the steps, for `ẞ`, we’d first uppercase without a
change, and then lowercase to `ß`, which would not match `ss`.

## Examples

```rust ignore
markdown::util::normalize_identifier::normalize_identifier;

assert_eq!(normalize_identifier(" a "), "a");
assert_eq!(normalize_identifier("a\t\r\nb"), "a b");
assert_eq!(normalize_identifier("ПРИВЕТ"), "привет");
assert_eq!(normalize_identifier("Привет"), "привет");
assert_eq!(normalize_identifier("привет"), "привет");
```

## References

*   [`micromark-util-normalize-identifier` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-normalize-identifier)

[definition]: crate::construct::definition
[label_end]: crate::construct::label_end
""".
-spec normalize_identifier(Value) -> Result when Value :: binary(), Result :: unicode:unicode_binary().
normalize_identifier(Value) when is_binary(Value) ->
    %% Note: it'll grow a bit smaller for consecutive whitespace.
    normalize_identifier(0, 0, Value, true, <<>>).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec normalize_identifier(Index, Start, Bytes, InWhitespace, Normalized) -> Normalized when
    Index :: non_neg_integer(),
    Start :: non_neg_integer(),
    Bytes :: binary(),
    InWhitespace :: boolean(),
    Normalized :: unicode:unicode_binary().
normalize_identifier(Index1, Start1, Bytes, InWhitespace1, Normalized1) when
    Index1 >= 0 andalso Index1 < byte_size(Bytes)
->
    case binary:at(Bytes, Index1) of
        C when (C =:= $\t orelse C =:= $\n orelse C =:= $\r orelse C =:= $\s) ->
            %% First whitespace we see after non-whitespace.
            case not InWhitespace1 of
                true ->
                    Slice = binary:part(Bytes, Start1, Index1 - Start1),
                    Normalized2 = <<Normalized1/bytes, Slice/bytes>>,
                    InWhitespace2 = true,
                    Index2 = Index1 + 1,
                    normalize_identifier(Index2, Start1, Bytes, InWhitespace2, Normalized2);
                false ->
                    Index2 = Index1 + 1,
                    normalize_identifier(Index2, Start1, Bytes, InWhitespace1, Normalized1)
            end;
        _ when InWhitespace1 =:= true ->
            %% First non-whitespace we see after whitespace.
            Normalized2 =
                case Start1 =/= 0 of
                    true ->
                        <<Normalized1/bytes, $\s:8>>;
                    false ->
                        Normalized1
                end,
            Start2 = Index1,
            InWhitespace2 = false,
            Index2 = Index1 + 1,
            normalize_identifier(Index2, Start2, Bytes, InWhitespace2, Normalized2);
        _ ->
            Index2 = Index1 + 1,
            normalize_identifier(Index2, Start1, Bytes, InWhitespace1, Normalized1)
    end;
normalize_identifier(_Index, Start, Bytes, InWhitespace, Normalized1) ->
    Normalized2 =
        case not InWhitespace of
            true ->
                Slice = binary:part(Bytes, Start, byte_size(Bytes) - Start),
                <<Normalized1/bytes, Slice/bytes>>;
            false ->
                Normalized1
        end,
    Normalized3 = markdown_types:unicode_string(Normalized2),
    Normalized4 = string:lowercase(Normalized3),
    Normalized5 = string:uppercase(Normalized4),
    Normalized6 = markdown_types:unicode_binary(Normalized5),
    Normalized6.
