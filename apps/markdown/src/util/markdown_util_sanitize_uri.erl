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
-module(markdown_util_sanitize_uri).
-moduledoc """
Make urls safe.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    protocols/1,
    sanitize/1,
    sanitize_with_protocols/2
]).

%% Types
-type protocol_list() :: list(binary()).
-type protocol_set() :: {set, sets:set(binary())}.
-type protocols() :: protocol_list() | protocol_set().

-export_type([
    protocol_list/0,
    protocol_set/0,
    protocols/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec protocols(ProtocolList | ProtocolSet) -> ProtocolSet when
    ProtocolList :: protocol_list(), ProtocolSet :: protocol_set().
protocols(ProtocolList) when is_list(ProtocolList) ->
    {set, sets:from_list(ProtocolList, [{version, 2}])};
protocols(ProtocolSet = {set, _}) ->
    ProtocolSet.

-doc """
Make a value safe for injection as a URL.

This encodes unsafe characters with percent-encoding and skips already
encoded sequences (see `normalize` below).
Further unsafe characters are encoded as character references (see
`encode`).

## Examples

```rust ignore
use markdown::util::sanitize_uri::sanitize;

assert_eq!(sanitize("javascript:alert(1)"), "javascript:alert(1)");
assert_eq!(sanitize("https://a👍b.c/%20/%"), "https://a%F0%9F%91%8Db.c/%20/%25");
```

## References

*   [`micromark-util-sanitize-uri` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-sanitize-uri)
""".
-spec sanitize(Value) -> SanitizedValue when Value :: unicode:unicode_binary(), SanitizedValue :: binary().
sanitize(Value) when is_binary(Value) ->
    markdown_util_encode:encode(normalize(Value), true).

-doc """
Make a value safe for injection as a URL, and check protocols.

This first uses [`sanitize`][].
Then, a vec of (lowercase) allowed protocols can be given, in which case
the URL is ignored or kept.

For example, `&["http", "https", "irc", "ircs", "mailto", "xmpp"]`
can be used for `a[href]`, or `&["http", "https"]` for `img[src]`.
If the URL includes an unknown protocol (one not matched by `protocol`, such
as a dangerous example, `javascript:`), the value is ignored.

## Examples

```rust ignore
use markdown::util::sanitize_uri::sanitize_with_protocols;

assert_eq!(sanitize_with_protocols("javascript:alert(1)", &["http", "https"]), "");
assert_eq!(sanitize_with_protocols("https://example.com", &["http", "https"]), "https://example.com");
assert_eq!(sanitize_with_protocols("https://a👍b.c/%20/%", &["http", "https"]), "https://a%F0%9F%91%8Db.c/%20/%25");
```

## References

*   [`micromark-util-sanitize-uri` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-sanitize-uri)
""".
-spec sanitize_with_protocols(Value, Protocols) -> SanitizedValue when
    Value :: unicode:unicode_binary(), Protocols :: protocols(), SanitizedValue :: binary().
sanitize_with_protocols(Value, Protocols) when is_binary(Value) ->
    {set, ProtocolSet} = protocols(Protocols),
    SanitizedValue = sanitize(Value),
    OptionEnd =
        case binary:match(SanitizedValue, [<<"?">>, <<"#">>, <<"/">>]) of
            nomatch -> none;
            {EndStart, _EndLength} -> {some, EndStart}
        end,
    OptionColon1 =
        case binary:match(SanitizedValue, [<<":">>]) of
            nomatch -> none;
            {ColonStart, _ColonLength} -> {some, ColonStart}
        end,
    %% If the first colon is after `?`, `#`, or `/`, it’s not a protocol.
    OptionColon2 =
        case {OptionEnd, OptionColon1} of
            {{some, End}, {some, Colon1}} when Colon1 > End ->
                none;
            _ ->
                OptionColon1
        end,
    %% If there is no protocol, it’s relative, and fine.
    case OptionColon2 of
        {some, Colon2} ->
            %% If it is a protocol, it should be allowed.
            Protocol = markdown_types:unicode_binary(
                string:casefold(markdown_types:unicode_string(binary:part(SanitizedValue, 0, Colon2)))
            ),
            case not sets:is_element(Protocol, ProtocolSet) of
                true ->
                    <<>>;
                false ->
                    SanitizedValue
            end;
        none ->
            SanitizedValue
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-doc """
Normalize a URL (such as used in [definitions][definition],
[references][label_end]).

It encodes unsafe characters with percent-encoding, skipping already encoded
sequences.

## Examples

```rust ignore
use markdown::util::sanitize_uri::normalize;

assert_eq!(sanitize_uri("https://example.com"), "https://example.com");
assert_eq!(sanitize_uri("https://a👍b.c/%20/%"), "https://a%F0%9F%91%8Db.c/%20/%25");
```

## References

*   [`micromark-util-sanitize-uri` in `micromark`](https://github.com/micromark/micromark/tree/main/packages/micromark-util-sanitize-uri)

[definition]: crate::construct::definition
[label_end]: crate::construct::label_end
""".
-spec normalize(Value) -> NormalizedValue when Value :: unicode:unicode_binary(), NormalizedValue :: binary().
normalize(Value) when is_binary(Value) ->
    %% Note: it’ll grow bigger for each non-ascii or non-safe character.
    normalize_loop(Value, <<>>).

%% @private
-spec normalize_loop(Bytes, Acc) -> Acc when Bytes :: unicode:unicode_binary(), Acc :: binary().
normalize_loop(<<A:8, B:8, C:8, Rest/bytes>>, Acc1) when
    A =:= $% andalso ?is_ascii_alphanumeric(B) andalso ?is_ascii_alphanumeric(C)
->
    %% A correct percent encoded value.
    Acc2 = <<Acc1/bytes, A:8, B:8, C:8>>,
    normalize_loop(Rest, Acc2);
normalize_loop(<<C/utf8, Rest/bytes>>, Acc1) when
    C >= 16#80 orelse
        not (C =:= $! orelse C =:= $# orelse C =:= $$ orelse (C >= $& andalso C =< $;) orelse C =:= $= orelse
            (C >= $? andalso C =< $Z) orelse C =:= $_ orelse (C >= $a andalso C =< $z) orelse C =:= $~)
->
    Acc2 = <<Acc1/bytes, "%", (binary:encode_hex(<<C/utf8>>, uppercase))/bytes>>,
    normalize_loop(Rest, Acc2);
normalize_loop(<<C/utf8, Rest/bytes>>, Acc1) ->
    Acc2 = <<Acc1/bytes, C/utf8>>,
    normalize_loop(Rest, Acc2);
normalize_loop(<<>>, Acc) ->
    Acc.
