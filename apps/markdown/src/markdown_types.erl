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
-module(markdown_types).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    dynamic_cast/1,
    unicode_binary/1,
    unicode_binary_lossy/1,
    unicode_length/1,
    unicode_next/1,
    unicode_string/1,
    unicode_string_lossy/1,
    usize_max/0
]).
%% Errors API
-export([
    format_error/2
]).

%% Types
-type option(T) :: none | {some, T}.
-type u8() :: 16#00..16#FF.
-type u16() :: 16#0000..16#FFFF.
-type u32() :: 16#00000000..16#FFFFFFFF.
-type u64() :: 16#0000000000000000..16#FFFFFFFFFFFFFFFF.
-type usize() :: u64().

-export_type([
    option/1,
    u8/0,
    u16/0,
    u32/0,
    u64/0,
    usize/0
]).

%% Macros
-define(REPLACEMENT_CHARACTER, 16#FFFD).
-define(is_unicode_codepoint1(C), ((C) >= 16#00 andalso (C) =< 16#7F)).
-define(is_unicode_codepoint2(C), ((C) >= 16#80 andalso (C) =< 16#7FF)).
-define(is_unicode_codepoint3(C),
    (((C) >= 16#800 andalso (C) =< 16#D7FF) orelse ((C) >= 16#E000 andalso (C) =< 16#FFFD))
).
-define(is_unicode_codepoint4(C), ((C) >= 16#10000 andalso (C) =< 16#10FFFF)).
-define(is_unicode_codepoint(C),
    (?is_unicode_codepoint1(C) orelse ?is_unicode_codepoint2(C) orelse ?is_unicode_codepoint3(C) orelse
        ?is_unicode_codepoint4(C))
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-compile({inline, [dynamic_cast/1]}).
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.

-spec unicode_binary(CharData) -> UnicodeBinary when
    CharData :: unicode:chardata(), UnicodeBinary :: unicode:unicode_binary().
unicode_binary(CharData) when is_binary(CharData) orelse is_list(CharData) ->
    case unicode:characters_to_binary(CharData, utf8) of
        UnicodeBinary when is_binary(UnicodeBinary) ->
            UnicodeBinary;
        {error, Encoded, Rest} ->
            error_with_info(badarg, [CharData], #{
                1 => {unicode_error, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            });
        {incomplete, Encoded, Rest} ->
            error_with_info(badarg, [CharData], #{
                1 => {unicode_incomplete, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            })
    end.

-spec unicode_binary_lossy(CharData) -> UnicodeBinary when
    CharData :: unicode:chardata(), UnicodeBinary :: unicode:unicode_binary().
unicode_binary_lossy(CharData) when is_binary(CharData) orelse is_list(CharData) ->
    case unicode:characters_to_binary(CharData, utf8) of
        UnicodeBinary when is_binary(UnicodeBinary) ->
            UnicodeBinary;
        {Reason, Encoded, InvalidCharData} when (Reason =:= error orelse Reason =:= incomplete) ->
            unicode_binary_lossy(Encoded, InvalidCharData)
    end.

-spec unicode_next(CharData) -> {Rest, Result} when
    CharData :: unicode:chardata(),
    Rest :: unicode:chardata(),
    Result :: {ok, OptionCodePoint} | {error, Error},
    OptionCodePoint :: markdown_option:t(CodePoint),
    CodePoint :: char(),
    Error ::
        {incomplete_sequence, SequenceLength}
        | {invalid_codepoint, InvalidCodePoint}
        | {malformed_utf8_byte, MalformedUtf8Byte}
        | {overlong_encoding, SequenceLength}
        | {unexpected_continuation_byte, ContinuationByte},
    InvalidCodePoint :: non_neg_integer(),
    MalformedUtf8Byte :: byte(),
    SequenceLength :: pos_integer(),
    ContinuationByte :: byte().
unicode_next(CharData) when is_binary(CharData) ->
    case CharData of
        Rest = <<>> ->
            {Rest, {ok, none}};
        <<CodePoint/utf8, Rest/bytes>> ->
            {Rest, {ok, {some, CodePoint}}};
        % 1-byte sequence (0xxxxxxx)
        <<2#0:1, CodePoint:7, Rest/bytes>> ->
            {Rest, {ok, {some, CodePoint}}};
        % 2-byte sequence (110xxxxx 10xxxxxx)
        <<2#110:3, FirstByte:5, 2#10:2, SecondByte:6, Rest/bytes>> ->
            CodePoint = (FirstByte bsl 6) + SecondByte,
            % Valid range check for 2-byte sequence: 0x80 to 0x7FF
            case CodePoint >= 16#80 andalso CodePoint =< 16#7FF of
                true ->
                    {Rest, {ok, {some, CodePoint}}};
                false ->
                    {Rest, {error, {invalid_codepoint, CodePoint}}}
            end;
        % 3-byte sequence (1110xxxx 10xxxxxx 10xxxxxx)
        <<2#1110:4, FirstByte:4, 2#10:2, SecondByte:6, 2#10:2, ThirdByte:6, Rest/bytes>> ->
            CodePoint = (FirstByte bsl 12) + (SecondByte bsl 6) + ThirdByte,
            % Valid range check for 3-byte sequence: 0x800 to 0xFFFF
            % Excluding surrogate pairs (0xD800-0xDFFF)
            case
                (CodePoint >= 16#800 andalso CodePoint =< 16#FFFF) andalso
                    not (CodePoint >= 16#D800 andalso CodePoint =< 16#DFFF)
            of
                true ->
                    {Rest, {ok, {some, CodePoint}}};
                false ->
                    {Rest, {error, {invalid_codepoint, CodePoint}}}
            end;
        % 4-byte sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        <<2#11110:5, FirstByte:3, 2#10:2, SecondByte:6, 2#10:2, ThirdByte:6, 2#10:2, FourthByte:6, Rest/bytes>> ->
            CodePoint = (FirstByte bsl 18) + (SecondByte bsl 12) + (ThirdByte bsl 6) + FourthByte,
            % Valid range check for 4-byte sequence: 0x10000 to 0x10FFFF
            case CodePoint >= 16#10000 andalso CodePoint =< 16#10FFFF of
                true ->
                    {Rest, {ok, {some, CodePoint}}};
                false ->
                    {Rest, {error, {invalid_codepoint, CodePoint}}}
            end;
        % Invalid starting byte
        <<FirstByte:8, Rest/bytes>> when (FirstByte band 2#11000000) =:= 2#10000000 ->
            {Rest, {error, {unexpected_continuation_byte, FirstByte}}};
        % Overlong encoding
        <<2#110:3, 0:4, _:1, Rest/bytes>> ->
            {Rest, {error, {overlong_encoding, 1}}};
        <<2#1110:4, 0:4, 2#10:2, 0:5, _:1, Rest/bytes>> ->
            {Rest, {error, {overlong_encoding, 2}}};
        <<2#11110:5, 0:3, 2#10:2, 0:6, 2#10:2, 0:6, Rest/bytes>> ->
            {Rest, {error, {overlong_encoding, 3}}};
        % Incomplete sequence detected
        <<2#11110:5, _:3, 2#10:2, _:6, 2#10:2, _:6, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 4}}};
        <<2#11110:5, _:3, 2#10:2, _:6, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 4}}};
        <<2#11110:5, _:3, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 4}}};
        <<2#1110:4, _:4, 2#10:2, _:6, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 3}}};
        <<2#1110:4, _:4, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 3}}};
        <<2#110:3, _:5, Rest/bytes>> ->
            {Rest, {error, {incomplete_sequence, 2}}};
        <<MalformedUtf8Byte:8, Rest/bytes>> ->
            {Rest, {error, {malformed_utf8_byte, MalformedUtf8Byte}}}
    end;
unicode_next(Rest = []) ->
    {Rest, {ok, none}};
unicode_next([CodePoint | Rest]) when ?is_non_neg_integer(CodePoint) andalso ?is_unicode_codepoint(CodePoint) ->
    {unicode_chardata(Rest), {ok, {some, CodePoint}}};
unicode_next([InvalidCodePoint | Rest]) when ?is_non_neg_integer(InvalidCodePoint) ->
    {unicode_chardata(Rest), {error, {invalid_codepoint, InvalidCodePoint}}};
unicode_next([<<>> | Rest]) ->
    unicode_next(unicode_chardata(Rest));
unicode_next([Bytes | Rest]) when is_binary(Bytes) ->
    case unicode_next(Bytes) of
        {<<>>, Result} ->
            {unicode_chardata(Rest), Result};
        {MoreBytes, Result} ->
            {[MoreBytes | Rest], Result}
    end;
unicode_next([[] | Rest]) ->
    unicode_next(unicode_chardata(Rest));
unicode_next([Chars | Rest]) when is_list(Chars) ->
    case unicode_next(Chars) of
        {[], Result} ->
            {unicode_chardata(Rest), Result};
        {MoreChars, Result} ->
            {[MoreChars | Rest], Result}
    end.

-spec unicode_length(UnicodeBinary | UnicodeString) -> UnicodeLength when
    UnicodeBinary :: unicode:unicode_binary(), UnicodeString :: string(), UnicodeLength :: non_neg_integer().
unicode_length(UnicodeBinary) when is_binary(UnicodeBinary) ->
    unicode_binary_length(UnicodeBinary, <<>>, 0);
unicode_length(UnicodeString) when is_list(UnicodeString) ->
    unicode_string_length(UnicodeString, <<>>, 0).

-spec unicode_string(CharData) -> UnicodeString when CharData :: unicode:chardata(), UnicodeString :: string().
unicode_string(CharData) when is_binary(CharData) orelse is_list(CharData) ->
    case unicode:characters_to_list(CharData, utf8) of
        UnicodeString when is_list(UnicodeString) ->
            UnicodeString;
        {error, Encoded, Rest} ->
            error_with_info(badarg, [CharData], #{
                1 => {unicode_error, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            });
        {incomplete, Encoded, Rest} ->
            error_with_info(badarg, [CharData], #{
                1 => {unicode_incomplete, #{encoded => Encoded, length => unicode_length(Encoded), rest => Rest}}
            })
    end.

-spec unicode_string_lossy(CharData) -> UnicodeString when CharData :: unicode:chardata(), UnicodeString :: string().
unicode_string_lossy(CharData) when is_binary(CharData) orelse is_list(CharData) ->
    case unicode:characters_to_list(CharData, utf8) of
        UnicodeString when is_list(UnicodeString) ->
            UnicodeString;
        {Reason, Encoded, InvalidCharData} when (Reason =:= error orelse Reason =:= incomplete) ->
            unicode_string_lossy(Encoded, InvalidCharData)
    end.

-compile({inline, [usize_max/0]}).
-spec usize_max() -> usize().
usize_max() ->
    16#FFFFFFFFFFFFFFFF.

%%%=============================================================================
%%% Errors API functions
%%%=============================================================================

%% @private
-compile({inline, [error_with_info/3]}).
-spec error_with_info(dynamic(), dynamic(), dynamic()) -> no_return().
error_with_info(Reason, Args, Cause) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE, cause => Cause}}]).

-spec format_error(dynamic(), dynamic()) -> dynamic().
format_error(_Reason, [{_M, _F, _As, Info} | _]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorDescription1 = maps:get(cause, ErrorInfo),
    ErrorDescription2 = maps:map(fun format_error_description/2, ErrorDescription1),
    ErrorDescription2.

%% @private
-spec format_error_description(dynamic(), dynamic()) -> dynamic().
format_error_description(_Key, {unicode_error, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("invalid Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, {unicode_incomplete, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("incomplete Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, {unicode_length_error, #{encoded := _Encoded, length := Length, rest := Rest}}) ->
    io_lib:format("invalid length calculation of Unicode chardata at position ~w: ~0tP", [Length, Rest, 5]);
format_error_description(_Key, Value) ->
    Value.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec unicode_binary_length(unicode:unicode_binary(), unicode:unicode_binary(), non_neg_integer()) -> non_neg_integer().
unicode_binary_length(<<C/utf8, Rest/bytes>>, Encoded, Length) ->
    unicode_binary_length(Rest, <<C/utf8, Encoded/bytes>>, Length + 1);
unicode_binary_length(<<>>, _Encoded, Length) ->
    Length;
unicode_binary_length(<<Rest/bytes>>, Encoded, Length) ->
    error_with_info(badarg, [Rest, Encoded, Length], #{
        1 => {unicode_length_error, #{encoded => Encoded, length => Length, rest => Rest}}
    }).

%% @private
-spec unicode_binary_lossy(Encoded, InvalidCharData) -> Encoded when
    Encoded :: unicode:unicode_binary(), InvalidCharData :: unicode:chardata().
unicode_binary_lossy(Encoded, InvalidCharData) ->
    case unicode_next(InvalidCharData) of
        {<<>>, {ok, none}} ->
            Encoded;
        {[], {ok, none}} ->
            Encoded;
        {_Rest, {ok, {some, _}}} ->
            Tail = unicode_binary_lossy(InvalidCharData),
            <<Encoded/bytes, Tail/bytes>>;
        {Rest, {error, _}} ->
            unicode_binary_lossy(<<Encoded/bytes, ?REPLACEMENT_CHARACTER/utf8>>, Rest)
    end.

%% @private
-spec unicode_chardata(CharData | CodePoint) -> CharData when CharData :: unicode:chardata(), CodePoint :: char().
unicode_chardata(CharData) when is_binary(CharData) ->
    CharData;
unicode_chardata([<<>> | CharData]) ->
    unicode_chardata(CharData);
unicode_chardata([[] | CharData]) ->
    unicode_chardata(CharData);
unicode_chardata(CharData) when is_list(CharData) ->
    CharData;
unicode_chardata(CodePoint) when ?is_non_neg_integer(CodePoint) ->
    [CodePoint].

%% @private
-spec unicode_string_length(string(), unicode:unicode_binary(), non_neg_integer()) -> non_neg_integer().
unicode_string_length([C | Rest], Encoded, Length) when ?is_unicode_codepoint(C) ->
    unicode_string_length(Rest, <<C/utf8, Encoded/bytes>>, Length + 1);
unicode_string_length([], _Encoded, Length) ->
    Length;
unicode_string_length(Rest, Encoded, Length) ->
    error_with_info(badarg, [Rest, Encoded, Length], #{
        1 => {unicode_length_error, #{encoded => Encoded, length => Length, rest => Rest}}
    }).

%% @private
-spec unicode_string_lossy(Encoded, InvalidCharData) -> Encoded when
    Encoded :: string(), InvalidCharData :: unicode:chardata().
unicode_string_lossy(Encoded, InvalidCharData) ->
    case unicode_next(InvalidCharData) of
        {<<>>, {ok, none}} ->
            Encoded;
        {[], {ok, none}} ->
            Encoded;
        {_Rest, {ok, {some, _}}} ->
            Tail = unicode_string_lossy(InvalidCharData),
            Encoded ++ Tail;
        {Rest, {error, _}} ->
            unicode_string_lossy(Encoded ++ [?REPLACEMENT_CHARACTER], Rest)
    end.
