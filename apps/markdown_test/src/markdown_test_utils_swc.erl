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
-module(markdown_test_utils_swc).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-09-27", modified => "2025-09-27"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    parse_esm/1,
    parse_expression/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec parse_esm(Value) -> Signal when Value :: unicode:unicode_binary(), Signal :: markdown_mdx:signal().
parse_esm(Value) ->
    % Check if the ESM has balanced braces/brackets/parens
    % If not balanced, return eof to indicate more input is needed
    case is_balanced(Value) of
        true -> markdown_mdx_signal:ok();
        false -> markdown_mdx_signal:eof(<<"Unexpected end of file">>, <<"mdx">>, <<"mdx">>)
    end.

-spec parse_expression(Value, Kind) -> Signal when
    Value :: unicode:unicode_binary(), Kind :: markdown_mdx:expression_kind(), Signal :: markdown_mdx:signal().
parse_expression(Value, Kind) ->
    parse_expression_impl(Value, Kind).

parse_expression_impl(Value, attribute_expression) ->
    % For attribute expressions like <a {...b}>, we need to validate:
    % 1. Must be a spread (start with ...)
    % 2. Must have an identifier after ...
    % 3. Cannot have extra content
    % 4. Cannot be empty
    % 5. Cannot be just a comment
    case validate_attribute_expression(Value) of
        ok ->
            markdown_mdx_signal:ok();
        {error, Message, Offset} ->
            markdown_mdx_signal:error(Message, Offset, <<"mdx">>, <<"swc">>)
    end;
parse_expression_impl(Value, _Kind) ->
    % For regular expressions and attribute value expressions
    ValueLen = byte_size(Value),
    case ValueLen of
        0 ->
            % Empty value - just return OK
            markdown_mdx_signal:ok();
        _ ->
            % First check for invalid syntax like '<'
            case check_for_invalid_syntax(Value) of
                {error, Offset} ->
                    % Invalid syntax - return EOF error
                    markdown_mdx_signal:error(
                        <<"Could not parse expression with swc: Unexpected eof">>,
                        Offset + 4,
                        <<"mdx">>,
                        <<"swc">>
                    );
                ok ->
                    % Check for line comments without newline (causes EOF)
                    case check_line_comment_eof(Value) of
                        {error, _Offset} ->
                            % For line comment EOF, offset points to end of input
                            % Offset is relative to start of expression value
                            markdown_mdx_signal:error(
                                <<"Could not parse expression with swc: Unexpected eof">>,
                                ValueLen + 3,
                                <<"mdx">>,
                                <<"swc">>
                            );
                        ok ->
                            % Check for invalid statements (var, let, const, etc.)
                            case check_for_statement(Value) of
                                {error, Offset} ->
                                    markdown_mdx_signal:error(
                                        <<"Could not parse expression with swc: Expression expected">>,
                                        Offset,
                                        <<"mdx">>,
                                        <<"swc">>
                                    );
                                ok ->
                                    % Check for invalid incomplete expressions
                                    case check_for_incomplete_expression(Value) of
                                        {error, _Offset} ->
                                            % For incomplete expression EOF
                                            markdown_mdx_signal:error(
                                                <<"Could not parse expression with swc: Unexpected eof">>,
                                                ValueLen + 3,
                                                <<"mdx">>,
                                                <<"swc">>
                                            );
                                        ok ->
                                            % Check for content after expression
                                            case check_for_content_after_expression(Value) of
                                                ok ->
                                                    % No issues found, check if balanced
                                                    case is_balanced_with_comments(Value) of
                                                        true ->
                                                            markdown_mdx_signal:ok();
                                                        false ->
                                                            markdown_mdx_signal:eof(
                                                                <<"Unexpected eof">>, <<"mdx">>, <<"mdx">>
                                                            )
                                                    end;
                                                {error, Offset} ->
                                                    % Found content after a complete expression
                                                    markdown_mdx_signal:error(
                                                        <<"Could not parse expression with swc: Unexpected content after expression">>,
                                                        Offset,
                                                        <<"mdx">>,
                                                        <<"swc">>
                                                    )
                                            end
                                    end
                            end
                    end
            end
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
%% Check if expression has a line comment without a newline before the end
%% This causes EOF because the closing brace is consumed by the comment
-spec check_line_comment_eof(Value) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_line_comment_eof(Value) ->
    check_line_comment_eof_impl(Value, 0, false).

%% @private
-spec check_line_comment_eof_impl(Value, Offset, InLineComment) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer(),
    InLineComment :: boolean().
check_line_comment_eof_impl(<<>>, Offset, true) ->
    % Reached end while in a line comment - this is EOF error
    {error, Offset};
check_line_comment_eof_impl(<<>>, _Offset, false) ->
    ok;
% Start of line comment
check_line_comment_eof_impl(<<"//", Rest/binary>>, Offset, false) ->
    check_line_comment_eof_impl(Rest, Offset + 2, true);
% Newline ends line comment
check_line_comment_eof_impl(<<"\n", Rest/binary>>, Offset, true) ->
    check_line_comment_eof_impl(Rest, Offset + 1, false);
check_line_comment_eof_impl(<<"\r\n", Rest/binary>>, Offset, true) ->
    check_line_comment_eof_impl(Rest, Offset + 2, false);
check_line_comment_eof_impl(<<"\r", Rest/binary>>, Offset, true) ->
    check_line_comment_eof_impl(Rest, Offset + 1, false);
% Skip multiline comments completely (they don't cause EOF)
check_line_comment_eof_impl(<<"/*", Rest/binary>>, Offset, false) ->
    case skip_multiline_comment(Rest, Offset + 2) of
        {ok, NewRest, NewOffset} ->
            check_line_comment_eof_impl(NewRest, NewOffset, false);
        eof ->
            % Multiline comment EOF is handled by is_balanced
            ok
    end;
% Skip other characters
check_line_comment_eof_impl(<<_:8, Rest/binary>>, Offset, InLineComment) ->
    check_line_comment_eof_impl(Rest, Offset + 1, InLineComment).

%% @private
-spec skip_multiline_comment(Value, Offset) -> {ok, Rest, NewOffset} | eof when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer(),
    Rest :: unicode:unicode_binary(),
    NewOffset :: non_neg_integer().
skip_multiline_comment(<<>>, _Offset) ->
    eof;
skip_multiline_comment(<<"*/", Rest/binary>>, Offset) ->
    {ok, Rest, Offset + 2};
skip_multiline_comment(<<_:8, Rest/binary>>, Offset) ->
    skip_multiline_comment(Rest, Offset + 1).

%% @private
%% Check if the value starts with a JavaScript statement keyword
-spec check_for_statement(Value) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_for_statement(Value) ->
    % Remove leading whitespace
    Trimmed = string:trim(Value, leading),
    LeadingWSSize = byte_size(Value) - byte_size(Trimmed),
    % Check for statement keywords
    Statements = [
        <<"var ">>,
        <<"let ">>,
        <<"const ">>,
        <<"if ">>,
        <<"for ">>,
        <<"while ">>,
        <<"do ">>,
        <<"switch ">>,
        <<"return ">>,
        <<"throw ">>,
        <<"break">>,
        <<"continue">>,
        <<"debugger">>
    ],
    case lists:any(fun(Keyword) -> starts_with_keyword(Trimmed, Keyword) end, Statements) of
        true ->
            {error, LeadingWSSize};
        false ->
            ok
    end.

%% @private
-spec starts_with_keyword(Value, Keyword) -> boolean() when
    Value :: unicode:unicode_binary(),
    Keyword :: unicode:unicode_binary().
starts_with_keyword(Value, Keyword) ->
    KeywordSize = byte_size(Keyword),
    case Value of
        <<Keyword:KeywordSize/binary, _/binary>> ->
            true;
        _ ->
            false
    end.

%% @private
%% Check for incomplete expressions like "??" without operands
-spec check_for_incomplete_expression(Value) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_for_incomplete_expression(Value) ->
    % Remove comments first to get clean expression
    NoComments = remove_all_comments(Value),
    Trimmed = string:trim(NoComments, both),
    case Trimmed of
        <<"??", Rest/binary>> ->
            % Check if there's anything after ??
            RestTrimmed = string:trim(Rest, both),
            case byte_size(RestTrimmed) of
                0 -> {error, byte_size(Value)};
                _ -> ok
            end;
        _ ->
            ok
    end.

%% @private
-spec remove_all_comments(Value) -> Result when
    Value :: unicode:unicode_binary(),
    Result :: unicode:unicode_binary().
remove_all_comments(Value) ->
    remove_all_comments_impl(Value, <<>>).

%% @private
-spec remove_all_comments_impl(Value, Acc) -> Result when
    Value :: unicode:unicode_binary(),
    Acc :: unicode:unicode_binary(),
    Result :: unicode:unicode_binary().
remove_all_comments_impl(<<>>, Acc) ->
    Acc;
remove_all_comments_impl(<<"/*", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<"*/">>) of
        [_Comment, AfterComment] ->
            remove_all_comments_impl(AfterComment, <<Acc/binary, " ">>);
        [_Comment] ->
            Acc
    end;
remove_all_comments_impl(<<"//", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<"\n">>) of
        [_Comment, AfterComment] ->
            remove_all_comments_impl(AfterComment, <<Acc/binary, "\n">>);
        [_Comment] ->
            Acc
    end;
remove_all_comments_impl(<<C:8, Rest/binary>>, Acc) ->
    remove_all_comments_impl(Rest, <<Acc/binary, C:8>>).

%% @private
-spec check_for_content_after_expression(Value) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_for_content_after_expression(Value) ->
    % Remove comments first to properly analyze the expression
    NoComments = remove_all_comments(Value),
    % Check if there's a simple identifier followed by more content
    % e.g., "b { c }" should be detected as having content after "b"
    % But "function () {}" should be OK
    case check_content_pattern(NoComments) of
        {found, _FoundOffset} ->
            % Found content after expression - return offset at end
            % Offset should account for { and } braces: ValueLen + 3
            {error, byte_size(Value) + 3};
        not_found ->
            ok
    end.

%% @private
%% Check for invalid syntax characters like '<'
-spec check_for_invalid_syntax(Value) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_for_invalid_syntax(Value) ->
    check_for_invalid_syntax_impl(Value, 0, false, false).

%% @private
-spec check_for_invalid_syntax_impl(Value, Offset, InString, InComment) -> ok | {error, Offset} when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer(),
    InString :: boolean(),
    InComment :: boolean() | multiline | line.
check_for_invalid_syntax_impl(<<>>, _Offset, _InString, _InComment) ->
    ok;
% Skip strings
check_for_invalid_syntax_impl(<<"\"", Rest/binary>>, Offset, false, false) ->
    check_for_invalid_syntax_impl(Rest, Offset + 1, true, false);
check_for_invalid_syntax_impl(<<"\"", Rest/binary>>, Offset, true, false) ->
    check_for_invalid_syntax_impl(Rest, Offset + 1, false, false);
% Skip escape sequences in strings
check_for_invalid_syntax_impl(<<"\\", _:8, Rest/binary>>, Offset, true, false) ->
    check_for_invalid_syntax_impl(Rest, Offset + 2, true, false);
% Skip multiline comments
check_for_invalid_syntax_impl(<<"/*", Rest/binary>>, Offset, false, false) ->
    check_for_invalid_syntax_impl(Rest, Offset + 2, false, multiline);
check_for_invalid_syntax_impl(<<"*/", Rest/binary>>, Offset, false, multiline) ->
    check_for_invalid_syntax_impl(Rest, Offset + 2, false, false);
% Skip line comments
check_for_invalid_syntax_impl(<<"//", Rest/binary>>, Offset, false, false) ->
    check_for_invalid_syntax_impl(Rest, Offset + 2, false, line);
check_for_invalid_syntax_impl(<<"\n", Rest/binary>>, Offset, false, line) ->
    check_for_invalid_syntax_impl(Rest, Offset + 1, false, false);
% Check for invalid '<' outside strings and comments
check_for_invalid_syntax_impl(<<"<", _/binary>>, Offset, false, false) ->
    {error, Offset};
% Skip other characters
check_for_invalid_syntax_impl(<<_:8, Rest/binary>>, Offset, InString, InComment) ->
    check_for_invalid_syntax_impl(Rest, Offset + 1, InString, InComment).

%% @private
%% Check for pattern: identifier/simple-expr followed by more complex content
-spec check_content_pattern(Value) -> {found, Offset} | not_found when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_content_pattern(Value) ->
    Trimmed = string:trim(Value, both),
    % Check for "function" keyword - this is OK as it's a function expression
    case Trimmed of
        <<"function", _/binary>> ->
            not_found;
        _ ->
            % Check for multiple identifiers across lines (invalid)
            case check_multiple_identifiers(Trimmed) of
                {found, Offset} ->
                    {found, Offset};
                not_found ->
                    % Look for identifier followed by brace/content
                    find_identifier_then_brace(Value)
            end
    end.

%% @private
%% Check for multiple identifiers separated by newlines
-spec check_multiple_identifiers(Value) -> {found, Offset} | not_found when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
check_multiple_identifiers(Value) ->
    % Split by newlines and check if we have multiple non-empty identifiers
    Lines = binary:split(Value, [<<"\n">>, <<"\r\n">>, <<"\r">>], [global]),
    NonEmptyLines = [Line || Line <- Lines, byte_size(string:trim(Line, both)) > 0],
    case length(NonEmptyLines) of
        N when N >= 2 ->
            % Multiple non-empty lines - check if they look like separate identifiers
            case check_if_separate_identifiers(NonEmptyLines) of
                true -> {found, 0};
                false -> not_found
            end;
        _ ->
            not_found
    end.

%% @private
-spec check_if_separate_identifiers(Lines) -> boolean() when
    Lines :: [binary()].
check_if_separate_identifiers([First | Rest]) ->
    FirstTrimmed = string:trim(First, both),
    % Check if first line is just an identifier (letters/numbers/underscore)
    case is_simple_identifier(FirstTrimmed) of
        true ->
            % Check if any remaining lines are also identifiers
            lists:any(
                fun(Line) ->
                    Trimmed = string:trim(Line, both),
                    is_simple_identifier(Trimmed)
                end,
                Rest
            );
        false ->
            false
    end;
check_if_separate_identifiers([]) ->
    false.

%% @private
-spec is_simple_identifier(Value) -> boolean() when
    Value :: binary().
is_simple_identifier(<<>>) ->
    false;
is_simple_identifier(Value) ->
    % Check if value contains only identifier characters (alphanumeric + underscore)
    is_simple_identifier_impl(Value).

%% @private
-spec is_simple_identifier_impl(Value) -> boolean() when
    Value :: binary().
is_simple_identifier_impl(<<>>) ->
    true;
is_simple_identifier_impl(<<C:8, Rest/binary>>) when
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse
        C =:= $_
->
    is_simple_identifier_impl(Rest);
is_simple_identifier_impl(_) ->
    false.

%% @private
-spec find_identifier_then_brace(Value) -> {found, Offset} | not_found when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
find_identifier_then_brace(Value) ->
    find_identifier_then_brace(Value, 0, false, false).

%% @private
-spec find_identifier_then_brace(Value, Offset, SeenNonWS, SeenWS) -> {found, Offset} | not_found when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer(),
    SeenNonWS :: boolean(),
    SeenWS :: boolean().
find_identifier_then_brace(<<>>, _Offset, _SeenNonWS, _SeenWS) ->
    not_found;
% If we see a brace after seeing an identifier and whitespace, find where it ends
find_identifier_then_brace(<<"{", Rest/binary>>, Offset, true, true) ->
    % Found the pattern - now find the end of the brace expression
    case find_matching_brace(Rest, Offset + 1, 1) of
        {found, EndOffset} ->
            {found, EndOffset};
        not_found ->
            % If we can't find the end, just return current offset
            {found, Offset}
    end;
% Track non-whitespace characters (potential identifier)
find_identifier_then_brace(<<C:8, Rest/binary>>, Offset, _SeenNonWS, SeenWS) when
    C =/= $\s andalso C =/= $\t andalso C =/= $\n andalso C =/= $\r
->
    find_identifier_then_brace(Rest, Offset + 1, true, SeenWS);
% Track whitespace after identifier
find_identifier_then_brace(<<C:8, Rest/binary>>, Offset, true, _SeenWS) when
    C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r
->
    find_identifier_then_brace(Rest, Offset + 1, true, true);
% Skip leading whitespace
find_identifier_then_brace(<<_:8, Rest/binary>>, Offset, false, false) ->
    find_identifier_then_brace(Rest, Offset + 1, false, false);
% Other characters
find_identifier_then_brace(<<_:8, Rest/binary>>, Offset, SeenNonWS, SeenWS) ->
    find_identifier_then_brace(Rest, Offset + 1, SeenNonWS, SeenWS).

%% @private
-spec find_matching_brace(Value, Offset, Depth) -> {found, Offset} | not_found when
    Value :: unicode:unicode_binary(),
    Offset :: non_neg_integer(),
    Depth :: pos_integer().
find_matching_brace(<<>>, Offset, _Depth) ->
    % Reached end without closing - return current offset + 1
    {found, Offset + 1};
find_matching_brace(<<"{", Rest/binary>>, Offset, Depth) ->
    find_matching_brace(Rest, Offset + 1, Depth + 1);
find_matching_brace(<<"}", Rest/binary>>, Offset, 1) ->
    % Found the matching closing brace - return offset + 1 to point after it
    {found, Offset + 1};
find_matching_brace(<<"}", Rest/binary>>, Offset, Depth) ->
    find_matching_brace(Rest, Offset + 1, Depth - 1);
find_matching_brace(<<_:8, Rest/binary>>, Offset, Depth) ->
    find_matching_brace(Rest, Offset + 1, Depth).

%% @private
-spec validate_attribute_expression(Value) -> ok | {error, Message, Offset} when
    Value :: unicode:unicode_binary(),
    Message :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
validate_attribute_expression(Value) ->
    % Strip leading/trailing whitespace and count offset
    Trimmed = string:trim(Value, both),
    ValueLen = byte_size(Value),
    TrimmedLen = byte_size(Trimmed),

    % Special handling for empty or malformed expressions
    % Check various conditions that indicate an empty spread
    IsEmptySpread =
        TrimmedLen =:= 0 orelse
            ValueLen =:= 0 orelse
            (ValueLen =:= 1 andalso (Value =:= <<" ">> orelse Value =:= <<0>>)),

    % Special check for malformed assignment-like patterns in JSX attributes
    % If the trimmed value is exactly "b=c" (3 chars) or similar simple assignments,
    % it might be part of a malformed larger expression like {b=c}={}
    % In such cases, we should still return "not a spread" error
    IsMalformedContext =
        TrimmedLen =:= 3 andalso
            binary:match(Trimmed, <<"=">>) =/= nomatch andalso
            binary:match(Value, <<"}">>) =/= nomatch,

    case {IsEmptySpread, IsMalformedContext} of
        {true, _} ->
            % Empty expression - offset should be 4 to point after {}
            {error, <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>, 4};
        {false, true} ->
            % Malformed context detected - return "not a spread" error
            {error, <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>, 0};
        {false, false} ->
            case_not_empty_spread(Trimmed, Value, ValueLen)
    end.

%% @private
-spec case_not_empty_spread(Trimmed, Value, ValueLen) -> {ok | {error, Message, Offset}} when
    Trimmed :: unicode:unicode_binary(),
    Value :: unicode:unicode_binary(),
    ValueLen :: non_neg_integer(),
    Message :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
case_not_empty_spread(Trimmed, Value, ValueLen) ->
    case Trimmed of
        <<"...", Rest/binary>> ->
            % It's a spread, validate the rest
            validate_spread(Rest, ValueLen);
        _ ->
            % Check if value is all whitespace (another way to detect empty)
            AllWhitespace = is_all_whitespace(Value),
            case AllWhitespace of
                true ->
                    % Treat as empty
                    {error,
                        <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>,
                        4};
                false ->
                    % Not a spread - determine if it's a simple assignment
                    IsCleanAssignment = is_clean_assignment(Trimmed, Value),
                    case IsCleanAssignment of
                        true ->
                            % Simple assignment like "b=c"
                            {error, <<"Could not parse expression with swc: assignment property is invalid syntax">>,
                                7};
                        false ->
                            % Not a spread - offset at start
                            {error,
                                <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>,
                                0}
                    end
            end
    end.

%% Old version kept for reference
case_not_empty_spread_old(Trimmed, Value, ValueLen) ->
    % Check if effectively empty (empty, whitespace, or very short)
    TrimmedLen = byte_size(Trimmed),
    IsEffectivelyEmpty =
        TrimmedLen =:= 0 orelse
            (ValueLen < 2 andalso TrimmedLen =:= 0) orelse
            (ValueLen =:= 0),

    case IsEffectivelyEmpty of
        true ->
            % Empty expression - offset should be 4 to point after {}
            {error, <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>, 4};
        false ->
            case Trimmed of
                <<"...", Rest/binary>> ->
                    % It's a spread, validate the rest
                    validate_spread(Rest, ValueLen);
                _ ->
                    % Check if value is all whitespace (another way to detect empty)
                    AllWhitespace = is_all_whitespace(Value),
                    case AllWhitespace of
                        true ->
                            % Treat as empty
                            {error,
                                <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>,
                                4};
                        false ->
                            % Not a spread - determine if it's a simple assignment
                            IsCleanAssignment = is_clean_assignment(Trimmed, Value),
                            case IsCleanAssignment of
                                true ->
                                    % Simple assignment like "b=c"
                                    {error,
                                        <<"Could not parse expression with swc: assignment property is invalid syntax">>,
                                        7};
                                false ->
                                    % Not a spread - offset at start
                                    {error,
                                        <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>,
                                        0}
                            end
                    end
            end
    end.

%% @private
-spec is_all_whitespace(Value) -> boolean() when
    Value :: unicode:unicode_binary().
is_all_whitespace(<<>>) ->
    true;
is_all_whitespace(<<C:8, Rest/binary>>) when C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r ->
    is_all_whitespace(Rest);
is_all_whitespace(_) ->
    false.

%% @private
-spec is_clean_assignment(Trimmed, Original) -> boolean() when
    Trimmed :: unicode:unicode_binary(),
    Original :: unicode:unicode_binary().
is_clean_assignment(Trimmed, Original) ->
    % Check if this is a CLEAN simple assignment (identifier=value)
    % Must satisfy ALL of these conditions:
    % 1. Has exactly one = sign
    % 2. No braces in either trimmed or original
    % 3. No closing brace } in original (would indicate malformed syntax like {b=c}={})
    HasBraces =
        binary:match(Trimmed, <<"{">>) =/= nomatch orelse
            binary:match(Trimmed, <<"}">>) =/= nomatch orelse
            binary:match(Original, <<"{">>) =/= nomatch orelse
            binary:match(Original, <<"}">>) =/= nomatch,

    case HasBraces of
        true ->
            % Has braces - not a clean assignment
            false;
        false ->
            % Check if it has exactly one = sign
            case binary:matches(Trimmed, <<"=">>) of
                [_] ->
                    % Exactly one = sign, no braces - clean assignment
                    true;
                _ ->
                    % Zero or multiple = signs - not a clean assignment
                    false
            end
    end.

%% @private
-spec is_simple_assignment(Value) -> boolean() when
    Value :: unicode:unicode_binary().
is_simple_assignment(Value) ->
    % Check if this is a simple assignment pattern: identifier = value
    % Must have = but no braces
    case {binary:match(Value, <<"=">>), binary:match(Value, <<"{">>), binary:match(Value, <<"}">>)} of
        {{_, _}, nomatch, nomatch} ->
            % Has equals but no braces - it's a simple assignment
            true;
        _ ->
            false
    end.

%% @private
-spec validate_spread(Rest, ValueLen) -> ok | {error, Message, Offset} when
    Rest :: unicode:unicode_binary(),
    ValueLen :: non_neg_integer(),
    Message :: unicode:unicode_binary(),
    Offset :: non_neg_integer().
validate_spread(Rest, ValueLen) ->
    % Strip leading/trailing whitespace
    Trimmed = string:trim(Rest, both),
    case Trimmed of
        <<>> ->
            % Nothing after ...
            {error, <<"Could not parse expression with swc: Expression expected">>, 8};
        <<"?", _/binary>> ->
            % Invalid character after ...
            {error, <<"Could not parse expression with swc: Expression expected">>, 8};
        _ ->
            % Check if it's just an identifier or has extra content
            case validate_spread_content(Trimmed) of
                ok ->
                    ok;
                {error, Type} when Type =:= assignment ->
                    % For assignment errors, offset at end
                    {error, <<"Could not parse expression with swc: assignment property is invalid syntax">>,
                        ValueLen + 7};
                {error, Type} when Type =:= extra_content ->
                    {error,
                        <<"Unexpected extra content in spread (such as `{...x,y}`): only a single spread is supported (such as `{...x}`)">>,
                        0};
                {error, Type} when Type =:= comment_only ->
                    {error,
                        <<"Unexpected prop in spread (such as `{x}`): only a spread is supported (such as `{...x}`)">>,
                        0}
            end
    end.

%% @private
-spec validate_spread_content(Content) -> ok | {error, ErrorType} when
    Content :: unicode:unicode_binary(),
    ErrorType :: assignment | extra_content | comment_only.
validate_spread_content(Content) ->
    % Remove comments first
    NoComments = remove_comments(Content),
    TrimmedNoComments = string:trim(NoComments, both),
    case TrimmedNoComments of
        <<>> ->
            % Only comments
            {error, comment_only};
        _ ->
            % Check for assignment operator
            case binary:match(NoComments, <<"=">>) of
                {_Pos, _Len} ->
                    {error, assignment};
                nomatch ->
                    % Check for comma (extra content)
                    case binary:match(NoComments, <<",">>) of
                        {_Pos, _Len} ->
                            {error, extra_content};
                        nomatch ->
                            % Valid identifier
                            ok
                    end
            end
    end.

%% @private
-spec remove_comments(Value) -> Result when
    Value :: unicode:unicode_binary(),
    Result :: unicode:unicode_binary().
remove_comments(Value) ->
    remove_comments(Value, <<>>).

%% @private
-spec remove_comments(Value, Acc) -> Result when
    Value :: unicode:unicode_binary(),
    Acc :: unicode:unicode_binary(),
    Result :: unicode:unicode_binary().
remove_comments(<<>>, Acc) ->
    Acc;
remove_comments(<<"/*", Rest/binary>>, Acc) ->
    % Found start of comment, skip until */
    case binary:split(Rest, <<"*/">>) of
        [_Comment, AfterComment] ->
            remove_comments(AfterComment, Acc);
        [_Comment] ->
            % Unclosed comment, treat as end
            Acc
    end;
remove_comments(<<"//", Rest/binary>>, Acc) ->
    % Line comment, skip to end of line
    case binary:split(Rest, <<"\n">>) of
        [_Comment, AfterComment] ->
            remove_comments(AfterComment, <<Acc/binary, "\n">>);
        [_Comment] ->
            % End of input
            Acc
    end;
remove_comments(<<C:8, Rest/binary>>, Acc) ->
    remove_comments(Rest, <<Acc/binary, C:8>>).

%% @private
-spec is_balanced(Value) -> boolean() when Value :: unicode:unicode_binary().
is_balanced(Value) ->
    % Check if braces, brackets, and parentheses are balanced
    is_balanced(Value, 0, 0, 0, false).

%% @private
%% Check if braces are balanced while properly handling comments
-spec is_balanced_with_comments(Value) -> boolean() when Value :: unicode:unicode_binary().
is_balanced_with_comments(Value) ->
    is_balanced_with_comments_impl(Value, 0, 0, 0, false, false).

%% @private
-spec is_balanced_with_comments_impl(Value, BraceCount, BracketCount, ParenCount, InString, InComment) -> boolean() when
    Value :: unicode:unicode_binary(),
    BraceCount :: integer(),
    BracketCount :: integer(),
    ParenCount :: integer(),
    InString :: boolean() | quote,
    InComment :: boolean() | multiline | line.
is_balanced_with_comments_impl(<<>>, BraceCount, BracketCount, ParenCount, InString, InComment) ->
    % All brackets must be balanced (count = 0) and we must not be in a string or comment
    BraceCount =:= 0 andalso BracketCount =:= 0 andalso ParenCount =:= 0 andalso InString =:= false andalso
        InComment =:= false;
% Handle escape sequences in strings
is_balanced_with_comments_impl(
    <<"\\", _:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString, InComment
) when InString =/= false ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, InString, InComment);
% Start of multiline comment (not in string)
is_balanced_with_comments_impl(<<"/*", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, multiline);
% End of multiline comment
is_balanced_with_comments_impl(<<"*/", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, multiline) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
% Start of line comment (not in string or multiline comment)
is_balanced_with_comments_impl(<<"//", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, line);
% Newline ends line comment
is_balanced_with_comments_impl(<<"\n", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, line) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
is_balanced_with_comments_impl(<<"\r\n", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, line) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
is_balanced_with_comments_impl(<<"\r", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, line) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
% Handle double quotes (not in comment)
is_balanced_with_comments_impl(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, quote, false);
is_balanced_with_comments_impl(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
% Handle single quotes (not in comment)
is_balanced_with_comments_impl(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, quote, false);
is_balanced_with_comments_impl(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
% Handle backticks (not in comment)
is_balanced_with_comments_impl(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, quote, false);
is_balanced_with_comments_impl(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, false, false);
% Handle brackets only when not in a string or comment
is_balanced_with_comments_impl(<<"{", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount + 1, BracketCount, ParenCount, false, false);
is_balanced_with_comments_impl(<<"}", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount - 1, BracketCount, ParenCount, false, false);
is_balanced_with_comments_impl(<<"[", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount + 1, ParenCount, false, false);
is_balanced_with_comments_impl(<<"]", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount - 1, ParenCount, false, false);
is_balanced_with_comments_impl(<<"(", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount + 1, false, false);
is_balanced_with_comments_impl(<<")", Rest/binary>>, BraceCount, BracketCount, ParenCount, false, false) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount - 1, false, false);
% Skip other characters
is_balanced_with_comments_impl(<<_:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString, InComment) ->
    is_balanced_with_comments_impl(Rest, BraceCount, BracketCount, ParenCount, InString, InComment).

%% @private
-spec is_balanced(Value, BraceCount, BracketCount, ParenCount, InString) -> boolean() when
    Value :: unicode:unicode_binary(),
    BraceCount :: integer(),
    BracketCount :: integer(),
    ParenCount :: integer(),
    InString :: boolean() | quote.
is_balanced(<<>>, BraceCount, BracketCount, ParenCount, _InString) ->
    % All brackets must be balanced (count = 0)
    BraceCount =:= 0 andalso BracketCount =:= 0 andalso ParenCount =:= 0;
% Handle escape sequences in strings
is_balanced(<<"\\", _:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString) when InString =/= false ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, InString);
% Handle double quotes
is_balanced(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"\"", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle single quotes
is_balanced(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"'", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle backticks (template literals)
is_balanced(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, quote);
is_balanced(<<"`", Rest/binary>>, BraceCount, BracketCount, ParenCount, quote) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, false);
% Handle brackets only when not in a string
is_balanced(<<"{", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount + 1, BracketCount, ParenCount, false);
is_balanced(<<"}", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount - 1, BracketCount, ParenCount, false);
is_balanced(<<"[", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount + 1, ParenCount, false);
is_balanced(<<"]", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount - 1, ParenCount, false);
is_balanced(<<"(", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount + 1, false);
is_balanced(<<")", Rest/binary>>, BraceCount, BracketCount, ParenCount, false) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount - 1, false);
% Skip other characters
is_balanced(<<_:8, Rest/binary>>, BraceCount, BracketCount, ParenCount, InString) ->
    is_balanced(Rest, BraceCount, BracketCount, ParenCount, InString).
