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
-module(markdown_util_gfm_tagfilter).
-moduledoc """
Make dangerous HTML a tiny bit safer.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    gfm_tagfilter/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Make dangerous HTML a tiny bit safer.

The tagfilter is kinda weird and kinda useless.
The tag filter is a naïve attempt at XSS protection.
You should use a proper HTML sanitizing algorithm.

## Examples

```rust ignore
use markdown::util::gfm_tagfilter::gfm_tagfilter;

assert_eq!(gfm_tagfilter("<iframe>"), "&lt;iframe>");
```

## References

*   [*§ 6.1 Disallowed Raw HTML (extension)* in GFM](https://github.github.com/gfm/#disallowed-raw-html-extension-)
*   [`cmark-gfm#extensions/tagfilter.c`](https://github.com/github/cmark-gfm/blob/master/extensions/tagfilter.c)
""".
-spec gfm_tagfilter(Value) -> GfmTagfilteredValue when Value :: binary(), GfmTagfilteredValue :: binary().
gfm_tagfilter(Value) when is_binary(Value) ->
    gfm_tagfilter_loop(Value, <<>>, 0, 0, byte_size(Value)).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec find_tag_name_end(Bytes, NameStart, NameEnd, Len) -> NameEnd when
    Bytes :: binary(), NameStart :: non_neg_integer(), NameEnd :: non_neg_integer(), Len :: non_neg_integer().
find_tag_name_end(Bytes, NameStart, NameEnd, Len) when
    NameEnd < Len andalso (NameEnd - NameStart) < ?GFM_HTML_TAGFILTER_SIZE_MAX
->
    case binary:at(Bytes, NameEnd) of
        Char when ?is_ascii_alphabetic(Char) ->
            find_tag_name_end(Bytes, NameStart, NameEnd + 1, Len);
        _ ->
            NameEnd
    end;
find_tag_name_end(_Bytes, _NameStart, NameEnd, _Len) ->
    NameEnd.

%% @private
-spec gfm_tagfilter_loop(Bytes, Result, Index, Start, Len) -> Result when
    Bytes :: binary(),
    Result :: binary(),
    Index :: non_neg_integer(),
    Start :: non_neg_integer(),
    Len :: non_neg_integer().
gfm_tagfilter_loop(Bytes, Result1, Index1, Start1, Len) when Index1 < Len ->
    case binary:at(Bytes, Index1) of
        $< ->
            %% Optional `/`.
            NameStart =
                case (Index1 + 1) < Len andalso binary:at(Bytes, Index1 + 1) =:= $/ of
                    true ->
                        Index1 + 2;
                    false ->
                        Index1 + 1
                end,
            %% Tag name.
            NameEnd = find_tag_name_end(Bytes, NameStart, NameStart, Len),
            %% Non-empty.
            {Result2, Start2} =
                case is_filtered_tag(Bytes, NameStart, NameEnd, Len) of
                    true ->
                        {
                            <<Result1/bytes, (binary:part(Bytes, Start1, Index1 - Start1))/bytes, "&lt;"/utf8>>,
                            Index1 + 1
                        };
                    false ->
                        {Result1, Start1}
                end,
            %% There was no `<` before `name_end`, so move to that next.
            Index2 = NameEnd,
            gfm_tagfilter_loop(Bytes, Result2, Index2, Start2, Len);
        _ ->
            Index2 = Index1 + 1,
            gfm_tagfilter_loop(Bytes, Result1, Index2, Start1, Len)
    end;
gfm_tagfilter_loop(Bytes, Result, _Index, Start, Len) ->
    <<Result/bytes, (binary:part(Bytes, Start, Len - Start))/bytes>>.

%% @private
-spec is_filtered_tag(Bytes, NameStart, NameEnd, Len) -> boolean() when
    Bytes :: binary(), NameStart :: non_neg_integer(), NameEnd :: non_neg_integer(), Len :: non_neg_integer().
is_filtered_tag(Bytes, NameStart, NameEnd, Len) ->
    case
        NameEnd =:= Len orelse
            (NameEnd =/= NameStart andalso is_html_whitespace_or_delimiter(binary:at(Bytes, NameEnd)))
    of
        true ->
            TagName = markdown_types:unicode_binary(
                string:casefold(binary:part(Bytes, NameStart, NameEnd - NameStart))
            ),
            %% Known name.
            maps:is_key(TagName, ?GFM_HTML_TAGFILTER_NAMES);
        false ->
            false
    end.

-doc """
HTML whitespace, closing slash, or closing angle bracket.
""".
-spec is_html_whitespace_or_delimiter(char()) -> boolean().
is_html_whitespace_or_delimiter(C) when
    C =:= $\t orelse C =:= $\n orelse C =:= $\f orelse C =:= $\r orelse C =:= $\s orelse C =:= $/ orelse C =:= $>
->
    true;
is_html_whitespace_or_delimiter(_) ->
    false.
