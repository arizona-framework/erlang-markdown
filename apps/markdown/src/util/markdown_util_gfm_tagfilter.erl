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
    %% TODO: implement this
    GfmTagfilteredValue = Value,
    GfmTagfilteredValue.
