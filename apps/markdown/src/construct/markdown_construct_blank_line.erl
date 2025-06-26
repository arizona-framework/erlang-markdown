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
-module(markdown_construct_blank_line).
-moduledoc """
Blank lines occur in the [flow][] content type.

## Grammar

Blank lines form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
blank_line ::= *space_or_tab
```

As this construct occurs in flow, like all flow constructs, it must be
followed by an eol (line ending) or eof (end of file).

Blank lines are sometimes needed, such as to differentiate a [paragraph][]
from a definition.
In several cases, blank lines are not needed between flow constructs,
such as between two [heading (atx)][heading_atx]s.
Sometimes, whether blank lines are present, changes the behavior of how
HTML is rendered, such as whether blank lines are present inside or between
[list items][list_item].
More than one blank line is never needed in `CommonMark`.

Because blank lines can be empty (line endings are not considered part of
it), and events cannot be empty, blank lines are not present as an event.

## HTML

Blank lines do not relate to an element in HTML, except for the role they
play when inside or between [list items][list_item].

## Recommendation

It is recommended to always use a blank line between every flow construct,
to use blank lines (consistently) between list items as desired, and to
never use more than one blank line.

## Tokens

*   [`SpaceOrTab`][crate::event::Name::SpaceOrTab]

## References

*   [`blank-line.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/blank-line.js)
*   [*§ 4.9 Blank lines* in `CommonMark`](https://spec.commonmark.org/0.31/#blank-lines)

[heading_atx]: crate::construct::heading_atx
[list_item]: crate::construct::list_item
[paragraph]: crate::construct::paragraph
[flow]: crate::construct::flow
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    'after'/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of blank line.

> 👉 **Note**: `␠` represents a space character.

```markdown
> | ␠␠␊
    ^
> | ␊
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\t orelse Current =:= $\s ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(blank_line_after),
        markdown_state:nok()
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab(Tokenizer2),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
start(Tokenizer) ->
    State = markdown_state:retry(blank_line_after),
    {Tokenizer, State}.

-doc """
At eof/eol, after optional whitespace.

```markdown
> | ␠␠␊
      ^
> | ␊
    ^
```
""".
-spec 'after'(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
'after'(Tokenizer = #markdown_tokenizer{current = none}) ->
    State = markdown_state:ok(),
    {Tokenizer, State};
'after'(Tokenizer = #markdown_tokenizer{current = {some, $\n}}) ->
    State = markdown_state:ok(),
    {Tokenizer, State};
'after'(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:nok(),
    {Tokenizer, State}.
