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
-module(markdown_construct_text).
-moduledoc """
The text content type.

**Text** contains phrasing content such as
[attention][crate::construct::attention] (emphasis, gfm strikethrough, strong),
[raw (text)][crate::construct::raw_text] (code (text), math (text)), and actual text.

The constructs found in text are:

*   [Attention][crate::construct::attention] (emphasis, gfm strikethrough, strong)
*   [Autolink][crate::construct::autolink]
*   [Character escape][crate::construct::character_escape]
*   [Character reference][crate::construct::character_reference]
*   [Raw (text)][crate::construct::raw_text] (code (text), math (text))
*   [GFM: Label start (footnote)][crate::construct::gfm_label_start_footnote]
*   [GFM: Task list item check][crate::construct::gfm_task_list_item_check]
*   [Hard break (escape)][crate::construct::hard_break_escape]
*   [HTML (text)][crate::construct::html_text]
*   [Label start (image)][crate::construct::label_start_image]
*   [Label start (link)][crate::construct::label_start_link]
*   [Label end][crate::construct::label_end]
*   [MDX: expression (text)][crate::construct::mdx_expression_text]
*   [MDX: JSX (text)][crate::construct::mdx_jsx_text]

> 👉 **Note**: for performance reasons, hard break (trailing) is formed by
> [whitespace][crate::construct::partial_whitespace].
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").

%% API
-export([
    start/1,
    before/1,
    before_html/1,
    before_mdx_jsx/1,
    before_hard_break_escape/1,
    before_label_start_link/1,
    before_data/1,
    resolve/1
]).

%% Macros
%% erlfmt-ignore
-define(MARKERS, <<
    "!",  %% `label_start_image`
    "$",  %% `raw_text` (math (text))
    "&",  %% `character_reference`
    "*",  %% `attention` (emphasis, strong)
    "<",  %% `autolink`, `html_text`, `mdx_jsx_text`
    "H",  %% `gfm_autolink_literal` (`protocol` kind)
    "W",  %% `gfm_autolink_literal` (`www.` kind)
    "[",  %% `label_start_link`
    "\\", %% `character_escape`, `hard_break_escape`
    "]",  %% `label_end`, `gfm_label_start_footnote`
    "_",  %% `attention` (emphasis, strong)
    "`",  %% `raw_text` (code (text))
    "h",  %% `gfm_autolink_literal` (`protocol` kind)
    "w",  %% `gfm_autolink_literal` (`www.` kind)
    "{",  %% `mdx_expression_text`
    "~"   %% `attention` (gfm strikethrough)
>>).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Start of text.

There is a slightly weird case where task list items have their check at
the start of the first paragraph.
So we start by checking for that.

```markdown
> | abc
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{markers = ?MARKERS},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2,
        markdown_state:next(text_before),
        markdown_state:next(text_before)
    ),
    State = markdown_state:retry(gfm_task_list_item_check_start),
    {Tokenizer3, State}.

-doc """
Before text.

```markdown
> | abc
    ^
```
""".
-spec before(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before(Tokenizer1 = #markdown_tokenizer{current = none}) ->
    Tokenizer2 = markdown_tokenizer:register_resolver(Tokenizer1, data),
    Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, text),
    State = markdown_state:ok(),
    {Tokenizer3, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, $!}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(label_start_image_start),
    {Tokenizer2, State};
%% raw (text) (code (text), math (text))
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $$ orelse Current =:= $` ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(raw_text_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, $&}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(character_reference_start),
    {Tokenizer2, State};
%% attention (emphasis, gfm strikethrough, strong)
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when
    Current =:= $* orelse Current =:= $_ orelse Current =:= $~
->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(attention_start),
    {Tokenizer2, State};
%% `autolink`, `html_text` (order does not matter), `mdx_jsx_text` (order matters).
before(Tokenizer1 = #markdown_tokenizer{current = {some, $<}}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_html)
    ),
    State = markdown_state:retry(autolink_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $H orelse Current =:= $h ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(gfm_autolink_literal_protocol_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $W orelse Current =:= $w ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(gfm_autolink_literal_www_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $[ ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_label_start_link)
    ),
    State = markdown_state:retry(gfm_label_start_footnote_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $\\ ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_hard_break_escape)
    ),
    State = markdown_state:retry(character_escape_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= $] ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(label_end_start),
    {Tokenizer2, State};
before(Tokenizer1 = #markdown_tokenizer{current = {some, Current}}) when Current =:= ${ ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(mdx_expression_text_start),
    {Tokenizer2, State};
before(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(text_before_data),
    {Tokenizer, State}.

-doc """
Before html (text).

At `<`, which wasn't an autolink.

```markdown
> | a <b>
      ^
```
""".
-spec before_html(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_html(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_mdx_jsx)
    ),
    State = markdown_state:retry(html_text_start),
    {Tokenizer2, State}.

-doc """
Before mdx jsx (text).

At `<`, which wasn't an autolink or html.

```markdown
> | a <b>
      ^
```
""".
-spec before_mdx_jsx(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_mdx_jsx(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(mdx_jsx_text_start),
    {Tokenizer2, State}.

-doc """
Before hard break escape.

At `\\`, which wasn't a character escape.

```markdown
> | a \\␊
      ^
```
""".
-spec before_hard_break_escape(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_hard_break_escape(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(hard_break_escape_start),
    {Tokenizer2, State}.

-doc """
Before label start (link).

At `[`, which wasn't a GFM label start (footnote).

```markdown
> | [a](b)
    ^
```
""".
-spec before_label_start_link(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_label_start_link(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:next(text_before_data)
    ),
    State = markdown_state:retry(label_start_link_start),
    {Tokenizer2, State}.

-doc """
Before data.

```markdown
> | a
    ^
```
""".
-spec before_data(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
before_data(Tokenizer1) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1,
        markdown_state:next(text_before),
        markdown_state:nok()
    ),
    State = markdown_state:retry(data_start),
    {Tokenizer2, State}.

-doc """
Resolve whitespace.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(
    Tokenizer1 = #markdown_tokenizer{
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{
                    gfm_autolink_literal = GfmAutolinkLiteral,
                    hard_break_trailing = HardBreakTrailing
                }
            }
        }
    }
) ->
    io:format("\n\n[~w] BEFORE text:resolve\n\n~ts\n\n", [
        markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer1)
    ]),
    Tokenizer2 = markdown_construct_partial_whitespace:resolve_whitespace(Tokenizer1, HardBreakTrailing, true),
    Tokenizer3 =
        case GfmAutolinkLiteral of
            true ->
                markdown_construct_gfm_autolink_literal:resolve(Tokenizer2);
            false ->
                Tokenizer2
        end,
    #markdown_tokenizer{events = Events3, map = EditMap3} = Tokenizer3,
    {EditMap4, Events4} = markdown_edit_map:consume(EditMap3, Events3),
    Tokenizer4 = Tokenizer3#markdown_tokenizer{events = Events4, map = EditMap4},
    io:format("\n\n[~w] AFTER text:resolve\n\n~ts\n\n", [
        markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer4)
    ]),
    {Tokenizer4, {ok, none}}.
