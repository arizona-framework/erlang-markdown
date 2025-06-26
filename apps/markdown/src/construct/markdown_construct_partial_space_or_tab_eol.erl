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
-module(markdown_construct_partial_space_or_tab_eol).
-moduledoc """
Space or tab (eol) occurs in [destination][], [label][], and [title][].

## Grammar

Space or tab (eol) forms with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
space_or_tab_eol ::= 1*space_or_tab | *space_or_tab eol *space_or_tab
```

Importantly, this allows one line ending, but not blank lines.

## References

*   [`micromark-factory-space/index.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-factory-space/dev/index.js)

[destination]: crate::construct::partial_destination
[label]: crate::construct::partial_label
[title]: crate::construct::partial_title
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_util.hrl").

%% API
-export([
    space_or_tab_eol/1,
    space_or_tab_eol_with_options/2,
    start/1,
    after_first/1,
    at_eol/1,
    after_eol/1,
    after_more/1
]).

%% Types
-type options() :: #markdown_space_or_tab_eol_options{}.

-export_type([
    options/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
`space_or_tab_eol`
""".
-spec space_or_tab_eol(Tokenizer) -> {Tokenizer, StateName} when
    Tokenizer :: markdown_tokenizer:t(), StateName :: markdown_state:name().
space_or_tab_eol(Tokenizer1 = #markdown_tokenizer{}) ->
    space_or_tab_eol_with_options(Tokenizer1, #markdown_space_or_tab_eol_options{
        connect = false,
        content = none
    }).

-doc """
`space_or_tab`, with the given options.
""".
-spec space_or_tab_eol_with_options(Tokenizer, Options) -> {Tokenizer, StateName} when
    Tokenizer :: markdown_tokenizer:t(),
    Options :: options(),
    StateName :: markdown_state:name().
space_or_tab_eol_with_options(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}, Options) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        space_or_tab_eol_connect = Options#markdown_space_or_tab_eol_options.connect,
        space_or_tab_eol_content = Options#markdown_space_or_tab_eol_options.content
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer2, space_or_tab_eol_start}.

-doc """
Start of whitespace with at most one eol.

```markdown
> | a␠␠b
     ^
> | a␠␠␊
     ^
  | ␠␠b
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = Current,
        tokenize_state = #markdown_tokenize_state{
            space_or_tab_eol_connect = SpaceOrTabEolConnect,
            space_or_tab_eol_content = OptionSpaceOrTabEolContent
        }
    }
) when (Current =:= {some, $\t} orelse Current =:= {some, $\s}) ->
    Tokenizer2 = markdown_tokenizer:attempt(
        Tokenizer1, markdown_state:next(space_or_tab_eol_after_first), markdown_state:next(space_or_tab_eol_at_eol)
    ),
    {Tokenizer3, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_with_options(
        Tokenizer2, #markdown_space_or_tab_options{
            kind = space_or_tab,
            min = 1,
            max = infinity,
            connect = SpaceOrTabEolConnect,
            content = OptionSpaceOrTabEolContent
        }
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
start(Tokenizer = #markdown_tokenizer{}) ->
    State = markdown_state:retry(space_or_tab_eol_at_eol),
    {Tokenizer, State}.

-doc """
After initial whitespace, at optional eol.

```markdown
> | a␠␠b
       ^
> | a␠␠␊
       ^
  | ␠␠b
```
""".
-spec after_first(Tokenizer) -> {Tokenizer, State} when
    Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_first(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{space_or_tab_eol_content = none}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{space_or_tab_eol_ok = true},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    %% If the above ever errors, set `tokenizer.tokenize_state.space_or_tab_eol_connect: true` in that case.
    State = markdown_state:retry(space_or_tab_eol_at_eol),
    {Tokenizer2, State}.

-doc """
After optional whitespace, at eol.

```markdown
> | a␠␠b
       ^
> | a␠␠␊
       ^
  | ␠␠b
> | a␊
     ^
  | ␠␠b
```
""".
-spec at_eol(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
at_eol(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, $\n},
        tokenize_state = #markdown_tokenize_state{
            space_or_tab_eol_content = OptionSpaceOrTabEolContent, space_or_tab_eol_connect = SpaceOrTabEolConnect
        }
    }
) ->
    Tokenizer2 =
        case OptionSpaceOrTabEolContent of
            {some, SpaceOrTabEolContent} ->
                Link = markdown_event_link:new(none, none, SpaceOrTabEolContent),
                markdown_tokenizer:enter_link(Tokenizer1, line_ending, Link);
            none ->
                markdown_tokenizer:enter(Tokenizer1, line_ending)
        end,
    Tokenizer3 =
        case SpaceOrTabEolConnect of
            true ->
                Events2 = Tokenizer2#markdown_tokenizer.events,
                Index = markdown_vec:size(Events2) - 1,
                Events3 = markdown_subtokenizer:link(Events2, Index),
                Tokenizer2#markdown_tokenizer{events = Events3};
            false when ?is_option_some(OptionSpaceOrTabEolContent) ->
                TokenizeState2 = Tokenizer2#markdown_tokenizer.tokenize_state,
                TokenizeState3 = TokenizeState2#markdown_tokenize_state{space_or_tab_eol_connect = true},
                Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState3};
            false ->
                Tokenizer2
        end,
    Tokenizer4 = markdown_tokenizer:consume(Tokenizer3),
    Tokenizer5 = markdown_tokenizer:exit(Tokenizer4, line_ending),
    State = markdown_state:next(space_or_tab_eol_after_eol),
    {Tokenizer5, State};
at_eol(
    Tokenizer1 = #markdown_tokenizer{
        tokenize_state = TokenizeState1 = #markdown_tokenize_state{space_or_tab_eol_ok = SpaceOrTabEolOk}
    }
) ->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        space_or_tab_eol_content = none, space_or_tab_eol_connect = false, space_or_tab_eol_ok = false
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State =
        case SpaceOrTabEolOk of
            true ->
                markdown_state:ok();
            false ->
                markdown_state:nok()
        end,
    {Tokenizer2, State}.

-doc """
After eol.

```markdown
  | a␠␠␊
> | ␠␠b
    ^
  | a␊
> | ␠␠b
    ^
```
""".
-spec after_eol(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_eol(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        tokenize_state = #markdown_tokenize_state{
            space_or_tab_eol_content = Content, space_or_tab_eol_connect = Connect
        }
    }
) when Current =:= $\t orelse Current =:= $\s ->
    Options = #markdown_space_or_tab_options{
        kind = space_or_tab,
        min = 1,
        max = infinity,
        content = Content,
        connect = Connect
    },
    {Tokenizer2, SpaceOrTabState} = markdown_construct_partial_space_or_tab:space_or_tab_with_options(
        Tokenizer1, Options
    ),
    Tokenizer3 = markdown_tokenizer:attempt(
        Tokenizer2, markdown_state:next(space_or_tab_eol_after_more), markdown_state:nok()
    ),
    State = markdown_state:retry(SpaceOrTabState),
    {Tokenizer3, State};
after_eol(Tokenizer) ->
    State = markdown_state:retry(space_or_tab_eol_after_more),
    {Tokenizer, State}.

-doc """
After optional final whitespace.

```markdown
  | a␠␠␊
> | ␠␠b
      ^
  | a␊
> | ␠␠b
      ^
```
""".
-spec after_more(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
after_more(
    Tokenizer1 = #markdown_tokenizer{current = Current, tokenize_state = TokenizeState1 = #markdown_tokenize_state{}}
) when Current =/= none andalso Current =/= {some, $\n} ->
    %% If the above ever starts erroring, gracefully `State::Nok` on it.
    %% Currently it doesn't happen, as we only use this in content, which does
    %% not allow blank lines.
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{
        space_or_tab_eol_content = none,
        space_or_tab_eol_connect = false,
        space_or_tab_eol_ok = false
    },
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    State = markdown_state:ok(),
    {Tokenizer2, State}.
