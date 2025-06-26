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
-module(markdown_construct_attention).
-moduledoc """
Attention (emphasis, strong, optionally GFM strikethrough) occurs in the
[text][] content type.

## Grammar

Attention sequences form with the following BNF
(<small>see [construct][crate::construct] for character groups</small>):

```bnf
attention_sequence ::= 1*'*' | 1*'_'
gfm_attention_sequence ::= 1*'~'
```

Sequences are matched together to form attention based on which character
they contain, how long they are, and what character occurs before and after
each sequence.
Otherwise they are turned into data.

## HTML

When asterisk/underscore sequences match, and two markers can be “taken”
from them, they together relate to the `<strong>` element in HTML.
When one marker can be taken, they relate to the `<em>` element.
See [*§ 4.5.2 The `em` element*][html-em] and
[*§ 4.5.3 The `strong` element*][html-strong] in the HTML spec for more
info.

When tilde sequences match, they together relate to the `<del>` element in
HTML.
See [*§ 4.7.2 The `del` element*][html-del] in the HTML spec for more info.

## Recommendation

It is recommended to use asterisks for emphasis/strong attention when
writing markdown.

There are some small differences in whether sequences can open and/or close
based on whether they are formed with asterisks or underscores.
Because underscores also frequently occur in natural language inside words,
while asterisks typically never do, `CommonMark` prohibits underscore
sequences from opening or closing when *inside* a word.

Because asterisks can be used to form the most markdown constructs, using
them has the added benefit of making it easier to gloss over markdown: you
can look for asterisks to find syntax while not worrying about other
characters.

For strikethrough attention, it is recommended to use two markers.
While `github.com` allows single tildes too, it technically prohibits it in
their spec.

## Tokens

*   [`Emphasis`][Name::Emphasis]
*   [`EmphasisSequence`][Name::EmphasisSequence]
*   [`EmphasisText`][Name::EmphasisText]
*   [`GfmStrikethrough`][Name::GfmStrikethrough]
*   [`GfmStrikethroughSequence`][Name::GfmStrikethroughSequence]
*   [`GfmStrikethroughText`][Name::GfmStrikethroughText]
*   [`Strong`][Name::Strong]
*   [`StrongSequence`][Name::StrongSequence]
*   [`StrongText`][Name::StrongText]

> 👉 **Note**: while parsing, [`AttentionSequence`][Name::AttentionSequence]
> is used, which is later compiled away.

## References

*   [`attention.js` in `micromark`](https://github.com/micromark/micromark/blob/main/packages/micromark-core-commonmark/dev/lib/attention.js)
*   [`micromark-extension-gfm-strikethrough`](https://github.com/micromark/micromark-extension-gfm-strikethrough)
*   [*§ 6.2 Emphasis and strong emphasis* in `CommonMark`](https://spec.commonmark.org/0.31/#emphasis-and-strong-emphasis)
*   [*§ 6.5 Strikethrough (extension)* in `GFM`](https://github.github.com/gfm/#strikethrough-extension-)

[text]: crate::construct::text
[html-em]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-em-element
[html-strong]: https://html.spec.whatwg.org/multipage/text-level-semantics.html#the-strong-element
[html-del]: https://html.spec.whatwg.org/multipage/edits.html#the-del-element
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    start/1,
    inside/1,
    resolve/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
At start of attention.

```markdown
> | **
    ^
```
""".
-spec start(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
start(
    Tokenizer1 = #markdown_tokenizer{
        current = {some, Current},
        parse_state = #markdown_parse_state{
            options = #markdown_parse_options{
                constructs = #markdown_construct_options{
                    attention = Attention,
                    gfm_strikethrough = GfmStrikethrough
                }
            }
        },
        tokenize_state = TokenizeState1
    }
) when
    (Attention =:= true andalso (Current =:= $* orelse Current =:= $_)) orelse
        (GfmStrikethrough =:= true andalso (Current =:= $~))
->
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = Current},
    Tokenizer2 = Tokenizer1#markdown_tokenizer{tokenize_state = TokenizeState2},
    Tokenizer3 = markdown_tokenizer:enter(Tokenizer2, attention_sequence),
    {Tokenizer3, markdown_state:retry(attention_inside)};
start(Tokenizer = #markdown_tokenizer{}) ->
    {Tokenizer, markdown_state:nok()}.

-doc """
In sequence.

```markdown
> | **
    ^^
```
""".
-spec inside(Tokenizer) -> {Tokenizer, State} when Tokenizer :: markdown_tokenizer:t(), State :: markdown_state:t().
inside(Tokenizer1 = #markdown_tokenizer{current = {some, Current}, tokenize_state = TokenizeState1}) when
    Current =:= TokenizeState1#markdown_tokenize_state.marker
->
    Tokenizer2 = markdown_tokenizer:consume(Tokenizer1),
    {Tokenizer2, markdown_state:next(attention_inside)};
inside(Tokenizer1 = #markdown_tokenizer{tokenize_state = TokenizeState1}) ->
    Tokenizer2 = markdown_tokenizer:exit(Tokenizer1, attention_sequence),
    Tokenizer3 = markdown_tokenizer:register_resolver(Tokenizer2, attention),
    TokenizeState2 = TokenizeState1#markdown_tokenize_state{marker = 0},
    Tokenizer4 = Tokenizer3#markdown_tokenizer{tokenize_state = TokenizeState2},
    {Tokenizer4, markdown_state:ok()}.

-doc """
Resolve sequences.
""".
-spec resolve(Tokenizer) -> {Tokenizer, Result} when
    Tokenizer :: markdown_tokenizer:t(),
    Result :: {ok, OptionSubresult} | {error, Message},
    OptionSubresult :: markdown_types:option(Subresult),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
resolve(Tokenizer1) ->
    %% Find all sequences, gather info about them.
    Sequences1 = get_sequences(Tokenizer1),
    % io:format("\n\n[~w] RESULT resolve:attention:sequences\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Sequences1)]),
    %% Now walk through them and match them.
    {Tokenizer2, Sequences2} = match_sequences_loop(Tokenizer1, Sequences1, 0),
    % io:format("\n\n[~w] RESULT resolve:attention:match_sequences_loop\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Sequences2)]),
    %% Mark remaining sequences as data.
    Tokenizer3 = mark_remaining_sequences_as_data(Tokenizer2, Sequences2),
    %% Apply edits
    Tokenizer4 = markdown_tokenizer:map_consume(Tokenizer3),
    {Tokenizer4, {ok, none}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec find_opener(Tokenizer, Sequences, Close, Open, NextIndex) -> {Tokenizer, Sequences, NextIndex} when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Close :: markdown_types:usize(),
    Open :: markdown_types:usize(),
    NextIndex :: markdown_types:usize().
find_opener(Tokenizer1, Sequences1, Close, Open1, NextIndex1) when Open1 > 0 ->
    Open2 = Open1 - 1,
    SequenceClose = markdown_vec:get(Sequences1, Close),
    SequenceOpen = markdown_vec:get(Sequences1, Open2),

    % io:format("\n\n[~w] INSPECT resolve:attention:find_opener:inside(open=~w, close=~w, next_index=~w)\n~ts\n~ts\n\n", [markdown:counter_get(), Open2, Close, NextIndex1, markdown_debug:rust_debug_string(SequenceClose), markdown_debug:rust_debug_string(SequenceOpen)]),

    %% An opener matching our closer:
    IsMatching =
        (SequenceOpen#markdown_sequence.open =:= true) andalso
            (SequenceClose#markdown_sequence.marker =:= SequenceOpen#markdown_sequence.marker) andalso
            (SequenceClose#markdown_sequence.stack =:= SequenceOpen#markdown_sequence.stack),

    case IsMatching of
        true ->
            %% If the opening can close or the closing can open,
            %% and the close size *is not* a multiple of three,
            %% but the sum of the opening and closing size *is*
            %% multiple of three, then **don't** match.
            CannotMatchDueToSizeRules =
                (SequenceOpen#markdown_sequence.close orelse SequenceClose#markdown_sequence.open) andalso
                    SequenceClose#markdown_sequence.size rem 3 =/= 0 andalso
                    (SequenceOpen#markdown_sequence.size + SequenceClose#markdown_sequence.size) rem 3 =:= 0,

            %% For GFM strikethrough:
            %% * both sequences must have the same size
            %% * more than 2 markers don't work
            %% * one marker is prohibited by the spec, but supported by GH
            CannotMatchDueToGfmRules =
                SequenceClose#markdown_sequence.marker =:= $~ andalso
                    (SequenceClose#markdown_sequence.size =/= SequenceOpen#markdown_sequence.size orelse
                        SequenceClose#markdown_sequence.size > 2 orelse
                        (SequenceClose#markdown_sequence.size =:= 1 andalso
                            not Tokenizer1#markdown_tokenizer.parse_state#markdown_parse_state.options#markdown_parse_options.gfm_strikethrough_single_tilde)),

            case CannotMatchDueToSizeRules orelse CannotMatchDueToGfmRules of
                % true when CannotMatchDueToSizeRules =:= true ->
                %     io:format("\n\n[~w] INSIDE[A] resolve:attention:match_sequences(open=~w, close=~w, next_index=~w)\n~ts\n\n", [markdown:counter_get(), Open2, Close, NextIndex1, markdown_debug:rust_debug_string(Sequences1)]),
                %     find_opener(Tokenizer1, Sequences1, Close, Open2, NextIndex1);
                true ->
                    % io:format("\n\n[~w] INSIDE[B] resolve:attention:match_sequences(open=~w, close=~w, next_index=~w)\n~ts\n\n", [markdown:counter_get(), Open2, Close, NextIndex1, markdown_debug:rust_debug_string(Sequences1)]),
                    find_opener(Tokenizer1, Sequences1, Close, Open2, NextIndex1);
                false ->
                    %% We found a match!
                    % io:format("\n\n[~w:~w] BEFORE resolve:attention:match_sequences(open=~w, close=~w, next_index=~w)\n~ts\n\n", [markdown:counter_get(), markdown:stack_push(), Open2, Close, NextIndex1, markdown_debug:rust_debug_string(Sequences1)]),
                    {Tokenizer2, Sequences2, NextIndex2} = match_sequences(Tokenizer1, Sequences1, Open2, Close),
                    % io:format("\n\n[~w:~w] AFTER resolve:attention:match_sequences(open=~w, close=~w, next_index=~w)\n~ts\n\n", [markdown:counter_get(), markdown:stack_pop(), Open2, Close, NextIndex2, markdown_debug:rust_debug_string(Sequences2)]),
                    {Tokenizer2, Sequences2, NextIndex2}
            end;
        false ->
            find_opener(Tokenizer1, Sequences1, Close, Open2, NextIndex1)
    end;
find_opener(Tokenizer, Sequences, _Close, _Open, NextIndex) ->
    {Tokenizer, Sequences, NextIndex}.

%% @private
-doc """
Get sequences.
""".
-spec get_sequences(Tokenizer) -> Sequences when
    Tokenizer :: markdown_tokenizer:t(), Sequences :: markdown_vec:t(Sequence), Sequence :: markdown_sequence:t().
get_sequences(_Tokenizer = #markdown_tokenizer{events = Events, parse_state = ParseState}) ->
    {Sequences, _Index, _Stack} = get_sequences_loop(Events, ParseState, 0, markdown_vec:new(), markdown_vec:new()),
    Sequences.

%% @private
-spec get_sequences_loop(Events, ParseState, Index, Stack, Sequences) -> {Sequences, Index, Stack} when
    Events :: markdown_vec:t(markdown_event:t()),
    ParseState :: markdown_parse_state:t(),
    Index :: non_neg_integer(),
    Stack :: markdown_vec:t(non_neg_integer()),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t().
get_sequences_loop(Events, _ParseState, Index, Stack, Sequences) when Index >= ?markdown_vec_size(Events) ->
    {Sequences, Index, Stack};
get_sequences_loop(
    Events, ParseState = #markdown_parse_state{bytes = Bytes, options = Options}, Index, Stack, Sequences
) ->
    Enter = markdown_vec:get(Events, Index),
    case Enter#markdown_event.name =:= attention_sequence of
        true ->
            case Enter#markdown_event.kind =:= enter of
                true ->
                    End = Index + 1,
                    Exit = markdown_vec:get(Events, End),

                    Marker = binary:at(Bytes, Enter#markdown_event.point#markdown_point.offset),
                    BeforeChar = markdown_util_char:before_index(
                        Bytes, Enter#markdown_event.point#markdown_point.offset
                    ),
                    Before = markdown_util_char:classify_opt(BeforeChar),
                    AfterChar = markdown_util_char:after_index(Bytes, Exit#markdown_event.point#markdown_point.offset),
                    After = markdown_util_char:classify_opt(AfterChar),

                    Open =
                        (After =:= other) orelse
                            (After =:= punctuation andalso Before =/= other) orelse
                            (Marker =/= $~ andalso (AfterChar =:= {some, $*} orelse AfterChar =:= {some, $_})) orelse
                            (Marker =/= $~ andalso
                                Options#markdown_parse_options.constructs#markdown_construct_options.gfm_strikethrough =:=
                                    true andalso AfterChar =:= {some, $~}),

                    Close =
                        (Before =:= other) orelse
                            (Before =:= punctuation andalso After =/= other) orelse
                            (Marker =/= $~ andalso (BeforeChar =:= {some, $*} orelse BeforeChar =:= {some, $_})) orelse
                            (Marker =/= $~ andalso
                                Options#markdown_parse_options.constructs#markdown_construct_options.gfm_strikethrough =:=
                                    true andalso BeforeChar =:= {some, $~}),

                    OpenFinal =
                        if
                            Marker =:= $_ -> Open andalso (Before =/= other orelse not Close);
                            true -> Open
                        end,

                    CloseFinal =
                        if
                            Marker =:= $_ -> Close andalso (After =/= other orelse not Open);
                            true -> Close
                        end,

                    Sequence = #markdown_sequence{
                        index = Index,
                        stack = Stack,
                        start_point = Enter#markdown_event.point,
                        end_point = Exit#markdown_event.point,
                        size =
                            Exit#markdown_event.point#markdown_point.offset -
                                Enter#markdown_event.point#markdown_point.offset,
                        open = OpenFinal,
                        close = CloseFinal,
                        marker = Marker
                    },

                    SequencesNew = markdown_vec:push(Sequences, Sequence),
                    get_sequences_loop(Events, ParseState, Index + 1, Stack, SequencesNew);
                false ->
                    get_sequences_loop(Events, ParseState, Index + 1, Stack, Sequences)
            end;
        false ->
            case Enter#markdown_event.kind of
                enter ->
                    StackNew = markdown_vec:push(Stack, Index),
                    get_sequences_loop(Events, ParseState, Index + 1, StackNew, Sequences);
                exit ->
                    {StackNew, _} = markdown_vec:pop(Stack),
                    get_sequences_loop(Events, ParseState, Index + 1, StackNew, Sequences)
            end
    end.

%% @private
-spec mark_remaining_sequences_as_data(Tokenizer, Sequences) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(markdown_sequence:t()).
mark_remaining_sequences_as_data(Tokenizer, Sequences) ->
    mark_remaining_sequences_as_data_loop(Tokenizer, Sequences, 0).

%% @private
-spec mark_remaining_sequences_as_data_loop(Tokenizer, Sequences, Index) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Index :: markdown_types:usize().
mark_remaining_sequences_as_data_loop(Tokenizer1, Sequences, Index) ->
    case markdown_vec:size(Sequences) =< Index of
        true ->
            Tokenizer1;
        false ->
            Sequence = markdown_vec:get(Sequences, Index),
            Tokenizer2 = markdown_tokenizer:update_event(Tokenizer1, Sequence#markdown_sequence.index, fun(Event) ->
                Event#markdown_event{name = data}
            end),
            Tokenizer3 = markdown_tokenizer:update_event(Tokenizer2, Sequence#markdown_sequence.index + 1, fun(Event) ->
                Event#markdown_event{name = data}
            end),
            mark_remaining_sequences_as_data_loop(Tokenizer3, Sequences, Index + 1)
    end.

%% @private
-doc """
Match two sequences.
""".
-spec match_sequences(Tokenizer, Sequences, Open, Close) -> {Tokenizer, Sequences, Next} when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Open :: markdown_types:usize(),
    Close :: markdown_types:usize(),
    Next :: markdown_types:usize().
match_sequences(Tokenizer1, Sequences1, Open, Close) ->
    %% Where to move to next.
    %% Stay on this closing sequence for the next iteration: it
    %% might close more things.
    %% It's changed if sequences are removed.
    Next1 = Close,

    %% Number of markers to use from the sequence.
    Take =
        case markdown_vec:get(Sequences1, Open) of
            #markdown_sequence{size = OpenSize} when OpenSize > 1 ->
                case markdown_vec:get(Sequences1, Close) of
                    #markdown_sequence{size = CloseSize} when CloseSize > 1 ->
                        2;
                    _ ->
                        1
                end;
            _ ->
                1
        end,

    %% We're *on* a closing sequence, with a matching opening
    %% sequence.
    %% Now we make sure that we can't have misnested attention:
    %%
    %% ```html
    %% <em>a <strong>b</em> c</strong>
    %% ```
    %%
    %% Do that by marking everything between it as no longer
    %% possible to open anything.
    %% Theoretically we should mark as `close: false` too, but
    %% we don't look for closers backwards, so it's not needed.
    Sequences2 = mark_sequences_as_non_openers(Sequences1, Open + 1, Close),

    OpenSequence = markdown_vec:get(Sequences2, Open),
    CloseSequence = markdown_vec:get(Sequences2, Close),

    {GroupName, SeqName, TextName} =
        case OpenSequence of
            #markdown_sequence{marker = $~} ->
                {gfm_strikethrough, gfm_strikethrough_sequence, gfm_strikethrough_text};
            _ ->
                case Take of
                    1 ->
                        {emphasis, emphasis_sequence, emphasis_text};
                    2 ->
                        {strong, strong_sequence, strong_text}
                end
        end,
    OpenIndex = OpenSequence#markdown_sequence.index,
    CloseIndex = CloseSequence#markdown_sequence.index,
    OpenExit = OpenSequence#markdown_sequence.end_point,
    CloseEnter = CloseSequence#markdown_sequence.start_point,

    %% No need to worry about `VS`, because sequences are only actual characters.
    Sequences3 = markdown_vec:update(Sequences2, Open, fun(Seq) ->
        Seq#markdown_sequence{
            size = Seq#markdown_sequence.size - Take,
            end_point = Seq#markdown_sequence.end_point#markdown_point{
                column = Seq#markdown_sequence.end_point#markdown_point.column - Take,
                offset = Seq#markdown_sequence.end_point#markdown_point.offset - Take
            }
        }
    end),
    Sequences4 = markdown_vec:update(Sequences3, Close, fun(Seq) ->
        Seq#markdown_sequence{
            size = Seq#markdown_sequence.size - Take,
            start_point = Seq#markdown_sequence.start_point#markdown_point{
                column = Seq#markdown_sequence.start_point#markdown_point.column + Take,
                offset = Seq#markdown_sequence.start_point#markdown_point.offset + Take
            }
        }
    end),
    % io:format("\n\n[~w] INSPECT[A] resolve:attention:match_sequences(open=~w, close=~w)\n~ts\n\n", [markdown:counter_get(), Open, Close, markdown_debug:rust_debug_string(Sequences4)]),

    %% Opening.
    OpeningEvents = markdown_vec:from_list([
        #markdown_event{
            kind = enter,
            name = GroupName,
            point = (markdown_vec:get(Sequences4, Open))#markdown_sequence.end_point,
            link = none
        },
        #markdown_event{
            kind = enter,
            name = SeqName,
            point = (markdown_vec:get(Sequences4, Open))#markdown_sequence.end_point,
            link = none
        },
        #markdown_event{
            kind = exit,
            name = SeqName,
            point = OpenExit,
            link = none
        },
        #markdown_event{
            kind = enter,
            name = TextName,
            point = OpenExit,
            link = none
        }
    ]),
    Tokenizer2 = markdown_tokenizer:map_add_before(Tokenizer1, OpenIndex + 2, 0, OpeningEvents),
    % io:format("\n\n[~w] INSPECT[B] resolve:attention:match_sequences(open=~w, close=~w):opening\n~ts\n\n", [markdown:counter_get(), Open, Close, markdown_debug:rust_debug_string(Tokenizer2)]),

    %% Closing.
    ClosingEvents = markdown_vec:from_list([
        #markdown_event{
            kind = exit,
            name = TextName,
            point = CloseEnter,
            link = none
        },
        #markdown_event{
            kind = enter,
            name = SeqName,
            point = CloseEnter,
            link = none
        },
        #markdown_event{
            kind = exit,
            name = SeqName,
            point = (markdown_vec:get(Sequences4, Close))#markdown_sequence.start_point,
            link = none
        },
        #markdown_event{
            kind = exit,
            name = GroupName,
            point = (markdown_vec:get(Sequences4, Close))#markdown_sequence.start_point,
            link = none
        }
    ]),
    Tokenizer3 = markdown_tokenizer:map_add(Tokenizer2, CloseIndex, 0, ClosingEvents),
    % io:format("\n\n[~w] INSPECT[C] resolve:attention:match_sequences(open=~w, close=~w):closing\n~ts\n\n", [markdown:counter_get(), Open, Close, markdown_debug:rust_debug_string(Tokenizer3)]),

    %% Handle close sequence
    {Tokenizer4, Sequences5, Next2} = match_sequences__close(Tokenizer3, Sequences4, Next1, Close, CloseIndex),

    %% Handle open sequence
    {Tokenizer5, Sequences6, Next3} = match_sequences__open(Tokenizer4, Sequences5, Next2, Open, OpenIndex),

    {Tokenizer5, Sequences6, Next3}.

%% @private
-compile({inline, [match_sequences__close/5]}).
-spec match_sequences__close(Tokenizer, Sequences, Next, Close, CloseIndex) -> {Tokenizer, Sequences, Next} when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Next :: markdown_types:usize(),
    Close :: markdown_types:usize(),
    CloseIndex :: markdown_types:usize().
match_sequences__close(Tokenizer1, Sequences1, Next1, Close, CloseIndex) ->
    case markdown_vec:get(Sequences1, Close) of
        #markdown_sequence{size = 0} ->
            %% Remove closing sequence if fully used.
            Sequences2 = markdown_vec:remove(Sequences1, Close),
            Tokenizer2 = markdown_tokenizer:map_add(Tokenizer1, CloseIndex, 2, markdown_vec:new()),
            {Tokenizer2, Sequences2, Next1};
        _ ->
            %% Shift remaining closing sequence forward.
            %% Do it here because a sequence can open and close different
            %% other sequences, and the remainder can be on any side or
            %% somewhere in the middle.
            CloseSequencePoint = (markdown_vec:get(Sequences1, Close))#markdown_sequence.start_point,
            Tokenizer2 = markdown_tokenizer:update_event(Tokenizer1, CloseIndex, fun(Event) ->
                Event#markdown_event{point = CloseSequencePoint}
            end),
            {Tokenizer2, Sequences1, Next1}
    end.

%% @private
-compile({inline, [match_sequences__open/5]}).
-spec match_sequences__open(Tokenizer, Sequences, Next, Open, OpenIndex) -> {Tokenizer, Sequences, Next} when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Next :: markdown_types:usize(),
    Open :: markdown_types:usize(),
    OpenIndex :: markdown_types:usize().
match_sequences__open(Tokenizer1, Sequences1, Next1, Open, OpenIndex) ->
    case markdown_vec:get(Sequences1, Open) of
        #markdown_sequence{size = 0} ->
            Sequences2 = markdown_vec:remove(Sequences1, Open),
            Tokenizer2 = markdown_tokenizer:map_add(Tokenizer1, OpenIndex, 2, markdown_vec:new()),
            %% Everything shifts one to the left, account for it in next iteration.
            Next2 = Next1 - 1,
            {Tokenizer2, Sequences2, Next2};
        _ ->
            OpenSequencePoint = (markdown_vec:get(Sequences1, Open))#markdown_sequence.end_point,
            Tokenizer2 = markdown_tokenizer:update_event(Tokenizer1, OpenIndex + 1, fun(Event) ->
                Event#markdown_event{point = OpenSequencePoint}
            end),
            {Tokenizer2, Sequences1, Next1}
    end.

%% @private
-spec mark_sequences_as_non_openers(Sequences, Between, Close) -> UpdatedSequences when
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Between :: markdown_types:usize(),
    Close :: markdown_types:usize(),
    UpdatedSequences :: markdown_vec:t(markdown_sequence:t()).
mark_sequences_as_non_openers(Sequences, Between, Close) when Between < Close ->
    UpdatedSequences = markdown_vec:update(Sequences, Between, fun(Sequence) ->
        Sequence#markdown_sequence{open = false}
    end),
    mark_sequences_as_non_openers(UpdatedSequences, Between + 1, Close);
mark_sequences_as_non_openers(Sequences, _Between, _Close) ->
    Sequences.

%% @private
-spec match_sequences_loop(Tokenizer, Sequences, Close) -> {Tokenizer, Sequences} when
    Tokenizer :: markdown_tokenizer:t(),
    Sequences :: markdown_vec:t(Sequence),
    Sequence :: markdown_sequence:t(),
    Close :: markdown_types:usize().
match_sequences_loop(Tokenizer1, Sequences1, Close) when Close >= 0 andalso Close < ?markdown_vec_size(Sequences1) ->
    SequenceClose = markdown_vec:get(Sequences1, Close),
    NextIndex1 = Close + 1,
    %% Find a sequence that can close.
    case SequenceClose of
        #markdown_sequence{close = true} ->
            % io:format("\n\n[~w] INSPECT resolve:attention:find_opener(open=~w, close=~w, next_index=~w)\n~ts\n\n", [markdown:counter_get(), Close, Close, NextIndex1, markdown_debug:rust_debug_string(Sequences1)]),
            {Tokenizer2, Sequences2, NextIndex2} = find_opener(Tokenizer1, Sequences1, Close, Close, NextIndex1),
            match_sequences_loop(Tokenizer2, Sequences2, NextIndex2);
        _ ->
            match_sequences_loop(Tokenizer1, Sequences1, NextIndex1)
    end;
match_sequences_loop(Tokenizer, Sequences, _Close) ->
    {Tokenizer, Sequences}.
