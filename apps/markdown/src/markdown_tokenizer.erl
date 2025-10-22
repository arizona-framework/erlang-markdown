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
-module(markdown_tokenizer).
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-05", modified => "2025-03-05"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_resolve.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

-include_lib("stdlib/include/assert.hrl").

%% API
-export([
    attempt/3,
    bytes/1,
    check/3,
    consume/1,
    define_skip/2,
    enter/2,
    enter_link/3,
    events_get/2,
    exit/2,
    flush/3,
    map_add/4,
    map_add_before/4,
    map_consume/1,
    new/2,
    push/4,
    register_resolver/2,
    register_resolver_before/2,
    update_event/3
]).

%% Types
-doc "How to handle a byte.".
-type byte_action() ::
    %% This is a normal byte.
    %%
    %% Includes replaced bytes.
    {normal, byte()}
    %% This byte must be ignored.
    | ignore
    %% This is a new byte.
    | {insert, byte()}.

-type t() :: #markdown_tokenizer{}.

-export_type([
    byte_action/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Stack an attempt, moving to `ok` on [`State::Ok`][] and `nok` on
[`State::Nok`][], reverting in the latter case.
""".
-spec attempt(Tokenizer, Ok, Nok) -> Tokenizer when
    Tokenizer :: t(), Ok :: markdown_state:t(), Nok :: markdown_state:t().
attempt(Tokenizer1 = #markdown_tokenizer{attempts = Attempts1}, Ok, Nok) ->
    %% Always capture (and restore) when checking.
    %% No need to capture (and restore) when `nok` is `State::Nok`, because the
    %% parent attempt will do it.
    OptionProgress =
        case Nok of
            nok ->
                none;
            _ ->
                {some, capture(Tokenizer1)}
        end,
    Attempt = markdown_attempt:attempt(Ok, Nok, OptionProgress),
    Attempts2 = markdown_vec:push(Attempts1, Attempt),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{attempts = Attempts2},
    Tokenizer2.

-compile({inline, [bytes/1]}).
-spec bytes(Tokenizer) -> Bytes when Tokenizer :: t(), Bytes :: binary().
bytes(#markdown_tokenizer{parse_state = #markdown_parse_state{bytes = Bytes}}) ->
    Bytes.

-doc """
Stack an attempt, moving to `ok` on [`State::Ok`][] and `nok` on
[`State::Nok`][], reverting in both cases.
""".
-spec check(Tokenizer, Ok, Nok) -> Tokenizer when
    Tokenizer :: t(), Ok :: markdown_state:t(), Nok :: markdown_state:t().
check(Tokenizer1 = #markdown_tokenizer{attempts = Attempts1}, Ok, Nok) ->
    %% Always capture (and restore) when checking.
    %% No need to capture (and restore) when `nok` is `State::Nok`, because the
    %% parent attempt will do it.
    OptionProgress = {some, capture(Tokenizer1)},
    Attempt = markdown_attempt:check(Ok, Nok, OptionProgress),
    Attempts2 = markdown_vec:push(Attempts1, Attempt),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{attempts = Attempts2},
    Tokenizer2.

-doc """
Consume the current byte.
Each state function is expected to call this to signal that this code is
used, or call a next function.
""".
-spec consume(Tokenizer) -> Tokenizer when Tokenizer :: t().
consume(Tokenizer1 = #markdown_tokenizer{consumed = false}) ->
    Tokenizer2 = move_one(Tokenizer1),
    Tokenizer3 = Tokenizer2#markdown_tokenizer{
        previous = Tokenizer2#markdown_tokenizer.current,
        %% While we’re not at eof, it is at least better to not have the
        %% same current code as `previous` *and* `current`.
        current = none,
        %% Mark as consumed.
        consumed = true
    },
    Tokenizer3.

-doc """
Define a jump between two places.

This defines to which future index we move after a line ending.
""".
-spec define_skip(Tokenizer, Point) -> Tokenizer when Tokenizer :: t(), Point :: markdown_point:t().
define_skip(
    Tokenizer1 = #markdown_tokenizer{
        column_start = ColumnStart1, first_line = FirstLine, parse_state = #markdown_parse_state{bytes = Bytes}
    },
    Point1 = #markdown_point{}
) ->
    Point2 = move_point_back(Bytes, Point1),
    At = Point2#markdown_point.line - FirstLine,
    Info = markdown_point:to_index(Point2),
    ColumnStart2 =
        case At >= markdown_vec:size(ColumnStart1) of
            true ->
                markdown_vec:push(ColumnStart1, Info);
            false ->
                markdown_vec:set(ColumnStart1, At, Info)
        end,
    Tokenizer2 = Tokenizer1#markdown_tokenizer{column_start = ColumnStart2},
    Tokenizer3 = account_for_potential_skip(Tokenizer2),
    Tokenizer3.

-doc """
Mark the start of a semantic label.
""".
-spec enter(Tokenizer, Name) -> Tokenizer when Tokenizer :: t(), Name :: markdown_event:name().
enter(Tokenizer = #markdown_tokenizer{}, Name) ->
    enter_impl(Tokenizer, Name, none).

-doc """
Enter with a link.
""".
-spec enter_link(Tokenizer, Name, Link) -> Tokenizer when
    Tokenizer :: t(), Name :: markdown_event:name(), Link :: markdown_event_link:t().
enter_link(Tokenizer = #markdown_tokenizer{}, Name, Link = #markdown_event_link{}) ->
    enter_impl(Tokenizer, Name, {some, Link}).

-spec events_get(Tokenizer, Index) -> Event when
    Tokenizer :: t(), Index :: markdown_vec:index(), Event :: markdown_event:t().
events_get(#markdown_tokenizer{events = Events}, Index) when Index >= 0 andalso Index < ?markdown_vec_size(Events) ->
    markdown_vec:get(Events, Index).

-doc """
Mark the end of a semantic label.
""".
-spec exit(Tokenizer, Name) -> Tokenizer when Tokenizer :: t(), Name :: markdown_event:name().
exit(
    Tokenizer1 = #markdown_tokenizer{
        events = Events1,
        line_start = LineStart,
        parse_state = #markdown_parse_state{bytes = Bytes},
        point = Point1,
        previous = OptionPrevious,
        stack = Stack1
    },
    Name
) ->
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Stack1), "cannot close w/o open tokens"),
    ?assertMatch({some, Name}, markdown_vec:last_option(Stack1), "expected exit event to match current event"),
    %% END: assertions
    {Stack2, Current} = markdown_vec:pop(Stack1),
    %% BEGIN: assertions
    ?assertMatch({some, _}, markdown_vec:last_option(Events1), "cannot close w/o open event"),
    %% END: assertions
    Previous = markdown_vec:last(Events1),
    %% BEGIN: assertions
    ?assert(
        Current =/= Previous#markdown_event.name orelse
            Previous#markdown_event.point#markdown_point.offset =/= Point1#markdown_point.offset orelse
            Previous#markdown_event.point#markdown_point.virtual =/= Point1#markdown_point.virtual,
        "expected non-empty event"
    ),
    case markdown_event:is_void(Name) of
        true ->
            ?assert(
                Current =:= Previous#markdown_event.name, "expected event to be void, instead of including something"
            );
        false ->
            ok
    end,
    %% END: assertions
    %% A bit weird, but if we exit right after a line ending, we *don’t* want to consider
    %% potential skips.
    Point2 =
        case OptionPrevious of
            {some, $\n} ->
                LineStart;
            _ ->
                move_point_back(Bytes, Point1)
        end,
    Event = markdown_event:Name(exit, Point2, none),
    Events2 = markdown_vec:push(Events1, Event),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{
        events = Events2,
        stack = Stack2
    },
    Tokenizer2.

-doc """
Flush.
""".
-spec flush(Tokenizer, State, Resolve) -> {Tokenizer, {ok, Subresult} | {error, Message}} when
    Tokenizer :: t(),
    State :: markdown_state:t(),
    Resolve :: boolean(),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
flush(Tokenizer1 = #markdown_tokenizer{point = Point}, State1, Resolve) when is_boolean(Resolve) ->
    To = markdown_point:to_index(Point),
    % io:format("\n\n[~w] BEFORE push_impl\n~ts\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State1), markdown_debug:rust_debug_string(Tokenizer1)]),
    {Tokenizer2, State2} = push_impl(Tokenizer1, To, To, State1, true),
    % io:format(user, "~ts\n", [markdown_debug:rust_debug_string(Tokenizer2#markdown_tokenizer.events)]),
    % io:format("\n\n[~w] AFTER push_impl\n~ts\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(State2), markdown_debug:rust_debug_string(Tokenizer2)]),
    case markdown_state:to_result(State2) of
        ok ->
            #markdown_tokenizer{
                resolvers = Resolvers,
                tokenize_state =
                    TokenizeState1 = #markdown_tokenize_state{gfm_footnote_definitions = FnDefs, definitions = Defs}
            } = Tokenizer2,
            TokenizeState2 = TokenizeState1#markdown_tokenize_state{
                gfm_footnote_definitions = markdown_vec:new(), definitions = markdown_vec:new()
            },
            Tokenizer3 = Tokenizer2#markdown_tokenizer{tokenize_state = TokenizeState2},
            Subresult1 = #markdown_subresult{
                done = false,
                gfm_footnote_definitions = FnDefs,
                definitions = Defs
            },
            case Resolve of
                true ->
                    % io:format("RESOLVERS: ~ts~n", [markdown_debug:rust_debug_string(Resolvers)]),
                    % io:format("\n\n[~w] BEFORE flush resolve\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer3)]),
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{resolvers = markdown_vec:new()},
                    ResolversIterator = markdown_vec:iterator(Resolvers),
                    case flush__resolve_loop(Tokenizer4, ResolversIterator, Subresult1) of
                        {Tokenizer5 = #markdown_tokenizer{events = Events1, map = EditMap1}, {ok, Subresult2}} ->
                            % io:format("\n\n[~w] AFTER flush resolve\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer5)]),
                            % io:format("\n\n[~w] BEFORE consume\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer5)]),
                            {EditMap2, Events2} = markdown_edit_map:consume(EditMap1, Events1),
                            Tokenizer6 = Tokenizer5#markdown_tokenizer{events = Events2, map = EditMap2},
                            % C = markdown:counter_get(),
                            % io:format("\n\n[~w] AFTER consume\n\n~ts\n\n", [C, markdown_debug:rust_debug_string(Tokenizer6)]),
                            % case C of
                            %     3 -> markdown_debug:redbug_start("markdown_vec:_->return", #{time => 5000, msgs => 1000, print_file => "/tmp/redbug.txt"});
                            %     _ -> ok
                            % end,
                            % io:format("\n\n[~w] AFTER consume\n\n~ts\n\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer6)]),
                            {Tokenizer6, {ok, Subresult2}};
                        {Tokenizer5, {error, Message}} ->
                            {Tokenizer5, {error, Message}}
                    end;
                false ->
                    {Tokenizer3, {ok, Subresult1}}
            end;
        {error, Message} ->
            {Tokenizer2, {error, Message}}
    end.

-spec map_add(Tokenizer, Index, Remove, Add) -> Tokenizer when
    Tokenizer :: t(),
    Index :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    Add :: markdown_vec:t(Event) | [Event],
    Event :: markdown_event:t().
map_add(Tokenizer1 = #markdown_tokenizer{map = EditMap1}, Index, Remove, Add) when
    ?is_usize(Index) andalso ?is_usize(Remove) andalso (is_record(Add, markdown_vec) orelse is_list(Add))
->
    EditMap2 = markdown_edit_map:add(EditMap1, Index, Remove, Add),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{map = EditMap2},
    Tokenizer2.

-spec map_add_before(Tokenizer, Index, Remove, Add) -> Tokenizer when
    Tokenizer :: t(),
    Index :: markdown_types:usize(),
    Remove :: markdown_types:usize(),
    Add :: markdown_vec:t(Event) | [Event],
    Event :: markdown_event:t().
map_add_before(Tokenizer1 = #markdown_tokenizer{map = EditMap1}, Index, Remove, Add) when
    ?is_usize(Index) andalso ?is_usize(Remove) andalso (is_record(Add, markdown_vec) orelse is_list(Add))
->
    EditMap2 = markdown_edit_map:add_before(EditMap1, Index, Remove, Add),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{map = EditMap2},
    Tokenizer2.

-spec map_consume(Tokenizer) -> Tokenizer when Tokenizer :: t().
map_consume(Tokenizer1 = #markdown_tokenizer{events = Events1, map = EditMap1}) ->
    {EditMap2, Events2} = markdown_edit_map:consume(EditMap1, Events1),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events2, map = EditMap2},
    Tokenizer2.

-spec new(Start, ParseState) -> Tokenizer when
    Start :: markdown_point:t(), ParseState :: markdown_parse_state:t(), Tokenizer :: t().
new(Start = #markdown_point{line = FirstLine}, ParseState = #markdown_parse_state{}) ->
    #markdown_tokenizer{
        previous = none,
        current = none,
        column_start = markdown_vec:new(),
        first_line = FirstLine,
        line_start = Start,
        consumed = true,
        attempts = markdown_vec:new(),
        point = Start,
        stack = markdown_vec:new(),
        events = markdown_vec:new(),
        parse_state = ParseState,
        tokenize_state = #markdown_tokenize_state{},
        map = markdown_edit_map:new(),
        interrupt = false,
        pierce = false,
        concrete = false,
        lazy = false,
        resolvers = markdown_vec:new()
    }.

-spec push(Tokenizer, From, To, State) -> {Tokenizer, State} when
    Tokenizer :: t(),
    From :: markdown_index:t(),
    To :: markdown_index:t(),
    State :: markdown_state:t().
push(Tokenizer = #markdown_tokenizer{}, From = #markdown_index{}, To = #markdown_index{}, State) ->
    push_impl(Tokenizer, From, To, State, false).

-doc """
Register a resolver.
""".
-spec register_resolver(Tokenizer, Resolver) -> Tokenizer when Tokenizer :: t(), Resolver :: markdown_resolve:name().
register_resolver(Tokenizer1 = #markdown_tokenizer{resolvers = Resolvers1}, Resolver) when
    ?is_markdown_resolve_name(Resolver)
->
    case markdown_vec:contains(Resolvers1, Resolver) of
        false ->
            Resolvers2 = markdown_vec:push(Resolvers1, Resolver),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{resolvers = Resolvers2},
            Tokenizer2;
        true ->
            Tokenizer1
    end.

-doc """
Register a resolver, before others.
""".
-spec register_resolver_before(Tokenizer, Resolver) -> Tokenizer when
    Tokenizer :: t(), Resolver :: markdown_resolve:name().
register_resolver_before(Tokenizer1 = #markdown_tokenizer{resolvers = Resolvers1}, Resolver) when
    ?is_markdown_resolve_name(Resolver)
->
    case markdown_vec:contains(Resolvers1, Resolver) of
        false ->
            Resolvers2 = markdown_vec:insert(Resolvers1, 0, Resolver),
            Tokenizer2 = Tokenizer1#markdown_tokenizer{resolvers = Resolvers2},
            Tokenizer2;
        true ->
            Tokenizer1
    end.

-spec update_event(Tokenizer, Index, UpdateFun) -> Tokenizer when
    Tokenizer :: t(), Index :: markdown_vec:index(), UpdateFun :: fun((Event) -> Event), Event :: markdown_event:t().
update_event(Tokenizer1 = #markdown_tokenizer{events = Events1}, Index, UpdateFun) when
    Index >= 0 andalso Index =< ?markdown_vec_size(Events1) andalso is_function(UpdateFun, 1)
->
    Events2 = markdown_vec:update(Events1, Index, UpdateFun),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{events = Events2},
    Tokenizer2.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-doc """
Increment the current positional info if we're right after a line
ending, which has a skip defined.
""".
-spec account_for_potential_skip(Tokenizer) -> Tokenizer when Tokenizer :: t().
account_for_potential_skip(
    Tokenizer = #markdown_tokenizer{
        column_start = ColumnStart = #markdown_vec{size = ColumnStartLength},
        first_line = FirstLine,
        point = #markdown_point{column = 1, line = Line}
    }
) when (Line - FirstLine) < ColumnStartLength ->
    move_to(Tokenizer, markdown_vec:get(ColumnStart, Line - FirstLine));
account_for_potential_skip(Tokenizer = #markdown_tokenizer{}) ->
    Tokenizer.

%% @private
-doc """
Figure out how to handle a byte.
""".
-spec byte_action(Bytes, Point) -> ByteAction when
    Bytes :: nonempty_binary(), Point :: markdown_point:t(), ByteAction :: byte_action().
byte_action(Bytes, #markdown_point{offset = Offset}) when binary_part(Bytes, Offset, 2) =:= <<$\r, $\n>> ->
    %% CLRF
    ignore;
byte_action(Bytes, #markdown_point{offset = Offset}) when binary_part(Bytes, Offset, 1) =:= <<$\r>> ->
    %% CR
    {normal, $\n};
byte_action(Bytes, #markdown_point{column = Column, offset = Offset, virtual = Virtual}) when
    binary_part(Bytes, Offset, 1) =:= <<$\t>>
->
    NextVirtual =
        case (Column rem ?TAB_SIZE) of
            0 -> 0;
            Remainder -> ?TAB_SIZE - Remainder
        end,
    case Virtual of
        0 when NextVirtual =:= 0 ->
            {normal, $\t};
        0 ->
            {insert, $\t};
        _ when NextVirtual =:= 0 ->
            {normal, $\s};
        _ ->
            {insert, $\s}
    end;
byte_action(Bytes, #markdown_point{offset = Offset}) when Offset < byte_size(Bytes) ->
    Byte = binary:at(Bytes, Offset),
    {normal, Byte}.

%% @private
-doc """
Capture the tokenizer progress.
""".
-spec capture(Tokenizer) -> Progress when Tokenizer :: t(), Progress :: markdown_progress:t().
capture(#markdown_tokenizer{
    previous = OptionPrevious, current = OptionCurrent, point = Point, events = Events, stack = Stack
}) ->
    EventsLen = markdown_vec:size(Events),
    StackLen = markdown_vec:size(Stack),
    markdown_progress:new(EventsLen, StackLen, OptionPrevious, OptionCurrent, Point).

%% @private
-doc """
Enter.
""".
-spec enter_impl(Tokenizer, Name, OptionLink) -> Tokenizer when
    Tokenizer :: t(),
    Name :: markdown_event:name(),
    OptionLink :: markdown_types:option(Link),
    Link :: markdown_event_link:t().
enter_impl(
    Tokenizer1 = #markdown_tokenizer{
        events = Events1, parse_state = #markdown_parse_state{bytes = Bytes}, point = Point1, stack = Stack1
    },
    Name,
    OptionLink
) when ?is_option_record(OptionLink, markdown_event_link) ->
    Point2 = move_point_back(Bytes, Point1),
    Stack2 = markdown_vec:push(Stack1, Name),
    Event = markdown_event:Name(enter, Point2, OptionLink),
    Events2 = markdown_vec:push(Events1, Event),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{
        events = Events2,
        stack = Stack2
    },
    Tokenizer2.

%% @private
-doc """
Prepare for a next byte to get consumed.
""".
-spec expect(Tokenizer, OptionByte) -> Tokenizer when Tokenizer :: t(), OptionByte :: markdown_types:option(byte()).
expect(Tokenizer1 = #markdown_tokenizer{consumed = true}, OptionByte) when ?is_option_u8(OptionByte) ->
    Tokenizer2 = Tokenizer1#markdown_tokenizer{
        consumed = false,
        current = OptionByte
    },
    Tokenizer2.

%% @private
-spec flush__resolve_loop(Tokenizer, ResolversIterator, Subresult) ->
    {Tokenizer, {ok, Subresult} | {error, Message}}
when
    Tokenizer :: t(),
    ResolversIterator :: markdown_vec:iterator(Resolver),
    Resolver :: markdown_resolve:name(),
    Subresult :: markdown_subresult:t(),
    Message :: markdown_message:t().
flush__resolve_loop(Tokenizer1 = #markdown_tokenizer{}, ResolversIterator1, Subresult1 = #markdown_subresult{}) ->
    case markdown_vec:next(ResolversIterator1) of
        {_Index, Resolver, ResolversIterator2} ->
            case markdown_resolve:call(Tokenizer1, Resolver) of
                {Tokenizer2, {ok, {some, Subresult2}}} ->
                    Subresult3 = markdown_subresult:append(Subresult1, Subresult2),
                    flush__resolve_loop(Tokenizer2, ResolversIterator2, Subresult3);
                {Tokenizer2, {ok, none}} ->
                    flush__resolve_loop(Tokenizer2, ResolversIterator2, Subresult1);
                {Tokenizer2, {error, Message}} ->
                    {Tokenizer2, {error, Message}}
            end;
        none ->
            {Tokenizer1, {ok, Subresult1}}
    end.

%% @private
-doc """
Apply tokenizer progress.
""".
-spec free(Tokenizer, Progress) -> Tokenizer when
    Tokenizer :: markdown_tokenizer:t(), Progress :: markdown_progress:t().
free(
    Tokenizer1 = #markdown_tokenizer{
        events = Events1 = #markdown_vec{size = Events1Length}, stack = Stack1 = #markdown_vec{size = Stack1Length}
    },
    #markdown_progress{
        events_len = EventsLen, stack_len = StackLen, previous = OptionPrevious, current = OptionCurrent, point = Point
    }
) when Events1Length >= EventsLen andalso Stack1Length >= StackLen ->
    Events2 = markdown_vec:truncate(Events1, EventsLen),
    Stack2 = markdown_vec:truncate(Stack1, StackLen),
    Tokenizer2 = Tokenizer1#markdown_tokenizer{
        previous = OptionPrevious,
        current = OptionCurrent,
        point = Point,
        events = Events2,
        stack = Stack2
    },
    Tokenizer2.

%% @private
-doc """
Move to the next (virtual) byte.
""".
-spec move_one(Tokenizer) -> Tokenizer when Tokenizer :: t().
move_one(
    Tokenizer1 = #markdown_tokenizer{
        column_start = ColumnStart1 = #markdown_vec{size = ColumnStart1Length},
        first_line = FirstLine,
        parse_state = #markdown_parse_state{bytes = Bytes},
        point = Point1
    }
) ->
    case byte_action(Bytes, Point1) of
        ignore ->
            Point2 = Point1#markdown_point{
                offset = Point1#markdown_point.offset + 1
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{
                point = Point2
            },
            Tokenizer2;
        {insert, Byte} ->
            Point2 = Point1#markdown_point{
                column = Point1#markdown_point.column + 1,
                virtual = Point1#markdown_point.virtual + 1
            },
            Tokenizer2 = Tokenizer1#markdown_tokenizer{
                point = Point2,
                previous = {some, Byte}
            },
            Tokenizer2;
        {normal, Byte} ->
            Point2 = Point1#markdown_point{
                offset = Point1#markdown_point.offset + 1,
                virtual = 0
            },
            case Byte of
                $\n ->
                    Point3 = Point2#markdown_point{
                        line = Point2#markdown_point.line + 1,
                        column = 1
                    },
                    ColumnStart2 =
                        case (Point3#markdown_point.line - FirstLine + 1) > ColumnStart1Length of
                            false ->
                                ColumnStart1;
                            true ->
                                markdown_vec:push(ColumnStart1, markdown_point:to_index(Point3))
                        end,
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{
                        column_start = ColumnStart2,
                        line_start = Point3,
                        point = Point3,
                        previous = {some, Byte}
                    },
                    Tokenizer3 = account_for_potential_skip(Tokenizer2),
                    Tokenizer3;
                _ ->
                    Point3 = Point2#markdown_point{
                        column = Point2#markdown_point.column + 1
                    },
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{
                        point = Point3,
                        previous = {some, Byte}
                    },
                    Tokenizer2
            end
    end.

%% @private
-doc """
Move back past ignored bytes.
""".
-spec move_point_back(Bytes, Point) -> Point when Bytes :: binary(), Point :: markdown_point:t().
move_point_back(Bytes, Point1 = #markdown_point{offset = Offset1}) when is_binary(Bytes) andalso Offset1 > 0 ->
    Offset2 = Offset1 - 1,
    Point2 = Point1#markdown_point{offset = Offset2},
    Action = byte_action(Bytes, Point2),
    case Action of
        ignore ->
            move_point_back(Bytes, Point2);
        _ ->
            Point1
    end;
move_point_back(Bytes, Point = #markdown_point{}) when is_binary(Bytes) ->
    Point.

%% @private
-doc """
Move (virtual) bytes.
""".
-spec move_to(Tokenizer, To) -> Tokenizer when Tokenizer :: t(), To :: markdown_index:t().
move_to(
    Tokenizer1 = #markdown_tokenizer{point = #markdown_point{offset = Offset, virtual = Virtual}},
    To = #markdown_index{offset = ToOffset, virtual = ToVirtual}
) when Offset < ToOffset orelse (Offset =:= ToOffset andalso Virtual < ToVirtual) ->
    Tokenizer2 = move_one(Tokenizer1),
    move_to(Tokenizer2, To);
move_to(Tokenizer = #markdown_tokenizer{}, _To = #markdown_index{}) ->
    Tokenizer.

%% @private
-spec push_impl(Tokenizer, From, To, State, Flush) -> {Tokenizer, State} when
    Tokenizer :: t(),
    From :: markdown_index:t(),
    To :: markdown_index:t(),
    State :: markdown_state:t(),
    Flush :: boolean().
push_impl(
    Tokenizer1 = #markdown_tokenizer{point = #markdown_point{offset = PointOffset, virtual = PointVirtual}},
    From = #markdown_index{offset = FromOffset, virtual = FromVirtual},
    To = #markdown_index{},
    State1,
    Flush
) when
    (FromOffset > PointOffset orelse (FromOffset =:= PointOffset andalso FromVirtual >= PointVirtual)) andalso
        is_boolean(Flush)
->
    % io:format("\n\n[~w] BEFORE move_to\n\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer1)]),
    Tokenizer2 = move_to(Tokenizer1, From),
    % io:format("\n\n[~w] AFTER move_to\n\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer2)]),
    % io:format("\n\n[~w] BEFORE loop\n\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer2)]),
    {Tokenizer3, State2} = push_impl_loop(Tokenizer2, To, State1, Flush),
    % io:format("\n\n[~w] AFTER loop\n\n~ts\n", [markdown:counter_get(), markdown_debug:rust_debug_string(Tokenizer3)]),
    Tokenizer4 = Tokenizer3#markdown_tokenizer{consumed = true},
    {Tokenizer4, State2}.

%% @private
-spec push_impl_loop(Tokenizer, To, State, Flush) -> {Tokenizer, State} when
    Tokenizer :: t(), To :: markdown_index:t(), State :: markdown_state:t(), Flush :: boolean().
push_impl_loop(
    Tokenizer1 = #markdown_tokenizer{
        attempts = Attempts1, parse_state = #markdown_parse_state{bytes = Bytes}, point = Point
    },
    To = #markdown_index{},
    State1,
    Flush
) when is_boolean(Flush) ->
    % io:format("\n\n[~w] PRE loop\n~ts\n~ts\n", [
    %     markdown:counter_get(), markdown_debug:rust_debug_string(State1), markdown_debug:rust_debug_string(Tokenizer1)
    % ]),
    case State1 of
        {error, _} ->
            {Tokenizer1, State1};
        _ when State1 =:= ok orelse State1 =:= nok ->
            case markdown_vec:pop_option(Attempts1) of
                {Attempts2,
                    {some, #markdown_attempt{
                        kind = AttemptKind, ok = AttemptOk, nok = AttemptNok, progress = OptionProgress
                    }}} ->
                    Tokenizer2 = Tokenizer1#markdown_tokenizer{attempts = Attempts2},
                    Tokenizer3 =
                        case (AttemptKind =:= check orelse State1 =:= nok) andalso ?is_option_some(OptionProgress) of
                            true ->
                                {some, Progress} = OptionProgress,
                                free(Tokenizer2, Progress);
                            false ->
                                Tokenizer2
                        end,
                    Tokenizer4 = Tokenizer3#markdown_tokenizer{consumed = true},
                    State2 =
                        case State1 of
                            ok -> AttemptOk;
                            nok -> AttemptNok
                        end,
                    push_impl_loop(Tokenizer4, To, State2, Flush);
                {Attempts1, none} ->
                    {Tokenizer1, State1}
            end;
        {next, Name} ->
            % io:format("\n\n[~w] BEFORE next(~ts)\n~ts\n~ts\n", [
            %     markdown:counter_get(), markdown_debug:rust_debug_string(Name), markdown_debug:rust_debug_string(State1), markdown_debug:rust_debug_string(Tokenizer1)
            % ]),
            MaybeOptionAction =
                case
                    Point#markdown_point.offset < To#markdown_index.offset orelse
                        (Point#markdown_point.offset =:= To#markdown_index.offset andalso
                            Point#markdown_point.virtual < To#markdown_index.virtual)
                of
                    true ->
                        {some, byte_action(Bytes, Point)};
                    false when Flush =:= true ->
                        none;
                    false ->
                        break
                end,
            case MaybeOptionAction of
                break ->
                    {Tokenizer1, State1};
                {some, ignore} ->
                    Tokenizer2 = move_one(Tokenizer1),
                    % io:format("\n\n[~w] AFTER next(~ts)\n~ts\n~ts\n", [
                    %     markdown:counter_get(), markdown_debug:rust_debug_string(Name), markdown_debug:rust_debug_string(State1), markdown_debug:rust_debug_string(Tokenizer2)
                    % ]),
                    push_impl_loop(Tokenizer2, To, State1, Flush);
                _ ->
                    OptionByte =
                        case MaybeOptionAction of
                            {some, {insert, Byte}} ->
                                {some, Byte};
                            {some, {normal, Byte}} ->
                                {some, Byte};
                            none ->
                                none
                        end,
                    Tokenizer2 = expect(Tokenizer1, OptionByte),
                    % io:format("\n\n[~w] BEFORE call(~ts)\n~ts\n~ts\n", [
                    %     markdown:counter_get(), markdown_debug:rust_debug_string(Name), markdown_debug:rust_debug_string(State1), markdown_debug:rust_debug_string(Tokenizer2)
                    % ]),
                    {Tokenizer3, State2} = markdown_state:call(Tokenizer2, Name),
                    % io:format("\n\n[~w] AFTER call(~ts)\n~ts\n~ts\n", [
                    %     markdown:counter_get(), markdown_debug:rust_debug_string(Name), markdown_debug:rust_debug_string(State2), markdown_debug:rust_debug_string(Tokenizer3)
                    % ]),
                    push_impl_loop(Tokenizer3, To, State2, Flush)
            end;
        {retry, Name} ->
            {Tokenizer2, State2} = markdown_state:call(Tokenizer1, Name),
            push_impl_loop(Tokenizer2, To, State2, Flush)
    end.
