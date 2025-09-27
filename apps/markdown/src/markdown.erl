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
%%% Created :  03 Dec 2024 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(markdown).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").
-wacov(ignore).

-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% Public API
-export([
    counter_get/0,
    counter_reset/0,
    stack_pop/0,
    stack_push/0,
    display/3,
    doc/0,
    parse/1,
    parse/2,
    pretty_format/1,
    pretty_print/1,
    record_defs/0,
    record_print_fun/0,
    record_print_fun/1,
    mdast/0,
    mdast/2,
    test/0,
    test/1,
    test/2,
    test/3,
    to_html/1,
    to_html_with_options/2,
    to_mdast/2
]).

%% Internal API
-export([
    priv_dir/0
]).

%% Types
-type encoding() ::
    'latin1' | 'unicode' | 'utf8' | 'utf16' | 'utf32' | {'utf16', 'big' | 'little'} | {'utf32', 'big' | 'little'}.

-export_type([
    encoding/0
]).

%% Macros
-define(RECORD_KEY(X), ({X, record_info(size, X) - 1})).
-define(RECORD_VAL(X), (record_info(fields, X))).
-define(RECORD(X), ({?RECORD_KEY(X), ?RECORD_VAL(X)})).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec counter_get() -> non_neg_integer().
counter_get() ->
    atomics:add_get(counter_ref(), 1, 1) - 1.

-spec counter_ref() -> atomics:atomics_ref().
counter_ref() ->
    case persistent_term:get(markdown_display_counter, undefined) of
        undefined ->
            CounterRef = atomics:new(2, [{signed, false}]),
            ok = persistent_term:put(markdown_display_counter, CounterRef),
            CounterRef;
        CounterRef ->
            CounterRef
    end.

-spec counter_reset() -> ok.
counter_reset() ->
    atomics:put(counter_ref(), 1, 0),
    atomics:put(counter_ref(), 2, 0),
    ok.

-spec doc() -> binary().
doc() ->
    <<"1. ```\n   foo\n   ```\n\n   bar"/utf8>>.
    % <<"*foo __bar *baz bim__ bam*"/utf8>>.
    % <<"[bar](/foo)">>.
    % %% erlfmt-ignore
    % <<16#EF, 16#BB, 16#BF, """
    % # Hello

    % ## World

    % I am _**strong**_ and I am _emphasized_ here, ~strike-through~.

    % * one
    % * two
    % * three

    % 1. hello
    % 2. world

    % """>>.
% test(<<10,42,42,102,111,111,32,98,97,114,42,42,10>>).
% test(<<10,35,32,72,101,108,108,111,10,10,35,35,32,87,111,114,108,100,10,10,73,32,97,109,32,42,115,116,114,111,110,103,42,32,97,110,100,32,73,32,97,109,32,95,101,109,112,104,97,115,105,122,101,100,95,32,104,101,114,101,46,10,10,42,42,119,101,105,114,100,10,99,97,115,101,42,42,10,10>>).
% test(<<"""

% # Hello

% ## World

% I am *strong* and I am _emphasized_ here.

% **weird
% case**

% """>>).
% test(<<16#EF, 16#BB, 16#BF, """
% # Hello

% ## World

% I am *strong* and I am _emphasized_ here.

% * one
% * two
% * three

% 1. hello
% 2. world

% """>>, #{}, #{}).

-spec stack_pop() -> non_neg_integer().
stack_pop() ->
    atomics:sub_get(counter_ref(), 2, 1) + 1.

-spec stack_push() -> non_neg_integer().
stack_push() ->
    atomics:add_get(counter_ref(), 2, 1) - 1.

-spec display(dynamic(), dynamic(), dynamic()) -> ok.
display(Format, Args, Term) ->
    io:format("\n\n[~w] ", [counter_get()]),
    io:format(Format, Args),
    markdown_debug:rust_debug(Term),
    io:format("~tp~n", [erlang:process_info(self(), current_stacktrace)]),
    ok.

-spec parse(MarkdownInput) -> Result when
    MarkdownInput :: binary(),
    Result :: {ok, {Events, ParseState}} | {error, Message},
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    Message :: markdown_message:t().
parse(MarkdownInput) when is_binary(MarkdownInput) ->
    parse(MarkdownInput, markdown_options:default()).

-spec parse(MarkdownInput, Options) -> Result when
    MarkdownInput :: binary(),
    Options :: markdown_options:t() | markdown_options:castable(),
    Result :: {ok, {Events, ParseState}} | {error, Message},
    Events :: markdown_vec:t(Event),
    Event :: markdown_event:t(),
    ParseState :: markdown_parse_state:t(),
    Message :: markdown_message:t().
parse(MarkdownInput, Options = #markdown_options{}) when is_binary(MarkdownInput) ->
    markdown_parser:parse(MarkdownInput, Options#markdown_options.parse);
parse(MarkdownInput, CastableOptions) when is_binary(MarkdownInput) ->
    Options = markdown_options:new(CastableOptions),
    parse(MarkdownInput, Options).

-spec pretty_format(term()) -> io_lib:chars().
pretty_format(Term) ->
    io_lib_pretty:print(
        Term,
        markdown_types:dynamic_cast([
            {column, 1},
            {line_length, columns()},
            {depth, -1},
            {max_chars, 60},
            {record_print_fun, record_print_fun(record_defs())}
            | enc()
        ])
    ).

-spec pretty_print(term()) -> ok.
pretty_print(Term) ->
    io:format("~ts~n", [pretty_format(Term)]).

-spec record_defs() -> dynamic().
record_defs() ->
    maps:from_list([
        {{array, 4}, [size, max, default, elements]},
        ?RECORD(markdown_attempt),
        ?RECORD(markdown_construct_options),
        ?RECORD(markdown_container),
        ?RECORD(markdown_edit_map),
        ?RECORD(markdown_event),
        ?RECORD(markdown_event_link),
        ?RECORD(markdown_index),
        ?RECORD(markdown_label_start),
        ?RECORD(markdown_label),
        ?RECORD(markdown_location),
        ?RECORD(markdown_message),
        ?RECORD(markdown_parse_options),
        ?RECORD(markdown_parse_state),
        ?RECORD(markdown_place),
        ?RECORD(markdown_point),
        ?RECORD(markdown_position),
        ?RECORD(markdown_progress),
        ?RECORD(markdown_slice),
        ?RECORD(markdown_space_or_tab_options),
        ?RECORD(markdown_subresult),
        ?RECORD(markdown_tokenize_state),
        ?RECORD(markdown_tokenizer),
        ?RECORD(markdown_vec)
    ]).
% record_info(fields, Record) -> [Field]
% record_info(size, Record) -> Size

-spec record_print_fun() -> dynamic().
record_print_fun() ->
    record_print_fun(record_defs()).

-spec record_print_fun(dynamic()) -> dynamic().
record_print_fun(RecDefs) ->
    fun(Tag, NoFields) ->
        % io:format("record_print_fun(~tp, ~tp)~n", [Tag, NoFields]),
        case RecDefs of
            #{{Tag, NoFields} := Fields} ->
                Fields;
            #{} ->
                no
        end
    end.

-spec mdast() -> dynamic().
mdast() ->
    mdast(doc(), #{}).

-spec mdast(dynamic(), dynamic()) -> dynamic().
mdast(Value, ParseOptions) ->
    case test(Value, ParseOptions) of
        {ok, {Events, ParseState}} ->
            Bytes = markdown_parse_state:bytes(ParseState),
            markdown_mdast:compile(Events, Bytes);
        Error = {error, _Message} ->
            Error
    end.

-spec test() -> dynamic().
test() ->
    test(doc(), #{}, #{}).

-spec test(dynamic()) -> dynamic().
test(Value) ->
    test(Value, #{}).

-spec test(dynamic(), dynamic()) -> dynamic().
test(Value, ParseOptions) ->
    atomics:put(counter_ref(), 1, 0),
    atomics:put(counter_ref(), 2, 0),
    ParseOptions1 = markdown_parse_options:new(ParseOptions),
    markdown_parser:parse(Value, ParseOptions1).

-spec test(dynamic(), dynamic(), dynamic()) -> dynamic().
test(Value, ParseOptions, CompileOptions) ->
    case test(Value, ParseOptions) of
        {ok, {Events, ParseState}} ->
            Bytes = markdown_parse_state:bytes(ParseState),
            CompileOptions1 = markdown_compile_options:new(CompileOptions),
            markdown_html:compile(Events, Bytes, CompileOptions1);
        Error = {error, _Message} ->
            Error
    end.

-doc """
Turn markdown into HTML.

Compiles markdown to HTML according to `CommonMark`.
Use [`to_html_with_options()`][] to configure how markdown is turned into
HTML.

## Examples

```
use markdown::to_html;

assert_eq!(to_html("# Hello, world!"), "<h1>Hello, world!</h1>");
```
""".
-spec to_html(MarkdownInput) -> Result when
    MarkdownInput :: binary(),
    Result :: {ok, HtmlOutput} | {error, Message},
    HtmlOutput :: binary(),
    Message :: markdown_message:t().
to_html(MarkdownInput) when is_binary(MarkdownInput) ->
    to_html_with_options(MarkdownInput, markdown_options:default()).

-doc """
Turn markdown into HTML, with configuration.

## Errors

`to_html_with_options()` never errors with normal markdown because markdown
does not have syntax errors, so feel free to `unwrap()`.
However, MDX does have syntax errors.
When MDX is turned on, there are several errors that can occur with how
expressions, ESM, and JSX are written.

## Examples

```
use markdown::{to_html_with_options, CompileOptions, Options};
# fn main() -> Result<(), markdown::message::Message> {

// Use GFM:
let result = to_html_with_options("~hi~hello!", &Options::gfm())?;

assert_eq!(result, "<p><del>hi</del>hello!</p>");

// Live dangerously / trust the author:
let result = to_html_with_options("<div>\n\n# Hello, world!\n\n</div>", &Options {
    compile: CompileOptions {
      allow_dangerous_html: true,
      allow_dangerous_protocol: true,
      ..CompileOptions::default()
    },
    ..Options::default()
})?;

assert_eq!(result, "<div>\n<h1>Hello, world!</h1>\n</div>");
# Ok(())
# }
```
""".
-spec to_html_with_options(MarkdownInput, Options) -> Result when
    MarkdownInput :: binary(),
    Options :: markdown_options:t() | markdown_options:castable(),
    Result :: {ok, HtmlOutput} | {error, Message},
    HtmlOutput :: binary(),
    Message :: markdown_message:t().
to_html_with_options(MarkdownInput, Options = #markdown_options{}) when is_binary(MarkdownInput) ->
    case parse(MarkdownInput, Options) of
        {ok, {Events, ParseState}} ->
            Bytes = markdown_parse_state:bytes(ParseState),
            {ok, markdown_html:compile(Events, Bytes, Options#markdown_options.compile)};
        Error = {error, _Message} ->
            Error
    end;
to_html_with_options(MarkdownInput, CastableOptions) when is_binary(MarkdownInput) ->
    Options = markdown_options:new(CastableOptions),
    to_html_with_options(MarkdownInput, Options).

-doc """
Turn markdown into a syntax tree.

## Errors

`to_mdast()` never errors with normal markdown because markdown does not
have syntax errors, so feel free to `unwrap()`.
However, MDX does have syntax errors.
When MDX is turned on, there are several errors that can occur with how
JSX, expressions, or ESM are written.

## Examples

```
use markdown::{to_mdast, ParseOptions};
# fn main() -> Result<(), markdown::message::Message> {

let tree = to_mdast("# Hey, *you*!", &ParseOptions::default())?;

println!("{:?}", tree);
// => Root { children: [Heading { children: [Text { value: "Hey, ", position: Some(1:3-1:8 (2-7)) }, Emphasis { children: [Text { value: "you", position: Some(1:9-1:12 (8-11)) }], position: Some(1:8-1:13 (7-12)) }, Text { value: "!", position: Some(1:13-1:14 (12-13)) }], position: Some(1:1-1:14 (0-13)), depth: 1 }], position: Some(1:1-1:14 (0-13)) }
# Ok(())
# }
```
""".
-spec to_mdast(MarkdownInput, ParseOptions) -> Result when
    MarkdownInput :: binary(),
    ParseOptions :: markdown_parse_options:t() | markdown_parse_options:castable(),
    Result :: {ok, Node} | {error, Message},
    Node :: markdown_mdast_node:t(),
    Message :: markdown_message:t().
to_mdast(MarkdownInput, ParseOptions = #markdown_parse_options{}) when is_binary(MarkdownInput) ->
    case markdown_parser:parse(MarkdownInput, ParseOptions) of
        {ok, {Events, ParseState}} ->
            Bytes = markdown_parse_state:bytes(ParseState),
            markdown_mdast:compile(Events, Bytes);
        Error = {error, _Message} ->
            Error
    end;
to_mdast(MarkdownInput, CastableParseOptions) when is_binary(MarkdownInput) ->
    ParseOptions = markdown_parse_options:new(CastableParseOptions),
    to_mdast(MarkdownInput, ParseOptions).

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec priv_dir() -> file:filename().
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec columns() -> non_neg_integer().
columns() ->
    case io:columns() of
        {ok, N} -> N;
        _ -> 80
    end.

%% @private
-spec enc() -> [{encoding, encoding()}].
enc() ->
    case lists:keyfind(encoding, 1, markdown_types:dynamic_cast(io:getopts())) of
        % should never happen
        false -> [{encoding, latin1}];
        Enc -> [Enc]
    end.
