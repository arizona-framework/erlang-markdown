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
%%% Created :  05 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_debug).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("markdown/include/markdown_const.hrl").
-include_lib("markdown/include/markdown_parser.hrl").
-include_lib("markdown/include/markdown_resolve.hrl").
-include_lib("markdown/include/markdown_util.hrl").
-include_lib("markdown/include/markdown_vec.hrl").

%% API
-export([
    erlang_debug/1,
    redbug_everything/1,
    redbug_modules/0,
    redbug_start/2,
    redbug_traces/0,
    rust_debug/1,
    rust_debug_string/1,
    walk_print/1,
    walk/3,
    xform/3,
    xform_with_hint/4
]).

%% Types
-type walk_action() :: cont | halt.
-type walk_event() ::
    {error, dynamic()}
    | improper_list_head
    | {improper_list_elem, pos_integer(), dynamic()}
    | {improper_list_tail, pos_integer(), dynamic()}
    | {map_head, non_neg_integer()}
    | {map_elem, non_neg_integer(), dynamic(), dynamic()}
    | {map_tail, non_neg_integer()}
    | {next, dynamic()}
    | none
    | {ok, dynamic()}
    | {proper_list_head, pos_integer()}
    | {proper_list_elem, pos_integer(), dynamic()}
    | {proper_list_tail, pos_integer()}
    | {record_head, markdown_debug_types:record_name(), non_neg_integer(), [atom()]}
    | {record_elem, markdown_debug_types:record_name(), pos_integer(), atom(), dynamic()}
    | {record_tail, markdown_debug_types:record_name(), non_neg_integer()}
    | {retry, dynamic()}
    | {some, dynamic()}
    | {term,
        integer() | float() | atom() | reference() | port() | pid() | [] | {} | binary() | bitstring() | function()}
    | {tuple_head, pos_integer()}
    | {tuple_elem, pos_integer(), dynamic()}
    | {tuple_tail, pos_integer()}.
-type walk_func() :: walk_func(dynamic(), dynamic()).
-type walk_func(AccIn, AccOut) ::
    fun((walk_event(), AccIn) -> walk_result(AccOut)).
-type walk_result(AccOut) :: walk_action() | {walk_action(), AccOut}.
-type xform_action() :: cont | skip.
-type xform_func(Type, Acc) :: xform_func(Type, Acc, Type, Acc).
-type xform_func(TypeIn, AccIn, TypeOut, AccOut) ::
    fun((TypeIn, xform_hint(), AccIn) -> xform_result(TypeOut, AccOut)).
-type xform_hint() ::
    none
    | {improper_list_head, pos_integer()}
    | {improper_list_tail, pos_integer()}
    | map_key
    | {map_value, dynamic()}
    | {proper_list, pos_integer()}
    | {record_field, pos_integer(), atom()}
    | {record_tag, [atom()]}
    | {tuple, pos_integer()}.
-type xform_result(TypeOut, AccOut) :: xform_action() | {xform_action(), AccOut} | {xform_action(), TypeOut, AccOut}.

-export_type([
    walk_event/0,
    walk_func/0,
    walk_func/2,
    walk_result/1,
    xform_action/0,
    xform_result/2,
    xform_func/2,
    xform_func/4
]).

%% Macros
% -define(DEFAULT_OPTIONS, #{}).
-define(is_maybe_record(X), (is_tuple((X)) andalso tuple_size((X)) > 0 andalso is_atom(element(1, (X))))).

% -define(RECORD_SIZE_DEF(X), record_size(X) -> record_info(X, X)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

% rust_print(Term) ->
%     Fun = fun(Term, Hint, Acc1) ->
%         case Hint of
%             {record_tag, _} ->

%     end,
%     xform(Term, ok, )

-spec erlang_debug(Term) -> ok when Term :: dynamic().
erlang_debug(Term) ->
    Formatter = markdown_formatter:new_io_device(standard_io, #{depth => 0, indent => 4}),
    {cont, Formatter} = walk(Term, Formatter, fun erlang_debug_fmt/2),
    io:format("~n", []),
    ok.

-spec redbug_everything(Options) -> Result when Options :: dynamic(), Result :: dynamic().
redbug_everything(Options) ->
    redbug_start(redbug_traces(), Options).

-spec redbug_modules() -> list(module()).
redbug_modules() ->
    {ok, Modules} = ensure_markdown_code_loaded(code:all_available(), [], []),
    Modules.

% redbug_print_fun({meta, _, PI, TS}) ->
% redbug_print_fun({call, {{Module, Function, Arguments}, Backtrace}, PI, TS}) ->
%     {meta,stop,dummy,{0,0,0,0}}

-spec redbug_start(dynamic(), dynamic()) -> dynamic().
redbug_start(Traces, Options) when is_map(Options) ->
    Options1 = maps:update_with(
        records,
        fun(Modules) ->
            Modules ++ redbug_modules()
        end,
        redbug_modules(),
        Options
    ),
    redbug:start(Traces, Options1).

-spec redbug_traces() -> list(dynamic()).
redbug_traces() ->
    [lists:flatten(io_lib:format("~ts:_->return", [Module])) || Module <- redbug_modules()].

% %%% call stack handler
% -spec stak(binary()) -> unicode:chardata().
% stak(Bin) ->
%     L = string:tokens(binary_to_list(Bin), "\n"),
%     lists:reverse(lists:foldl(fun munge/2, [], L)).

% -spec munge(string(), string()) -> unicode:chardata().
% munge(I, Out) ->
%     case lists:reverse(I) of
%         "..."++_ -> [truncated|Out];
%         _ ->
%             case string:str(I, "Return addr") of
%                 0 ->
%                     case string:str(I, "cp = ") of
%                         0 -> Out;
%                         _ -> [mfaf(I)|Out]
%                     end;
%                 _ ->
%                     case string:str(I, "erminate process normal") of
%                         0 -> [mfaf(I)|Out];
%                         _ -> Out
%                     end
%             end
%     end.

% -spec mfaf(string()) -> string().
% mfaf(I) ->
%     [_, C|_] = string:tokens(I, "()+"),
%     C.

%% @private
-spec ensure_markdown_code_loaded([{ModuleName, Filename, Loaded}], LoadedModules, UnloadedModules) ->
    {ok, LoadedModules} | {error, [{Module, What}]}
when
    ModuleName :: string(),
    Filename :: file:filename() | cover_compiled | preloaded,
    Loaded :: boolean(),
    LoadedModules :: [Module],
    UnloadedModules :: [Module],
    Module :: module(),
    What :: badfile | nofile | on_load_failure.
ensure_markdown_code_loaded([{Name = "markdown_" ++ _, _File, Loaded} | Rest], LoadedModules, UnloadedModules) ->
    % elp:ignore W0023 (atoms_exhaustion) - Only used for internal operations, safe
    Module = erlang:list_to_atom(Name),
    case Loaded of
        false ->
            ensure_markdown_code_loaded(Rest, LoadedModules, [Module | UnloadedModules]);
        true ->
            ensure_markdown_code_loaded(Rest, [Module | LoadedModules], UnloadedModules)
    end;
ensure_markdown_code_loaded([_ | Rest], LoadedModules, UnloadedModules) ->
    ensure_markdown_code_loaded(Rest, LoadedModules, UnloadedModules);
ensure_markdown_code_loaded([], LoadedModules, UnloadedModules) ->
    case code:ensure_modules_loaded(UnloadedModules) of
        ok ->
            {ok, lists:usort(LoadedModules ++ UnloadedModules)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec rust_debug(Term) -> ok when Term :: dynamic().
rust_debug(Term) ->
    Formatter = markdown_formatter:new_io_device(standard_io, #{depth => 0, indent => 4}),
    {cont, Formatter} = walk(Term, Formatter, fun rust_debug_fmt/2),
    io:format("~n", []),
    ok.

-spec rust_debug_string(Term) -> iolist() when Term :: dynamic().
rust_debug_string(Term) ->
    Formatter1 = markdown_formatter:new_string(#{depth => 0, indent => 4}),
    {cont, Formatter2} = walk(Term, Formatter1, fun rust_debug_fmt/2),
    case markdown_formatter:finalize(Formatter2) of
        Iolist when is_list(Iolist) ->
            Iolist
    end.

-spec walk_print(dynamic()) -> ok.
walk_print(T) ->
    {cont, ok} = walk(T, ok, fun walk_inspect_internal/2),
    ok.

%% @private
-spec walk_inspect_internal(Event, AccIn) -> Result when
    Event :: walk_event(), AccIn :: ok, Result :: walk_result(AccOut), AccOut :: ok.
walk_inspect_internal(Event, AccIn = ok) ->
    io:format("event = ~0tp~n", [Event]),
    case Event of
        {error, Error} ->
            walk(Error, AccIn, fun walk_inspect_internal/2);
        improper_list_head ->
            cont;
        {improper_list_elem, _Index, Elem} ->
            walk(Elem, AccIn, fun walk_inspect_internal/2);
        {improper_list_tail, _Index, Elem} ->
            walk(Elem, AccIn, fun walk_inspect_internal/2);
        {map_head, _Arity} ->
            cont;
        {map_elem, _Index, Key, Value} ->
            maybe
                {cont, AccOut1} ?= walk(Key, AccIn, fun walk_inspect_internal/2),
                {cont, AccOut2} ?= walk(Value, AccOut1, fun walk_inspect_internal/2),
                {cont, AccOut2}
            end;
        none ->
            cont;
        {ok, Value} ->
            walk(Value, AccIn, fun walk_inspect_internal/2);
        {proper_list_head, _Length} ->
            cont;
        {proper_list_elem, _Index, Elem} ->
            walk(Elem, AccIn, fun walk_inspect_internal/2);
        {proper_list_tail, _Length} ->
            cont;
        {record_head, _Tag, _Size, _Fields} ->
            cont;
        {record_elem, _Index, _Field, Elem} ->
            walk(Elem, AccIn, fun walk_inspect_internal/2);
        {record_tail, _Tag, _Size} ->
            cont;
        {some, Value} ->
            walk(Value, AccIn, fun walk_inspect_internal/2);
        {term, _Value} ->
            cont;
        {tuple_head, _Size} ->
            cont;
        {tuple_elem, _Index, Elem} ->
            walk(Elem, AccIn, fun walk_inspect_internal/2);
        {tuple_tail, _Size} ->
            cont
    end.

-spec walk(Term, AccIn, Fun) -> {Action, AccOut} when
    Term :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    Action :: walk_action(),
    AccOut :: dynamic().
walk(T, AccIn, Fun) when is_function(Fun, 2) ->
    case T of
        _ when is_integer(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_float(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        none ->
            walk_normalize(Fun(none, AccIn), AccIn);
        _ when is_atom(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_reference(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_port(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_pid(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        {} ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        {error, V} ->
            walk_normalize(Fun({error, V}, AccIn), AccIn);
        {next, V} ->
            walk_normalize(Fun({next, V}, AccIn), AccIn);
        {ok, V} ->
            walk_normalize(Fun({ok, V}, AccIn), AccIn);
        {retry, V} ->
            walk_normalize(Fun({retry, V}, AccIn), AccIn);
        {some, V} ->
            walk_normalize(Fun({some, V}, AccIn), AccIn);
        _ when ?is_maybe_record(T) ->
            RecordTag = element(1, T),
            RecordSize = tuple_size(T) - 1,
            case markdown_debug_types:is_record(RecordTag, RecordSize) of
                true ->
                    RecordFields = markdown_debug_types:record_fields(RecordTag),
                    case walk_normalize(Fun({record_head, RecordTag, RecordSize, RecordFields}, AccIn), AccIn) of
                        {cont, AccOut} ->
                            walk_record(T, AccOut, Fun, RecordFields, 2);
                        {halt, AccOut} ->
                            {halt, AccOut}
                    end;
                false ->
                    TupleSize = tuple_size(T),
                    case walk_normalize(Fun({tuple_head, TupleSize}, AccIn), AccIn) of
                        {cont, AccOut} ->
                            walk_tuple(T, AccOut, Fun, 1);
                        {halt, AccOut} ->
                            {halt, AccOut}
                    end
            end;
        _ when is_tuple(T) ->
            TupleSize = tuple_size(T),
            case walk_normalize(Fun({tuple_head, TupleSize}, AccIn), AccIn) of
                {cont, AccOut} ->
                    walk_tuple(T, AccOut, Fun, 1);
                {halt, AccOut} ->
                    {halt, AccOut}
            end;
        [] ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_list(T) andalso length(T) > 0 ->
            Length = length(T),
            case walk_normalize(Fun({proper_list_head, Length}, AccIn), AccIn) of
                {cont, AccOut} ->
                    walk_proper_list(T, AccOut, Fun, 1);
                {halt, AccOut} ->
                    {halt, AccOut}
            end;
        _ when is_list(T) ->
            case walk_normalize(Fun(improper_list_head, AccIn), AccIn) of
                {cont, AccOut} ->
                    walk_improper_list(T, AccOut, Fun, 1);
                {halt, AccOut} ->
                    {halt, AccOut}
            end;
        _ when is_binary(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_bitstring(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_function(T) ->
            walk_normalize(Fun({term, T}, AccIn), AccIn);
        _ when is_map(T) ->
            Arity = maps:size(T),
            case walk_normalize(Fun({map_head, Arity}, AccIn), AccIn) of
                {cont, AccOut} ->
                    MapIterator = maps:iterator(T, ordered),
                    walk_map(MapIterator, AccOut, Fun, 0);
                {halt, AccOut} ->
                    {halt, AccOut}
            end
    end.

-spec xform(TypeIn, AccIn, Fun) -> {TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(TypeIn, AccIn, TypeOut, AccOut),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform(T1, Acc1, Fun) when is_function(Fun, 3) ->
    xform_with_hint(T1, none, Acc1, Fun).

-spec xform_with_hint(TypeIn, Hint, AccIn, Fun) -> {TypeOut, AccOut} when
    TypeIn :: dynamic(),
    Hint :: xform_hint(),
    AccIn :: dynamic(),
    Fun :: xform_func(TypeIn, AccIn, TypeOut, AccOut),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform_with_hint(T1, Hint, Acc1, Fun) when is_function(Fun, 3) ->
    case xform_normalize(T1, Acc1, Fun(T1, Hint, Acc1)) of
        {cont, T2, Acc2} ->
            case T2 of
                _ when is_integer(T2) ->
                    {T2, Acc2};
                _ when is_float(T2) ->
                    {T2, Acc2};
                _ when is_atom(T2) ->
                    {T2, Acc2};
                _ when is_reference(T2) ->
                    {T2, Acc2};
                _ when is_port(T2) ->
                    {T2, Acc2};
                _ when is_pid(T2) ->
                    {T2, Acc2};
                _ when ?is_maybe_record(T2) ->
                    xform_maybe_record(T2, Acc2, Fun);
                _ when is_tuple(T2) ->
                    xform_tuple(T2, Acc2, Fun, 1);
                [] ->
                    {T2, Acc2};
                _ when is_list(T2) andalso length(T2) > 0 ->
                    xform_proper_list(T2, Acc2, Fun, 1, []);
                _ when is_list(T2) ->
                    xform_improper_list(T2, Acc2, Fun, 1, []);
                _ when is_binary(T2) ->
                    {T2, Acc2};
                _ when is_bitstring(T2) ->
                    {T2, Acc2};
                _ when is_function(T2) ->
                    {T2, Acc2};
                _ when is_map(T2) ->
                    MapIterator = maps:iterator(T2),
                    xform_map(MapIterator, Acc2, Fun, maps:new())
            end;
        {skip, T2, Acc2} ->
            {T2, Acc2}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec erlang_debug_fmt(Event, Formatter) -> Result when
    Event :: walk_event(), Formatter :: markdown_formatter:t(), Result :: walk_result(Formatter).
erlang_debug_fmt(Event, Formatter1) ->
    case Event of
        {error, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{error,\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, "\n"},
                    shift_left,
                    write_indent,
                    {write, "}"}
                ]),
                {cont, Formatter4}
            end;
        improper_list_head ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "["},
                shift_right
            ]),
            {cont, Formatter2};
        {improper_list_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {improper_list_tail, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ",\n"},
                    shift_left,
                    write_indent,
                    {write, "]"}
                ]),
                {cont, Formatter4}
            end;
        {map_head, _Arity} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "#{"},
                shift_right
            ]),
            {cont, Formatter2};
        {map_elem, _Index, Key, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Key, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, " => "}
                ]),
                {cont, Formatter5} ?= walk(Value, Formatter4, fun erlang_debug_fmt/2),
                Formatter6 = markdown_formatter:do(Formatter5, [
                    {write, ","}
                ]),
                {cont, Formatter6}
            end;
        {map_tail, _Arity} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2};
        {next, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{next,\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, "\n"},
                    shift_left,
                    write_indent,
                    {write, "}"}
                ]),
                {cont, Formatter4}
            end;
        none ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "none"}
            ]),
            {cont, Formatter2};
        {ok, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{ok,\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, "\n"},
                    shift_left,
                    write_indent,
                    {write, "}"}
                ]),
                {cont, Formatter4}
            end;
        {proper_list_head, _Length} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "["},
                shift_right
            ]),
            {cont, Formatter2};
        {proper_list_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {proper_list_tail, _Length} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "]"}
            ]),
            {cont, Formatter2};
        {record_head, markdown_index, _Size, _Fields} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{"},
                shift_right
            ]),
            {cont, Formatter2};
        {record_elem, markdown_index, _Index, _Field, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {record_tail, markdown_index, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2};
        {record_head, markdown_vec, _Size, _Fields} ->
            cont;
        {record_elem, markdown_vec, _Index, items, Items} ->
            List = [Item || {some, Item} <- array:to_list(Items)],
            walk(List, Formatter1, fun erlang_debug_fmt/2);
        {record_elem, markdown_vec, _Index, size, _Size} ->
            cont;
        {record_tail, markdown_vec, _Size} ->
            cont;
        {record_head, Tag, _Size, _Fields} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "#~ts{", [Tag]},
                shift_right
            ]),
            {cont, Formatter2};
        {record_elem, Tag, _Index, Field, Value} ->
            RustField =
                case {Tag, Field} of
                    {markdown_point, offset} -> index;
                    {markdown_point, virtual} -> vs;
                    _ -> Field
                end,
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent,
                {format, "~ts = ", [RustField]}
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {record_tail, _Tag, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2};
        {retry, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{retry,\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, "\n"},
                    shift_left,
                    write_indent,
                    {write, "}"}
                ]),
                {cont, Formatter4}
            end;
        {some, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{some,\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, "\n"},
                    shift_left,
                    write_indent,
                    {write, "}"}
                ]),
                {cont, Formatter4}
            end;
        {term, []} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "[]"}
            ]),
            {cont, Formatter2};
        {term, {}} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{}"}
            ]),
            {cont, Formatter2};
        {term, Bytes} when is_binary(Bytes) ->
            List = erlang:binary_to_list(Bytes),
            walk(List, Formatter1, fun erlang_debug_fmt/2);
        {term, Boolean} when is_boolean(Boolean) ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~ts", [Boolean]}
            ]),
            {cont, Formatter2};
        {term, infinity} ->
            walk(16#FFFFFFFFFFFFFFFF, Formatter1, fun erlang_debug_fmt/2);
        {term, Atom} when is_atom(Atom) ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~ts", [Atom]}
            ]),
            {cont, Formatter2};
        {term, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~0tp", [Value]}
            ]),
            {cont, Formatter2};
        {tuple_head, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{"},
                shift_right
            ]),
            {cont, Formatter2};
        {tuple_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun erlang_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {tuple_tail, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2}
    end.

%% @private
-spec rust_debug_fmt(Event, Formatter) -> Result when
    Event :: walk_event(), Formatter :: markdown_formatter:t(), Result :: walk_result(Formatter).
rust_debug_fmt(Event, Formatter1) ->
    case Event of
        {error, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "Err(\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    shift_left,
                    {write, ",\n"},
                    write_indent,
                    {write, ")"}
                ]),
                {cont, Formatter4}
            end;
        improper_list_head ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "["},
                shift_right
            ]),
            {cont, Formatter2};
        {improper_list_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {improper_list_tail, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ",\n"},
                    shift_left,
                    write_indent,
                    {write, "]"}
                ]),
                {cont, Formatter4}
            end;
        {map_head, _Arity} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{"},
                shift_right
            ]),
            {cont, Formatter2};
        {map_elem, _Index, Key, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Key, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ": "}
                ]),
                {cont, Formatter5} ?= walk(Value, Formatter4, fun rust_debug_fmt/2),
                Formatter6 = markdown_formatter:do(Formatter5, [
                    {write, ","}
                ]),
                {cont, Formatter6}
            end;
        {map_tail, _Arity} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2};
        {next, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "Next(\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    shift_left,
                    {write, ",\n"},
                    write_indent,
                    {write, ")"}
                ]),
                {cont, Formatter4}
            end;
        none ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "None"}
            ]),
            {cont, Formatter2};
        {ok, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "Ok(\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    shift_left,
                    {write, ",\n"},
                    write_indent,
                    {write, ")"}
                ]),
                {cont, Formatter4}
            end;
        {proper_list_head, _Length} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "["},
                shift_right
            ]),
            {cont, Formatter2};
        {proper_list_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {proper_list_tail, _Length} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "]"}
            ]),
            {cont, Formatter2};
        {record_head, Tag, _Size, _Fields} when
            Tag =:= markdown_index orelse Tag =:= markdown_indices orelse Tag =:= markdown_list_item
        ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "("},
                shift_right
            ]),
            {cont, Formatter2};
        {record_elem, Tag, _Index, _Field, Value} when
            Tag =:= markdown_index orelse Tag =:= markdown_indices orelse Tag =:= markdown_list_item
        ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {record_tail, Tag, _Size} when
            Tag =:= markdown_index orelse Tag =:= markdown_indices orelse Tag =:= markdown_list_item
        ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, ")"}
            ]),
            {cont, Formatter2};
        {record_head, markdown_vec, _Size, _Fields} ->
            cont;
        {record_elem, markdown_vec, _Index, items, Items} ->
            List = [Item || {some, Item} <- array:to_list(Items)],
            walk(List, Formatter1, fun rust_debug_fmt/2);
        {record_elem, markdown_vec, _Index, size, _Size} ->
            cont;
        {record_tail, markdown_vec, _Size} ->
            cont;
        {record_head, Tag, _Size, _Fields} ->
            RustName = markdown_debug_types:record_rust_name(Tag),
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~ts {", [RustName]},
                shift_right
            ]),
            {cont, Formatter2};
        {record_elem, Tag, _Index, Field, Value} ->
            RustField =
                case {Tag, Field} of
                    {markdown_point, offset} -> index;
                    {markdown_point, virtual} -> vs;
                    _ -> Field
                end,
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent,
                {format, "~ts: ", [RustField]}
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {record_tail, _Tag, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, "}"}
            ]),
            {cont, Formatter2};
        {retry, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "Retry(\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    shift_left,
                    {write, ",\n"},
                    write_indent,
                    {write, ")"}
                ]),
                {cont, Formatter4}
            end;
        {some, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "Some(\n"},
                shift_right,
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Value, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    shift_left,
                    {write, ",\n"},
                    write_indent,
                    {write, ")"}
                ]),
                {cont, Formatter4}
            end;
        {term, []} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "[]"}
            ]),
            {cont, Formatter2};
        {term, {}} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "{}"}
            ]),
            {cont, Formatter2};
        {term, Bytes} when is_binary(Bytes) ->
            List = erlang:binary_to_list(Bytes),
            walk(List, Formatter1, fun rust_debug_fmt/2);
        {term, Boolean} when is_boolean(Boolean) ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~ts", [Boolean]}
            ]),
            {cont, Formatter2};
        {term, infinity} ->
            walk(16#FFFFFFFFFFFFFFFF, Formatter1, fun rust_debug_fmt/2);
        {term, Atom} when is_atom(Atom) ->
            AtomString = erlang:atom_to_list(Atom),
            Replacement = fun(_, Groups) -> string:uppercase(Groups) end,
            RustEnum = re:replace(AtomString, "(?:^|_)(.)", Replacement, [global, {return, binary}]),
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~ts", [RustEnum]}
            ]),
            {cont, Formatter2};
        {term, Value} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {format, "~0tp", [Value]}
            ]),
            {cont, Formatter2};
        {tuple_head, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "("},
                shift_right
            ]),
            {cont, Formatter2};
        {tuple_elem, _Index, Elem} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                write_indent
            ]),
            maybe
                {cont, Formatter3} ?= walk(Elem, Formatter2, fun rust_debug_fmt/2),
                Formatter4 = markdown_formatter:do(Formatter3, [
                    {write, ","}
                ]),
                {cont, Formatter4}
            end;
        {tuple_tail, _Size} ->
            Formatter2 = markdown_formatter:do(Formatter1, [
                {write, "\n"},
                shift_left,
                write_indent,
                {write, ")"}
            ]),
            {cont, Formatter2}
    end.

%% @private
-spec walk_improper_list(ImproperList | Tail, AccIn, Fun, Index) -> {Action, AccOut} when
    ImproperList :: maybe_improper_list(Elem, Tail),
    Elem :: dynamic(),
    Tail :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    Action :: walk_action().
walk_improper_list([Elem | ImproperList], AccIn, Fun, Index) ->
    case walk_normalize(Fun({improper_list_elem, Index, Elem}, AccIn), AccIn) of
        {cont, AccOut} ->
            walk_improper_list(ImproperList, AccOut, Fun, Index + 1);
        {halt, AccOut} ->
            {halt, AccOut}
    end;
walk_improper_list(Tail, AccIn, Fun, Index) ->
    walk_normalize(Fun({improper_list_tail, Index, Tail}, AccIn), AccIn).

%% @private
-spec walk_map(MapIterator, AccIn, Fun, Index) -> {Action, AccOut} when
    MapIterator :: maps:iterator(Key, Value),
    Key :: dynamic(),
    Value :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    Action :: walk_action().
walk_map(MapIterator, AccIn, Fun, Index) ->
    case maps:next(MapIterator) of
        none ->
            walk_normalize(Fun({map_tail, Index}, AccIn), AccIn);
        {Key, Value, NextMapIterator} ->
            case walk_normalize(Fun({map_elem, Index, Key, Value}, AccIn), AccIn) of
                {cont, AccOut} ->
                    walk_map(NextMapIterator, AccOut, Fun, Index + 1);
                {halt, AccOut} ->
                    {halt, AccOut}
            end
    end.

%% @private
-spec walk_normalize(Result, AccIn) -> {Action, AccOut} when
    Result :: walk_result(AccOut),
    AccOut :: dynamic(),
    AccIn :: dynamic(),
    Action :: walk_action().
walk_normalize(Action, AccIn) when Action =:= 'cont' orelse Action =:= 'halt' ->
    {Action, AccIn};
walk_normalize({Action, AccOut}, _AccIn) when Action =:= 'cont' orelse Action =:= 'halt' ->
    {Action, AccOut}.

%% @private
-spec walk_proper_list(ProperList, AccIn, Fun, Index) -> {Action, AccOut} when
    ProperList :: list(Elem),
    Elem :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    Action :: walk_action().
walk_proper_list([Elem | ProperList], AccIn, Fun, Index) ->
    case walk_normalize(Fun({proper_list_elem, Index, Elem}, AccIn), AccIn) of
        {cont, AccOut} ->
            walk_proper_list(ProperList, AccOut, Fun, Index + 1);
        {halt, AccOut} ->
            {halt, AccOut}
    end;
walk_proper_list([], AccIn, Fun, Index) ->
    walk_normalize(Fun({proper_list_tail, Index}, AccIn), AccIn).

%% @private
-spec walk_record(Record, AccIn, Fun, Fields, Index) -> {Action, AccOut} when
    Record :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    AccOut :: dynamic(),
    Fields :: [atom()],
    Index :: pos_integer(),
    Action :: walk_action().
walk_record(Record, AccIn, Fun, [Field | Fields], Index) when Index =< tuple_size(Record) ->
    RecordTag = element(1, Record),
    Elem = element(Index, Record),
    case walk_normalize(Fun({record_elem, RecordTag, Index, Field, Elem}, AccIn), AccIn) of
        {cont, AccOut} ->
            walk_record(Record, AccOut, Fun, Fields, Index + 1);
        {halt, AccOut} ->
            {halt, AccOut}
    end;
walk_record(Record, AccIn, Fun, [], Index) ->
    RecordTag = element(1, Record),
    walk_normalize(Fun({record_tail, RecordTag, Index}, AccIn), AccIn).

%% @private
-spec walk_tuple(Tuple, AccIn, Fun, Index) -> {Action, AccOut} when
    Tuple :: dynamic(),
    AccIn :: dynamic(),
    Fun :: walk_func(AccIn, AccOut),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    Action :: walk_action().
walk_tuple(Tuple, AccIn, Fun, Index) when Index =< tuple_size(Tuple) ->
    Elem = element(Index, Tuple),
    case walk_normalize(Fun({tuple_elem, Index, Elem}, AccIn), AccIn) of
        {cont, AccOut} ->
            walk_tuple(Tuple, AccOut, Fun, Index + 1);
        {halt, AccOut} ->
            {halt, AccOut}
    end;
walk_tuple(_Tuple, AccIn, Fun, Index) ->
    walk_normalize(Fun({tuple_tail, Index}, AccIn), AccIn).

%% @private
-spec xform_improper_list(TypeIn, AccIn, Fun, Index, TypeAcc) -> {TypeOut, AccOut} when
    TypeIn :: maybe_improper_list(ElementIn, TailIn),
    ElementIn :: dynamic(),
    TailIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(ElementIn, AccIn, ElementOut, AccOut) | xform_func(TailIn, AccIn, TailOut, AccOut),
    ElementOut :: dynamic(),
    AccOut :: dynamic(),
    TailOut :: dynamic(),
    Index :: pos_integer(),
    TypeAcc :: list(ElementOut),
    TypeOut :: maybe_improper_list(ElementOut, TailOut).
xform_improper_list([T1 | ImproperList], Acc1, Fun, Index, TypeAcc1) ->
    {T2, Acc2} = xform_with_hint(T1, {improper_list_head, Index}, Acc1, Fun),
    TypeAcc2 = [T2 | TypeAcc1],
    xform_improper_list(ImproperList, Acc2, Fun, Index + 1, TypeAcc2);
xform_improper_list(Tail1, Acc1, Fun, Index, TypeAcc1) ->
    {Tail2, Acc2} = xform_with_hint(Tail1, {improper_list_tail, Index}, Acc1, Fun),
    TypeAcc2 = lists:reverse(TypeAcc1, Tail2),
    {TypeAcc2, Acc2}.

%% @private
-spec xform_map(MapIterator, AccIn, Fun, TypeOut) -> {TypeOut, AccOut} when
    MapIterator :: maps:iterator(KeyIn, ValueIn),
    KeyIn :: dynamic(),
    ValueIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(KeyIn, AccIn, KeyOut, AccOut) | xform_func(ValueIn, AccIn, ValueOut, AccOut),
    KeyOut :: dynamic(),
    AccOut :: dynamic(),
    ValueOut :: dynamic(),
    TypeOut :: #{KeyOut => ValueOut}.
xform_map(MapIterator1, Acc1, Fun, TypeAcc1) ->
    case maps:next(MapIterator1) of
        {Key1, Value1, MapIterator2} ->
            {Key2, Acc2} = xform_with_hint(Key1, map_key, Acc1, Fun),
            {Value2, Acc3} = xform_with_hint(Value1, {map_value, Key2}, Acc2, Fun),
            TypeAcc2 = TypeAcc1#{Key2 => Value2},
            xform_map(MapIterator2, Acc3, Fun, TypeAcc2);
        none ->
            {TypeAcc1, Acc1}
    end.

%% @private
-spec xform_maybe_record(TypeIn, AccIn, Fun) -> {TypeOut, AccOut} when
    TypeIn :: {ElementIn} | tuple() | dynamic(),
    ElementIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(ElementIn, AccIn, ElementOut, AccOut),
    ElementOut :: dynamic(),
    AccOut :: dynamic(),
    TypeOut :: {ElementOut} | tuple() | dynamic().
xform_maybe_record(MaybeRecord1, Acc1, Fun) ->
    RecordTag1 = element(1, MaybeRecord1),
    RecordSize = tuple_size(MaybeRecord1) - 1,
    case markdown_debug_types:is_record(RecordTag1, RecordSize) of
        true ->
            RecordFields = markdown_debug_types:record_fields(RecordTag1),
            {RecordTag2, Acc2} = xform_with_hint(RecordTag1, {record_tag, RecordFields}, Acc1, Fun),
            MaybeRecord2 = setelement(1, MaybeRecord1, RecordTag2),
            xform_record(MaybeRecord2, Acc2, Fun, 2, RecordFields);
        false ->
            xform_tuple(MaybeRecord1, Acc1, Fun, 1)
    end.

%% @private
-spec xform_normalize(TypeIn, AccIn, Result) -> {Action, TypeOut, AccOut} when
    TypeIn :: dynamic(),
    AccIn :: dynamic(),
    Result :: xform_result(TypeOut, AccOut),
    Action :: xform_action(),
    TypeOut :: dynamic(),
    AccOut :: dynamic().
xform_normalize(TypeIn, AccIn, Action) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccIn};
xform_normalize(TypeIn, _AccIn, {Action, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeIn, AccOut};
xform_normalize(_TypeIn, _AccIn, {Action, TypeOut, AccOut}) when Action =:= 'cont' orelse Action =:= 'skip' ->
    {Action, TypeOut, AccOut}.

%% @private
-spec xform_proper_list(TypeIn, AccIn, Fun, Index, TypeOut) -> {TypeOut, AccOut} when
    TypeIn :: list(ElementIn),
    ElementIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(ElementIn, AccIn, ElementOut, AccOut),
    ElementOut :: dynamic(),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    TypeOut :: list(ElementOut).
xform_proper_list([T1 | ProperList], Acc1, Fun, Index, TypeAcc1) ->
    {T2, Acc2} = xform_with_hint(T1, {improper_list_head, Index}, Acc1, Fun),
    TypeAcc2 = [T2 | TypeAcc1],
    xform_proper_list(ProperList, Acc2, Fun, Index + 1, TypeAcc2);
xform_proper_list([], Acc1, _Fun, _Index, TypeAcc1) ->
    TypeAcc2 = lists:reverse(TypeAcc1),
    {TypeAcc2, Acc1}.

%% @private
-spec xform_record(TypeIn | TypeOut, AccIn, Fun, Index, Fields) -> {TypeOut, AccOut} when
    TypeIn :: {ElementIn} | tuple() | dynamic(),
    ElementIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(ElementIn, AccIn, ElementOut, AccOut),
    ElementOut :: dynamic(),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    Fields :: [atom()],
    TypeOut :: {ElementOut} | tuple() | dynamic().
xform_record(Record1, Acc1, Fun, Index, [Field | Fields]) when Index =< tuple_size(Record1) ->
    T1 = element(Index, Record1),
    {T2, Acc2} = xform_with_hint(T1, {record_field, Index, Field}, Acc1, Fun),
    Record2 = setelement(Index, Record1, T2),
    xform_record(Record2, Acc2, Fun, Index + 1, Fields);
xform_record(Record, Acc, _Fun, _Index, []) ->
    {Record, Acc}.

%% @private
-spec xform_tuple(TypeIn | TypeOut, AccIn, Fun, Index) -> {TypeOut, AccOut} when
    TypeIn :: {ElementIn} | tuple() | dynamic(),
    ElementIn :: dynamic(),
    AccIn :: dynamic(),
    Fun :: xform_func(ElementIn, AccIn, ElementOut, AccOut),
    ElementOut :: dynamic(),
    AccOut :: dynamic(),
    Index :: pos_integer(),
    TypeOut :: {ElementOut} | tuple() | dynamic().
xform_tuple(Tuple1, Acc1, Fun, Index) when Index =< tuple_size(Tuple1) ->
    T1 = element(Index, Tuple1),
    {T2, Acc2} = xform_with_hint(T1, {tuple, Index}, Acc1, Fun),
    Tuple2 = setelement(Index, Tuple1, T2),
    xform_tuple(Tuple2, Acc2, Fun, Index + 1);
xform_tuple(Tuple, Acc, _Fun, _Index) ->
    {Tuple, Acc}.
