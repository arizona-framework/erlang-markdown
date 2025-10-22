###-----------------------------------------------------------------------------
### %CopyrightBegin%
###
### SPDX-License-Identifier: Apache-2.0
###
### Copyright (c) Meta Platforms, Inc. and affiliates.
### Copyright (c) WhatsApp LLC
###
### Licensed under the Apache License, Version 2.0 (the "License");
### you may not use this file except in compliance with the License.
### You may obtain a copy of the License at
###
###     http://www.apache.org/licenses/LICENSE-2.0
###
### Unless required by applicable law or agreed to in writing, software
### distributed under the License is distributed on an "AS IS" BASIS,
### WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
### See the License for the specific language governing permissions and
### limitations under the License.
###
### %CopyrightEnd%
###-----------------------------------------------------------------------------

"""Context models for codegen."""
import abc
import bisect
import glob
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional

import yaml

from option import Option

from ..schema import Root as RootSchema
from ..test_schema import TestRoot as TestRootSchema


@dataclass(frozen=True)
class Atom:
    ctx: "Context" = field(repr=False, compare=False, hash=False)
    name: str

    @classmethod
    def make(cls, ctx: "Context", name: str) -> "Atom":
        if name in ctx.atoms:
            return ctx.atoms[name]
        else:
            atom = Atom(ctx=ctx, name=name)
            ctx.atoms[name] = atom
            return atom

    @property
    def as_erlang(self) -> str:
        if len(self.name) == 0:
            return "''"
        elif self.name[0].isalpha() and self.name[0].islower() and all(c.isalnum() or c == "_" for c in self.name):
            return self.name
        else:
            return f"'{self.name}'"


@dataclass
class Export:
    ctx: "Context" = field(repr=False)
    module: Atom
    function: Atom
    arity: int

    @classmethod
    def make(cls, ctx: "Context", module: Atom, function: Atom, arity: int) -> "Export":
        if (module, function, arity) in ctx.exports:
            return ctx.exports[(module, function, arity)]
        else:
            export = Export(ctx=ctx, module=module, function=function, arity=arity)
            ctx.exports[(module, function, arity)] = export
            return export

    @classmethod
    def from_erlang(cls, ctx: "Context", value: str) -> "Export":
        """Parse an Erlang function string like "fun 'module':'function'/2" into an Export."""
        # Remove 'fun ' prefix and split by ':'
        if not value.startswith("fun "):
            raise ValueError(f"Invalid Erlang export format: {value}")

        content = value[4:]  # Remove 'fun ' prefix

        # Split by ':' to separate module and function/arity
        parts = content.split(":")
        if len(parts) != 2:
            raise ValueError(f"Invalid Erlang export format: {value}")

        module_part = parts[0].strip()
        func_arity_part = parts[1].strip()

        # Extract module name (remove quotes)
        if module_part.startswith("'") and module_part.endswith("'"):
            module_name = module_part[1:-1]
        else:
            module_name = module_part

        # Split function and arity by '/'
        func_arity_split = func_arity_part.split("/")
        if len(func_arity_split) != 2:
            raise ValueError(f"Invalid function/arity format: {func_arity_part}")

        function_part = func_arity_split[0].strip()
        arity_part = func_arity_split[1].strip()

        # Extract function name (remove quotes)
        if function_part.startswith("'") and function_part.endswith("'"):
            function_name = function_part[1:-1]
        else:
            function_name = function_part

        # Parse arity as integer
        try:
            arity = int(arity_part)
        except ValueError:
            raise ValueError(f"Invalid arity format: {arity_part}")

        # Create Atom objects and Export
        module_atom = Atom.make(ctx, module_name)
        function_atom = Atom.make(ctx, function_name)

        return cls.make(ctx, module_atom, function_atom, arity)

    @property
    def as_erlang(self) -> str:
        return f"fun {self.module.as_erlang}:{self.function.as_erlang}/{self.arity}"


class NamesMixin:
    """A mixin that provides various name format variations."""

    # def __str__(self) -> str:
    #     return self.name

    @abc.abstractmethod
    def name_prefix(self) -> str:
        pass

    @abc.abstractmethod
    def names(self) -> list[str]:
        pass

    @property
    def dot_name(self) -> str:
        """Returns the name in dot.notation format."""
        return ".".join(self.names())

    @property
    def var_name(self) -> str:
        """Returns the name in snake_case format."""
        return "_".join(self.names())

    # @property
    # def full_underscore_name(self) -> str:
    #     return f"{self.name_prefix().lower()}_{self.underscore_name.lower()}"

    # @property
    # def macro_name(self) -> str:
    #     return f"{self.name_prefix().upper()}_{self.underscore_name.upper()}"

    # @property
    # def underscore_name(self) -> str:
    #     return "_".join(self.names())


@dataclass
class Context:
    _output_path: str = field(repr=False)
    schema: RootSchema = field(repr=False)
    commonmark: "CommonMark" = field(init=False)
    event: "Event" = field(init=False)
    resolve: "Resolve" = field(init=False)
    state: "State" = field(init=False)
    test: OrderedDict[str, "TestSuite"] = field(init=False)
    atoms: OrderedDict[str, Atom] = field(init=False, default_factory=OrderedDict)
    exports: OrderedDict[tuple[Atom, Atom, int], Export] = field(init=False, default_factory=OrderedDict)

    def __post_init__(self) -> None:
        from .commonmark import CommonMark
        from .event import Event
        from .records import Records
        from .resolve import Resolve
        from .state import State
        from .test import Test

        self.commonmark = CommonMark(ctx=self, schema=self.schema.commonmark)
        self.event = Event(ctx=self, schema=self.schema.event)
        self.records = Records(ctx=self, schema=self.schema.records)
        self.resolve = Resolve(ctx=self, schema=self.schema.resolve)
        self.state = State(ctx=self, schema=self.schema.state)
        self.test = OrderedDict()
        for test_config_file in glob.iglob("*.yaml", root_dir=self.schema.test_path, recursive=True):
            with open(f"{self.schema.test_path}/{test_config_file}", "r", encoding="utf-8") as f:
                data: Any = yaml.safe_load(f)
                test_schema: TestRootSchema = TestRootSchema(**data)
                if test_schema.suite.name in self.test:
                    raise ValueError(f"duplicate TestSuite name found: {test_schema.suite.name}")
                self.test[test_schema.suite.name] = Test(ctx=self, schema=test_schema)

        return

    def debug(self, name: str) -> bool:
        if name in self.debug_names():
            return True
        else:
            return False

    def debug_names(self) -> set[str]:
        return set()

    def debug_print_fmt(self, str_fmt: Optional[str]) -> str:
        if str_fmt:
            return self.escape_string_for_c(f"%s:%d {str_fmt}")
        else:
            return self.escape_string_for_c("%s:%d")

    def debug_print_args(self, str_args: Optional[str]) -> str:
        if str_args:
            return f", {str_args}"
        else:
            return ""

    def erlang_atom(self, value: str) -> str:
        return f"'{value}'"

    def erlang_binary(self, value: str) -> str:
        return f'<<"{self.escape_string_for_erlang(value)}"/utf8>>'

    def erlang_comment(self, original_str: str) -> str:
        comment: str = original_str.strip()
        lines: list[str] = comment.split("\n")
        return "\n".join(f"%% {line}" for line in lines)

    def erlang_map(self, data: OrderedDict[str, Any]) -> str:
        return f"#{{{''.join([f'{k} => {self.erlang_map_value(v)},' for k, v in data.items()])}}}"

    def erlang_map_value(self, value: Any) -> str:
        if isinstance(value, str):
            return f'<<"{self.escape_string_for_erlang(value)}"/utf8>>'
        elif isinstance(value, bool):
            return "true" if value else "false"
        elif isinstance(value, int):
            return str(value)
        elif isinstance(value, float):
            return str(value)
        elif isinstance(value, list):
            return f"[{','.join([self.erlang_map_value(v) for v in value])}]"
        elif isinstance(value, OrderedDict):
            return self.erlang_map(value)
        elif isinstance(value, Atom):
            return value.as_erlang
        elif isinstance(value, Export):
            return value.as_erlang
        elif isinstance(value, Option):
            if value.is_none:
                return "none"
            else:
                return f"{{some, {self.erlang_map_value(value.unwrap())}}}"
        else:
            raise ValueError(f"unsupported type: {type(value)} -> {repr(value)}")

    def erlang_multiline_binary(self, value: str) -> str:
        if len(value) > 120 and "\n" in value and '"""' not in value:
            return f'<<"""\n{value}\n"""/utf8>>'
        else:
            return self.erlang_binary(value)

    def escape_string_for_erlang(self, original_str: str) -> str:
        result = []
        for char in original_str:
            code = ord(char)

            # Handle special escape sequences
            if char == "\\":
                result.append("\\\\")
            elif char == '"':
                result.append('\\"')
            elif char == "\n":
                result.append("\\n")
            elif char == "\r":
                result.append("\\r")
            elif char == "\t":
                result.append("\\t")
            elif char == "\b":
                result.append("\\b")
            elif char == "\f":
                result.append("\\f")
            elif char == "\v":
                result.append("\\v")
            # Handle printable ASCII characters
            elif 32 <= code < 127:
                result.append(char)
            # Handle other characters as octal escapes
            else:
                result.append(char)
                # # For UTF-8 bytes outside ASCII range, encode and escape each byte
                # for byte in char.encode("utf-8"):
                #     result.append(f"\\x{byte:03o}")
        return "".join(result)

        # escaped_str: str = original_str.encode("unicode_escape").decode("utf-8")
        # escaped_str = escaped_str.replace('"', r"\"")  # escape double quotes
        # escaped_str = escaped_str.replace("'", r"\'")  # escape single quotes
        # return f"{escaped_str}"
        # chars: list[str] = []
        # for char in original_str:

        # comment: str = original_str.strip()
        # lines: list[str] = comment.split("\n")
        # return "\\n".join(f"{line}" for line in lines)

    def escape_string_for_c(self, original_str: str) -> str:
        escaped_str: str = original_str.encode("unicode_escape").decode("utf-8")
        escaped_str = escaped_str.replace('"', r"\"")  # escape double quotes
        escaped_str = escaped_str.replace("'", r"\'")  # escape single quotes
        return f'"{escaped_str}"'
