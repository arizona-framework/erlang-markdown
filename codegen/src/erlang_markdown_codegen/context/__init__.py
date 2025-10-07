# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

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
