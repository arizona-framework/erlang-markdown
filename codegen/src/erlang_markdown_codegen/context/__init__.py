# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context models for codegen."""
import abc
import bisect
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional

from ..schema import Root as RootSchema


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
    event: "Event" = field(init=False)
    resolve: "Resolve" = field(init=False)
    state: "State" = field(init=False)

    def __post_init__(self) -> None:
        from .event import Event
        from .records import Records
        from .resolve import Resolve
        from .state import State

        self.event = Event(ctx=self, schema=self.schema.event)
        self.records = Records(ctx=self, schema=self.schema.records)
        self.resolve = Resolve(ctx=self, schema=self.schema.resolve)
        self.state = State(ctx=self, schema=self.schema.state)

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

    def escape_string_for_c(self, original_str: str) -> str:
        escaped_str: str = original_str.encode("unicode_escape").decode("utf-8")
        escaped_str = escaped_str.replace('"', r"\"")  # escape double quotes
        escaped_str = escaped_str.replace("'", r"\'")  # escape single quotes
        return f'"{escaped_str}"'
