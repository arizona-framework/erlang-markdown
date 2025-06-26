# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context models for codegen."""
from abc import abstractclassmethod
import bisect
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional


class NamesMixin:
    """A mixin that provides various name format variations."""

    # def __str__(self) -> str:
    #     return self.name

    @abstractclassmethod
    def name_prefix(self) -> str:
        pass

    @abstractclassmethod
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
    _output_path: str
    _raw_config: Any = field(repr=False)
    event: "Event" = field(init=False)
    resolve: "Resolve" = field(init=False)
    state: "State" = field(init=False)

    def __post_init__(self) -> None:
        from .event import Event
        from .records import Records
        from .resolve import Resolve
        from .state import State

        self.event = Event(self, self._raw_config["event"])
        self.records = Records(self, self._raw_config["records"])
        self.resolve = Resolve(self, self._raw_config["resolve"])
        self.state = State(self, self._raw_config["state"])
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
