# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Event models for codegen."""
from abc import abstractclassmethod
import bisect
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional

from . import Context


@dataclass
class Name:
    ctx: Context = field(repr=False)
    _raw_config: Any = field(repr=False)
    key: str = field(init=False)
    doc: str = field(init=False)
    doc_lines: list[str] = field(init=False, repr=False)

    def __post_init__(self) -> None:
        self.key = self._raw_config["key"]
        self.doc = self._raw_config["doc"]
        self.doc_lines = self.doc.split("\n")


@dataclass
class Event:
    ctx: Context = field(repr=False)
    _raw_config: Any = field(repr=False)
    name_dict: OrderedDict[str, Name] = field(init=False)
    name_list: list[Name] = field(init=False)
    void_list: list[Name] = field(init=False, repr=False)

    def __post_init__(self) -> None:
        self.name_dict = OrderedDict()
        self.name_list = []
        self.void_list = []

        for name_config in self._raw_config["name"]:
            name: Name = Name(self.ctx, name_config)
            self.name_dict[name.key] = name
            self.name_list.append(name)

        for void_name in self._raw_config["void"]:
            if void_name not in self.name_dict:
                raise ValueError(f"void event name not found: {void_name}")
            else:
                self.void_list.append(self.name_dict[void_name])
