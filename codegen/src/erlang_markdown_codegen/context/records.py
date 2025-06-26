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
from typing import Any, Iterator, Optional, Union

from . import Context


@dataclass
class Record:
    ctx: Context = field(repr=False)
    _raw_config: Any = field(repr=False)
    name: str = field(init=False)
    static: bool = field(init=False)
    fields: Optional[list[str]] = field(init=False)
    rust_name: str = field(init=False)
    size: Optional[int] = field(init=False)
    typ: str = field(init=False)

    def __post_init__(self) -> None:
        self.name = self._raw_config["name"]
        self.rust_name = self._raw_config["rust_name"]
        self.static = self._raw_config["static"]
        if self.static:
            self.fields = self._raw_config["fields"]
            self.size = self._raw_config["size"]
            self.typ = self._raw_config["type"]
        else:
            self.fields = None
            self.size = None
            self.typ = "#" + self.name + "{}"

    @property
    def fields_string(self) -> str:
        return "[" + ", ".join(self.fields) + "]"


@dataclass
class Records:
    ctx: Context = field(repr=False)
    _raw_config: Any = field(repr=False)
    record_dict: OrderedDict[str, Record] = field(init=False)
    record_list: list[Record] = field(init=False)

    def __post_init__(self) -> None:
        self.record_dict = OrderedDict()
        self.record_list = []

        for record_config in self._raw_config:
            name: str = record_config["name"]
            if name in self.record_dict:
                raise ValueError(f"duplicate record name found: {name}")
            else:
                record: Record = Record(self.ctx, record_config)
                self.record_dict[record.name] = record
                self.record_list.append(record)
