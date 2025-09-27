# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Event models for codegen."""
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional

from ..schema import Resolve as ResolveSchema, ResolveName as ResolveNameSchema
from . import Context


@dataclass
class Name:
    ctx: Context = field(repr=False)
    schema: ResolveNameSchema = field(repr=False)
    key: str = field(init=False)
    mod: str = field(init=False)
    doc: str = field(init=False)
    doc_lines: list[str] = field(init=False, repr=False)

    def __post_init__(self) -> None:
        self.key = self.schema.key
        self.mod = self.schema.mod
        self.doc = self.schema.doc
        self.doc_lines = self.doc.split("\n")


@dataclass
class Resolve:
    ctx: Context = field(repr=False)
    schema: ResolveSchema = field(repr=False)
    name_dict: OrderedDict[str, Name] = field(init=False)
    name_list: list[Name] = field(init=False)

    def __post_init__(self) -> None:
        self.name_dict = OrderedDict()
        self.name_list = []

        for name_schema in self.schema.name:
            name: Name = Name(ctx=self.ctx, schema=name_schema)
            self.name_dict[name.key] = name
            self.name_list.append(name)
