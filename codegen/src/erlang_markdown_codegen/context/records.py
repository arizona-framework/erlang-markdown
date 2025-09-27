# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Event models for codegen."""
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional, Union

from ..schema import Record as RecordSchema
from . import Context


@dataclass
class Record:
    ctx: Context = field(repr=False)
    schema: RecordSchema = field(repr=False)
    name: str = field(init=False)
    static: bool = field(init=False)
    fields: Optional[list[str]] = field(init=False)
    rust_enum: bool = field(init=False)
    rust_name: str = field(init=False)
    size: Optional[int] = field(init=False)
    typ: str = field(init=False)

    def __post_init__(self) -> None:
        self.name = self.schema.name
        self.rust_enum = self.schema.rust_enum  # optional
        self.rust_name = self.schema.rust_name
        self.static = self.schema.static  # optional
        if self.static:
            self.fields = self.schema.fields
            self.size = self.schema.size
            self.typ = self.schema.type
        else:
            self.fields = None
            self.size = None
            self.typ = "#" + self.name + "{}"

    @property
    def fields_string(self) -> str:
        if self.fields is None:
            return "[]"
        else:
            return "[" + ", ".join(self.fields) + "]"


@dataclass
class Records:
    ctx: Context = field(repr=False)
    schema: list[RecordSchema] = field(repr=False)
    record_dict: OrderedDict[str, Record] = field(init=False)
    record_list: list[Record] = field(init=False)

    def __post_init__(self) -> None:
        self.record_dict = OrderedDict()
        self.record_list = []

        for record_schema in self.schema:
            name: str = record_schema.name
            if name in self.record_dict:
                raise ValueError(f"duplicate record name found: {name}")
            else:
                record: Record = Record(ctx=self.ctx, schema=record_schema)
                self.record_dict[record.name] = record
                self.record_list.append(record)
