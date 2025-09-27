# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Models for codegen."""
from typing import Literal, Union

from pydantic import BaseModel, ConfigDict, Field, PositiveInt


class Model(BaseModel):
    model_config = ConfigDict(extra="forbid")


class Root(Model):
    event: "Event"
    records: list["Record"] = Field(default_factory=list)
    resolve: "Resolve"
    state: "State"


class Event(Model):
    name: list["EventName"] = Field(default_factory=list)
    void: list[str] = Field(default_factory=list)


class EventName(Model):
    key: str
    doc: str


class RecordBase(Model):
    name: str
    rust_name: str
    rust_enum: bool = False


class StaticRecord(RecordBase):
    static: Literal[True] = True
    fields: list[str]
    size: PositiveInt
    type: str


class DynamicRecord(RecordBase):
    static: Literal[False] = False


# Union type for Record
Record = Union[StaticRecord, DynamicRecord]


class Resolve(Model):
    name: list["ResolveName"] = Field(default_factory=list)


class ResolveName(Model):
    key: str
    mod: str
    doc: str


class State(Model):
    name: list["StateName"] = Field(default_factory=list)


class StateName(Model):
    key: str
    mod: str
    fun: str
