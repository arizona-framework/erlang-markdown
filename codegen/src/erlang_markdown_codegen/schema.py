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

"""Models for codegen."""
from typing import Literal, Union

from pydantic import BaseModel, ConfigDict, Field, PositiveInt


class Model(BaseModel):
    model_config = ConfigDict(extra="forbid")


class Root(Model):
    commonmark: "CommonMark"
    event: "Event"
    records: list["Record"] = Field(default_factory=list)
    resolve: "Resolve"
    state: "State"
    test_path: str


class CommonMark(Model):
    spec: str
    unicode: str


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
