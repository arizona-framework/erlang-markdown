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

"""Context Unicode models for codegen."""
import re
from collections import deque, OrderedDict
from dataclasses import dataclass, field
import sys
from typing import Annotated, Iterator, Optional

from ..schema import Unicode as UnicodeSchema
from . import Context

CHUNK: int = 64


def new_chunk_factory(chunkmap: dict[tuple[int, ...], int], dense: list[list[int]]) -> callable:
    """Factory function to create a new_chunk closure."""

    def new_chunk(chunk: list[int]) -> int:
        chunk_tuple = tuple(chunk)
        if chunk_tuple in chunkmap:
            return chunkmap[chunk_tuple]
        else:
            dense.append(chunk.copy())
            new_idx = len(chunkmap)
            if new_idx >= 256:
                raise ValueError("exceeded 256 unique chunks")
            chunkmap[chunk_tuple] = new_idx
            return new_idx

    return new_chunk


@dataclass
class Unicode:
    ctx: Context = field(repr=False)
    schema: UnicodeSchema = field(repr=False)
    chunk: int = field(init=False)
    halfdense: list[int] = field(init=False)
    id_start: set[int] = field(init=False)
    id_start_list: list[int] = field(init=False, repr=False)
    id_continue: set[int] = field(init=False)
    id_continue_list: list[int] = field(init=False, repr=False)
    index_start: list[int] = field(init=False)
    index_continue: list[int] = field(init=False)
    punctuation_list: list["UnicodePunctuation"] = field(init=False)

    def ascii_start_rows(self) -> list[int]:
        return list(range(0, 4))

    def ascii_start_cols(self, i: int) -> list[bool]:
        return [((i * 32) + j) in self.id_start for j in range(0, 32)]

    def ascii_continue_rows(self) -> list[int]:
        return list(range(0, 4))

    def ascii_continue_cols(self, i: int) -> list[bool]:
        return [((i * 32) + j) in self.id_continue for j in range(0, 32)]

    def derived_core_properties(self) -> str:
        with open(self.schema.derived_core_properties, "r", encoding="utf-8") as f:
            return f.read()

    def unicode_data(self) -> str:
        with open(self.schema.data, "r", encoding="utf-8") as f:
            return f.read()

    def is_id_start(self, ch: str) -> bool:
        return ord(ch) in self.id_start

    def is_id_continue(self, ch: str) -> bool:
        return ord(ch) in self.id_continue

    def __post_init__(self) -> None:
        self.__post_init_punctuation__()
        self.__post_init_tables__()

    def __post_init_punctuation__(self) -> None:
        self.punctuation_list = []

        content: str = self.unicode_data()

        # Unicode categories for punctuation and symbols
        search_categories: set[str] = {
            "Pc",  # Punctuation, Connector
            "Pd",  # Punctuation, Dash
            "Pe",  # Punctuation, Close
            "Pf",  # Punctuation, FinalQuote
            "Pi",  # Punctuation, InitialQuote
            "Po",  # Punctuation, Other
            "Ps",  # Punctuation, Open
            "Sc",  # Symbol, Currency
            "Sk",  # Symbol, Modifier
            "Sm",  # Symbol, Math
            "So",  # Symbol, Other
        }

        for line in content.splitlines():
            cells: list[str] = line.split(";")
            if len(cells) > 2:
                char_code: str = cells[0]
                category: str = cells[2]

                if category in search_categories:
                    punctuation: UnicodePunctuation = UnicodePunctuation(ctx=self.ctx, char_code=char_code)
                    self.punctuation_list.append(punctuation)

    def __post_init_tables__(self) -> None:
        content: str = self.derived_core_properties()

        def parse_codepoint(s: str) -> int | None:
            try:
                return int(s, 16)
            except ValueError:
                return None

        def parse_line(line: str) -> tuple[int, int, str] | None:
            codepoint, rest = line.split(sep=";", maxsplit=1)
            codepoint = codepoint.strip()
            range = [parse_codepoint(x) for x in codepoint.split(sep="..", maxsplit=1)]
            if len(range) == 1:
                if range[0] is None:
                    return None
                else:
                    lo: int = range[0]
                    hi: int = lo
                    parts = rest.strip().split("#")
                    if len(parts) == 0:
                        return None
                    else:
                        name: str = parts[0].rstrip()
                        return (lo, hi, name)
            elif len(range) == 2:
                if range[0] is None or range[1] is None:
                    return None
                else:
                    lo: int = range[0]
                    hi: int = range[1]
                    parts = rest.strip().split("#")
                    if len(parts) == 0:
                        return None
                    else:
                        name: str = parts[0].rstrip()
                        return (lo, hi, name)
            else:
                return None

        self.id_start = set()
        self.id_continue = set()

        for i, line in enumerate(content.splitlines()):
            if line.startswith("#") or len(line.strip()) == 0:
                continue
            else:
                parsed = parse_line(line)
                if parsed is None:
                    raise ValueError(f"{self.schema.derived_core_properties} line {i} is unexpected:\n{line}")
                else:
                    lo, hi, name = parsed
                    if name == "ID_Start":
                        self.id_start.update(range(lo, hi + 1))
                    elif name == "ID_Continue":
                        self.id_continue.update(range(lo, hi + 1))
                    else:
                        continue

        self.id_start_list = list(sorted(self.id_start))
        self.id_continue_list = list(sorted(self.id_continue))

        chunkmap: dict[tuple[int, ...], int] = {}
        dense: list[list[int]] = []
        new_chunk = new_chunk_factory(chunkmap, dense)

        empty_chunk: list[int] = [0] * CHUNK
        new_chunk(empty_chunk)

        index_start: list[int] = []
        index_continue: list[int] = []

        max_unicode: int = sys.maxunicode

        for i in range((max_unicode + 1) // CHUNK // 8):
            start_bits: list[int] = [0] * CHUNK
            continue_bits: list[int] = [0] * CHUNK
            for j in range(CHUNK):
                for k in range(8):
                    code: int = (i * CHUNK + j) * 8 + k
                    if code >= 0x80 and code <= max_unicode:
                        try:
                            ch: str = chr(code)
                            if self.is_id_start(ch):
                                start_bits[j] |= 1 << k
                            if self.is_id_continue(ch):
                                continue_bits[j] |= 1 << k
                        except ValueError:
                            # Invalid code point
                            pass
            index_start.append(new_chunk(start_bits))
            index_continue.append(new_chunk(continue_bits))

        # Remove trailing zeros
        while index_start and index_start[-1] == 0:
            index_start.pop()
        while index_continue and index_continue[-1] == 0:
            index_continue.pop()

        # Build halfchunkmap
        halfchunkmap: dict[tuple[int, ...], deque[list[int]]] = {}
        for chunk in dense:
            front: list[int] = chunk[: CHUNK // 2]
            back: list[int] = chunk[CHUNK // 2 :]
            front_tuple = tuple(front)

            if front_tuple not in halfchunkmap:
                halfchunkmap[front_tuple] = deque()
            halfchunkmap[front_tuple].append(back)

        # Build halfdense and dense_to_halfdense mapping
        halfdense: list[int] = []
        dense_to_halfdense: dict[int, int] = {}

        for chunk in dense:
            chunk_tuple = tuple(chunk)
            original_pos: int = chunkmap[chunk_tuple]

            if original_pos in dense_to_halfdense:
                continue

            front: list[int] = chunk[: CHUNK // 2]
            back: list[int] = chunk[CHUNK // 2 :]
            front_tuple = tuple(front)

            halfdense_idx = len(halfdense) // (CHUNK // 2)
            if halfdense_idx >= 256:
                raise ValueError("exceeded 256 half-chunks")

            dense_to_halfdense[original_pos] = halfdense_idx
            halfdense.extend(front)
            halfdense.extend(back)

            # Process chained half-chunks
            back_tuple = tuple(back)
            while back_tuple in halfchunkmap and halfchunkmap[back_tuple]:
                next_back: list[int] = halfchunkmap[back_tuple].popleft()

                concat: list[int] = back + next_back
                concat_tuple = tuple(concat)
                original_pos = chunkmap[concat_tuple]

                if original_pos in dense_to_halfdense:
                    continue

                halfdense_idx = len(halfdense) // (CHUNK // 2) - 1
                if halfdense_idx >= 256:
                    raise ValueError("exceeded 256 half-chunks")

                dense_to_halfdense[original_pos] = halfdense_idx
                halfdense.extend(next_back)
                back = next_back
                back_tuple = tuple(back)

        # Remap indices
        index_start = [dense_to_halfdense[idx] for idx in index_start]
        index_continue = [dense_to_halfdense[idx] for idx in index_continue]

        # chunkmap: dict[FixedBytes64, int] = dict()
        # dense: list[FixedBytes64] = list()

        # def new_chunk(chunk: FixedBytes64) -> int:
        #     if chunk in chunkmap:
        #         return chunkmap[chunk]
        #     else:
        #         dense.append(chunk)
        #         if len(chunkmap) >= 256:
        #             raise ValueError("exceeded 256 unique chunks")
        #         else:
        #             chunkmap[chunk] = len(chunkmap)
        #             return chunkmap[chunk]

        # empty_chunk: FixedBytes64 = b"\x00" * 64
        # new_chunk(empty_chunk)

        # def char_from_u32(code: int) -> str | None:
        #     try:
        #         return chr(code)
        #     except (ValueError, OverflowError):
        #         return None

        # index_start: list[int] = []
        # index_continue: list[int] = []
        # for i in range(0, ((sys.maxunicode + 1) // 64) // 8):
        #     start_bits: MutableBytes64 = MutableBytes64(empty_chunk)
        #     continue_bits: MutableBytes64 = MutableBytes64(empty_chunk)
        #     for j in range(0, 64):
        #         for k in range(0, 8):
        #             code: int = (i * 64 + j) * 8 + k
        #             if code >= 0x80:
        #                 codepoint: str | None = char_from_u32(code)
        #                 if codepoint is not None:
        #                     start_bits[j] |= self.is_id_start(codepoint) << k
        #                     continue_bits[j] |= self.is_id_continue(codepoint) << k
        #     index_start.append(new_chunk(start_bits.to_bytes()))
        #     index_continue.append(new_chunk(continue_bits.to_bytes()))

        # while len(index_start) > 0 and index_start[-1] == 0:
        #     index_start.pop()

        # while len(index_continue) > 0 and index_continue[-1] == 0:
        #     index_continue.pop()

        # halfchunkmap: dict[bytes, deque[bytes]] = dict()
        # for chunk in dense:
        #     front: bytes = chunk[0:32]
        #     back: bytes = chunk[32:64]
        #     if front not in halfchunkmap:
        #         halfchunkmap[front] = deque()
        #     halfchunkmap[front].append(back)

        # halfdense: list[int] = list()
        # dense_to_halfdense: dict[int, int] = dict()
        # for chunk in dense:
        #     original_pos: int = chunkmap[chunk]
        #     if original_pos in dense_to_halfdense:
        #         continue
        #     else:
        #         front: bytes = chunk[0:32]
        #         back: bytes = chunk[32:64]
        #         if (len(halfdense) // 32) >= 256:
        #             raise ValueError("exceeded 256 half-chunks")
        #         dense_to_halfdense[original_pos] = len(halfdense) // 32
        #         halfdense.extend(front)
        #         halfdense.extend(back)
        #         while back in halfchunkmap and len(halfchunkmap[back]) > 0:
        #             next: bytes = halfchunkmap[back].popleft()
        #             concat: bytes = back + next
        #             original_pos: int = chunkmap[concat]
        #             if original_pos in dense_to_halfdense:
        #                 continue
        #             if (len(halfdense) // 32 - 1) >= 256:
        #                 raise ValueError("exceeded 256 half-chunks")
        #             dense_to_halfdense[original_pos] = len(halfdense) // 32 - 1
        #             halfdense.extend(next)
        #             back = next

        # for index in index_start:
        #     index_start[index] = dense_to_halfdense[index]

        # for index in index_continue:
        #     index_continue[index] = dense_to_halfdense[index]

        self.chunk = CHUNK
        self.index_start = index_start
        self.index_continue = index_continue
        self.halfdense = halfdense


# @dataclass
# class UnicodeChunkEntry:
#     ctx: Context = field(repr=False)
#     key: FixedBytes64
#     val: int


@dataclass
class UnicodePunctuation:
    ctx: Context = field(repr=False)
    char_code: str
