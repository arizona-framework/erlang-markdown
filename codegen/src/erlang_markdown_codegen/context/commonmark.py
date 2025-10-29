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

"""Context CommonMark models for codegen."""
import re
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Optional

from ..schema import CommonMark as CommonMarkSchema
from . import Context


@dataclass
class CommonMark:
    ctx: Context = field(repr=False)
    schema: CommonMarkSchema = field(repr=False)
    test_case_list: list["CommonMarkTestCase"] = field(init=False)

    def commonmark_spec(self) -> str:
        with open(self.schema.spec, "r", encoding="utf-8") as f:
            return f.read()

    def __post_init__(self) -> None:
        self.test_case_list = []

        content: str = self.commonmark_spec()

        # Regular expressions for parsing
        example_pattern = re.compile(r"(?:^`{32} example\n[\s\S]*?\n`{32}$|^#{1,6} *(.*)$)", re.MULTILINE)
        heading_prefix_pattern = re.compile(r"#{1,6} ")
        in_out_separator_pattern = re.compile(r"\n\.(?:\n|$)")

        # Clean up content
        content = re.sub(r"<!-- END TESTS -->[\s\S]*", "", content)
        content = re.sub(r"→", "\t", content)

        current_heading: Optional[str] = None
        test_number: int = 1

        for match in example_pattern.finditer(content):
            lines: list[str] = match.group(0).split("\n")

            if len(lines) == 1:
                # This is a heading
                current_heading = heading_prefix_pattern.sub("", lines[0])
            else:
                # This is a test case
                # Remove first and last lines (backticks)
                lines = lines[1:-1]

                if current_heading is None:
                    raise ValueError("Found test case without section heading")

                case_content: str = "\n".join(lines)
                parts: list[str] = in_out_separator_pattern.split(case_content)

                input_text: str = f"{parts[0]}\n"
                output_text: str = "" if len(parts) < 2 or not parts[1] else f"{parts[1]}\n"

                test_case: CommonMarkTestCase = CommonMarkTestCase(
                    ctx=self.ctx,
                    index=test_number,
                    heading=current_heading,
                    input_text=input_text,
                    output_text=output_text,
                )

                self.test_case_list.append(test_case)
                test_number += 1


@dataclass
class CommonMarkTestCase:
    ctx: Context = field(repr=False)
    index: int
    heading: str
    input_text: str
    output_text: str

    @property
    def heading_underscored(self) -> str:
        return "_".join([part.lower() for part in self.heading.split(" ")])

    @property
    def name(self) -> str:
        return f"test_{self.heading_underscored}_case_{self.index}"
