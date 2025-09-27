# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

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
    punctuation_list: list["CommonMarkPuncuation"] = field(init=False)
    test_case_list: list["CommonMarkTestCase"] = field(init=False)

    def commonmark_spec(self) -> str:
        with open(self.schema.spec, "r", encoding="utf-8") as f:
            return f.read()

    def unicode_data(self) -> str:
        with open(self.schema.unicode, "r", encoding="utf-8") as f:
            return f.read()

    def __post_init__(self) -> None:
        self.__post_init_commonmark__()
        self.__post_init_punctuation__()

    def __post_init_commonmark__(self) -> None:
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
                    punctuation: CommonMarkPuncuation = CommonMarkPuncuation(ctx=self.ctx, char_code=char_code)
                    self.punctuation_list.append(punctuation)


@dataclass
class CommonMarkPuncuation:
    ctx: Context = field(repr=False)
    char_code: str


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
