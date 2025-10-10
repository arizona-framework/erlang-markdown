# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Models for codegen for tests."""
import datetime
from typing import Literal, Optional

from pydantic import BaseModel, ConfigDict, Field


class Model(BaseModel):
    model_config = ConfigDict(extra="forbid")


class CompileOptions(Model):
    allow_dangerous_html: Optional[bool] = None
    allow_dangerous_protocol: Optional[bool] = None
    allow_any_img_src: Optional[bool] = None
    default_line_ending: Optional[Literal["carriage_return_line_feed", "carriage_return", "line_feed"]] = None
    gfm_footnote_label: Optional[str] = None
    gfm_footnote_label_tag_name: Optional[str] = None
    gfm_footnote_label_attributes: Optional[str] = None
    gfm_footnote_back_label: Optional[str] = None
    gfm_footnote_clobber_prefix: Optional[str] = None
    gfm_task_list_item_checkable: Optional[bool] = None
    gfm_tagfilter: Optional[bool] = None


class ConstructOptions(Model):
    attention: Optional[bool] = None
    attribute_list_flow: Optional[bool] = None
    attribute_list_text: Optional[bool] = None
    autolink: Optional[bool] = None
    block_quote: Optional[bool] = None
    character_escape: Optional[bool] = None
    character_reference: Optional[bool] = None
    code_indented: Optional[bool] = None
    code_fenced: Optional[bool] = None
    code_text: Optional[bool] = None
    definition: Optional[bool] = None
    frontmatter: Optional[bool] = None
    gfm_autolink_literal: Optional[bool] = None
    gfm_label_start_footnote: Optional[bool] = None
    gfm_footnote_definition: Optional[bool] = None
    gfm_strikethrough: Optional[bool] = None
    gfm_table: Optional[bool] = None
    gfm_task_list_item: Optional[bool] = None
    hard_break_escape: Optional[bool] = None
    hard_break_trailing: Optional[bool] = None
    heading_atx: Optional[bool] = None
    heading_setext: Optional[bool] = None
    html_flow: Optional[bool] = None
    html_text: Optional[bool] = None
    label_start_image: Optional[bool] = None
    label_start_link: Optional[bool] = None
    label_end: Optional[bool] = None
    list_item: Optional[bool] = None
    math_flow: Optional[bool] = None
    math_text: Optional[bool] = None
    mdx_esm: Optional[bool] = None
    mdx_expression_flow: Optional[bool] = None
    mdx_expression_text: Optional[bool] = None
    mdx_jsx_flow: Optional[bool] = None
    mdx_jsx_text: Optional[bool] = None
    thematic_break: Optional[bool] = None


class Options(Model):
    extend: Optional[Literal["commonmark", "default", "gfm", "mdx"]] = None
    compile: Optional["CompileOptions"] = None
    parse: Optional["ParseOptions"] = None


class ParseOptions(Model):
    constructs: Optional["ConstructOptions"] = None
    extend: Optional[Literal["commonmark", "default", "gfm", "mdx"]] = None
    gfm_strikethrough_single_tilde: Optional[bool] = None
    math_text_single_dollar: Optional[bool] = None
    mdx_expression_parse: Optional[str] = None
    mdx_esm_parse: Optional[str] = None


class SharedOptions(Model):
    name: str
    options: Options


class SharedParseOptions(Model):
    name: str
    parse_options: ParseOptions


class TestCaseBase(Model):
    name: str
    input: str
    output: str
    message: str
    comment: Optional[str] = None


class TestCaseToHtml(TestCaseBase):
    kind: Literal["to_html"] = "to_html"


class TestCaseToHtmlWithOptions(TestCaseBase):
    kind: Literal["to_html_with_options"] = "to_html_with_options"
    options: str | Options


class TestCaseToMdast(TestCaseBase):
    kind: Literal["to_mdast"] = "to_mdast"
    parse_options: str | ParseOptions


class TestSuite(Model):
    name: str
    created: datetime.date = Field(default_factory=lambda: datetime.date(2025, 3, 4))
    modified: datetime.date = Field(default_factory=lambda: datetime.date(2025, 10, 3))


class TestRoot(Model):
    suite: TestSuite
    shared_options: list[SharedOptions] = Field(default_factory=list)
    shared_parse_options: list[SharedParseOptions] = Field(default_factory=list)
    to_html: list[TestCaseToHtml] = Field(default_factory=list)
    to_html_with_options: list[TestCaseToHtmlWithOptions] = Field(default_factory=list)
    to_mdast: list[TestCaseToMdast] = Field(default_factory=list)
