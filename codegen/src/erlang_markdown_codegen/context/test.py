# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Test models for codegen."""
import abc
import bisect
from collections import OrderedDict
from dataclasses import dataclass, field
import re
from typing import Any, Literal, Optional, Union

from option import Option

from ..test_schema import (
    CompileOptions as CompileOptionsSchema,
    ConstructOptions as ConstructOptionsSchema,
    Options as OptionsSchema,
    ParseOptions as ParseOptionsSchema,
    SharedOptions as SharedOptionsSchema,
    SharedParseOptions as SharedParseOptionsSchema,
    TestCaseBase as TestCaseBaseSchema,
    TestCaseToHtml as TestCaseToHtmlSchema,
    TestCaseToHtmlWithOptions as TestCaseToHtmlWithOptionsSchema,
    TestCaseToMdast as TestCaseToMdastSchema,
    TestRoot as TestRootSchema,
)
from . import Context


TestCase = Union["TestCaseToHtml", "TestCaseToHtmlWithOptions", "TestCaseToMdast"]
TestShared = Union["TestSharedOptions", "TestSharedParseOptions"]


@dataclass
class Test:
    ctx: Context = field(repr=False)
    schema: TestRootSchema = field(repr=False)
    suite: "TestSuite" = field(init=False)
    shared_dict: OrderedDict[str, TestShared] = field(init=False)
    shared_list: list[TestShared] = field(init=False)
    case_dict: OrderedDict[str, TestCase] = field(init=False)
    case_list: list[TestCase] = field(init=False)

    def __post_init__(self) -> None:
        self.suite = TestSuite(ctx=self.ctx, test=self, schema=self.schema.suite)
        self.shared_dict = OrderedDict()
        self.shared_list = []
        for shared_options_schema in self.schema.shared_options:
            shared_options: TestSharedOptions = TestSharedOptions(ctx=self.ctx, schema=shared_options_schema)
            if shared_options.name in self.shared_dict:
                raise ValueError(f"duplicate SharedOptions name found: {shared_options.name}")
            self.shared_dict[shared_options.name] = shared_options
            self.shared_list.append(shared_options)
        for shared_parse_options_schema in self.schema.shared_parse_options:
            shared_parse_options: TestSharedParseOptions = TestSharedParseOptions(ctx=self.ctx, schema=shared_parse_options_schema)
            if shared_parse_options.name in self.shared_dict:
                raise ValueError(f"duplicate SharedParseOptions name found: {shared_parse_options.name}")
            self.shared_dict[shared_parse_options.name] = shared_parse_options
            self.shared_list.append(shared_parse_options)
        self.case_dict = OrderedDict()
        self.case_list = []
        for test_case_to_html_schema in self.schema.to_html:
            test_case_to_html: TestCaseToHtml = TestCaseToHtml(ctx=self.ctx, test=self, schema=test_case_to_html_schema)
            if test_case_to_html.name in self.case_dict:
                raise ValueError(f"duplicate TestCaseToHtml name found: {test_case_to_html.name}")
            self.case_dict[test_case_to_html.name] = test_case_to_html
            bisect.insort(self.case_list, test_case_to_html, key=lambda x: x.key)
        for test_case_to_html_with_options_schema in self.schema.to_html_with_options:
            test_case_to_html_with_options: TestCaseToHtmlWithOptions = TestCaseToHtmlWithOptions(
                ctx=self.ctx, test=self, schema=test_case_to_html_with_options_schema
            )
            if test_case_to_html_with_options.name in self.case_dict:
                raise ValueError(f"duplicate TestCaseToHtmlWithOptions name found: {test_case_to_html_with_options.name}")
            self.case_dict[test_case_to_html_with_options.name] = test_case_to_html_with_options
            bisect.insort(self.case_list, test_case_to_html_with_options, key=lambda x: x.key)
        for test_case_to_mdast_schema in self.schema.to_mdast:
            test_case_to_mdast: TestCaseToMdast = TestCaseToMdast(ctx=self.ctx, test=self, schema=test_case_to_mdast_schema)
            if test_case_to_mdast.name in self.case_dict:
                raise ValueError(f"duplicate TestCaseToMdast name found: {test_case_to_mdast.name}")
            self.case_dict[test_case_to_mdast.name] = test_case_to_mdast
            bisect.insort(self.case_list, test_case_to_mdast, key=lambda x: x.key)

    @property
    def has_shared(self) -> bool:
        return len(self.shared_list) > 0


@dataclass
class TestSuite:
    ctx: Context = field(repr=False)
    schema: TestRootSchema = field(repr=False)
    test: "Test" = field(repr=False)

    @property
    def created(self) -> str:
        return self.schema.created.strftime("%Y-%m-%d")

    @property
    def modified(self) -> str:
        return self.schema.modified.strftime("%Y-%m-%d")

    @property
    def name(self) -> str:
        return self.schema.name


class TestCaseMixin:
    """A mixin that provides various test case functionality."""

    @abc.abstractmethod
    def _schema(self) -> TestCaseBaseSchema:
        pass

    @property
    def key(self) -> list[int | str]:
        return [int(text) if text.isdigit() else text.lower() for text in re.split(r"(\d+)", self.name)]

    @property
    def name(self) -> str:
        return self._schema().name

    @property
    def input(self) -> str:
        return self._schema().input

    @property
    def output(self) -> str:
        return self._schema().output

    @property
    def message(self) -> str:
        return self._schema().message

    @property
    def comment(self) -> Optional[str]:
        return self._schema().comment


@dataclass
class TestCaseToHtml(TestCaseMixin):
    ctx: Context = field(repr=False)
    test: "Test" = field(repr=False)
    schema: TestCaseToHtmlSchema = field(repr=False)

    def _schema(self) -> TestCaseBaseSchema:
        return self.schema

    @property
    def kind(self) -> Literal["to_html"]:
        return "to_html"


@dataclass
class TestCaseToHtmlWithOptions(TestCaseMixin):
    ctx: Context = field(repr=False)
    test: "Test" = field(repr=False)
    schema: TestCaseToHtmlWithOptionsSchema = field(repr=False)
    options: Optional[Union[str, "TestSharedOptions", "TestSuiteOptions"]] = field(default=None)

    def __post_init__(self) -> None:
        if isinstance(self.schema.options, str):
            if self.schema.options in self.test.shared_dict:
                shared_options: TestShared = self.test.shared_dict[self.schema.options]
                if not isinstance(shared_options, TestSharedOptions):
                    raise ValueError(f"For TestCaseToHtmlWithOptions, options must be a TestSharedOptions or TestSuiteOptions")
                self.options = shared_options
            elif self.schema.options == "Options::gfm()":
                self.options = "markdown_options:gfm()"
            else:
                raise ValueError(f"For TestCaseToHtmlWithOptions, options must be a TestSharedOptions or TestSuiteOptions")
        else:
            self.options = TestSuiteOptions(ctx=self.ctx, schema=self.schema.options)

    def _schema(self) -> TestCaseBaseSchema:
        return self.schema

    @property
    def kind(self) -> Literal["to_html_with_options"]:
        return "to_html_with_options"

    @property
    def options_for_erlang(self) -> str:
        if isinstance(self.options, str):
            return self.options
        elif isinstance(self.options, TestSharedOptions):
            return f"?{self.options.name}"
        elif isinstance(self.options, TestSuiteOptions):
            return self.ctx.erlang_map(self.options.as_dict)
        else:
            raise ValueError(f"For TestCaseToHtmlWithOptions, options is invalid: {repr(self.options)}")


@dataclass
class TestCaseToMdast(TestCaseMixin):
    ctx: Context = field(repr=False)
    test: "Test" = field(repr=False)
    schema: TestCaseToMdastSchema = field(repr=False)
    parse_options: Union[str, "TestSharedParseOptions", "TestSuiteParseOptions"] = field(init=False)

    def __post_init__(self) -> None:
        if isinstance(self.schema.parse_options, str):
            if self.schema.parse_options in self.test.shared_dict:
                shared_parse_options: TestShared = self.test.shared_dict[self.schema.parse_options]
                if not isinstance(shared_parse_options, TestSharedParseOptions):
                    raise ValueError(
                        f"For TestCaseToMdast, parse_options must be a TestSharedParseOptions or TestSuiteParseOptions"
                    )
                self.parse_options = shared_parse_options
            elif self.schema.parse_options == "ParseOptions::gfm()":
                self.parse_options = "markdown_parse_options:gfm()"
            else:
                raise ValueError(f"For TestCaseToMdast, parse_options must be a TestSharedParseOptions or TestSuiteParseOptions")
        else:
            self.parse_options = TestSuiteParseOptions(ctx=self.ctx, schema=self.schema.parse_options)

    def _schema(self) -> TestCaseBaseSchema:
        return self.schema

    @property
    def kind(self) -> Literal["to_mdast"]:
        return "to_mdast"

    @property
    def parse_options_for_erlang(self) -> str:
        if isinstance(self.parse_options, str):
            return self.parse_options
        elif isinstance(self.parse_options, TestSharedParseOptions):
            return f"?{self.parse_options.name}"
        elif isinstance(self.parse_options, TestSuiteParseOptions):
            return self.ctx.erlang_map(self.parse_options.as_dict)
        else:
            raise ValueError(f"For TestCaseToMdast, parse_options is invalid: {repr(self.parse_options)}")


@dataclass
class TestSharedOptions:
    ctx: Context = field(repr=False)
    schema: SharedOptionsSchema = field(repr=False)
    options: "TestSuiteOptions" = field(init=False)

    def __post_init__(self) -> None:
        self.options = TestSuiteOptions(ctx=self.ctx, schema=self.schema.options)

    @property
    def name(self) -> str:
        return self.schema.name

    @property
    def kind(self) -> Literal["shared_options"]:
        return "shared_options"


@dataclass
class TestSharedParseOptions:
    ctx: Context = field(repr=False)
    schema: SharedParseOptionsSchema = field(repr=False)
    parse_options: "TestSuiteParseOptions" = field(init=False)

    def __post_init__(self) -> None:
        self.parse_options = TestSuiteParseOptions(ctx=self.ctx, schema=self.schema.parse_options)

    @property
    def name(self) -> str:
        return self.schema.name

    @property
    def kind(self) -> Literal["shared_parse_options"]:
        return "shared_parse_options"


@dataclass
class TestSuiteOptions:
    ctx: Context = field(repr=False)
    schema: OptionsSchema = field(repr=False)
    compile: Optional["TestSuiteCompileOptions"] = field(default=None, init=False)
    parse: Optional["TestSuiteParseOptions"] = field(default=None, init=False)

    def __post_init__(self) -> None:
        if self.schema.compile is not None:
            self.compile = TestSuiteCompileOptions(ctx=self.ctx, schema=self.schema.compile)
        if self.schema.parse is not None:
            self.parse = TestSuiteParseOptions(ctx=self.ctx, schema=self.schema.parse)

    @property
    def as_dict(self) -> OrderedDict[str, Any]:
        out: OrderedDict[str, Any] = OrderedDict()
        if self.compile is not None:
            out["compile"] = self.compile.as_dict
        if self.parse is not None:
            out["parse"] = self.parse.as_dict
        return out


@dataclass
class TestSuiteCompileOptions:
    ctx: Context = field(repr=False)
    schema: CompileOptionsSchema = field(repr=False)

    @property
    def as_dict(self) -> OrderedDict[str, Any]:
        out: OrderedDict[str, Any] = OrderedDict()
        if self.allow_dangerous_html is not None:
            out["allow_dangerous_html"] = self.allow_dangerous_html
        if self.allow_dangerous_protocol is not None:
            out["allow_dangerous_protocol"] = self.allow_dangerous_protocol
        if self.allow_any_img_src is not None:
            out["allow_any_img_src"] = self.allow_any_img_src
        if self.default_line_ending is not None:
            out["default_line_ending"] = self.default_line_ending
        if self.gfm_footnote_label is not None:
            out["gfm_footnote_label"] = self.gfm_footnote_label
        if self.gfm_footnote_label_tag_name is not None:
            out["gfm_footnote_label_tag_name"] = self.gfm_footnote_label_tag_name
        if self.gfm_footnote_label_attributes is not None:
            out["gfm_footnote_label_attributes"] = self.gfm_footnote_label_attributes
        if self.gfm_footnote_back_label is not None:
            out["gfm_footnote_back_label"] = self.gfm_footnote_back_label
        if self.gfm_footnote_clobber_prefix is not None:
            out["gfm_footnote_clobber_prefix"] = self.gfm_footnote_clobber_prefix
        if self.gfm_task_list_item_checkable is not None:
            out["gfm_task_list_item_checkable"] = self.gfm_task_list_item_checkable
        if self.gfm_tagfilter is not None:
            out["gfm_tagfilter"] = self.gfm_tagfilter
        return out

    @property
    def allow_dangerous_html(self) -> Optional[bool]:
        return self.schema.allow_dangerous_html

    @property
    def allow_dangerous_protocol(self) -> Optional[bool]:
        return self.schema.allow_dangerous_protocol

    @property
    def allow_any_img_src(self) -> Optional[bool]:
        return self.schema.allow_any_img_src

    @property
    def default_line_ending(self) -> Optional[str]:
        return self.schema.default_line_ending

    @property
    def gfm_footnote_label(self) -> Optional[Option[str]]:
        if self.schema.gfm_footnote_label is None:
            return None
        else:
            return Option.Some(self.schema.gfm_footnote_label)

    @property
    def gfm_footnote_label_tag_name(self) -> Optional[Option[str]]:
        if self.schema.gfm_footnote_label_tag_name is None:
            return None
        else:
            return Option.Some(self.schema.gfm_footnote_label_tag_name)

    @property
    def gfm_footnote_label_attributes(self) -> Optional[Option[str]]:
        if self.schema.gfm_footnote_label_attributes is None:
            return None
        else:
            return Option.Some(self.schema.gfm_footnote_label_attributes)

    @property
    def gfm_footnote_back_label(self) -> Optional[Option[str]]:
        if self.schema.gfm_footnote_back_label is None:
            return None
        else:
            return Option.Some(self.schema.gfm_footnote_back_label)

    @property
    def gfm_footnote_clobber_prefix(self) -> Optional[Option[str]]:
        if self.schema.gfm_footnote_clobber_prefix is None:
            return None
        else:
            return Option.Some(self.schema.gfm_footnote_clobber_prefix)

    @property
    def gfm_task_list_item_checkable(self) -> Optional[bool]:
        return self.schema.gfm_task_list_item_checkable

    @property
    def gfm_tagfilter(self) -> Optional[bool]:
        return self.schema.gfm_tagfilter


@dataclass
class TestSuiteConstructOptions:
    ctx: Context = field(repr=False)
    schema: ConstructOptionsSchema = field(repr=False)

    @property
    def as_dict(self) -> OrderedDict[str, Any]:
        out: OrderedDict[str, Any] = OrderedDict()
        if self.autolink is not None:
            out["autolink"] = self.autolink
        if self.character_escape is not None:
            out["character_escape"] = self.character_escape
        if self.character_reference is not None:
            out["character_reference"] = self.character_reference
        if self.code_indented is not None:
            out["code_indented"] = self.code_indented
        if self.code_fenced is not None:
            out["code_fenced"] = self.code_fenced
        if self.code_text is not None:
            out["code_text"] = self.code_text
        if self.definition is not None:
            out["definition"] = self.definition
        if self.frontmatter is not None:
            out["frontmatter"] = self.frontmatter
        if self.gfm_autolink_literal is not None:
            out["gfm_autolink_literal"] = self.gfm_autolink_literal
        if self.gfm_label_start_footnote is not None:
            out["gfm_label_start_footnote"] = self.gfm_label_start_footnote
        if self.gfm_footnote_definition is not None:
            out["gfm_footnote_definition"] = self.gfm_footnote_definition
        if self.gfm_strikethrough is not None:
            out["gfm_strikethrough"] = self.gfm_strikethrough
        if self.gfm_table is not None:
            out["gfm_table"] = self.gfm_table
        if self.gfm_task_list_item is not None:
            out["gfm_task_list_item"] = self.gfm_task_list_item
        if self.hard_break_escape is not None:
            out["hard_break_escape"] = self.hard_break_escape
        if self.hard_break_trailing is not None:
            out["hard_break_trailing"] = self.hard_break_trailing
        if self.heading_atx is not None:
            out["heading_atx"] = self.heading_atx
        if self.heading_setext is not None:
            out["heading_setext"] = self.heading_setext
        if self.html_flow is not None:
            out["html_flow"] = self.html_flow
        if self.html_text is not None:
            out["html_text"] = self.html_text
        if self.label_start_image is not None:
            out["label_start_image"] = self.label_start_image
        if self.label_start_link is not None:
            out["label_start_link"] = self.label_start_link
        if self.label_end is not None:
            out["label_end"] = self.label_end
        if self.list_item is not None:
            out["list_item"] = self.list_item
        if self.math_flow is not None:
            out["math_flow"] = self.math_flow
        if self.math_text is not None:
            out["math_text"] = self.math_text
        if self.mdx_esm is not None:
            out["mdx_esm"] = self.mdx_esm
        if self.mdx_expression_flow is not None:
            out["mdx_expression_flow"] = self.mdx_expression_flow
        if self.mdx_expression_text is not None:
            out["mdx_expression_text"] = self.mdx_expression_text
        if self.mdx_jsx_flow is not None:
            out["mdx_jsx_flow"] = self.mdx_jsx_flow
        if self.mdx_jsx_text is not None:
            out["mdx_jsx_text"] = self.mdx_jsx_text
        if self.thematic_break is not None:
            out["thematic_break"] = self.thematic_break
        return out

    @property
    def attention(self) -> Optional[bool]:
        return self.schema.attention

    @property
    def attribute_list_flow(self) -> Optional[bool]:
        return self.schema.attribute_list_flow

    @property
    def attribute_list_text(self) -> Optional[bool]:
        return self.schema.attribute_list_text

    @property
    def autolink(self) -> Optional[bool]:
        return self.schema.autolink

    @property
    def block_quote(self) -> Optional[bool]:
        return self.schema.block_quote

    @property
    def character_escape(self) -> Optional[bool]:
        return self.schema.character_escape

    @property
    def character_reference(self) -> Optional[bool]:
        return self.schema.character_reference

    @property
    def code_indented(self) -> Optional[bool]:
        return self.schema.code_indented

    @property
    def code_fenced(self) -> Optional[bool]:
        return self.schema.code_fenced

    @property
    def code_text(self) -> Optional[bool]:
        return self.schema.code_text

    @property
    def definition(self) -> Optional[bool]:
        return self.schema.definition

    @property
    def frontmatter(self) -> Optional[bool]:
        return self.schema.frontmatter

    @property
    def gfm_autolink_literal(self) -> Optional[bool]:
        return self.schema.gfm_autolink_literal

    @property
    def gfm_label_start_footnote(self) -> Optional[bool]:
        return self.schema.gfm_label_start_footnote

    @property
    def gfm_footnote_definition(self) -> Optional[bool]:
        return self.schema.gfm_footnote_definition

    @property
    def gfm_strikethrough(self) -> Optional[bool]:
        return self.schema.gfm_strikethrough

    @property
    def gfm_table(self) -> Optional[bool]:
        return self.schema.gfm_table

    @property
    def gfm_task_list_item(self) -> Optional[bool]:
        return self.schema.gfm_task_list_item

    @property
    def hard_break_escape(self) -> Optional[bool]:
        return self.schema.hard_break_escape

    @property
    def hard_break_trailing(self) -> Optional[bool]:
        return self.schema.hard_break_trailing

    @property
    def heading_atx(self) -> Optional[bool]:
        return self.schema.heading_atx

    @property
    def heading_setext(self) -> Optional[bool]:
        return self.schema.heading_setext

    @property
    def html_flow(self) -> Optional[bool]:
        return self.schema.html_flow

    @property
    def html_text(self) -> Optional[bool]:
        return self.schema.html_text

    @property
    def label_start_image(self) -> Optional[bool]:
        return self.schema.label_start_image

    @property
    def label_start_link(self) -> Optional[bool]:
        return self.schema.label_start_link

    @property
    def label_end(self) -> Optional[bool]:
        return self.schema.label_end

    @property
    def list_item(self) -> Optional[bool]:
        return self.schema.list_item

    @property
    def math_flow(self) -> Optional[bool]:
        return self.schema.math_flow

    @property
    def math_text(self) -> Optional[bool]:
        return self.schema.math_text

    @property
    def mdx_esm(self) -> Optional[bool]:
        return self.schema.mdx_esm

    @property
    def mdx_expression_flow(self) -> Optional[bool]:
        return self.schema.mdx_expression_flow

    @property
    def mdx_expression_text(self) -> Optional[bool]:
        return self.schema.mdx_expression_text

    @property
    def mdx_jsx_flow(self) -> Optional[bool]:
        return self.schema.mdx_jsx_flow

    @property
    def mdx_jsx_text(self) -> Optional[bool]:
        return self.schema.mdx_jsx_text

    @property
    def thematic_break(self) -> Optional[bool]:
        return self.schema.thematic_break


@dataclass
class TestSuiteParseOptions:
    ctx: Context = field(repr=False)
    schema: ParseOptionsSchema = field(repr=False)
    constructs: Optional["TestSuiteConstructOptions"] = field(default=None, init=False)

    def __post_init__(self) -> None:
        if self.schema.constructs is not None:
            self.constructs = TestSuiteConstructOptions(ctx=self.ctx, schema=self.schema.constructs)

    @property
    def as_dict(self) -> OrderedDict[str, Any]:
        out: OrderedDict[str, Any] = OrderedDict()
        if self.constructs is not None:
            out["constructs"] = self.constructs.as_dict
        if self.gfm_strikethrough_single_tilde is not None:
            out["gfm_strikethrough_single_tilde"] = self.gfm_strikethrough_single_tilde
        if self.math_text_single_dollar is not None:
            out["math_text_single_dollar"] = self.math_text_single_dollar
        if self.mdx_expression_parse is not None:
            out["mdx_expression_parse"] = self.mdx_expression_parse
        if self.mdx_esm_parse is not None:
            out["mdx_esm_parse"] = self.mdx_esm_parse
        return out

    @property
    def gfm_strikethrough_single_tilde(self) -> Optional[bool]:
        return self.schema.gfm_strikethrough_single_tilde

    @property
    def math_text_single_dollar(self) -> Optional[bool]:
        return self.schema.math_text_single_dollar

    @property
    def mdx_expression_parse(self) -> Optional[str]:
        return self.schema.mdx_expression_parse

    @property
    def mdx_esm_parse(self) -> Optional[str]:
        return self.schema.mdx_esm_parse
