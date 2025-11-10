%%%-----------------------------------------------------------------------------
%%% %CopyrightBegin%
%%%
%%% SPDX-License-Identifier: Apache-2.0
%%%
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
%%%-----------------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%%% % @oncall whatsapp_clr
-ifndef(MARKDOWN_CONFIG_HRL).
-define(MARKDOWN_CONFIG_HRL, 1).

%% Constructs record.
-record(markdown_config_constructs, {
    %% Attention.
    %%
    %% ```markdown
    %% > | a *b* c **d**.
    %%       ^^^   ^^^^^
    %% ```
    attention = false :: boolean(),

    %% Autolink.
    %%
    %% ```markdown
    %% > | a <https://example.com> b <user@example.org>.
    %%       ^^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^
    %% ```
    autolink = false :: boolean(),

    %% Block quote.
    %%
    %% ```markdown
    %% > | > a
    %%     ^^^
    %% ```
    block_quote = false :: boolean(),

    %% Character escape.
    %%
    %% ```markdown
    %% > | a \* b
    %%       ^^
    %% ```
    character_escape = false :: boolean(),

    %% Character reference.
    %%
    %% ```markdown
    %% > | a &amp; b
    %%       ^^^^^
    %% ```
    character_reference = false :: boolean(),

    %% Code (indented).
    %%
    %% ```markdown
    %% > |     a
    %%     ^^^^^
    %% ```
    code_indented = false :: boolean(),

    %% Code (fenced).
    %%
    %% ```markdown
    %% > | ~~~js
    %%     ^^^^^
    %% > | console.log(1)
    %%     ^^^^^^^^^^^^^^
    %% > | ~~~
    %%     ^^^
    %% ```
    code_fenced = false :: boolean(),

    %% Code (text).
    %%
    %% ```markdown
    %% > | a `b` c
    %%       ^^^
    %% ```
    code_text = false :: boolean(),

    %% Definition.
    %%
    %% ```markdown
    %% > | [a]: b "c"
    %%     ^^^^^^^^^^
    %% ```
    definition = false :: boolean(),

    %% Frontmatter.
    %%
    %% ````markdown
    %% > | ---
    %%     ^^^
    %% > | title: Neptune
    %%     ^^^^^^^^^^^^^^
    %% > | ---
    %%     ^^^
    %% ````
    frontmatter = false :: boolean(),

    %% GFM: autolink literal.
    %%
    %% ```markdown
    %% > | https://example.com
    %%     ^^^^^^^^^^^^^^^^^^^
    %% ```
    gfm_autolink_literal = false :: boolean(),

    %% GFM: footnote definition.
    %%
    %% ```markdown
    %% > | [^a]: b
    %%     ^^^^^^^
    %% ```
    gfm_footnote_definition = false :: boolean(),

    %% GFM: footnote label start.
    %%
    %% ```markdown
    %% > | a[^b]
    %%      ^^
    %% ```
    gfm_label_start_footnote = false :: boolean(),

    %%
    %% ```markdown
    %% > | a ~b~ c.
    %%       ^^^
    %% ```
    gfm_strikethrough = false :: boolean(),

    %% GFM: table.
    %%
    %% ```markdown
    %% > | | a |
    %%     ^^^^^
    %% > | | - |
    %%     ^^^^^
    %% > | | b |
    %%     ^^^^^
    %% ```
    gfm_table = false :: boolean(),

    %% GFM: task list item.
    %%
    %% ```markdown
    %% > | * [x] y.
    %%       ^^^
    %% ```
    gfm_task_list_item = false :: boolean(),

    %% Hard break (escape).
    %%
    %% ```markdown
    %% > | a\
    %%      ^
    %%   | b
    %% ```
    hard_break_escape = false :: boolean(),

    %% Hard break (trailing).
    %%
    %% ```markdown
    %% > | a␠␠
    %%      ^^
    %%   | b
    %% ```
    hard_break_trailing = false :: boolean(),

    %% Heading (atx).
    %%
    %% ```markdown
    %% > | # a
    %%     ^^^
    %% ```
    heading_atx = false :: boolean(),

    %% Heading (setext).
    %%
    %% ```markdown
    %% > | a
    %%     ^^
    %% > | ==
    %%     ^^
    %% ```
    heading_setext = false :: boolean(),

    %% HTML (flow).
    %%
    %% ```markdown
    %% > | <div>
    %%     ^^^^^
    %% ```
    html_flow = false :: boolean(),

    %% HTML (text).
    %%
    %% ```markdown
    %% > | a <b> c
    %%       ^^^
    %% ```
    html_text = false :: boolean(),

    %% Label start (image).
    %%
    %% ```markdown
    %% > | a ![b](c) d
    %%       ^^
    %% ```
    label_start_image = false :: boolean(),

    %% Label start (link).
    %%
    %% ```markdown
    %% > | a [b](c) d
    %%       ^
    %% ```
    label_start_link = false :: boolean(),

    %% Label end.
    %%
    %% ```markdown
    %% > | a [b](c) d
    %%         ^^^^
    %% ```
    label_end = false :: boolean(),

    %% List items.
    %%
    %% ```markdown
    %% > | * a
    %%     ^^^
    %% ```
    list_item = false :: boolean(),

    %% Math (flow).
    %%
    %% ```markdown
    %% > | $$
    %%     ^^
    %% > | \frac{1}{2}
    %%     ^^^^^^^^^^^
    %% > | $$
    %%     ^^
    %% ```
    math_flow = false :: boolean(),

    %% Math (text).
    %%
    %% ```markdown
    %% > | a $b$ c
    %%       ^^^
    %% ```
    math_text = false :: boolean(),

    %% MDX: ESM.
    %%
    %% ```markdown
    %% > | import a from 'b'
    %%     ^^^^^^^^^^^^^^^^^
    %% ```
    %%
    %% > 👉 **Note**: to support ESM, you *must* pass
    %% > [`mdx_esm_parse`][MdxEsmParse] in [`ParseOptions`][] too.
    %% > Otherwise, ESM is treated as normal markdown.
    mdx_esm = false :: boolean(),

    %% MDX: expression (flow).
    %%
    %% ```markdown
    %% > | {Math.PI}
    %%     ^^^^^^^^^
    %% ```
    %%
    %% > 👉 **Note**: You *can* pass
    %% > [`mdx_expression_parse`][MdxExpressionParse] in [`ParseOptions`][]
    %% > too, to parse expressions according to a certain grammar (typically,
    %% > a programming language).
    %% > Otherwise, expressions are parsed with a basic algorithm that only
    %% > cares about braces.
    mdx_expression_flow = false :: boolean(),

    %% MDX: expression (text).
    %%
    %% ```markdown
    %% > | a {Math.PI} c
    %%       ^^^^^^^^^
    %% ```
    %%
    %% > 👉 **Note**: You *can* pass
    %% > [`mdx_expression_parse`][MdxExpressionParse] in [`ParseOptions`][]
    %% > too, to parse expressions according to a certain grammar (typically,
    %% > a programming language).
    %% > Otherwise, expressions are parsed with a basic algorithm that only
    %% > cares about braces.
    mdx_expression_text = false :: boolean(),

    %% MDX: JSX (flow).
    %%
    %% ```markdown
    %% > | <Component />
    %%     ^^^^^^^^^^^^^
    %% ```
    %%
    %% > 👉 **Note**: You *must* pass `html_flow: false` to use this,
    %% > as it's preferred when on over `mdx_jsx_flow`.
    %%
    %% > 👉 **Note**: You *can* pass
    %% > [`mdx_expression_parse`][MdxExpressionParse] in [`ParseOptions`][]
    %% > too, to parse expressions in JSX according to a certain grammar
    %% > (typically, a programming language).
    %% > Otherwise, expressions are parsed with a basic algorithm that only
    %% > cares about braces.
    mdx_jsx_flow = false :: boolean(),

    %% MDX: JSX (text).
    %%
    %% ```markdown
    %% > | a <Component /> c
    %%       ^^^^^^^^^^^^^
    %% ```
    %%
    %% > 👉 **Note**: You *must* pass `html_text: false` to use this,
    %% > as it's preferred when on over `mdx_jsx_text`.
    %%
    %% > 👉 **Note**: You *can* pass
    %% > [`mdx_expression_parse`][MdxExpressionParse] in [`ParseOptions`][]
    %% > too, to parse expressions in JSX according to a certain grammar
    %% > (typically, a programming language).
    %% > Otherwise, expressions are parsed with a basic algorithm that only
    %% > cares about braces.
    mdx_jsx_text = false :: boolean(),

    %% Thematic break.
    %%
    %% ```markdown
    %% > | ***
    %%     ^^^
    %% ```
    thematic_break = false :: boolean()
}).

%% Record for parsing options.
%%
%% @type parse_options() :: #parse_options{
%%    constructs              :: constructs(),
%%    gfm_strikethrough_single_tilde :: boolean(),
%%    math_text_single_dollar :: boolean(),
%%    mdx_expression_parse    :: function() | undefined,
%%    mdx_esm_parse :: fun((binary()) -> term()) | undefined           :: function() | undefined
%% }.

-record(markdown_config_parse_opts, {
    %% Which constructs to enable and disable.
    %% @type constructs() :: #constructs{}
    %%
    %% The default is to follow `CommonMark`.
    %%
    %% == Examples ==
    %%
    %% ```
    %% % `erlang-markdown` follows CommonMark by default:
    %% <<"<pre><code>indented code?\n</code></pre>">> =
    %%     to_html(<<"    indented code?">>),
    %%
    %% % Pass `constructs` to choose what to enable and disable:
    %% <<"<p>indented code?</p>">> =
    %%     to_html_with_options(
    %%         <<"    indented code?">>,
    %%         #options{
    %%             parse = #parse_options{
    %%                 constructs = #constructs{
    %%                     code_indented = false
    %%                 }
    %%             }
    %%         }
    %%     )
    %% ```
    constructs = markdown_config_constructs:default() :: markdown_config_constructs:t(),

    %% Whether to support GFM strikethrough with a single tilde
    %%
    %% This option does nothing if `gfm_strikethrough` is not turned on in
    %% `constructs`.
    %% This option does not affect strikethrough with double tildes.
    %%
    %% The default is `true`, which follows how markdown on `github.com`
    %% works, as strikethrough with single tildes is supported.
    %% Pass `false`, to follow the GFM spec more strictly, by not allowing
    %% strikethrough with single tildes.
    %%
    %% == Examples ==
    %%
    %% ```
    %% % `erlang-markdown` supports single tildes by default:
    %% <<"<p><del>a</del></p>">> =
    %%     to_html_with_options(
    %%         <<"~a~">>,
    %%         #options{
    %%             parse = #parse_options{
    %%                 constructs = constructs:gfm()
    %%             }
    %%         }
    %%     ),
    %%
    %% % Pass `gfm_strikethrough_single_tilde: false` to turn that off:
    %% <<"<p>~a~</p>">> =
    %%     to_html_with_options(
    %%         <<"~a~">>,
    %%         #options{
    %%             parse = #parse_options{
    %%                 constructs = constructs:gfm(),
    %%                 gfm_strikethrough_single_tilde = false
    %%             }
    %%         }
    %%     )
    %% ```
    gfm_strikethrough_single_tilde = false :: boolean(),

    %% Whether to support math (text) with a single dollar
    %%
    %% This option does nothing if `math_text` is not turned on in
    %% `constructs`.
    %% This option does not affect math (text) with two or more dollars.
    %%
    %% The default is `true`, which is more close to how code (text) and
    %% Pandoc work, as it allows math with a single dollar to form.
    %% However, single dollars can interfere with "normal" dollars in text.
    %% Pass `false`, to only allow math (text) to form when two or more
    %% dollars are used.
    %% If you pass `false`, you can still use two or more dollars for text
    %% math.
    %%
    %% == Examples ==
    %%
    %% ```
    %% % `erlang-markdown` supports single dollars by default:
    %% <<"<p><code class=\"language-math math-inline\">a</code></p>">> =
    %%     to_html_with_options(
    %%         <<"$a$">>,
    %%         #options{
    %%             parse = #parse_options{
    %%                 constructs = #constructs{
    %%                     math_text = true
    %%                 }
    %%             }
    %%         }
    %%     ),
    %%
    %% % Pass `math_text_single_dollar: false` to turn that off:
    %% <<"<p>$a$</p>">> =
    %%     to_html_with_options(
    %%         <<"$a$">>,
    %%         #options{
    %%             parse = #parse_options{
    %%                 constructs = #constructs{
    %%                     math_text = true
    %%                 },
    %%                 math_text_single_dollar = false
    %%             }
    %%         }
    %%     )
    %% ```
    math_text_single_dollar = false :: boolean(),

    %% Function to parse expressions with.
    %%
    %% This function can be used to add support for arbitrary programming
    %% languages within expressions.
    %%
    %% It only makes sense to pass this when compiling to a syntax tree
    %% with `to_mdast()`.
    %%
    %% For an example that adds support for JavaScript with SWC, see
    %% `tests/test_utils/mod.rs`.
    % mdx_expression_parse :: fun((binary()) -> term()) | undefined,
    mdx_expression_parse = none :: markdown_types:option(markdown_config_parse_opts:mdx_expression_parse()),

    %% Function to parse ESM with.
    %%
    %% This function can be used to add support for arbitrary programming
    %% languages within ESM blocks, however, the keywords (`export`,
    %% `import`) are currently hardcoded JavaScript-specific.
    %%
    %% @note Please raise an issue if you're interested in working on
    %% MDX that is aware of, say, Rust, or other programming languages.
    %%
    %% It only makes sense to pass this when compiling to a syntax tree
    %% with `to_mdast()`.
    %%
    %% For an example that adds support for JavaScript with SWC, see
    %% `tests/test_utils/mod.rs`.
    mdx_esm_parse = none :: markdown_types:option(markdown_config_parse_opts:mdx_esm_parse())
}).

-endif.
