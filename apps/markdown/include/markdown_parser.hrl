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
-ifndef(MARKDOWN_PARSER_HRL).
-define(MARKDOWN_PARSER_HRL, 1).

%% How to handle [`State::Ok`][] or [`State::Nok`][].
-record(markdown_attempt, {
    %% Where to go to when successful.
    ok :: markdown_state:t(),
    %% Where to go to when unsuccessful.
    nok :: markdown_state:t(),
    %% Kind of attempt.
    kind :: markdown_attempt:kind(),
    %% If needed, the progress to revert to.
    %%
    %% It is not needed to discard an [`AttemptKind::Attempt`] that has a
    %% `nok` of [`State::Nok`][], because that means it is used in *another*
    %% attempt, which will receive that `Nok`, and has to handle it.
    progress :: markdown_types:option(markdown_progress:t())
}).

%% Configuration that describes how to compile to HTML.
%%
%% You likely either want to turn on the dangerous options
%% (`allow_dangerous_html`, `allow_dangerous_protocol`) when dealing with
%% input you trust, or want to customize how GFM footnotes are compiled
%% (typically because the input markdown is not in English).
%%
%% ## Examples
%%
%% ```
%% use markdown::CompileOptions;
%% # fn main() {
%%
%% // Use the default trait to get safe defaults:
%% let safe = CompileOptions::default();
%%
%% // Live dangerously / trust the author:
%% let danger = CompileOptions {
%%   allow_dangerous_html: true,
%%   allow_dangerous_protocol: true,
%%   ..CompileOptions::default()
%% };
%%
%% // In French:
%% let enFrançais = CompileOptions {
%%   gfm_footnote_label: Some("Notes de bas de page".into()),
%%   gfm_footnote_back_label: Some("Arrière".into()),
%%   ..CompileOptions::default()
%% };
%% # }
%% ```
-record(markdown_compile_options, {
    %% Whether to allow (dangerous) HTML.
    %%
    %% The default is `false`, which still parses the HTML according to
    %% `CommonMark` but shows the HTML as text instead of as elements.
    %%
    %% Pass `true` for trusted content to get actual HTML elements.
    %%
    %% When using GFM, make sure to also turn off `gfm_tagfilter`.
    %% Otherwise, some dangerous HTML is still ignored.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html, to_html_with_options, CompileOptions, Options};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `erlang-markdown` is safe by default:
    %% assert_eq!(
    %%     to_html("Hi, <i>venus</i>!"),
    %%     "<p>Hi, &lt;i&gt;venus&lt;/i&gt;!</p>"
    %% );
    %%
    %% // Turn `allow_dangerous_html` on to allow potentially dangerous HTML:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "Hi, <i>venus</i>!",
    %%         &Options {
    %%             compile: CompileOptions {
    %%               allow_dangerous_html: true,
    %%               ..CompileOptions::default()
    %%             },
    %%             ..Options::default()
    %%         }
    %%     )?,
    %%     "<p>Hi, <i>venus</i>!</p>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    allow_dangerous_html = false :: boolean(),

    %% Whether to allow dangerous protocols in links and images.
    %%
    %% The default is `false`, which drops URLs in links and images that use
    %% dangerous protocols.
    %%
    %% Pass `true` for trusted content to support all protocols.
    %%
    %% URLs that have no protocol (which means it’s relative to the current
    %% page, such as `./some/page.html`) and URLs that have a safe protocol
    %% (for images: `http`, `https`; for links: `http`, `https`, `irc`,
    %% `ircs`, `mailto`, `xmpp`), are safe.
    %% All other URLs are dangerous and dropped.
    %%
    %% When the option `allow_all_protocols_in_img` is enabled,
    %% `allow_dangerous_protocol` only applies to links.
    %%
    %% This is safe because the
    %% [HTML specification][whatwg-html-image-processing]
    %% does not allow executable code in images.
    %% All modern browsers respect this.
    %%
    %% [whatwg-html-image-processing]: https://html.spec.whatwg.org/multipage/images.html#images-processing-model
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html, to_html_with_options, CompileOptions, Options};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `erlang-markdown` is safe by default:
    %% assert_eq!(
    %%     to_html("<javascript:alert(1)>"),
    %%     "<p><a href=\"\">javascript:alert(1)</a></p>"
    %% );
    %%
    %% // Turn `allow_dangerous_protocol` on to allow potentially dangerous protocols:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "<javascript:alert(1)>",
    %%         &Options {
    %%             compile: CompileOptions {
    %%               allow_dangerous_protocol: true,
    %%               ..CompileOptions::default()
    %%             },
    %%             ..Options::default()
    %%         }
    %%     )?,
    %%     "<p><a href=\"javascript:alert(1)\">javascript:alert(1)</a></p>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    allow_dangerous_protocol = false :: boolean(),

    %% Whether to allow all values in images.
    %%
    %% The default is `false`,
    %% which lets `allow_dangerous_protocol` control protocol safety for
    %% both links and images.
    %%
    %% Pass `true` to allow all values as `src` on images,
    %% regardless of `allow_dangerous_protocol`.
    %% This is safe because the
    %% [HTML specification][whatwg-html-image-processing]
    %% does not allow executable code in images.
    %%
    %% [whatwg-html-image-processing]: https://html.spec.whatwg.org/multipage/images.html#images-processing-model
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // By default, some protocols in image sources are dropped:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "![](data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==)",
    %%         &Options::default()
    %%     )?,
    %%     "<p><img src=\"\" alt=\"\" /></p>"
    %% );
    %%
    %% // Turn `allow_any_img_src` on to allow all values as `src` on images.
    %% // This is safe because browsers do not execute code in images.
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "![](javascript:alert(1))",
    %%         &Options {
    %%             compile: CompileOptions {
    %%               allow_any_img_src: true,
    %%               ..CompileOptions::default()
    %%             },
    %%             ..Options::default()
    %%         }
    %%     )?,
    %%     "<p><img src=\"javascript:alert(1)\" alt=\"\" /></p>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    allow_any_img_src = false :: boolean(),

    %% Default line ending to use when compiling to HTML, for line endings not
    %% in `value`.
    %%
    %% Generally, `erlang-markdown` copies line endings (`\r`, `\n`, `\r\n`) in
    %% the markdown document over to the compiled HTML.
    %% In some cases, such as `> a`, CommonMark requires that extra line
    %% endings are added: `<blockquote>\n<p>a</p>\n</blockquote>`.
    %%
    %% To create that line ending, the document is checked for the first line
    %% ending that is used.
    %% If there is no line ending, `default_line_ending` is used.
    %% If that isn’t configured, `\n` is used.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html, to_html_with_options, CompileOptions, LineEnding, Options};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `erlang-markdown` uses `\n` by default:
    %% assert_eq!(
    %%     to_html("> a"),
    %%     "<blockquote>\n<p>a</p>\n</blockquote>"
    %% );
    %%
    %% // Define `default_line_ending` to configure the default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "> a",
    %%         &Options {
    %%             compile: CompileOptions {
    %%               default_line_ending: LineEnding::CarriageReturnLineFeed,
    %%               ..CompileOptions::default()
    %%             },
    %%             ..Options::default()
    %%         }
    %%     )?,
    %%     "<blockquote>\r\n<p>a</p>\r\n</blockquote>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    default_line_ending = markdown_line_ending:default() :: markdown_line_ending:t(),

    %% Textual label to use for the footnotes section.
    %%
    %% The default value is `"Footnotes"`.
    %% Change it when the markdown is not in English.
    %%
    %% This label is typically hidden visually (assuming a `sr-only` CSS class
    %% is defined that does that), and thus affects screen readers only.
    %% If you do have such a class, but want to show this section to everyone,
    %% pass different attributes with the `gfm_footnote_label_attributes`
    %% option.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `"Footnotes"` is used by default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options::gfm()
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %%
    %% // Pass `gfm_footnote_label` to use something else:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               gfm_footnote_label: Some("Notes de bas de page".into()),
    %%               ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Notes de bas de page</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_footnote_label = none :: markdown_option:t(binary()),

    %% HTML tag name to use for the footnote label element.
    %%
    %% The default value is `"h2"`.
    %% Change it to match your document structure.
    %%
    %% This label is typically hidden visually (assuming a `sr-only` CSS class
    %% is defined that does that), and thus affects screen readers only.
    %% If you do have such a class, but want to show this section to everyone,
    %% pass different attributes with the `gfm_footnote_label_attributes`
    %% option.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `"h2"` is used by default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options::gfm()
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %%
    %% // Pass `gfm_footnote_label_tag_name` to use something else:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               gfm_footnote_label_tag_name: Some("h1".into()),
    %%               ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h1 id=\"footnote-label\" class=\"sr-only\">Footnotes</h1>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_footnote_label_tag_name = none :: markdown_option:t(binary()),

    %% Attributes to use on the footnote label.
    %%
    %% The default value is `"class=\"sr-only\""`.
    %% Change it to show the label and add other attributes.
    %%
    %% This label is typically hidden visually (assuming a `sr-only` CSS class
    %% is defined that does that), and thus affects screen readers only.
    %% If you do have such a class, but want to show this section to everyone,
    %% pass an empty string.
    %% You can also add different attributes.
    %%
    %% > 👉 **Note**: `id="footnote-label"` is always added, because footnote
    %% > calls use it with `aria-describedby` to provide an accessible label.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `"class=\"sr-only\""` is used by default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options::gfm()
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %%
    %% // Pass `gfm_footnote_label_attributes` to use something else:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               gfm_footnote_label_attributes: Some("class=\"footnote-heading\"".into()),
    %%               ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"footnote-heading\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_footnote_label_attributes = none :: markdown_option:t(binary()),

    %% Textual label to describe the backreference back to footnote calls.
    %%
    %% The default value is `"Back to content"`.
    %% Change it when the markdown is not in English.
    %%
    %% This label is used in the `aria-label` attribute on each backreference
    %% (the `↩` links).
    %% It affects users of assistive technology.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `"Back to content"` is used by default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options::gfm()
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %%
    %% // Pass `gfm_footnote_back_label` to use something else:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               gfm_footnote_back_label: Some("Arrière".into()),
    %%               ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Arrière\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_footnote_back_label = none :: markdown_option:t(binary()),

    %% Prefix to use before the `id` attribute on footnotes to prevent them
    %% from *clobbering*.
    %%
    %% The default is `"user-content-"`.
    %% Pass `Some("".into())` for trusted markdown and when you are careful
    %% with polyfilling.
    %% You could pass a different prefix.
    %%
    %% DOM clobbering is this:
    %%
    %% ```html
    %% <p id="x"></p>
    %% <script>alert(x) // `x` now refers to the `p#x` DOM element</script>
    %% ```
    %%
    %% The above example shows that elements are made available by browsers,
    %% by their ID, on the `window` object.
    %% This is a security risk because you might be expecting some other
    %% variable at that place.
    %% It can also break polyfills.
    %% Using a prefix solves these problems.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // `"user-content-"` is used by default:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options::gfm()
    %%     )?,
    %%     "<p><sup><a href=\"#user-content-fn-a\" id=\"user-content-fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"user-content-fn-a\">\n<p>b <a href=\"#user-content-fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %%
    %% // Pass `gfm_footnote_clobber_prefix` to use something else:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "[^a]\n\n[^a]: b",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               gfm_footnote_clobber_prefix: Some("".into()),
    %%               ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<p><sup><a href=\"#fn-a\" id=\"fnref-a\" data-footnote-ref=\"\" aria-describedby=\"footnote-label\">1</a></sup></p>\n<section data-footnotes=\"\" class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">Footnotes</h2>\n<ol>\n<li id=\"fn-a\">\n<p>b <a href=\"#fnref-a\" data-footnote-backref=\"\" aria-label=\"Back to content\" class=\"data-footnote-backref\">↩</a></p>\n</li>\n</ol>\n</section>\n"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_footnote_clobber_prefix = none :: markdown_option:t(binary()),

    %% Whether or not GFM task list html `<input>` items are enabled.
    %%
    %% This determines whether or not the user of the browser is able
    %% to click and toggle generated checkbox items. The default is false.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // With `gfm_task_list_item_checkable`, generated `<input type="checkbox" />`
    %% // tags do not contain the attribute `disabled=""` and are thus toggleable by
    %% // browser users.
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "* [x] y.",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%                 gfm_task_list_item_checkable: true,
    %%                 ..CompileOptions::gfm()
    %%             }
    %%         }
    %%     )?,
    %%     "<ul>\n<li><input type=\"checkbox\" checked=\"\" /> y.</li>\n</ul>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    gfm_task_list_item_checkable = false :: boolean(),

    %% Whether to support the GFM tagfilter.
    %%
    %% This option does nothing if `allow_dangerous_html` is not turned on.
    %% The default is `false`, which does not apply the GFM tagfilter to HTML.
    %% Pass `true` for output that is a bit closer to GitHub’s actual output.
    %%
    %% The tagfilter is kinda weird and kinda useless.
    %% The tag filter is a naïve attempt at XSS protection.
    %% You should use a proper HTML sanitizing algorithm instead.
    %%
    %% ## Examples
    %%
    %% ```
    %% use markdown::{to_html_with_options, CompileOptions, Options, ParseOptions};
    %% # fn main() -> Result<(), markdown::message::Message> {
    %%
    %% // With `allow_dangerous_html`, `erlang-markdown` passes HTML through untouched:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "<iframe>",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               allow_dangerous_html: true,
    %%               ..CompileOptions::default()
    %%             }
    %%         }
    %%     )?,
    %%     "<iframe>"
    %% );
    %%
    %% // Pass `gfm_tagfilter: true` to make some of that safe:
    %% assert_eq!(
    %%     to_html_with_options(
    %%         "<iframe>",
    %%         &Options {
    %%             parse: ParseOptions::gfm(),
    %%             compile: CompileOptions {
    %%               allow_dangerous_html: true,
    %%               gfm_tagfilter: true,
    %%               ..CompileOptions::default()
    %%             }
    %%         }
    %%     )?,
    %%     "&lt;iframe>"
    %% );
    %% # Ok(())
    %% # }
    %% ```
    %%
    %% ## References
    %%
    %% *   [*§ 6.1 Disallowed Raw HTML (extension)* in GFM](https://github.github.com/gfm/#disallowed-raw-html-extension-)
    %% *   [`cmark-gfm#extensions/tagfilter.c`](https://github.com/github/cmark-gfm/blob/master/extensions/tagfilter.c)
    gfm_tagfilter = false :: boolean()
}).

%% Constructs record.
-record(markdown_construct_options, {
    %% Attention.
    %%
    %% ```markdown
    %% > | a *b* c **d**.
    %%       ^^^   ^^^^^
    %% ```
    attention = false :: boolean(),

    %% Attribute list (flow).
    %%
    %% ```markdown
    %% > | {.red}
    %%     ^^^^^^
    %% ```
    attribute_list_flow = false :: boolean(),

    %% Attribute list (text).
    %%
    %% ```markdown
    %% > | a {.red}
    %%       ^^^^^^
    %% ```
    attribute_list_text = false :: boolean(),

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

    % %% EXP: Kramdown-style attributes.
    % %%
    % %% ```markdown
    % %% > | # a {: .red}
    % %%         ^^^^^^^^
    % %% ```
    % exp_kramdown_attributes = false :: boolean(),

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

%% Info used to tokenize a container.
%%
%% Practically, these fields are only used for list items.
-record(markdown_container, {
    %% Kind.
    kind :: markdown_container:kind(),
    %% Whether the first line was blank.
    blank_initial :: boolean(),
    %% Size.
    size :: markdown_types:usize()
}).

%% Tracks a bunch of edits.
-record(markdown_edit_map, {
    %% Record of changes.
    map :: markdown_vec:t({markdown_types:usize(), markdown_types:usize(), markdown_vec:t(markdown_event:t())})
}).

%% Something semantic happening somewhere.
-record(markdown_event, {
    %% Kind of event.
    kind :: markdown_event:kind(),
    %% Name of event.
    name :: markdown_event:name(),
    %% Place where this happens.
    point :: markdown_point:t(),
    %% Link to another event.
    link :: markdown_types:option(markdown_event_link:t())
}).

%% Link to another event.
-record(markdown_event_link, {
    %% Previous event.
    previous :: markdown_types:option(markdown_types:usize()),
    %% Next event.
    next :: markdown_types:option(markdown_types:usize()),
    %% Content type.
    content :: markdown_event:content()
}).

-record(markdown_index, {
    %% 0-indexed integer representing a byte in a source file.
    offset :: markdown_index:offset(),
    %% 0-indexed integer representing a virtual byte in a source file.
    virtual :: markdown_index:virtual()
}).

-record(markdown_indices, {
    start :: markdown_indices:offset(),
    'end' :: markdown_indices:offset()
}).

%% Label start, looking for an end.
-record(markdown_label_start, {
    %% Kind of start.
    kind :: markdown_label:kind(),
    %% Indices of where the label starts and ends in `events`.
    start :: markdown_indices:t(),
    %% A boolean used internally to figure out if a (link) label start can’t
    %% be used anymore (because it would contain another link).
    %% That link start is still looking for a balanced closing bracket though,
    %% so we can’t remove it just yet.
    inactive :: boolean()
}).

%% Valid label.
-record(markdown_label, {
    kind :: markdown_label:kind(),
    %% Indices of label start.
    start :: markdown_indices:t(),
    %% Indices of label end.
    'end' :: markdown_indices:t()
}).

-record(markdown_list_item, {
    marker :: byte(),
    balance :: non_neg_integer(),
    start :: non_neg_integer(),
    'end' :: non_neg_integer()
}).

% Deal with positions in a file.
-record(markdown_location, {
    indices :: array:array(markdown_point:offset())
}).

-record(markdown_message, {
    %% Place of message.
    place :: markdown_types:option(markdown_place:t()),
    %% Reason for message (should use markdown).
    reason :: unicode:unicode_binary(),
    %% Category of message.
    rule_id :: unicode:unicode_binary(),
    %% Namespace of message.
    source :: unicode:unicode_binary()
}).

%% Configuration that describes how to parse from markdown and compile to
%% HTML.
%%
%% In most cases, you will want to use the default trait or `gfm` method.
%%
%% ## Examples
%%
%% ```
%% use markdown::Options;
%% # fn main() {
%%
%% // Use the default trait to compile markdown to HTML according to `CommonMark`:
%% let commonmark = Options::default();
%%
%% // Use the `gfm` method to compile markdown to HTML according to GFM:
%% let gfm = Options::gfm();
%% # }
%% ```
-record(markdown_options, {
    %% Configuration that describes how to parse from markdown.
    parse :: markdown_parse_options:t(),
    %% Configuration that describes how to compile to HTML.
    compile :: markdown_compile_options:t()
}).

-record(markdown_parse_options, {
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
    constructs = markdown_construct_options:default() :: markdown_construct_options:t(),

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
    mdx_expression_parse = none :: markdown_types:option(markdown_parse_options:mdx_expression_parse()),

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
    mdx_esm_parse = none :: markdown_types:option(markdown_parse_options:mdx_esm_parse())
}).

%% Info needed, in all content types, when parsing markdown.
%%
%% Importantly, this contains a set of known definitions.
%% It also references the input value as bytes (`u8`).
-record(markdown_parse_state, {
    %% Location.
    location :: markdown_types:option(markdown_location:t()),
    %% Configuration.
    options :: markdown_parse_options:t(),
    %% List of chars.
    bytes :: binary(),
    %% Set of defined definition identifiers.
    definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary()),
    %% Set of defined GFM footnote definition identifiers.
    gfm_footnote_definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary())
}).

%% Somewhere.
-record(markdown_place, {
    inner :: markdown_place:inner()
}).

%% One place in a source file.
%%
%% The interface for the location in the document comes from unist
%% [`Point`](https://github.com/syntax-tree/unist#point).
-record(markdown_point, {
    %% 1-indexed integer representing a line in a source file.
    line :: markdown_point:line(),
    %% 1-indexed integer representing a column in a source file.
    %%
    %% This is increased up to a tab stop for tabs.
    column :: markdown_point:column(),
    %% 0-indexed integer representing a byte in a source file.
    offset :: markdown_point:offset(),
    %% 0-indexed integer representing a virtual byte in a source file.
    virtual :: markdown_point:virtual()
}).

%% Location of a node in a source file.
-record(markdown_position, {
    %% Represents the place of the first character of the parsed source region.
    start :: markdown_point:t(),
    %% Represents the place of the first character after the parsed source region, whether it exists or not.
    'end' :: markdown_point:t()
}).

%% The internal state of a tokenizer.
%%
%% Not to be confused with states from the state machine, this instead is all
%% the information on where we currently are and what’s going on.
-record(markdown_progress, {
    %% Length of `events`.
    %%
    %% It’s not allowed to remove events, so reverting will just pop stuff off.
    events_len :: markdown_types:usize(),
    %% Length of the stack.
    %%
    %% It’s not allowed to decrease the stack in an attempt.
    stack_len :: markdown_types:usize(),
    %% Previous code.
    previous :: markdown_types:option(byte()),
    %% Current code.
    current :: markdown_types:option(byte()),
    %% Current place in the file.
    point :: markdown_point:t()
}).

%% Attention sequence that we can take markers from.
-record(markdown_sequence, {
    %% Marker as a byte (`u8`) used in this sequence.
    marker :: byte(),
    %% We track whether sequences are in balanced events, and where those
    %% events start, so that one attention doesn't start in say, one link, and
    %% end in another.
    stack :: markdown_vec:t(markdown_types:usize()),
    %% The index into events where this sequence's `Enter` currently resides.
    index :: markdown_types:usize(),
    %% The (shifted) point where this sequence starts.
    start_point :: markdown_point:t(),
    %% The (shifted) point where this sequence end.
    end_point :: markdown_point:t(),
    %% The number of markers we can still use.
    size :: markdown_types:usize(),
    %% Whether this sequence can open attention.
    open :: boolean(),
    %% Whether this sequence can close attention.
    close :: boolean()
}).

%% Bytes belonging to a range.
%%
%% Includes info on virtual spaces before and after the bytes.
-record(markdown_slice, {
    %% Bytes.
    bytes :: binary(),
    %% Number of virtual spaces before the bytes.
    before :: markdown_types:usize(),
    %% Number of virtual spaces after the bytes.
    'after' :: markdown_types:usize()
}).

-record(markdown_space_or_tab_eol_options, {
    %% Connect this whitespace to the previous.
    connect :: boolean(),
    %% Embedded content type to use.
    content :: markdown_types:option(markdown_event:content())
}).

-record(markdown_space_or_tab_options, {
    %% Minimum allowed bytes (inclusive).
    min :: markdown_types:usize(),
    %% Maximum allowed bytes (inclusive).
    max :: markdown_types:usize() | infinity,
    %% Name to use for events.
    kind :: markdown_event:name(),
    %% Connect this event to the previous.
    connect :: boolean(),
    %% Embedded content type to use.
    content :: markdown_types:option(markdown_event:content())
}).

%% Relative byte index into a string, to an absolute byte index into the whole document.
-record(markdown_stop, {
    relative :: non_neg_integer(),
    absolute :: non_neg_integer()
}).

-record(markdown_subresult, {
    done = false :: boolean(),
    gfm_footnote_definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary()),
    definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary())
}).

%% A lot of shared fields used to tokenize things.
-record(markdown_tokenize_state, {
    %% Couple complex fields used to tokenize the document.
    %% Tokenizer, used to tokenize flow in document.
    document_child = none :: markdown_types:option(markdown_tokenizer:t()),
    %% State, used to tokenize containers.
    document_child_state = none :: markdown_types:option(markdown_state:t()),
    %% Stack of currently active containers.
    document_container_stack = markdown_vec:new() :: markdown_vec:t(markdown_container:t()),
    %% How many active containers continued.
    document_continued = 0 :: markdown_types:usize(),
    %% Index of last `data`.
    document_data_index = none :: markdown_types:option(markdown_types:usize()),
    %% Container exits by line number.
    document_exits = markdown_vec:new() :: markdown_vec:t(markdown_types:option(markdown_vec:t(markdown_event:t()))),
    %% Whether the previous flow was a paragraph or a definition.
    document_lazy_accepting_before = false :: boolean(),
    %% Whether this is the first paragraph (potentially after definitions) in
    %% a list item.
    %% Used for GFM task list items.
    document_at_first_paragraph_of_list_item = false :: boolean(),

    %% Couple of very frequent settings for parsing whitespace.
    space_or_tab_eol_content = none :: markdown_types:option(markdown_event:content()),
    space_or_tab_eol_connect = false :: boolean(),
    space_or_tab_eol_ok = false :: boolean(),
    space_or_tab_connect = false :: boolean(),
    space_or_tab_content = none :: markdown_types:option(markdown_event:content()),
    space_or_tab_min = 0 :: markdown_types:usize(),
    space_or_tab_max = 0 :: markdown_types:usize() | infinity,
    space_or_tab_size = 0 :: markdown_types:usize(),
    space_or_tab_token = space_or_tab :: markdown_event:name(),

    %% Couple of media related fields.
    %% List of usable label starts.
    %%
    %% Used when tokenizing [text content][crate::construct::text].
    label_starts = markdown_vec:new() :: markdown_vec:t(markdown_label_start:t()),
    %% List of unusable label starts.
    %%
    %% Used when tokenizing [text content][crate::construct::text].
    label_starts_loose = markdown_vec:new() :: markdown_vec:t(markdown_label_start:t()),
    %% Stack of images and links.
    %%
    %% Used when tokenizing [text content][crate::construct::text].
    labels = markdown_vec:new() :: markdown_vec:t(markdown_label:t()),

    %% List of defined definition identifiers.
    definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary()),
    %% List of defined GFM footnote definition identifiers.
    gfm_footnote_definitions = markdown_vec:new() :: markdown_vec:t(unicode:unicode_binary()),

    %% Last error message provided at an EOF of an expression.
    mdx_last_parse_error = none :: markdown_types:option({
        unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()
    }),

    %% Whether to connect events.
    connect = false :: boolean(),
    %% Marker.
    marker = 0 :: byte(),
    %% Secondary marker.
    marker_b = 0 :: byte(),
    %% Several markers.
    markers = <<>> :: binary(),
    %% Whether something was seen.
    seen = false :: boolean(),
    %% Size.
    size = 0 :: markdown_types:usize(),
    %% Secondary size.
    size_b = 0 :: markdown_types:usize(),
    %% Tertiary size.
    size_c = 0 :: markdown_types:usize(),
    %% Index.
    start = 0 :: markdown_types:usize(),
    %% Index.
    'end' = 0 :: markdown_types:usize(),
    %% Slot for an event name.
    token_1 = data :: markdown_event:name(),
    %% Slot for an event name.
    token_2 = data :: markdown_event:name(),
    %% Slot for an event name.
    token_3 = data :: markdown_event:name(),
    %% Slot for an event name.
    token_4 = data :: markdown_event:name(),
    %% Slot for an event name.
    token_5 = data :: markdown_event:name(),
    %% Slot for an event name.
    token_6 = data :: markdown_event:name()
}).

%% A tokenizer itself.
-record(markdown_tokenizer, {
    %% Jump between line endings.
    column_start = markdown_vec:new() :: markdown_vec:t(markdown_index:t()),
    %% First line where this tokenizer starts.
    first_line :: markdown_point:line(),
    %% Current point after the last line ending (excluding jump).
    line_start :: markdown_point:t(),
    %% Track whether the current byte is already consumed (`true`) or expected
    %% to be consumed (`false`).
    %%
    %% Tracked to make sure everything’s valid.
    consumed = true :: boolean(),
    %% Stack of how to handle attempts.
    attempts = markdown_vec:new() :: markdown_vec:t(markdown_attempt:t()),
    %% Current byte.
    current = none :: markdown_types:option(byte()),
    %% Previous byte.
    previous = none :: markdown_types:option(byte()),
    %% Current relative and absolute place in the file.
    point :: markdown_point:t(),
    %% Semantic labels.
    events = markdown_vec:new() :: markdown_vec:t(markdown_event:t()),
    %% Hierarchy of semantic labels.
    %%
    %% Tracked to make sure everything’s valid.
    stack = markdown_vec:new() :: markdown_vec:t(markdown_event:name()),
    %% Edit map, to batch changes.
    map = markdown_edit_map:new() :: markdown_edit_map:t(),
    %% List of resolvers.
    resolvers = markdown_vec:new() :: markdown_vec:t(markdown_resolve:name()),
    %% Shared parsing state across tokenizers.'
    parse_state :: markdown_parse_state:t(),
    %% A lot of shared fields used to tokenize things.
    tokenize_state = #markdown_tokenize_state{} :: markdown_tokenize_state:t(),
    %% Whether we would be interrupting something.
    %%
    %% Used when tokenizing [flow content][crate::construct::flow].
    interrupt = false :: boolean(),
    %% Whether containers cannot “pierce” into the current construct.
    %%
    %% Used when tokenizing [document content][crate::construct::document].
    concrete = false :: boolean(),
    %% Whether this row is piercing into the current construct with more
    %% containers.
    %%
    %% Used when tokenizing [document content][crate::construct::document].
    pierce = false :: boolean(),
    %% Whether this line is lazy: there are less containers than before.
    lazy = false :: boolean()
}).

%% Macros
-define(is_markdown_label_kind(X),
    ((X) =:= image orelse (X) =:= link orelse (X) =:= gfm_footnote orelse (X) =:= gfm_undefined_footnote)
).
-define(is_markdown_line_ending(X),
    ((X) =:= carriage_return_line_feed orelse (X) =:= carriage_return orelse (X) =:= line_feed)
).

-endif.
