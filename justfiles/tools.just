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

[linux]
[working-directory: '/project/codegen']
codegen: poetry-install format
    poetry run erlang_markdown_codegen json-schema /project/codegen/schema.json
    poetry run erlang_markdown_codegen json-schema-test /project/codegen/test/schema.json
    poetry run erlang_markdown_codegen generate /project/codegen/config.yaml /project /project/codegen/templates0 /project/codegen/templates1

[macos]
codegen: docker-codegen
    gmake format
    just sign

[linux]
[working-directory: '/project/codegen']
commonmark:
    curl -Lfo /project/codegen/commonmark-spec.txt https://raw.githubusercontent.com/commonmark/commonmark-spec/0.31.2/spec.txt
    curl -Lfo /project/codegen/DerivedCoreProperties.txt https://www.unicode.org/Public/UCD/latest/ucd/DerivedCoreProperties.txt
    curl -Lfo /project/codegen/UnicodeData.txt https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt

[macos]
commonmark: docker-commonmark

[linux]
format: format-python-src

[linux]
[working-directory: '/project/codegen']
format-python-src:
    poetry run black /project/codegen/src/

[macos]
format: docker-format
    gmake format

[linux]
lock:
    just poetry-lock

[macos]
lock: docker-lock

[macos]
shell: docker-shell

[linux]
[working-directory: '/project/codegen']
sign: poetry-install
    poetry run erlang_markdown_codegen sign /project/codegen/config.yaml /project /project/codegen/templates0 /project/codegen/templates1

[macos]
sign: docker-sign

[macos]
watch:
    watchexec -w codegen -- just codegen
