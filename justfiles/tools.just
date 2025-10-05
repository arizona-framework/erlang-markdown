# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

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
