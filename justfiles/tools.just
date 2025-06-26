# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

[linux]
[working-directory: '/project/codegen']
codegen: poetry-install format
    poetry run erlang_markdown_codegen /project/codegen/config.yaml /project /project/codegen/templates0 /project/codegen/templates1

[macos]
codegen: docker-codegen

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
