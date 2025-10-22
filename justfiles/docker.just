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

docker_image := "erlang-markdown-codegen"

[macos]
_docker-exec +args: docker-build
    docker run --rm --volume $(pwd):/project {{docker_image}} {{args}}

[macos]
_docker-interactive +args: docker-build
    docker run --rm --volume $(pwd):/project -it {{docker_image}} {{args}}

[group('docker')]
[macos]
docker-build:
    docker build -f ./codegen/Dockerfile -t {{docker_image}} .

[group('docker')]
[macos]
docker-codegen:
    @just _docker-exec just codegen

[group('docker')]
[macos]
docker-commonmark:
    @just _docker-exec just commonmark

[group('docker')]
[macos]
docker-format:
    @just _docker-exec just format

[group('docker')]
[macos]
docker-lock:
    @just _docker-exec just poetry-lock

[group('docker')]
[macos]
docker-shell:
    @just _docker-interactive /bin/bash

[group('docker')]
[macos]
docker-sign:
    @just _docker-exec just sign
