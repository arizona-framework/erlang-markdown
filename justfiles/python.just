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
setup-python:
    # Use the latest available pip
    pip install --upgrade pip
    # Use the latest available poetry
    pip install --upgrade poetry
    # Setup poetry export plugin
    poetry self add poetry-plugin-export
    # Ensure that virtualenvs are installed in .venv
    poetry config virtualenvs.in-project true
    # Setup poetry shell plugin
    poetry self add poetry-plugin-shell

[linux]
[working-directory: '/project/codegen']
poetry-install:
    poetry install --no-interaction

[linux]
[working-directory: '/project/codegen']
poetry-lock:
    poetry lock --no-interaction
    poetry install --no-interaction
    (poetry export --no-interaction --without-hashes --without-urls --format=requirements.txt | awk '{ print $1 }' FS=';' | awk '{$1=$1};1') > requirements.txt
