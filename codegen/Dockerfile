# syntax=docker/dockerfile:1

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

# Use an official Python image.
FROM python:3.13-alpine

# Install apk dependencies.
RUN apk add --no-cache bash clang-extra-tools curl
RUN apk add --no-cache -X https://dl-cdn.alpinelinux.org/alpine/edge/community just

# Set the working directory.
RUN mkdir -p /project
WORKDIR /project
COPY ./justfile .

# Setup Python.
COPY ./justfiles/python.just ./justfiles/python.just
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
RUN just setup-python
ENV POETRY_VIRTUALENVS_IN_PROJECT=true
