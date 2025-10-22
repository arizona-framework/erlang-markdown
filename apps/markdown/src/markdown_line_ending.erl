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
%%% % @format
-module(markdown_line_ending).
-moduledoc """
Type of line endings in markdown.

Particularly when working with Windows, you might want to use
`LineEnding::CarriageReturnLineFeed`.

## Examples

```
use markdown::LineEnding;
# fn main() {

// Use a CR + LF combination:
let crlf = LineEnding::CarriageReturnLineFeed;
# }
```
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-03-04", modified => "2025-03-04"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% New API
-export([
    carriage_return/0,
    carriage_return_line_feed/0,
    default/0,
    from_binary/1,
    line_feed/0
]).
%% Instance API
-export([
    as_binary/1
]).

%% Types
-doc """
Sole carriage return (`\r`).

## Example

```markdown
a␍
b
```
""".
-type carriage_return() :: carriage_return.
-doc """
Both a carriage return (`\r`) and a line feed (`\n`).

## Example

```markdown
a␍␊
b
```
""".
-type carriage_return_line_feed() :: carriage_return_line_feed.
-doc """
Sole line feed (`\n`).

## Example

```markdown
a␊
b
```
""".
-type line_feed() :: line_feed.
-type t() :: carriage_return_line_feed() | carriage_return() | line_feed().

-export_type([
    carriage_return/0,
    carriage_return_line_feed/0,
    line_feed/0,
    t/0
]).

%%%=============================================================================
%%% New API functions
%%%=============================================================================

-doc """
Sole carriage return (`\r`).

## Example

```markdown
a␍
b
```
""".
-compile({inline, [carriage_return/0]}).
-spec carriage_return() -> LineEnding when LineEnding :: t().
carriage_return() -> carriage_return.

-doc """
Both a carriage return (`\r`) and a line feed (`\n`).

## Example

```markdown
a␍␊
b
```
""".
-compile({inline, [carriage_return_line_feed/0]}).
-spec carriage_return_line_feed() -> LineEnding when LineEnding :: t().
carriage_return_line_feed() -> carriage_return_line_feed.

-compile({inline, [default/0]}).
-spec default() -> LineEnding when LineEnding :: t().
default() -> line_feed().

-spec from_binary(Bytes) -> LineEnding when Bytes :: binary(), LineEnding :: t().
from_binary(<<"\r\n">>) -> carriage_return_line_feed();
from_binary(<<"\r">>) -> carriage_return();
from_binary(<<"\n">>) -> line_feed().

-doc """
Sole line feed (`\n`).

## Example

```markdown
a␊
b
```
""".
-compile({inline, [line_feed/0]}).
-spec line_feed() -> LineEnding when LineEnding :: t().
line_feed() -> line_feed.

%%%=============================================================================
%%% Instance API functions
%%%=============================================================================

-spec as_binary(LineEnding) -> Bytes when LineEnding :: t(), Bytes :: binary().
as_binary(carriage_return_line_feed) -> <<"\r\n">>;
as_binary(carriage_return) -> <<"\r">>;
as_binary(line_feed) -> <<"\n">>.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
