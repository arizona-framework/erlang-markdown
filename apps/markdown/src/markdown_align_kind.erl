%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_align_kind).
-moduledoc """
GFM: alignment of phrasing content.

Used to align the contents of table cells within a table.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% API
-export([
    center/0,
    left/0,
    none/0,
    right/0
]).

%% Types
-doc """
Center alignment.

See the `center` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | :-: |
      ^^^
```
""".
-type center() :: center.
-doc """
Left alignment.

See the `left` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | :-- |
      ^^^
```
""".
-type left() :: left.
-doc """
No alignment.

Phrasing content is aligned as defined by the host environment.

```markdown
  | | aaa |
> | | --- |
      ^^^
```
""".
-type nope() :: none.
-doc """
Right alignment.

See the `right` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | --: |
      ^^^
```
""".
-type right() :: right.
-type t() :: left() | right() | center() | nope().

-export_type([
    center/0,
    left/0,
    nope/0,
    right/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-doc """
Center alignment.

See the `center` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | :-: |
      ^^^
```
""".
-compile({inline, [center/0]}).
-spec center() -> AlignKind when AlignKind :: t().
center() -> center.

-doc """
Left alignment.

See the `left` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | :-- |
      ^^^
```
""".
-compile({inline, [left/0]}).
-spec left() -> AlignKind when AlignKind :: t().
left() -> left.

-doc """
No alignment.

Phrasing content is aligned as defined by the host environment.

```markdown
  | | aaa |
> | | --- |
      ^^^
```
""".
-compile({inline, [none/0]}).
-spec none() -> AlignKind when AlignKind :: t().
none() -> none.

-doc """
Right alignment.

See the `right` value of the `text-align` CSS property.

```markdown
  | | aaa |
> | | --: |
      ^^^
```
""".
-compile({inline, [right/0]}).
-spec right() -> AlignKind when AlignKind :: t().
right() -> right.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
