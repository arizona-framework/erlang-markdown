%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% Created :  26 Jun 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(markdown_mdast_align).
-moduledoc """
GFM: alignment of phrasing content.

Used to align the contents of table cells within a table.
""".
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

% -include_lib("markdown/include/markdown_mdast.hrl").

%% New API
-export([]).

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
GFM: alignment of phrasing content.

Used to align the contents of table cells within a table.
""".
-type kind() :: left() | right() | center() | none().
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
-type none() :: none.
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

-export_type([
    center/0,
    kind/0,
    left/0,
    none/0,
    right/0
]).

%% Macros
% -define(is_kind(X), ((X) =:= left orelse (X) =:= right orelse (X) =:= center orelse (X) =:= none)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
