%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  04 Mar 2025 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
-ifndef(MARKDOWN_VEC_HRL).
-define(MARKDOWN_VEC_HRL, 1).

-record(markdown_vec, {
    size :: non_neg_integer(),
    items :: array:array(none | {some, markdown_vec:item()})
}).

-define(markdown_vec_size(X), ((X)#markdown_vec.size)).

-endif.
