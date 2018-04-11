%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("dark_k3y").

-include("dependencies.hrl").

-define(VERSION, "0.1").

-define(INITIAL_IP,24).              %% initial max 1/n for basic patterns)
-define(REMUTATE_PROBABILITY, {4, 5}).
-define(MAX_BURST_MUTATIONS, 16).
-define(MIN_BLOCK_SIZE, 256).
-define(AVG_BLOCK_SIZE, 2048). %% 2048 in radamsa
-define(AVG_BLOCK_SIZE_BITS, 16384). %% 2048 * 8
-define(MAX_BLOCK_SIZE, 2*?AVG_BLOCK_SIZE). %% 
-define(ABSMAXHALF_BINARY_BLOCK, 500000).
-define(ABSMAX_BINARY_BLOCK, 2*?ABSMAXHALF_BINARY_BLOCK).
-define(TCP_TIMEOUT, 5000).
-define(DEFAULT_UDPPROXY_CLIENTPORT, 44443).

-ifdef(USE_PROCKET).
-define(LOAD_PROCKET(R), code:add_pathz(R ++ "/deps/procket/ebin")).
-else.
-define(LOAD_PROCKET(R), true).
-endif.

%% Common inter-module types.

-type prioritized_list() :: [{number(), any()}].

-type lazy_list_of_bins() :: [binary() | fun()].

-type list_of_bins() :: [binary()].

-type input_inc() :: file:io_device() | stdin.

-type output_dest() :: file:io_device() | stdout | return.

-type meta() :: {atom(), any()}.

-type meta_list() :: [meta()].

-type chunk_type() :: text | byte | delimited.

-type chunk() :: {text | byte, list(byte())} | {delimited, byte(), list(byte()), byte()}.

-type chunk_list() :: [chunk()].

-type mutator() :: fun((any(), meta_list()) -> {mutator(), any(), meta_list()}).

-type pattern_fun() :: fun((any(), mutator(), meta_list()) -> list()).

-type pattern() :: {non_neg_integer(), fun(), atom(), string()}.

-type mutation_res() :: {mutation_fun(), list_of_bins(), meta_list(), integer()}.

-type mutation_fun() :: fun((list_of_bins(), meta_list()) -> mutation_res()).

-type mutation() :: {non_neg_integer(), non_neg_integer(), mutation_fun(), atom()}.

%% /Common inter-module types.
