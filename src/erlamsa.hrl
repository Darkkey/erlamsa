% Copyright (c) 2011-2014 Aki Helin
% Copyright (c) 2014-2019 Alexander Bolshev aka dark_k3y
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
% SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%
%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Header file
%%% @end
%%%-------------------------------------------------------------------

-author("dark_k3y").

-include("dependencies.hrl").
-include("version.hrl").

-define(VERSION, "0.1.5").

-ifndef(GITVER).
-define(GITVER, "").
-endif.

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
-define(DEFAULT_UDPPROXY_CLIENTPORT, 0).
-define(TOO_MANY_FAILED_ATTEMPTS, 10).
-define(MAX_LOG_DATA, 10000000).
-define(SIZER_MAX_FIRST_BYTES, 512).
-define(PREAMBLE_MAX_BYTES, 32).
-define(START_MONITOR_ATTEMPTS, 5).
-define(EXEC_SLEEP, 5000).
-define(PROCKET_DIR, "/deps/procket/ebin").
-define(SERIAL_DIR, "/deps/erlserial/ebin").
-define(ERLEXEC_DIR, "/deps/erlexec/ebin").

%% log levels:
%% crit, error, warning, issue, info, decision, meta, debug
-define(CRITICAL, 0).
-define(ERROR, 1).
-define(WARNING, 2).
-define(FINDING, 3).
-define(INFO, 4).
-define(META, 5).
-define(DECISION, 6).
-define(DEBUG, 7).

-ifdef(USE_PROCKET).
-define(LOAD_PROCKET(R), code:add_pathz(R ++ ?PROCKET_DIR)).
-else.
-define(LOAD_PROCKET(R), true).
-endif.

-ifdef(USE_SERIAL).
-define(LOAD_SERIAL(R), code:add_pathz(R ++ ?SERIAL_DIR)).
-else.
-define(LOAD_SERIAL(R), true).
-endif.

-ifdef(USE_ERLEXEC).
-define(LOAD_ERLEXEC(R), code:add_pathz(R ++ ?ERLEXEC_DIR)).
-else.
-define(LOAD_ERLEXEC(R), true).
-endif.

%% Logging table structure

-record(log_entry, {date, type, pid, message, data}).

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

-type options() :: map().

%% /Common inter-module types.
