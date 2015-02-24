%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("dark_k3y"). 

-define(INITIAL_IP,24).              %% initial max 1/n for basic patterns)
-define(REMUTATE_PROBABILITY, {4, 5}).
-define(MIN_BLOCK_SIZE, 256).
-define(AVG_BLOCK_SIZE, 2048).
-define(AVG_BLOCK_SIZE_BITS, 16384).
-define(MAX_BLOCK_SIZE, 2*?AVG_BLOCK_SIZE).