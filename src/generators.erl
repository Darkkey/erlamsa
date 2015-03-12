%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(generators). 
-author("dark_k3y").

-include("erlamsa.hrl").

-compile([export_all]).

%% API
-export([]).

check_empty([<<>>]) -> [];
check_empty(X) -> X.

%% fill the rest of the stream with padding random bytes
finish(Len) ->
    N = erlamsa_rnd:rand(Len + 1), %% 1/(n+1) probability of possibly adding extra data
    if
        N =:= 0 ->
            Bits = erlamsa_rnd:rand_range(1,16),
            NLen = erlamsa_rnd:rand(1 bsl Bits),
            check_empty([list_to_binary(erlamsa_rnd:random_numbers(256, NLen))]);
        true -> []
    end.

%% get random size of block
rand_block_size() ->
    max(erlamsa_rnd:rand(?MAX_BLOCK_SIZE), ?MIN_BLOCK_SIZE).

%% work with input stream (from file/other stream)
port_stream(Port) ->
    fun () -> stream_port(Port, false, rand_block_size(), 0) end.

stream_port(Port, Last, Wanted, Len) ->
    case file:read(Port, Wanted) of
        {ok, Data} ->
            DataLen = byte_size(Data),
            if
                DataLen =:= Wanted ->
                    Block = erlamsa_utils:merge(Last, Data),
                    Next = rand_block_size(),
                    [Block | stream_port(Port, false, Next, Len + byte_size(Block))];
                true ->
                    stream_port(Port, erlamsa_utils:merge(Last, Data), Wanted - DataLen, Len)
            end;
        eof ->
            file:close(Port),
            case Last of
                false -> finish(Len);
                _Else ->
                    [Last | finish(Len + byte_size(Last))]
            end;
        {error, _} ->
            file:close(Port),
            Last
    end.

%% stdin input
stdin_generator(Online) ->
    %% TODO: investigate what is  (stdin-generator rs online?) in radamsa code and how force-ll correctly works...
    Ll = port_stream(stdin),
    LlM = if
              Online =:= true andalso is_function(Ll) -> owllisp:forcell(Ll);
              true -> Ll
          end,
    fun () -> {LlM, {generator, stdin}} end.

%% file input
file_streamer(Paths) ->
    N = length(Paths),
    fun () ->
        P = random:uniform(N), %% lists indexing from 1 in erlang
        Path = lists:nth(P, Paths),
        {Res, Port} = file:open(Path, [read, raw, binary]), %% TODO: FIXME: could we use raw?
        case Res of
            ok ->
                Ll = port_stream(Port),
                {Ll, [{generator, file}, {source, path}]};
            _Else ->
                Err = "Error opening file",  %% TODO: add printing filename, handling -r and other things...
                io:write(Err),
                throw(Err)
        end
    end.



%% Random input stream
random_stream() ->
    N = erlamsa_rnd:rand_range(32, ?MAX_BLOCK_SIZE),
    B = erlamsa_rnd:random_block(N),
    Ip = erlamsa_rnd:rand_range(1, 100),
    O = erlamsa_rnd:rand(Ip),
    case O of
        0 ->
            [B];
        _Else ->
            [B | random_stream()]
    end.

%% random generator
random_generator() ->
    {random_stream(), [{generator, random}]}.  

%% [{Pri, Gen}, ...] -> Gen(rs) -> output end
mux_generators([], Fail) -> Fail("No generators!");
mux_generators([[_,T]|[]], _) -> T; %% TODO: Check here!
mux_generators(Generators, _) ->
    {SortedGenerators, N} = erlamsa_utils:sort_by_priority(Generators),
    fun () ->
        F = erlamsa_utils:choose_pri(SortedGenerators, erlamsa_rnd:rand(N)),
        F()
    end.

%% create a lambda-generator function based on the array
make_generator_fun(Args, Fail, N) ->
    fun (false) -> Fail("Bad generator priority!");
        ({Name, Pri}) ->
            case Name of
                stdin when hd(Args) == "-" ->
                    {Pri, stdin_generator(N == 0)}; %% TODO: <<-- 1 in Radamsa
                stdin ->
                    false;
                file when Args =/= [] ->
                    {Pri, file_streamer(Args)};
                file ->
                    false;
                random ->
                    {Pri, fun random_generator/0};
                _Else ->
                    Fail("Unknown generator name."),
                    false
            end
    end.

%% get a list of {GenAtom, Pri} and output the list of {Pri, Gen}
generators_priorities_to_generator(Pris, Args, Fail, N) ->
    Gs = [ A || A <- [(make_generator_fun(Args, Fail, N))(V1) || V1 <- Pris], A =/= false],
    mux_generators(Gs, Fail).

default() -> [{random, 1}, {file, 1000}, {stdin, 100000}].