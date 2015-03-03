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
check_empty(X) -> shared:debug(X), X.

%% fill the rest of the stream with padding random bytes
finish(_Rs, Len) ->
    N = owllisp:rand(Len + 1), %% 1/(n+1) probability of possibly adding extra data
    shared:debug("finish -- getting more", {Len, N}),
    if
        N =:= 0 ->
            Bits = owllisp:rand_range(1,16),
            NLen = owllisp:rand(1 bsl Bits),
            check_empty([list_to_binary(owllisp:random_numbers(256, NLen))]);
        true -> []
    end.

%% get random size of block
rand_block_size() ->
    max(owllisp:rand(?MAX_BLOCK_SIZE), ?MIN_BLOCK_SIZE).

%% work with input stream (from file/other stream)
port_stream(Rs, Port) ->
    {Rs, fun () -> stream_port(Rs, Port, false, rand_block_size(), 0) end}.

stream_port(Rs, Port, Last, Wanted, Len) ->
    case file:read(Port, Wanted) of
        {ok, Data} ->
            DataLen = byte_size(Data),
            if
                DataLen =:= Wanted ->
                    Block = shared:merge(Last, Data),
                    Next = rand_block_size(),
                    [Block | stream_port(Rs, Port, false, Next, Len + byte_size(Block))];
                true ->
                    stream_port(Rs, Port, shared:merge(Last, Data), Wanted - DataLen, Len)
            end;
        eof ->
            file:close(Port),
            case Last of
                false -> finish(Rs, Len);
                _Else ->
                    Res = [Last | finish(Rs, Len + byte_size(Last))],
                    shared:debug("dbg: Read&Rand", Res),
                    Res
            end;
        {error, _} ->
            file:close(Port),
            Last
    end.

%% stdin input
stdin_generator(Rs, Online) ->
    shared:debug_str("Stdin generator stream"),
    %% TODO: investigate what is  (stdin-generator rs online?) in radamsa code and how force-ll correctly works...
    Ll = port_stream(Rs, stdin),
    LlM = if
              Online =:= true andalso is_function(Ll) -> owllisp:forcell(Ll);
              true -> Ll
          end,
    fun (_Rs) -> {Rs, LlM, {generator, stdin}} end.

%% file input
file_streamer(Paths) ->
    N = length(Paths),
    fun (Rs) ->
        shared:debug_str("File generator stream"),
        P = random:uniform(N), %% lists indexing from 1 in erlang
        Path = lists:nth(P, Paths),
        {Res, Port} = file:open(Path, [read, raw, binary]), %% TODO: FIXME: could we use raw?
        case Res of
            ok ->
                {Rs, Ll} = port_stream(Rs, Port),
                {Rs, Ll, [{generator, file}, {source, path}]};
            _Else ->
                Err = "Error opening file",  %% TODO: add printing filename, handling -r and other things...
                io:write(Err),
                throw(Err)
        end
    end.



%% Random input stream
random_stream() ->
    shared:debug_str("Random generator stream"),
    N = owllisp:rand_range(32, ?MAX_BLOCK_SIZE),
    B = owllisp:random_block(N),
    Ip = owllisp:rand_range(1, 100),
    O = owllisp:rand(Ip),
    case O of
        0 ->
            [B];
        _Else ->
            [B | random_stream()]
    end.

%% random generator
random_generator(Rs) ->
    {Rs = 1, random_stream(), [{generator, random}]}.  %% Rs = 1

%% [{Pri, Gen}, ...] -> Gen(rs) -> output end
mux_generators([], Fail) -> Fail("No generators!");
mux_generators([[_,T]|[]], _) -> T; %% TODO: Check here!
mux_generators(Generators, _) ->
    {SortedGenerators, N} = shared:sort_by_priority(Generators),
    fun (Rs) ->
        F = shared:choose_pri(SortedGenerators, owllisp:rand(N)),
        F(Rs)
    end.

%% create a lambda-generator function based on the array
make_generator_fun(Rs, Args, Fail, N) ->
    fun (false) -> Fail("Bad generator priority!");
        ({Name, Pri}) ->
            case Name of
                stdin when hd(Args) == "-" ->
                    {Pri, stdin_generator(Rs, N == 0)}; %% TODO: <<-- 1 in Radamsa
                stdin ->
                    false;
                file when Args =/= [] ->
                    {Pri, file_streamer(Args)};
                file ->
                    false;
                random ->
                    {Pri, fun random_generator/1};
                _Else ->
                    shared:debug(Name),
                    Fail("Unknown generator name."),
                    false
            end
    end.

%% get a list of {GenAtom, Pri} and output the list of {Pri, Gen}
generators_priorities_to_generator(Rs, Pris, Args, Fail, N) ->
    Gs = [ A || A <- [(make_generator_fun(Rs, Args, Fail, N))(V1) || V1 <- Pris], A =/= false],
    shared:debug("Generators before MUX:", Gs),
    mux_generators(Gs, Fail).

default() -> [{random, 1}, {file, 1000}, {stdin, 100000}].