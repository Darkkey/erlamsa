%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @doc
%%% Output to real-world
%%% @end
%%%-------------------------------------------------------------------
-module(erlamsa_out).
-author("dark_k3y").

-include("erlamsa.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% API
-export([output/2, string_outputs/1, flush/1]).

%% convert list-of-bvecs to one big binary
-spec flush(list(nil | binary())) -> any(). 
flush([]) -> <<>>;
flush([H|T]) when is_binary(H) -> R = flush(T), <<H/binary, R/binary>>.

-spec output(lazy_list_of_bins(), output_dest()) -> {fun(), meta(), integer(), binary()}.
output(Ll, Fd) ->
    {NLl, NewFd, Data, N} = blocks_port(Ll, Fd),
    %% (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written? <-- do we really need to check this?
    {Muta, Meta} = erlamsa_utils:last(NLl),
    close_port(NewFd),
    {Muta, Meta, N, flush(Data)}.

-spec stdout_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
stdout_stream(_, Meta) -> {fun stdout_stream/2, stdout, [{output, stdout} | Meta]}.

-spec return_stream(non_neg_integer(), meta()) -> {fun(), output_dest(), meta_list()}.
return_stream(_, Meta) -> {fun return_stream/2, return, [{output, return} | Meta]}.

-spec build_name(list(binary()), integer(), list(list())) -> string().
build_name([H], _N, Acc) -> 
    lists:flatten(lists:reverse([binary_to_list(H)|Acc]));
build_name([H|T], N, Acc) ->     
    build_name(T, N, [[binary_to_list(H),N]|Acc]).

-spec file_writer(string()) -> fun().
file_writer(Str) ->  
    {ok, SRe} = re:compile("%n"),
    Tokens = re:split(Str, SRe), 
    fun F(N, Meta) ->
        Filename = build_name(Tokens, integer_to_list(N), []),
        {Res, Fd} = file:open(Filename, [write, raw, binary]),
        case Res of 
            ok -> {F, Fd, [{output, return} | Meta]};
            _Else ->
                Err = lists:flatten(io_lib:format("Error opening file '~s'", [Filename])),  %% TODO: add printing filename, handling -r and other things...
                erlamsa_utils:error(Err)
        end
    end.

%% TODO: possibly add tcp, udp, files, multiple files output.
-spec string_outputs(string()) -> fun().
string_outputs(Str) ->
    case Str of
        "-" -> fun stdout_stream/2;
        return -> fun return_stream/2;
        _Else -> file_writer(Str)
    end.

%% write blocks to port
-spec blocks_port(lazy_list_of_bins(), output_dest()) -> {lazy_list_of_bins(), list(), non_neg_integer()}.
blocks_port(Ll, Fd) -> blocks_port(Ll, Fd, [], 0).

%% TODO: UGLY, need rewrite and handle errors
-spec blocks_port(lazy_list_of_bins(), output_dest(), list(), non_neg_integer()) -> {lazy_list_of_bins(), list(), non_neg_integer()}.
blocks_port([], Fd, Data, N) -> {[], Fd, Data, N};
blocks_port([Ll], Fd, Data, N) when is_function(Ll) -> blocks_port(Ll(), Fd, Data, N);
blocks_port(Ll = [H|T], Fd, Data, N) when is_binary(H) ->
    {Res, NewData} = write_really(H, Fd),
    case Res of
        ok when is_binary(H) -> blocks_port(T, Fd, [NewData|Data], N + byte_size(H));
        _Else -> {Ll, Fd, Data, N}
    end;
blocks_port(Ll, Fd, Data, N) -> {Ll, Fd, Data, N}.


%% closes the port
-spec close_port(output_dest()) -> ok | {'error', file:posix() | badarg | terminated}.
close_port(stdout) -> ok;
close_port(return) -> ok;
close_port(Fd) -> file:close(Fd).

%% write to Fd or stdout
%% TODO: UGLY, need rewrite and handle errors, also rewrite spec
%%write_really(Data, stdout) -> io:put_chars("\n\n********************* write_really stdout: "), io:write({byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok;
%%write_really(Data, Fd) -> io:put_chars("\n\n********************* write_really file: "), file:write(Fd, {byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok.
-spec write_really(binary(), output_dest()) -> {any(), binary()}.
write_really(Data, return) -> {ok, Data};
write_really(Data, stdout) -> {file:write(standard_io, Data), <<>>};
write_really(Data, Fd) -> {file:write(Fd, Data), <<>>}.
