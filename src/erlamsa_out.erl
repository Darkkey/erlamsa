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
-export([output/2, stdout_stream/1, file_writer/1, string_outputs/2]).

%% convert list-of-bvecs to one big binary
-spec flush(list_of_bins()) -> binary().
flush([[]]) -> <<>>; %% dump output
flush([H|T]) -> R = flush(T), <<R/binary, H/binary>>;
flush(H) -> <<H/binary>>.

-spec output(lazy_list_of_bins(), output_dest()) -> {fun(), meta(), integer()}.
output(Ll, Fd) ->
    {NLl, Data, N} = blocks_port(Ll, Fd),
    %% (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written? <-- do we really need to check this?
    {Muta, Meta} = erlamsa_utils:last(NLl),
    close_port(Fd),
    {Muta, Meta, N, flush(Data)}.

-spec stdout_stream(meta()) -> {fun(), output_dest(), meta_list()}.
stdout_stream(Meta) -> {fun stdout_stream/1, stdout, [{output, stdout} | Meta]}.

-spec return_stream(meta()) -> {fun(), output_dest(), meta_list()}.
return_stream(Meta) -> {fun return_stream/1, return, [{output, return} | Meta]}.

-spec file_writer(string()) -> fun().
file_writer(_Str) -> throw("Not yet supported!").

%% TODO: possibly add tcp, udp, files, multiple files output.
-spec string_outputs(string(), non_neg_integer()) -> fun().
string_outputs(Str, _N) ->
    case Str of
        "-" -> fun stdout_stream/1;
        return -> fun return_stream/1;
        _Else -> file_writer(Str)
    end.

%% write blocks to port
-spec blocks_port(lazy_list_of_bins(), output_dest()) -> {lazy_list_of_bins(), list_of_bins(), non_neg_integer()}.
blocks_port(Ll, Fd) -> blocks_port(Ll, Fd, [], 0).

%% TODO: UGLY, need rewrite and handle errors
-spec blocks_port(lazy_list_of_bins(), output_dest(), list_of_bins(), non_neg_integer()) -> {lazy_list_of_bins(), list_of_bins(), non_neg_integer()}.
blocks_port([], _, Data, N) -> {[], Data, N};
blocks_port([Ll], Fd, Data, N) when is_function(Ll) -> blocks_port(Ll(), Fd, Data, N);
blocks_port(Ll = [H|T], Fd, Data, N) when is_binary(H) ->
    {Res, NewData} = write_really(H, Fd),
    case Res of
        ok when is_binary(H) -> blocks_port(T, Fd, [NewData|Data], N + byte_size(H));
        _Else -> {Ll, N}
    end;
blocks_port(Ll, _, Data, N) -> {Ll, Data, N}.


%% closes the port
-spec close_port(output_dest()) -> ok | {'error', file:posix() | badarg | terminated}.
close_port(stdout) -> ok;
close_port(return) -> ok;
close_port(Fd) -> file:close(Fd).

%% write to Fd or stdout
%% TODO: UGLY, need rewrite and handle errors
%%write_really(Data, stdout) -> io:put_chars("\n\n********************* write_really stdout: "), io:write({byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok;
%%write_really(Data, Fd) -> io:put_chars("\n\n********************* write_really file: "), file:write(Fd, {byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok.
-spec write_really(binary(), output_dest()) -> ok.
write_really(Data, return) -> {ok, Data};
write_really(Data, stdout) -> {file:write(standard_io, Data), []};
write_really(Data, Fd) -> {file:write(Fd, Data), []}.
