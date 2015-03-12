%%%-------------------------------------------------------------------
%%% @author dark_k3y
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(output).
-author("dark_k3y").

-compile([export_all]).

%% API
-export([]).


output(Ll, Fd) ->
    io:format("~n"),
    shared:debug("Got to output", Ll),    
    {NLl, N} = blocks_port(Ll, Fd),
    shared:debug("Left after output:", NLl),
    %% (ok? (and (pair? ll) (tuple? (car ll)))) ;; all written? <-- do we really need to check this?
    {Rs, Muta, Meta} = owllisp:last(NLl),
    shared:debug("Meta: ", Meta),
    close_port(Fd),
    {Rs, Muta, Meta, N}.

stdout_stream(Meta) -> {fun stdout_stream/1, stdout, [{output, stdout} | Meta]}.

file_writer(_Str) -> throw("Not yet supported!").

%% TODO: possibly add tcp, udp, files, multiple files output.
string_outputs(Str, _N) ->
    case Str of
        "-" -> fun stdout_stream/1;
        _Else -> file_writer(Str)
    end.

%% write blocks to port
blocks_port(Ll, Fd) -> blocks_port(Ll, Fd, 0).

%% TODO: UGLY, need rewrite and handle errors
blocks_port([], _, N) -> {[], N};
blocks_port([Ll], Fd, N) when is_function(Ll) -> blocks_port(Ll(), Fd, N);
blocks_port(Ll = [H|T], Fd, N) when is_binary(H) ->
    Res = write_really(H, Fd),
    case Res of
        ok when is_binary(H) -> blocks_port(T, Fd, N + byte_size(H));
        _Else -> {Ll, N}
    end;
blocks_port(Ll, _, N) -> {Ll, N}.


%% closes the port
close_port(stdout) -> ok;
close_port(Fd) -> file:close(Fd).

%% write to Fd or stdout
%% TODO: UGLY, need rewrite and handle errors
%%write_really(Data, stdout) -> io:put_chars("\n\n********************* write_really stdout: "), io:write({byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok;
%%write_really(Data, Fd) -> io:put_chars("\n\n********************* write_really file: "), file:write(Fd, {byte_size(Data)}), io:put_chars(Data), io:put_chars("\n\n"), ok.
write_really(Data, stdout) -> io:put_chars(Data), ok;
write_really(Data, Fd) -> file:write(Fd,Data), ok.
