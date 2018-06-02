% Copyright (c) 2014-2018 Alexander Bolshev aka dark_k3y
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
%%% Web Service process config module.
%%% @end
%%%-------------------------------------------------------------------

-module(erlamsa_httpsvc).
-export([get_supervisor_opts/1]).

-include("erlamsa.hrl").

-spec get_supervisor_opts(options()) -> supervisor:child_spec().
get_supervisor_opts(Opts) ->
    #{id => inets,
    start => {inets, start,
    [
        httpd,
        [{port, maps:get(svcport, Opts, 17771)},
        {server_name, maps:get(svchost, Opts, "localhost")},
        {document_root, "."},
        {modules, [mod_esi]},
        {server_root, "."},
        {erl_script_alias, {"/erlamsa", [erlamsa_esi]}}],
        stand_alone
    ]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [inets]}.