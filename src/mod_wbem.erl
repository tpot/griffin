-module(mod_wbem).

-export([do/1]).

-include_lib("inets/src/httpd.hrl").

%% Entry point for inets module

do(Info) ->
    case {Info#mod.method, Info#mod.request_uri} of
        {"POST", "/cimom"} ->
            mod_wbem_cimxml:do(Info);
        {"POST", "/wsman"} ->
            mod_wbem_wsman:do(Info);
        _ ->
            {proceed, [{status, {404, Info#mod.request_uri, ""}}]}
    end.
