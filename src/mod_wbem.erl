-module(mod_wbem).

-export([do/1]).

-include_lib("inets/src/httpd.hrl").

do(Info) ->
    case Info#mod.method of
        _ ->
            {proceed, [{status, {404, Info#mod.request_uri, ""}}]}
    end.
