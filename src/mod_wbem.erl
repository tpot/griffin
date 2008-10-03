-module(mod_wbem).

-export([do/1]).

-include_lib("inets/src/httpd.hrl").

do_request(_Headers, _Body) ->
    {400, [{"CIMError", "request-not-valid"}], ""}.    

do(Info) ->
    case Info#mod.method of
        "POST" ->
            {Status, Headers, Body} = do_request(Info#mod.parsed_header,
                                                 Info#mod.entity_body),
            Length = integer_to_list(httpd_util:flatlength(Body)),
            Head = [{code, Status},
                    {content_type, "application/xml"},
                    {content_length, Length}] ++ Headers,
            {proceed, [{response, {response, Head, Body}}]};
        _ ->
            {proceed, [{status, {404, Info#mod.request_uri, ""}}]}
    end.
