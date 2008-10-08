-module(mod_wbem).

-export([do/1, do/2]).

-include_lib("inets/src/httpd.hrl").

%% Parsing and validation of request

cimxml_request_not_well_formed(Reason) ->
    {400, 
     [{"CIMError", "request-not-well-formed"}, 
      {"GriffinErrorDetail", lists:flatten(io_lib:format("~w", [Reason]))}], 
     ""}.

cimxml_request_not_valid(Reason) ->
    {400, 
     [{"CIMError", "request-not-valid"}, 
      {"GriffinErrorDetail", lists:flatten(io_lib:format("~w", [Reason]))}], 
     ""}.

cimxml_request(Doc) ->
    case (catch cimxml_parse:doc(Doc)) of
        {'EXIT', Reason} ->
            cimxml_request_not_valid(Reason);
        {error, {Type, Reason}} ->
            cimxml_request_not_valid({Type, Reason});
        RequestTT ->
            cimxml_request_not_valid("unimplemented")
    end.

%% Return a tuple of {Status, Headers, Body} for the incoming request
%% headers and XML body.

do(_Headers, Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', {fatal, XmlError}} ->
            cimxml_request_not_well_formed(XmlError);
        {Doc, _Trailer} ->
            cimxml_request(Doc)
    end.

%% Entry point for inets module

do(Info) ->
    case Info#mod.method of
        "POST" ->
            {Status, Headers, Body} = 
                do(Info#mod.parsed_header, Info#mod.entity_body),
            Length = integer_to_list(httpd_util:flatlength(Body)),
            Head = [{code, Status},
                    {content_type, "application/xml"},
                    {content_length, Length}] ++ Headers,
            {proceed, [{response, {response, Head, Body}}]};
        _ ->
            {proceed, [{status, {404, Info#mod.request_uri, ""}}]}
    end.
