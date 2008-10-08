-module(mod_wbem).

-export([do/1]).

-include_lib("inets/src/httpd.hrl").

%% Parsing and validation of request

cimxml_request_not_well_formed() ->
    {400, [{"CIMError", "request-not-well-formed"}], ""}.

cimxml_request_not_valid() ->
    {400, [{"CIMError", "request-not-valid"}], ""}.

cimxml_request(Doc) ->
    case (catch cimxml_parse:parse_xml(Doc)) of
        {'EXIT', _Reason} ->
            %% TODO: return error reason in HTTP header
            cimxml_request_not_well_formed();
        {error, {_ErrorType, _Description}} ->
            %% TODO: return error reason in HTTP header
            cimxml_request_not_valid();
        RequestTT ->
            cixml_request_not_valid()
    end.

%% Return a tuple of {Status, Headers, Body} for the incoming request
%% headers and XML body.

do(_Headers, Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        {'EXIT', {fatal, _XmlError}} ->
            cimxml_request_not_well_formed();
        {'EXIT', _XmlError} ->
            cimxml_request_not_well_formed();
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
