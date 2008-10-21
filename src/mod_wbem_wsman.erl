-module(mod_wbem_wsman).

-export([do/1, do/2]).

-include_lib("inets/src/httpd.hrl").
-include_lib("xmerl/include/xmerl.hrl").

exec(RequestTT) ->
    error_logger:info_msg("Request = ~p~n", [RequestTT]),
    {'FOO', [], []}.

%% Generate a SOAP fault

%% TODO: Fix hardcoded xml namespace names

soap_fault(Code, Subcode, Reason, Detail) ->
    {'env:Envelope', 
     [{'xmlns:env', "http://www.w3.org/2003/05/soap-envelope"}],
     [{'env:Body', 
       [],
       [{'env:Fault', 
         [], 
         [soap_fault_code(Code, Subcode), 
          soap_fault_reason(Reason), 
          soap_fault_detail(Detail)]}]}]}.

soap_fault_code(Code, Subcode) ->
    {'env:Code', [],
     [{'env:Value', [], [#xmlText{value = Code}]},
      {'env:Subcode', [], 
       [{'env:Value', [], [#xmlText{value = Subcode}]}]}]}.

soap_fault_reason(Reason) ->
    {'env:Reason', 
     [],
     case Reason of
         undefined ->
             [];
         _ ->
             [#xmlText{value = Reason}]
     end}.

soap_fault_detail(Detail) ->
    {'env:Detail', 
     [],
     case Detail of
         undefined ->
             [];
         _ ->
             [Detail]
     end}.

soap_fault(Code, Subcode, Reason) ->
    soap_fault(Code, Subcode, Reason, undefined).

soap_fault(Code, Subcode) ->
    soap_fault(Code, Subcode, undefined, undefined).

%% Process a WS-Man request

wsman_request(Doc) ->
    case (catch wsman_parse:doc(Doc)) of
        %% XML did not conform to DTD
        {error, {_ErrorType, _Description}} ->
            Fault = soap_fault("env:Sender", "wsman:SchemaValidationError"),
            {500, [], lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
        %% WS-Man validator crashed - oops
        {'EXIT', Reason} ->
            error_logger:error_msg("wsman_parse: ~p~n", [Reason]),
            Fault = soap_fault("env:Receiver", "wsman:InternalError"),
            {500, [], lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
        %% Request succeeded
        RequestTT ->
            case (catch exec(RequestTT)) of
                %% Exception
                {'EXIT', _Reason} ->
                    Fault = soap_fault("env:Receiver", "wsman:InternalError"),
                    {500, [], 
                     lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
                %% Normal result
                ResponseTT -> 
                    {200, [], 
                     lists:flatten(
                       xmerl:export_simple([ResponseTT], xmerl_xml))}
            end
    end.

do(_Headers, Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        %% TODO: pass error description back in HTTP headers
        {'EXIT', _} -> 
            Fault = soap_fault("env:Sender", "wsman:SchemaValidationError"),
            {500, [], lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
        {Doc, _Trailer} ->
            wsman_request(Doc)
    end.

%% Entry point for processing WS-Man request

do(Info) ->
    {Status, Headers, Body} = 
        do(Info#mod.parsed_header, Info#mod.entity_body),
    Length = integer_to_list(httpd_util:flatlength(Body)),
    Head = [{code, Status},
            {content_type, "application/soap+xml"},
            {content_length, Length}] ++ Headers,
    {proceed, [{response, {response, Head, Body}}]}.
