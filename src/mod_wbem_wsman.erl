-module(mod_wbem_wsman).

-export([do/1, do/2]).

-include_lib("inets/src/httpd.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("wsman.hrl").

%% Execute request

exec(Body, Header) ->
    Props = lists:map(fun({Key, _, Value}) -> {Key, Value} end, Header),
    To = proplists:get_value('wsa:To', Props),
    Action = proplists:get_value('wsa:Action', Props),
    ResourceURI = proplists:get_value('wsa:ResourceURI', Props),
    MessageID = proplists:get_value('wsa:MessageID', Props),
    [{'wsa:Address', [], [ReplyTo]}] = 
        proplists:get_value('wsa:ReplyTo', Props),
    SelectorSet = 
        case proplists:get_value('wsman:SelectorSet', Props) of
            [] ->
                [];
            undefined ->
                [];
            Selectors ->
                lists:map(
                  fun({'wsman:Selector', [{'wsman:Name', Name}], [Value]}) ->
                          {Name, Value} 
                  end, 
                  Selectors)
        end,
    error_logger:info_msg("SelectorSet = ~p~n", [SelectorSet]),
    [{'s:Header', [], []},
     {'s:Body', [], []}].
     
exec({'s:Envelope', [], [{'s:Header', [], Header}, {'s:Body', [], Body}]}) ->
    {'s:Envelope',
     [{'xmlns:s', ?s}],     
     exec(Body, Header)};

exec(TT) ->
    error_logger:info_msg("TT = ~p~n", [TT]),
    throw({error, io_lib:format("Don't know how to parse tupletree ~p", [TT])}).

%% Generate a SOAP fault

%% TODO: Fix hardcoded xml namespace names

soap_fault(Code, Subcode, Reason, Detail) ->
    {'s:Envelope', 
     [{'xmlns:s', ?s}],
     [{'s:Body', 
       [],
       [{'s:Fault', 
         [], 
         [soap_fault_code(Code, Subcode), 
          soap_fault_reason(Reason), 
          soap_fault_detail(Detail)]}]}]}.

soap_fault_code(Code, Subcode) ->
    {'s:Code', [],
     [{'s:Value', [], [#xmlText{value = Code}]},
      {'s:Subcode', [], 
       [{'s:Value', [], [#xmlText{value = Subcode}]}]}]}.

soap_fault_reason(Reason) ->
    {'s:Reason', 
     [],
     case Reason of
         undefined ->
             [];
         _ ->
             [#xmlText{value = Reason}]
     end}.

soap_fault_detail(Detail) ->
    {'s:Detail', 
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
            Fault = soap_fault("s:Sender", "wsman:SchemaValidationError"),
            {500, [], lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
        %% WS-Man validator crashed - oops
        {'EXIT', Reason} ->
            error_logger:error_msg("wsman_parse: ~p~n", [Reason]),
            Fault = soap_fault("s:Receiver", "wsman:InternalError"),
            {500, [], lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
        %% Request succeeded
        RequestTT ->
            case (catch exec(RequestTT)) of
                %% Exit
                {'EXIT', Reason} ->
                    error_logger:error_msg("~p~n", [Reason]),
                    Fault = soap_fault("s:Receiver", "wsman:InternalError"),
                    {500, [], 
                     lists:flatten(xmerl:export_simple([Fault], xmerl_xml))};
                %% Exception
                {error, Reason} ->
                    error_logger:error_msg("~p~n", [Reason]),
                    Fault = soap_fault("s:Receiver", "wsman:InternalError"),
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
            Fault = soap_fault("s:Sender", "wsman:SchemaValidationError"),
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
