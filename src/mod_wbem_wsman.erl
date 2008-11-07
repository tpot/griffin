-module(mod_wbem_wsman).

-export([do/1, do/2]).

-include_lib("inets/src/httpd.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("wsman.hrl").

%% TODO: Fix hardcoded xml namespace names

soap_fault(Code, Subcode, Reason, Detail) ->
    {'s:Envelope', 
     [{'xmlns:s', ?s}],
     [soap_fault_body(Code, Subcode, Reason, Detail)]}.

soap_fault(Code, Subcode, Reason) ->
    soap_fault(Code, Subcode, Reason, undefined).

soap_fault(Code, Subcode) ->
    soap_fault(Code, Subcode, undefined, undefined).

soap_fault_body(Code, Subcode, Reason, Detail) ->
    {'s:Body', 
     [],
     [{'s:Fault', 
       [], 
       [soap_fault_code(Code, Subcode), 
        soap_fault_reason(Reason), 
        soap_fault_detail(Detail)]}]}.

soap_fault_code(Code, Subcode) ->
    {'s:Code', [],
     [{'s:Value', [], [Code]},
      {'s:Subcode', [], 
       [{'s:Value', [], [Subcode]}]}]}.

soap_fault_reason(Reason) ->
    {'s:Reason', 
     [],
     case Reason of
         undefined ->
             [];
         _ ->
             [Reason]
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

%% http://github.com/travis/erlang-uuid/tree/master/uuid.erl
%% TODO: check license

uuid() ->
    ["urn:uuid:"] ++ uuid:to_string(uuid:v4()).

%% Execute request

exec(?ActionGet, To, ResourceURI, MessageID, ReplyTo, SelectorSet) ->
    {'s:Envelope',
     [{'xmlns:s', ?s}, {'xmlns:wsa', ?wsa}],
     [{'s:Header', 
       [],
       [{'wsa:To', [], [ReplyTo]},
        {'wsa:RelatesTo', [], [MessageID]},
        {'wsa:MessageID', [], [uuid()]},
        {'wsa:Action', [], [atom_to_list(?ActionFault)]}]}]};

exec(Action, To, ResourceURI, MessageID, ReplyTo, SelectorSet) ->
    {'s:Envelope',
     [{'xmlns:s', ?s}, {'xmlns:wsa', ?wsa}],
     [{'s:Header', 
       [],
       [{'wsa:To', [], [ReplyTo]},
        {'wsa:RelatesTo', [], [MessageID]},
        {'wsa:MessageID', [], [uuid()]},
        {'wsa:Action', [], [atom_to_list(?ActionFault)]}]},
      soap_fault_body('s:Sender', 'wsa:ActionNotSupported', 
                      "Action not supported", [atom_to_list(Action)])]}.

exec(_Body, Header) ->
    Props = lists:map(fun({Key, _, Value}) -> {Key, Value} end, Header),
    To = proplists:get_value('wsa:To', Props),
    [Action] = proplists:get_value('wsa:Action', Props),
    ResourceURI = proplists:get_value('wsa:ResourceURI', Props),
    [MessageID] = proplists:get_value('wsa:MessageID', Props),
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
    exec(list_to_atom(Action), To, ResourceURI, MessageID, ReplyTo, SelectorSet).
     
exec({'s:Envelope', [], [{'s:Header', [], Header}, {'s:Body', [], Body}]}) ->
     exec(Body, Header);

exec(TT) ->
    error_logger:info_msg("TT = ~p~n", [TT]),
    throw({error, io_lib:format("Don't know how to parse tupletree ~p", [TT])}).

%% Generate a SOAP fault

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
