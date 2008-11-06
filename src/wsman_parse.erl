-module(wsman_parse).

-export([doc/1, string/1, any_string/1]).
-export(['Envelope'/3, 'Header'/3, 'Action'/3, 'To'/3, 'ResourceURI'/3,
         'MessageID'/3, 'ReplyTo'/3, 'Address'/3, 'SelectorSet'/3, 'Body'/3]).

-include_lib("xmerl/include/xmerl.hrl").

-define(s, 'http://www.w3.org/2003/05/soap-envelope').
-define(wsman, 'http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd').
-define(wsa, 'http://schemas.xmlsoap.org/ws/2004/08/addressing').

%% SOAP envelope

'Envelope'(?s, _Elt, 
           [{{?s, 'Header'}, [Header]}, {{?s, 'Body'}, [Body]}]) ->
    {'s:Envelope',
     [],
     [parse(Header), parse(Body)]}.

%% SOAP header

'Header'(?s, _Elt, Children) ->
    Required = [{?wsa, 'To'}, {?wsa, 'Action'}, {?wsa, 'MessageID'},
                {?wsman, 'ResourceURI'}, {?wsman, 'SelectorSet'}],
    Optional = [{?wsa, 'ReplyTo'}, {?wsa, 'FaultTo'}],
    {'s:Header',
     [],
     parse_children(Children, Required, Optional)}.

%% Addressing elements

'To'(?wsa, _Elt, [{'#xmlText', Value}]) ->
    {'wsa:To', [], [Value]}.

'Action'(?wsa, _Elt, [{'#xmlText', Value}]) ->
    {'wsa:Action', [], [Value]}.

'MessageID'(?wsa, _Elt, [{'#xmlText', Value}]) ->
    {'wsa:MessageID', [], [Value]}.

'ReplyTo'(?wsa, _Elt, [{{?wsa, 'Address'}, [Address]}]) ->
    {'wsa:ReplyTo', 
     [], 
     [parse(Address)]}.

'Address'(?wsa, _Elt, [{'#xmlText', Value}]) ->
    {'wsa:Address', [], [Value]}.

'FaultTo'(?wsa, _Elt, [{'#xmlText', Value}]) ->
    {'wsa:FaultTo', [], [Value]}.

'ResourceURI'(?wsman, _Elt, [{'#xmlText', Value}]) ->
    {'wsman:ResourceURI', [], [Value]}.

'SelectorSet'(?wsman, _Elt, [{{?wsman, Selector}, Selectors}]) ->
    {'wsman:SelectorSet', 
     [], 
     [parse(Child) || {_Tag, Child} <- Selectors]}.

'Selector'(?wsman, Elt, [{'xmlText', Value}]) ->
    Name = xml:get_attr('Name', Elt),
    {'wsman:Selector',
     [{'Name', Name}],
     [Value]}.

%% SOAP body

'Body'(?s, _Elt, _Children) ->
    {'Body',
     [],
     []}.

%% Convert list of xmlElement records to tuples

elts_to_tuple(EltList) ->
    elts_to_tuple(EltList, []).

elts_to_tuple([H | T], Result) when is_record(H, xmlElement) ->
    {Elts, Rest} = lists:splitwith(
                     fun(X) -> X#xmlElement.name == H#xmlElement.name end, T),
    elts_to_tuple(Rest, [{xmlns(H), [H] ++ Elts}] ++ Result);

elts_to_tuple([H | T], Result) when is_record(H, xmlText) ->
    {Elts, Rest} = lists:splitwith(
                     fun(X) -> is_record(X, xmlText) end, T),
    Text = lists:flatten(lists:map(fun(X) -> X#xmlText.value end, [H] ++ Elts)),
    elts_to_tuple(Rest, [{'#xmlText', Text}] ++ Result);

elts_to_tuple([H | _T], _Result) ->
    throw({error, {unknown_content_type,
                   io_lib:format("Unknown content: ~p~n", [H])}});

elts_to_tuple([], Result) ->
    lists:reverse(Result).

%% Return XML namespace and tag name of an xmlElement record
%%
%% @spec xmlns(xmlElement()) -> {atom(), atom()}

xmlns(Elt) ->
    #xmlElement{name = Name, nsinfo = NSInfo, namespace = Namespace} = Elt,
    case NSInfo of
        [] ->
            DefaultNS = case Namespace#xmlNamespace.default of
                            [] ->
                                null_namespace;
                            NS ->
                                NS
                        end,
            {DefaultNS, Name};
        {Prefix, Local} ->
            {proplists:get_value(Prefix, Namespace#xmlNamespace.nodes), 
             list_to_atom(Local)}
    end.

%% Execute a callback based on the XML namespace and tag name of an element
%%
%% @spec parse(xmlElement()) -> term()

parse(Elt) ->
    {Namespace, Tag} = xmlns(Elt),
    Params = [Namespace, Elt] ++ [elts_to_tuple(Elt#xmlElement.content)],
    apply(?MODULE, Tag, Params).

%% Parse a list of single-valued children from a list of required and
%% optional elements.
%%
%% @spec parse_children(list(xmlElement()), list(elementWithNS()),
%%                      list(elementWithNS()))
%%
%% @type elementWithNS() -> {Namespace, Tag}

parse_children(Children, Required, Optional) ->
    lists:map(fun({NameSpace, [Elt]}) ->
                      parse(Elt)
              end, 
              Children).

%% Parse an xmerl document

doc(XML) ->
    %% TODO: check we are at top level
    parse(XML).

%% Parse a string - for unit testing only.

string(Str) ->
    {Elt, _Rest} = xmerl_scan:string(Str),
    %% TODO: check we are at top level
    parse(Elt).

%% Parse any element in the CIM-XML schema.  For testing only.

any_string(Str) ->
    {Elt, _Rest} = xmerl_scan:string(Str),
    parse(Elt).
