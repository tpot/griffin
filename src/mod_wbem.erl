-module(mod_wbem).

-export([do/1, do/2]).

-include_lib("inets/src/httpd.hrl").
-include_lib("cimxml.hrl").
-include_lib("cim.hrl").

%% Join a list of strings together with a separator, aka Python's
%% string.join() function.

string_join(L, Sep) ->
    lists:concat(lists:reverse(string_join(L, Sep, []))).

string_join([H | []], _Sep, Acc) ->
    [H] ++ Acc;

string_join([H | T], Sep, Acc) ->
    string_join(T, Sep, [H ++ Sep] ++ Acc);

string_join([], _Sep, Acc) ->
    Acc.

%% Convert a CIM error code to a string

cim_error_string(Code) ->
    case Code of
        ?CIM_ERR_FAILED ->
            "CIM_ERR_FAILED";
        ?CIM_ERR_ACCESS_DENIED ->
            "CIM_ERR_ACCESS_DENIED";
        ?CIM_ERR_INVALID_NAMESPACE ->
            "CIM_ERR_INVALID_NAMESPACE";
        ?CIM_ERR_INVALID_PARAMETER ->
            "CIM_ERR_INVALID_PARAMETER";
        ?CIM_ERR_INVALID_CLASS ->
            "CIM_ERR_INVALID_CLASS";
        ?CIM_ERR_NOT_FOUND ->
            "CIM_ERR_NOT_FOUND";
        ?CIM_ERR_NOT_SUPPORTED ->
            "CIM_ERR_NOT_SUPPORTED";
        ?CIM_ERR_CLASS_HAS_CHILDREN ->
            "CIM_ERR_CLASS_HAS_CHILDREN";
        ?CIM_ERR_CLASS_HAS_INSTANCES ->
            "CIM_ERR_CLASS_HAS_INSTANCES";
        ?CIM_ERR_INVALID_SUPERCLASS ->
            "CIM_ERR_INVALID_SUPERCLASS";
        ?CIM_ERR_ALREADY_EXISTS ->
            "CIM_ERR_ALREADY_EXISTS";
        ?CIM_ERR_NO_SUCH_PROPERTY ->
            "CIM_ERR_NO_SUCH_PROPERTY";
        ?CIM_ERR_TYPE_MISMATCH ->
            "CIM_ERR_TYPE_MISMATCH";
        ?CIM_ERR_QUERY_LANGUAGE_NOT_SUPPORTED ->
            "CIM_ERR_QUERY_LANGUAGE_NOT_SUPPORTED";
        ?CIM_ERR_INVALID_QUERY ->
            "CIM_ERR_INVALID_QUERY";
        ?CIM_ERR_METHOD_NOT_AVAILABLE ->
            "CIM_ERR_METHOD_NOT_AVAILABLE";
        ?CIM_ERR_METHOD_NOT_FOUND ->
            "CIM_ERR_METHOD_NOT_FOUND";
        _ ->
            io_lib:format("Error code ~s", [Code])
    end.

%% Convert a tupletree to an erlang term

to_term({'NAMESPACE', Attrs, []}) ->
    proplists:get_value(?NAME, Attrs);

to_term({'LOCALNAMESPACEPATH', _, Namespaces}) ->
    string_join([to_term(Namespace) || Namespace <- Namespaces], "/");

to_term({'VALUE', _Attrs, [PCData]}) ->
    PCData;

to_term({'VALUE.ARRAY', _Attrs, Children}) ->
    lists:map(fun(X) -> to_term(X) end, Children);

to_term({'KEYVALUE', Attrs, [PCData]}) ->
    #keyvalue{valuetype = proplists:get_value(?VALUETYPE, Attrs),
              cimtype = proplists:get_value(?TYPE, Attrs),
              value = PCData};

to_term({'CLASSNAME', Attrs, []}) ->
    #instancename{classname = proplists:get_value(?NAME, Attrs)};

to_term({'INSTANCENAME', Attrs, Keybindings}) ->
    ClassName = proplists:get_value(?CLASSNAME, Attrs),
    #instancename{classname = ClassName, 
                  keybindings = [to_term(KB) || KB <- Keybindings]};

to_term({'KEYBINDING', Attrs, [Child]}) ->
    {proplists:get_value(?NAME, Attrs), to_term(Child)};

to_term({'IPARAMVALUE', Attrs, []}) ->
    {proplists:get_value(?NAME, Attrs), empty};

to_term({'IPARAMVALUE', Attrs, [Child]}) ->
    {proplists:get_value(?NAME, Attrs), to_term(Child)};

to_term({'LOCALCLASSPATH', _Attrs, [LocalNamespacePath, ClassName]}) ->
    Term = to_term(ClassName),
    Term#instancename{namespace = to_term(LocalNamespacePath)};

to_term({'LOCALINSTANCEPATH', _Attrs, [LocalNamespacePath, InstanceName]}) ->
    Term = to_term(InstanceName),
    Term#instancename{namespace = to_term(LocalNamespacePath)};

to_term({'SCOPE', Attrs, []}) ->
    lists:foldl(
      fun({MetaElement, Value}, Acc) ->
              case Value of
                  "true" ->
                      [MetaElement] ++ Acc;
                  _ -> Acc
              end 
      end, [], Attrs);

to_term({'PROPERTY', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #property{name = proplists:get_value(?NAME, Attrs),
              type = proplists:get_value(?TYPE, Attrs),
              classorigin = proplists:get_value(?CLASSORIGIN, Attrs),
              propagated = proplists:get_value(?PROPAGATED, Attrs),
              qualifiers = lists:map(fun(X) -> to_term(X) end, Qualifiers)};

to_term({'PROPERTY.REFERENCE', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #property_reference{name = proplists:get_value(?NAME, Attrs),
                        referenceclass = 
                        proplists:get_value(?REFERENCECLASS, Attrs),
                        classorigin = proplists:get_value(?CLASSORIGIN, Attrs),
                        propagated = proplists:get_value(?PROPAGATED, Attrs),
                        qualifiers = 
                        lists:map(fun(X) -> to_term(X) end, Qualifiers)};

to_term({'PARAMETER.REFERENCE', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #parameter_reference{name = proplists:get_value(?NAME, Attrs),
                         referenceclass = 
                         proplists:get_value(?REFERENCECLASS, Attrs),
                         qualifiers = 
                         lists:map(fun(X) -> to_term(X) end, Qualifiers)};

to_term({'PARAMETER.ARRAY', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #parameter_array{name = proplists:get_value(?NAME, Attrs),
                     type = proplists:get_value(?TYPE, Attrs),
                     arraysize = proplists:get_value(?ARRAYSIZE, Attrs),
                     qualifiers = 
                     lists:map(fun(X) -> to_term(X) end, Qualifiers)};

to_term({'PARAMETER', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #parameter_array{name = proplists:get_value(?NAME, Attrs),
                     type = proplists:get_value(?TYPE, Attrs),
                     qualifiers = 
                     lists:map(fun(X) -> to_term(X) end, Qualifiers)};  

to_term({'PARAMETER.REFARRAY', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    #parameter_refarray{name = proplists:get_value(?NAME, Attrs),
                        referenceclass = 
                        proplists:get_value(?REFERENCECLASS, Attrs),
                        arraysize = proplists:get_value(?ARRAYSIZE, Attrs),
                        qualifiers = 
                        lists:map(fun(X) -> to_term(X) end, Qualifiers)};  

to_term({'METHOD', Attrs, Children}) ->
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    Parameters = lists:filter(
                   fun({Tag, _, _}) ->
                           lists:member(Tag, ['PARAMETER', 'PARAMETER.ARRAY', 
                                              'PARAMETER.REFERENCE',
                                              'PARAMETER.REFARRAY']) end,
                   Children),
    #method{name = proplists:get_value(?NAME, Attrs),
            type = proplists:get_value(?TYPE, Attrs),
            classorigin = proplists:get_value(?CLASSORIGIN, Attrs),
            propagated = proplists:get_value(?PROPAGATED, Attrs),
            qualifiers = lists:map(fun(X) -> to_term(X) end, Qualifiers),
            parameters = lists:map(fun(X) ->to_term(X) end, Parameters)};

to_term({'QUALIFIER.DECLARATION', Attrs, Children}) ->
    Name = proplists:get_value(?NAME, Attrs),
    Type = proplists:get_value(?TYPE, Attrs),
    IsArray = proplists:get_value(?ISARRAY, Attrs),
    ArraySize = proplists:get_value(?ARRAYSIZE, Attrs),
    Overridable = proplists:get_value(?OVERRIDABLE, Attrs),
    ToSubclass = proplists:get_value(?TOSUBCLASS, Attrs),
    ToInstance = proplists:get_value(?TOINSTANCE, Attrs),
    Translatable = proplists:get_value(?TRANSLATABLE, Attrs),
    Term = #qualifier_declaration{
      name = Name, type = Type, isarray = IsArray, arraysize = ArraySize,
      overridable = Overridable, tosubclass = ToSubclass, 
      toinstance = ToInstance, translatable = Translatable},    
    lists:foldl(
      fun(Child, Acc) -> 
              case Child of
                  {'SCOPE', _, []} -> 
                      Acc#qualifier_declaration{scope = to_term(Child)};
                  {_, _, _Value} -> 		% VALUE or VALUE.ARRAY
                      Acc#qualifier_declaration{value = to_term(Child)}
              end 
      end, Term, Children);

to_term({'QUALIFIER', Attrs, Children}) ->
    Value = case Children of
                [] -> undefined;
                [V | []] -> to_term(V)
            end,
    #qualifier{name = proplists:get_value(?NAME, Attrs), 
               value = Value,
               propagated = proplists:get_value(?PROPAGATED, Attrs)};

to_term({'CLASS', Attrs, Children}) ->
    Name = proplists:get_value(?NAME, Attrs),
    SuperClass = proplists:get_value(?SUPERCLASS, Attrs),
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, 
                   Children),
    Properties = lists:filter(
                   fun({Tag, _, _}) ->
                           lists:member(Tag, ['PROPERTY', 'PROPERTY.ARRAY', 
                                              'PROPERTY.REFERENCE']) end,
                   Children),
    Methods = lists:filter(
                fun({Tag, _, _}) -> Tag == 'METHOD' end,
                Children),
    #class{name = Name, 
           superclass = SuperClass,
           qualifiers = lists:map(fun(X) -> to_term(X) end, Qualifiers),
           properties = lists:map(fun(X) -> to_term(X) end, Properties),
           methods = lists:map(fun(X) -> to_term(X) end, Methods)};

to_term({'INSTANCE', Attrs, Children}) ->
    ClassName = proplists:get_value(?CLASSNAME, Attrs),
    Qualifiers = lists:filter(
                   fun({Tag, _, _}) -> Tag == 'QUALIFIER' end, Children),
    Properties = lists:filter(
                   fun({Tag, _, _}) ->
                           lists:member(Tag, ['PROPERTY', 'PROPERTY.ARRAY', 
                                              'PROPERTY.REFERENCE']) end,
                   Children),
    #instance{classname = ClassName,
              qualifiers = lists:map(fun(X) -> to_term(X) end, Qualifiers),
              properties = lists:map(fun(X) -> to_term(X) end, Properties)};

to_term({'VALUE.NAMEDINSTANCE', _Attrs, [InstanceName, Instance]}) ->
    Result = to_term(Instance),
    Result#instance{path = to_term(InstanceName)};

to_term({Tag, _Attrs, _Children}) ->
    throw({error, io_lib:format("unconverted tag: ~s", [Tag])}).

%% Convert an erlang term to a tupletree

from_term({error, {Code}}) ->
    {'ERROR', [{'CODE', Code}, {'DESCRIPTION', cim_error_string(Code)}], []};

from_term({error, {Code, Description}}) ->
    {'ERROR', [{'CODE', Code}, {'DESCRIPTION', Description}], []};

from_term(Term) ->
    throw({error, io_lib:format("unconverted term: ~w", [Term])}).

%% Execute an intrinsic method

imethod(MethodName, _NameSpace, _Params) ->
    {error, {?CIM_ERR_INVALID_PARAMETER, 
             io_lib:format("Unknown method \"~s\"", [MethodName])}}.

%% Execute request

exec({'CIM', _Attrs, [Child]}) ->
    {'CIM', 
     [{?CIMVERSION, "2.0"}, {?DTDVERSION, "2.2"}],
     [exec(Child)]};

exec({'MESSAGE', Attrs, [Child]}) ->
    Id = proplists:get_value(?ID, Attrs),
    {'MESSAGE',
     [{?ID, Id}, {?PROTOCOLVERSION, "1.0"}],
     [exec(Child)]};

exec({'MULTIREQ', _Attrs, Children}) ->
    {'MULTIRSP', [], [exec(Child) || Child <- Children]};

exec({'SIMPLEREQ', _Attrs, [Child]}) ->
    {'SIMPLERSP', [], [exec(Child)]};

exec({'IMETHODCALL', Attrs, [LocalNamespacePath | Params]}) ->
    Name = proplists:get_value(?NAME, Attrs),
    Namespace = to_term(LocalNamespacePath),
    ParamList = [to_term(Param) || Param <- Params],
    {'IMETHODRESPONSE',
     [{?NAME, Name}],
     [from_term(imethod(Name, Namespace, ParamList))]};

exec(TT) ->
    throw({error, io_lib:format("Don't know how to parse tupletree ~p", [TT])}).

%% Parsing and validation of request

cimxml_request_not_well_formed() ->
    {400, [{"CIMError", "request-not-well-formed"}], ""}.

cimxml_request_not_valid() ->
    {400, [{"CIMError", "request-not-valid"}], ""}.

cimxml_request_good(ResponseTT) ->
    {200, [], lists:flatten(xmerl:export_simple([ResponseTT], xmerl_xml))}.

cimxml_request(Doc) ->
    case (catch cimxml_parse:doc(Doc)) of
        {'EXIT', _Reason} ->
            %% TODO: return error reason in HTTP header
            cimxml_request_not_well_formed();
        {error, {_ErrorType, _Description}} ->
            %% TODO: return error reason in HTTP header
            cimxml_request_not_valid();
        RequestTT ->
            case (catch exec(RequestTT)) of
                {'EXIT', _Reason} ->
                    %% TODO: return error reason in HTTP header
                    cimxml_request_not_valid();
                {error, _Description} ->
                    %% TODO: return error reason in HTTP header
                    cimxml_request_not_valid();
                ResponseTT -> 
                    cimxml_request_good(ResponseTT) 
            end
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
