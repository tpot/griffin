-module(mod_wbem_cimxml).

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

%% Fetch a boolean value from a property list

get_bool_value(Key, List) ->
    case proplists:get_value(Key, List) of
        undefined ->
            undefined;
        Result ->
            list_to_atom(string:to_lower(Result))
    end.

get_bool_value(Key, List, Default) ->
    case get_bool_value(Key, List) of
        undefined ->
            Default;
        Result ->
            Result
    end.

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

from_term(Term) when is_record(Term, classname) ->
    {'CLASSNAME', [{?NAME, Term#classname.name}], []};

from_term(Term) when is_record(Term, qualifier) ->
    Attrs = [{?NAME, Term#qualifier.name},
             {?TYPE, Term#qualifier.type}] ++
        case Term#qualifier.propagated of
            undefined -> [];
            _ -> [{?PROPAGATED, Term#qualifier.propagated}]
        end,
    %% TODO: flavours
    %% TODO: (VALUE | VALUE.ARRAY)? 
    {'QUALIFIER', Attrs, []};

from_term(Term) when is_record(Term, property) ->
    Attrs = [{?NAME, Term#property.name},
             {?TYPE, Term#property.type}] ++
        case Term#property.classorigin of
            undefined -> [];
            _ -> [{?CLASSORIGIN, Term#property.classorigin}]
        end ++
        case Term#property.propagated of
            undefined -> [];
            _ -> [{?PROPAGATED, Term#property.propagated}]
        end,
    Child = case Term#property.value of
                undefined -> [];
                _ -> to_term(Term#property.value)
            end,
    {'PROPERTY', Attrs, Child};

from_term(Term) when is_record(Term, property_reference) ->
    Attrs = [{?NAME, Term#property_reference.name}] ++
        case Term#property_reference.referenceclass of
            undefined -> [];
            _ -> [{?REFERENCECLASS, Term#property_reference.referenceclass}]
        end ++
        case Term#property_reference.classorigin of
            undefined -> [];
            _ -> [{?CLASSORIGIN, Term#property_reference.classorigin}]
        end ++
        case Term#property_reference.propagated of
            undefined -> [];
            _ -> [{?PROPAGATED, Term#property_reference.propagated}]
        end,
    Children = [from_term(E) || E <- Term#property_reference.qualifiers] ++
        case Term#property_reference.value of
            undefined -> [];
            Value -> [from_term(Value)]
        end,
    {'PROPERTY.REFERENCE', Attrs, Children};

from_term(Term) when is_record(Term, method) ->
    Attrs = [{?NAME, Term#method.name}] ++
        case Term#method.type of
            undefined -> [];
            _ -> [{?TYPE, Term#method.type}]
        end ++
        case Term#method.classorigin of
            undefined -> [];
            _ -> [{?CLASSORIGIN, Term#method.classorigin}]
        end ++
        case Term#method.propagated of
            undefined -> [];
            _ -> [{?PROPAGATED, Term#method.propagated}]
        end,
    {'METHOD', Attrs, 
     [from_term(E) || E <- Term#method.qualifiers ++ Term#method.parameters]};

from_term(Term) when is_record(Term, parameter_reference) ->
    Attrs = [{?NAME, Term#parameter_reference.name}] ++
        case Term#parameter_reference.referenceclass of
            undefined -> [];
            _ -> [{?REFERENCECLASS, Term#parameter_reference.referenceclass}]
        end,
    {'PARAMETER.REFERENCE', Attrs, []};

from_term(Term) when is_record(Term, parameter_array) ->
    Attrs = [{?NAME, Term#parameter_array.name},
             {?TYPE, Term#parameter_array.type}] ++
        case Term#parameter_array.arraysize of
            undefined -> [];
            _ -> [{?ARRAYSIZE, Term#parameter_array.arraysize}]
        end,
    {'PARAMETER.ARRAY', Attrs, 
     [from_term(E) || E <- Term#parameter_array.qualifiers]};    

from_term(Term) when is_record(Term, parameter_refarray) ->
    Attrs = [{?NAME, Term#parameter_refarray.name}] ++
        case Term#parameter_refarray.referenceclass of
            undefined -> [];
            _ -> [{?REFERENCECLASS, Term#parameter_refarray.referenceclass}]
        end,
        case Term#parameter_refarray.arraysize of
            undefined -> [];
            _ -> [{?ARRAYSIZE, Term#parameter_refarray.arraysize}]
        end,
    {'PARAMETER.REFARRAY', Attrs, 
     [from_term(E) || E <- Term#parameter_refarray.qualifiers]};    

from_term(Term) when is_record(Term, class) ->
    Attrs = [{?NAME, Term#class.name}] ++
        case Term#class.superclass of
            undefined -> [];
            _ -> [{?SUPERCLASS, Term#class.superclass}]
        end,
    {'CLASS', 
     Attrs, 
     [from_term(E) || 
         E <- Term#class.qualifiers ++ Term#class.properties ++ 
             Term#class.methods]};

from_term(Term) when is_record(Term, qualifier_declaration) ->
    Attrs = [{?NAME, Term#qualifier_declaration.name},
             {?TYPE, Term#qualifier_declaration.type},
             {?ISARRAY, Term#qualifier_declaration.isarray}] ++
        case Term#qualifier_declaration.arraysize of
            undefined -> [];
            _ -> [{?ARRAYSIZE, Term#qualifier_declaration.arraysize}]
        end ++
        case Term#qualifier_declaration.overridable of
            "true" -> [];
            "false" -> [{?OVERRIDABLE, Term#qualifier_declaration.overridable}]
        end ++
        case Term#qualifier_declaration.tosubclass of
            "true" -> [];
            "false" -> [{?TOSUBCLASS, Term#qualifier_declaration.tosubclass}]
        end ++
        case Term#qualifier_declaration.toinstance of
            "false" -> [];
            "true" -> [{?TOINSTANCE, Term#qualifier_declaration.toinstance}]
        end ++
        case Term#qualifier_declaration.translatable of
            "false" -> [];
            "true" -> [{?TRANSLATABLE, Term#qualifier_declaration.translatable}]
        end,
    Scope = {'SCOPE', 
             lists:map(fun(MetaElement) -> 
                               {MetaElement, "true"} end, 
                       Term#qualifier_declaration.scope), []},
    Value = case Term#qualifier_declaration.value of
                   undefined -> [];
                   _ -> [{'VALUE', [], [Term#qualifier_declaration.value]}]
               end,
    {'QUALIFIER.DECLARATION', Attrs, [Scope] ++ Value};

from_term({ok, List}) when is_list(List) ->
    {'IRETURNVALUE', [], [from_term(Elt) || Elt <- List]};

from_term({ok, Elt}) ->
    {'IRETURNVALUE', [], [from_term(Elt)]};

from_term(ok) ->
    {'IRETURNVALUE', [], []};

from_term({error, {Code}}) ->
    {'ERROR', [{'CODE', Code}, {'DESCRIPTION', cim_error_string(Code)}], []};

from_term({error, {Code, Description}}) ->
    {'ERROR', [{'CODE', Code}, {'DESCRIPTION', Description}], []};

from_term(Term) ->
    throw({error, io_lib:format("unconverted term: ~w", [Term])}).

%% Execute an intrinsic method

imethod("EnumerateInstanceNames", NameSpace, Params) ->
    case proplists:split(Params, ["ClassName"]) of
        {[[{_, ClassName}]], []} ->
            gen_server:call(
              cimomhandle, 
              {enumerateInstanceNames, NameSpace, 
               ClassName#instancename.classname});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("EnumerateInstances", NameSpace, Params) ->
    LocalOnly = get_bool_value("LocalOnly", Params, true),
    DeepInheritance = get_bool_value("DeepInheritance", Params, false),
    IncludeQualifiers = get_bool_value("IncludeQualifiers", Params, true),
    IncludeClassOrigin = get_bool_value("IncludeClassOrigin", Params, false),
    PropertyList = proplists:get_value("PropertyList", Params, []),
    case proplists:split(Params, ["ClassName", "LocalOnly", "DeepInheritance",
                                  "IncludeQualifiers", "IncludeClassOrigin", 
                                  "PropertyList"]) of
        {[[{_, ClassName}], _, _, _, _, _], []} ->
            gen_server:call(
              cimomhandle,
              {enumerateInstances, NameSpace, ClassName, LocalOnly, 
               DeepInheritance, IncludeQualifiers, IncludeClassOrigin,
               PropertyList});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("GetInstance", NameSpace, Params) ->
    LocalOnly = get_bool_value("LocalOnly", Params, true),
    IncludeQualifiers = get_bool_value("IncludeQualifiers", Params, false),
    IncludeClassOrigin = get_bool_value("IncludeClassOrigin", Params, false),
    PropertyList = proplists:get_value("PropertyList", Params, []),
    case proplists:split(Params, ["InstanceName", "LocalOnly", 
                                  "IncludeQualifiers", "IncludeClassOrigin", 
                                  "PropertyList"]) of
        {[[{_, InstanceName}], _, _, _, _], []} ->
            gen_server:call(
              cimomhandle,
              {getInstance, NameSpace, InstanceName, LocalOnly, 
               IncludeQualifiers, IncludeClassOrigin, PropertyList});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;    

imethod("CreateInstance", NameSpace, Params) ->
    case proplists:split(Params, ["NewInstance"]) of
        {[[{_, NewInstance}]], []} ->
            gen_server:call(
              cimomhandle,
              {createInstance, NameSpace, NewInstance});
        _ ->
            from_term({error, {?CIM_ERR_INVALID_PARAMETER}})
    end;

imethod("DeleteInstance", NameSpace, Params) ->
    case proplists:split(Params, ["InstanceName"]) of
        {[[{_, InstanceName}]], []} ->
            gen_server:call(
              cimomhandle,
              {deleteInstance, NameSpace, InstanceName});
        _ ->
            from_term({error, {?CIM_ERR_INVALID_PARAMETER}})
    end;

imethod("ModifyInstance", NameSpace, Params) ->
    IncludeQualifiers = get_bool_value("IncludeQualifiers", Params, false),
    PropertyList = proplists:get_value("PropertyList", Params, []),
    case proplists:split(Params, ["ModifiedInstance", "IncludeQualifiers",
                                  "PropertyList"]) of
        {[[{_, ModifiedInstance}], _, _], []} ->
            gen_server:call(
              cimomhandle,
              {modifyInstance, NameSpace, ModifiedInstance, IncludeQualifiers,
               PropertyList});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("SetQualifier", NameSpace, Params) ->
    case proplists:split(Params, ["QualifierDeclaration"]) of
        {[[{_, QualifierDeclaration}]], []} ->
            gen_server:call(
              cimomhandle, 
              {setQualifier, NameSpace, QualifierDeclaration});
        _ -> 
            from_term({error, {?CIM_ERR_INVALID_PARAMETER}})
    end;

imethod("EnumerateQualifiers", NameSpace, Params) ->
    case proplists:split(Params, []) of
        {[], []} ->
            gen_server:call(
              cimomhandle, 
              {enumerateQualifiers, NameSpace});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("GetQualifier", NameSpace, Params) ->
    case proplists:split(Params, ["QualifierName"]) of
        {[[{_, QualifierName}]], []} ->
            gen_server:call(
              cimomhandle, {getQualifier, NameSpace, QualifierName});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("DeleteQualifier", NameSpace, Params) ->
    case proplists:split(Params, ["QualifierName"]) of
        {[[{_, QualifierName}]], []} ->
            gen_server:call(
              cimomhandle, {deleteQualifier, NameSpace, QualifierName});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("GetClass", NameSpace, Params) ->
    LocalOnly = get_bool_value("LocalOnly", Params, true),
    IncludeQualifiers = get_bool_value("IncludeQualifiers", Params, true),
    IncludeClassOrigin = get_bool_value("IncludeClassOrigin", Params, false),
    PropertyList = proplists:get_value("PropertyList", Params, []),
    case proplists:split(Params, ["ClassName", "LocalOnly", 
                                  "IncludeQualifiers", "IncludeClassOrigin", 
                                  "PropertyList"]) of
        {[[{_, ClassName}], _, _, _, _], []} ->
            gen_server:call(
              cimomhandle, 
              {getClass, NameSpace, ClassName#instancename.classname, 
               LocalOnly, IncludeQualifiers, IncludeClassOrigin, PropertyList});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("DeleteClass", NameSpace, Params) ->
    case proplists:split(Params, ["ClassName"]) of
        {[[{_, ClassName}]], []} ->
            gen_server:call(
              cimomhandle, 
              {deleteClass, NameSpace, ClassName#instancename.classname});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("CreateClass", NameSpace, Params) ->
    case proplists:split(Params, ["NewClass"]) of
        {[[{_, NewClass}]], []} ->
            gen_server:call(
              cimomhandle, {createClass, NameSpace, NewClass});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("ModifyClass", NameSpace, Params) ->
    case proplists:split(Params, ["ModifiedClass"]) of
        {[[{_, ModifiedClass}]], []} ->
            gen_server:call(
              cimomhandle, {modifyClass, NameSpace, ModifiedClass});
        _ -> 
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("EnumerateClasses", NameSpace, Params) ->
    ClassName = case proplists:get_value("ClassName", Params, undefined) of
                    undefined ->
                        undefined;
                    Result ->
                        Result#instancename.classname
                end,
    DeepInheritance = get_bool_value("DeepInheritance", Params, false),
    LocalOnly = get_bool_value("LocalOnly", Params, true),
    IncludeQualifiers = get_bool_value("IncludeQualifiers", Params, true),
    IncludeClassOrigin = get_bool_value("IncludeClassOrigin", Params, false),
    case proplists:split(Params, ["ClassName", "DeepInheritance",
                                  "LocalOnly", "IncludeQualifiers",
                                  "IncludeClassOrigin"]) of
        {_, []} ->
            gen_server:call(
              cimomhandle, {enumerateClasses, NameSpace, ClassName, 
                            DeepInheritance, LocalOnly, IncludeQualifiers,
                            IncludeClassOrigin});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

imethod("EnumerateClassNames", NameSpace, Params) ->
    ClassName = case proplists:get_value("ClassName", Params, undefined) of
                    undefined ->
                        undefined;
                    Result ->
                        Result#instancename.classname
                end,
    DeepInheritance = get_bool_value("DeepInheritance", Params, false),
    case proplists:split(Params, ["ClassName", "DeepInheritance"]) of
        {_, []} ->
            gen_server:call(
              cimomhandle, {enumerateClassNames, NameSpace, ClassName, 
                            DeepInheritance});
        _ ->
            {error, {?CIM_ERR_INVALID_PARAMETER}}
    end;

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

cimxml_request_not_valid(Description) ->
    {400, 
     [{"CIMError", "request-not-valid"}, {"GriffinError", Description}], ""}.

cimxml_request_good(ResponseTT) ->
    {200, [], lists:flatten(xmerl:export_simple([ResponseTT], xmerl_xml))}.

cimxml_request(Doc) ->
    case (catch cimxml_parse:doc(Doc)) of
        {error, {ErrorType, Description}} ->
            %% XML did not conform to DTD
            cimxml_request_not_valid(
              io_lib:format("~s: ~s", [atom_to_list(ErrorType), Description]));
        {'EXIT', Reason} ->
            %% CIM-XML validator crashed - oops
            cimxml_request_not_valid(io_lib:format("~w", [Reason]));
        RequestTT ->
            case (catch exec(RequestTT)) of
                %% Exception
                {'EXIT', Reason} ->
                    cimxml_request_not_valid(io_lib:format("~s", [Reason]));
                %% Normal result
                ResponseTT -> 
                    cimxml_request_good(ResponseTT)
            end
    end.

%% Process a CIM-XML request

do(_Headers, Body) ->
    case (catch xmerl_scan:string(Body, [{quiet, true}])) of
        %% TODO: pass error description back in HTTP headers
        {'EXIT', {fatal, _XmlError}} ->
            cimxml_request_not_well_formed();
        {'EXIT', _XmlError} ->
            cimxml_request_not_well_formed();
        %% Well-formed XML document
        {Doc, _Trailer} ->
            cimxml_request(Doc)
    end.

%% Entry point for processing CIM-XML request

do(Info) ->
    {Status, Headers, Body} = 
        do(Info#mod.parsed_header, Info#mod.entity_body),
    Length = integer_to_list(httpd_util:flatlength(Body)),
    Head = [{code, Status},
            {content_type, "application/xml"},
            {content_length, Length}] ++ Headers,
    {proceed, [{response, {response, Head, Body}}]}.
