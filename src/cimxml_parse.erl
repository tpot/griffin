%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(cimxml_parse).

%% TODO: Qualifier flavours

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Module API

-export([doc/1, string/1, any_string/1]).

%% Exports to keep apply() BIF happy.  Do not call directly.

-export(['CIM'/2, 
         'MESSAGE'/2, 
         'SIMPLEREQ'/2, 
         'MULTIREQ'/2,
         'IMETHODCALL'/2,
         'LOCALNAMESPACEPATH'/2,
         'IPARAMVALUE'/2,
         'CLASSNAME'/2,
         'NAMESPACE'/2,
         'VALUE'/2,
         'VALUE.ARRAY'/2,
         'VALUE.NULL'/2,
         'HOST'/2,
         'KEYVALUE'/2,
         'KEYBINDING'/2,
         'INSTANCENAME'/2, 'INSTANCEPATH'/2, 'LOCALINSTANCEPATH'/2,
         'NAMESPACEPATH'/2,
         'VALUE.OBJECT'/2, 'VALUE.REFERENCE'/2, 'VALUE.REFARRAY'/2,
         'VALUE.NAMEDINSTANCE'/2, 'VALUE.NAMEDOBJECT'/2,
         'VALUE.OBJECTWITHLOCALPATH'/2, 'VALUE.OBJECTWITHPATH'/2,
         'OBJECTPATH'/2, 'SCOPE'/2,
         'CLASSPATH'/2, 'LOCALCLASSPATH'/2, 'QUALIFIER.DECLARATION'/2,
         'QUALIFIER'/2, 'PARAMETER'/2, 'PARAMVALUE'/2,
         'PROPERTY'/2, 'PROPERTY.ARRAY'/2, 'PROPERTY.REFERENCE'/2,
         'CLASS'/2, 'INSTANCE'/2,
         'RESPONSEDESTINATION'/2,
         'METHOD'/2, 'METHODCALL'/2,
         'PARAMETER.REFERENCE'/2, 'PARAMETER.ARRAY'/2, 'PARAMETER.REFARRAY'/2]).

%% Used by cimxml_parse_test module

-export([elts_to_tuple/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include_lib("cimxml.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Utility functions

%% Convert a list of #xmlElement records to a list of tuples keyed by
%% the element name.  Consecutive runs of elements are folded into a
%% single tuple.
%%
%% @spec elts_to_tuple(list(xmlElement())) -> list(tupleList())
%%
%% @type tupleList() -> list({atom(), list(xmlElement())})

elts_to_tuple(EltList) ->
    elts_to_tuple(EltList, []).

elts_to_tuple([H | T], Result) when is_record(H, xmlElement) ->
    {Elts, Rest} = lists:splitwith(
                     fun(X) -> X#xmlElement.name == H#xmlElement.name end, T),
    elts_to_tuple(Rest, [{H#xmlElement.name, [H] ++ Elts}] ++ Result);

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

%% Parse an XML element by calling functions named after the element
%% with arguments generated from the element's child list.
%%
%% @spec parse(xmlElement) -> any()

parse(Elt) ->
    Params = [Elt] ++ [elts_to_tuple(Elt#xmlElement.content)],
    apply(?MODULE, Elt#xmlElement.name, Params).

%% Fetch and validate a VALUETYPE attribute

get_valuetype_attr(Elt) ->
    Attr = xml:get_attr(?VALUETYPE, Elt, #xmlAttribute{value = "string"}),
    case Attr#xmlAttribute.value of
        "string" -> Attr;
        "boolean" -> Attr;
        "numeric" -> Attr;
        _ ->
            throw(
              {error, 
               {invalid_attribute_error, 
                io_lib:format("Invalid value for ~p attribute", [?VALUETYPE])}})
    end.

%% Fetch and validate a TYPE attribute 

get_type_attr(Elt) ->
    Attr = xml:get_attr(?TYPE, Elt, #xmlAttribute{value = undefined}),
    case Attr#xmlAttribute.value of
        "boolean" -> Attr;
        "string" -> Attr;
        "char16" -> Attr;
        "uint8" -> Attr;
        "sint8" -> Attr;
        "uint16" -> Attr;
        "sint16" -> Attr;
        "uint32" -> Attr;
        "sint32" -> Attr;
        "uint64" -> Attr;
        "sint64" -> Attr;
        "datetime" -> Attr;
        "real32" -> Attr;
        "real64" -> Attr;
        undefined -> Attr;
        _ ->
            throw({error, 
                   {invalid_attribute_error, 
                    io_lib:format("Invalid value for ~p attribute", [?TYPE])}})
    end.

%% Fetch an optional TYPE attribute

get_type_attr(Elt, Default) ->
    case xml:get_attr(?TYPE, Elt, undefined) of
        undefined -> Default;
        _ -> get_type_attr(Elt)
    end.

%% Fetch and validate an optional boolean attribute

get_bool_attr(Name, Elt, Default) ->
    Attr = xml:get_attr(Name, Elt, Default),
    case Attr#xmlAttribute.value of
        "true" -> Attr;
        "false" -> Attr;
        undefined -> Attr;
        _ -> throw({error, {invalid_attribute_error,
                            io_lib:format("Invalid boolean attribute ~p",
                                          [Attr#xmlAttribute.value])}})
    end,
    Attr.

%% Fetch and validate a boolean attribute

get_bool_attr(Name, Elt) ->
    Value = get_bool_attr(Name, Elt, undefined),
    case Value of
        undefined ->
            throw({error, 
                   {invalid_attribute_error,
                    io_lib:format("No value for ~p attribute", [Name])}});
        _ -> Value

    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Root element

%% <!ELEMENT CIM (MESSAGE | DECLARATION)>
%% <!ATTLIST CIM
%% 	CIMVERSION CDATA #REQUIRED
%% 	DTDVERSION CDATA #REQUIRED>

'CIM'(Elt, [{'MESSAGE', [Message]}]) ->
    CimVersion = xml:get_attr(?CIMVERSION, Elt),
    DtdVersion = xml:get_attr(?DTDVERSION, Elt),
    {'CIM',
     [{?CIMVERSION, CimVersion#xmlAttribute.value},
      {?DTDVERSION, DtdVersion#xmlAttribute.value}],
     [parse(Message)]};

'CIM'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (MESSAGE)"}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Object declaration elements

%% <!ELEMENT DECLARATION (DECLGROUP | DECLGROUP.WITHNAME | DECLGROUP.WITHPATH)+>

%% <!ELEMENT DECLGROUP ((LOCALNAMESPACEPATH | NAMESPACEPATH)?, 
%%                      QUALIFIER.DECLARATION*, VALUE.OBJECT*)>

%% <!ELEMENT DECLGROUP.WITHNAME ((LOCALNAMESPACEPATH | NAMESPACEPATH)?, 
%%                               QUALIFIER.DECLARATION*, VALUE.NAMEDOBJECT*)>

%% <!ELEMENT DECLGROUP.WITHPATH (VALUE.OBJECTWITHPATH | 
%%                               VALUE.OBJECTWITHLOCALPATH)*>

%% <!ELEMENT QUALIFIER.DECLARATION (SCOPE?, (VALUE | VALUE.ARRAY)?)>
%% <!ATTLIST QUALIFIER.DECLARATION 
%%          %CIMName;               
%%          %CIMType;               #REQUIRED
%%          ISARRAY    (true|false) #IMPLIED
%%          %ArraySize;
%%          %QualifierFlavor;>

'QUALIFIER.DECLARATION'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt),
    IsArray = get_bool_attr(?ISARRAY, Elt),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    Overridable = get_bool_attr(?OVERRIDABLE, Elt, "true"),
    ToSubclass = get_bool_attr(?TOSUBCLASS, Elt, "true"),
    ToInstance = get_bool_attr(?TOINSTANCE, Elt, "false"),
    Translatable = get_bool_attr(?TRANSLATABLE, Elt, "false"),
    case Children of
        [] -> pass;
        [{Tag, _}] when Tag == 'SCOPE' orelse Tag == 'VALUE' orelse
        Tag == 'VALUE.ARRAY' -> pass;
        [{'SCOPE', _}, {Tag, _}]
        when Tag == 'VALUE' orelse Tag == 'VALUE.ARRAY' -> pass;
        _ -> throw(
               {error, 
                {parse_error, "Expecting (SCOPE?, (VALUE | VALUE.ARRAY)?)"}})
    end,
    {'QUALIFIER.DECLARATION', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}, 
      {?ISARRAY, IsArray#xmlAttribute.value}, 
      {?ARRAYSIZE, ArraySize#xmlAttribute.value},
      {?OVERRIDABLE, Overridable#xmlAttribute.value},
      {?TOSUBCLASS, ToSubclass#xmlAttribute.value},
      {?TOINSTANCE, ToInstance#xmlAttribute.value},
      {?TRANSLATABLE, Translatable#xmlAttribute.value}],
     [parse(Child) || {_ChildTag, [Child]} <- Children]}.

%% <!ELEMENT SCOPE EMPTY>
%% <!ATTLIST SCOPE
%% 	CLASS (true | false) "false"
%% 	ASSOCIATION (true | false) "false"
%% 	REFERENCE (true | false) "false"
%% 	PROPERTY (true | false) "false"
%% 	METHOD (true | false) "false"
%% 	PARAMETER (true | false) "false"
%% 	INDICATION (true | false) "false">

'SCOPE'(Elt, []) ->
    Attrs = lists:map(fun(X) -> get_bool_attr(X, Elt, "false") end,
                      [?CLASS, ?ASSOCIATION, ?REFERENCE, ?PROPERTY,
                       ?METHOD, ?PARAMETER, ?INDICATION]),
    {'SCOPE', 
     lists:map(fun(X) -> {X#xmlAttribute.name, X#xmlAttribute.value} end, 
               Attrs),
     []};

'SCOPE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting EMPTY"}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Object value elements

%% <!ELEMENT VALUE (#PCDATA)>

'VALUE'(_Elt, []) ->
    {'VALUE', [], [""]};

'VALUE'(_Elt, [{'#xmlText', Value}]) ->
    {'VALUE', [], [Value]};

'VALUE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (#PCDATA)"}}).

%% <!ELEMENT VALUE.ARRAY (VALUE*)>

'VALUE.ARRAY'(_Elt, []) ->
    {'VALUE.ARRAY', [], []};

'VALUE.ARRAY'(_Elt, [{'VALUE', Values}]) ->
    {'VALUE.ARRAY', [], [parse(Value) || Value <- Values]};

'VALUE.ARRAY'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (VALUE*)"}}).

%% <!ELEMENT VALUE.REFERENCE (CLASSPATH | LOCALCLASSPATH | CLASSNAME | 
%%                            INSTANCEPATH | LOCALINSTANCEPATH | INSTANCENAME)>

'VALUE.REFERENCE'(_Elt, [{ChildTag, [Child]}])
  when ChildTag == 'CLASSPATH' orelse ChildTag == 'LOCALCLASSPATH' orelse
       ChildTag == 'CLASSNAME' orelse ChildTag == 'INSTANCEPATh' orelse
       ChildTag == 'LOCALINSTANCEPATH' orelse ChildTag == 'INSTANCENAME' ->
    {'VALUE.REFERENCE', [], [parse(Child)]};

'VALUE.REFERENCE'(_Elt, _) ->
    throw({error, 
           {parse_error, "Expecting (CLASSPATH | LOCALCLASSPATH | CLASSNAME | "
            "INSTANCEPATH | LOCALINSTANCEPATH | INSTANCENAME)"}}).

%% <!ELEMENT VALUE.REFARRAY (VALUE.REFERENCE*)>

'VALUE.REFARRAY'(_Elt, []) ->
    {'VALUE.REFARRAY', [], []};

'VALUE.REFARRAY'(_Elt, [{'VALUE.REFERENCE', [ValueReferences]}]) ->
    {'VALUE.REFERENCE', 
     [],
     [parse(ValueReference) || ValueReference <- ValueReferences]};

'VALUE.REFARRAY'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (VALUE.REFERENCE*)"}}).

%% <!ELEMENT VALUE.OBJECT (CLASS | INSTANCE)>

'VALUE.OBJECT'(_Elt, [{'CLASS', [Class]}]) ->
    {'VALUE.OBJECT', [], [parse(Class)]};

'VALUE.OBJECT'(_Elt, [{'INSTANCE', [Instance]}]) ->
    {'VALUE.OBJECT', [], [parse(Instance)]};

'VALUE.OBJECT'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (CLASS | INSTANCE)"}}).

%% <!ELEMENT VALUE.NAMEDINSTANCE (INSTANCENAME, INSTANCE)>

'VALUE.NAMEDINSTANCE'(_Elt, [{'INSTANCENAME', [InstanceName]},
                             {'INSTANCE', [Instance]}]) ->
    {'VALUE.NAMEDINSTANCE', [], [parse(InstanceName), parse(Instance)]};

'VALUE.NAMEDINSTANCE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (INSTANCENAME, INSTANCE)"}}).

%% <!ELEMENT VALUE.NAMEDOBJECT (CLASS | (INSTANCENAME, INSTANCE))>

'VALUE.NAMEDOBJECT'(_Elt, [{'CLASS', [Class]}]) ->
    {'VALUE.NAMEDOBJECT', [], [parse(Class)]};

'VALUE.NAMEDOBJECT'(_Elt, [{'INSTANCENAME', [InstanceName]},
                           {'INSTANCE', [Instance]}]) ->
    {'VALUE.NAMEDOBJECT', [], [parse(InstanceName), parse(Instance)]};

'VALUE.NAMEDOBJECT'(_Elt, _) ->
    throw({error, 
           {parse_error, "Expecting (CLASS | (INSTANCENAME, INSTANCE))"}}).

%% <!ELEMENT VALUE.OBJECTWITHLOCALPATH ((LOCALCLASSPATH, CLASS) | 
%%                                      (LOCALINSTANCEPATH, INSTANCE))>

'VALUE.OBJECTWITHLOCALPATH'(_Elt, [{'LOCALCLASSPATH', [LocalClassPath]},
                                   {'CLASS', [Class]}]) ->
    {'VALUE.OBJECTWITHLOCALPATH', [], [parse(LocalClassPath), parse(Class)]};

'VALUE.OBJECTWITHLOCALPATH'(_Elt, [{'LOCALINSTANCEPATH', [LocalInstancePath]},
                                   {'INSTANCE', [Instance]}]) ->
    {'VALUE.OBJECTWITHLOCALPATH', [], 
     [parse(LocalInstancePath), [parse(Instance)]]};

'VALUE.OBJECTWITHLOCALPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting ((LOCALCLASSPATH, CLASS) | "
                   "(LOCALINSTANCEPATH, INSTANCE))"}}).

%% <!ELEMENT VALUE.OBJECTWITHPATH ((CLASSPATH, CLASS) | 
%%                                 (INSTANCEPATH, INSTANCE))>

'VALUE.OBJECTWITHPATH'(_Elt, [{'CLASSPATH', [ClassPath]},
                              {'CLASS', [Class]}]) ->
    {'VALUE.OBJECTWITHPATH', [], [parse(ClassPath), parse(Class)]};

'VALUE.OBJECTWITHPATH'(_Elt, [{'INSTANCEPATH', [InstancePath]},
                              {'INSTANCE', [Instance]}]) ->
    {'VALUE.OBJECTWITHPATH', [], [parse(InstancePath), parse(Instance)]};

'VALUE.OBJECTWITHPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting ((CLASSPATH, CLASS) | "
                   "(INSTANCEPATH, INSTANCE))"}}).

%% <!ELEMENT VALUE.NULL EMPTY>

'VALUE.NULL'(_Elt, []) ->
    {'VALUE.NULL', [], []};

'VALUE.NULL'(_Elt, _) ->
    throw({error, {parse_error, "Expecting EMPTY"}}).

%% Object naming and locating elements

%% <!ELEMENT NAMESPACEPATH (HOST, LOCALNAMESPACEPATH)>

'NAMESPACEPATH'(_Elt, [{'HOST', [Host]}, 
                       {'LOCALNAMESPACEPATH', [LocalNamespacepath]}]) ->
    {'NAMESPACEPATH', [], [parse(Host), parse(LocalNamespacepath)]};

'NAMESPACEPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (HOST, LOCALNAMESPACEPATH)"}}).

%% <!ELEMENT LOCALNAMESPACEPATH (NAMESPACE+)>

'LOCALNAMESPACEPATH'(_Elt, [{'NAMESPACE', Namespaces}]) ->
    {'LOCALNAMESPACEPATH', [], [parse(Namespace) || Namespace <- Namespaces]};

'LOCALNAMESPACEPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (NAMESPACE+)"}}).

%% <!ELEMENT HOST (#PCDATA)>

'HOST'(_Elt, []) ->
    {'HOST', [], [""]};

'HOST'(_Elt, [{'#xmlText', Value}]) ->
    {'HOST', [], [Value]};

'HOST'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (#PCDATA)"}}).

%% <!ELEMENT NAMESPACE EMPTY>
%% <!ATTLIST NAMESPACE
%% 	%CIMName;>

'NAMESPACE'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    {'NAMESPACE', [{?NAME, Name#xmlAttribute.value}], []};

'NAMESPACE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting EMPTY"}}).

%% <!ELEMENT CLASSPATH (NAMESPACEPATH, CLASSNAME)>

'CLASSPATH'(_Elt, [{'NAMESPACEPATH', [NamespacePath]},
                   {'CLASSNAME', [ClassName]}]) ->
    {'CLASSPATH', [], [parse(NamespacePath), parse(ClassName)]};

'CLASSPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (NAMESPACEPATH, CLASSNAME)"}}).

%% <!ELEMENT LOCALCLASSPATH (LOCALNAMESPACEPATH, CLASSNAME)>

'LOCALCLASSPATH'(_Elt, [{'LOCALNAMESPACEPATH', [LocalNamespacePath]},
                        {'CLASSNAME', [ClassName]}]) ->
    {'LOCALCLASSPATH', [], [parse(LocalNamespacePath), parse(ClassName)]};

'LOCALCLASSPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (LOCALNAMESPACEPATH, CLASSNAME)"}}).

%% <!ELEMENT CLASSNAME EMPTY>
%% <!ATTLIST CLASSNAME
%%	%CIMName;>

'CLASSNAME'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    {'CLASSNAME', [{?NAME, Name#xmlAttribute.value}], []};

'CLASSNAME'(_Elt, _) ->
    throw({error, {parse_error, "Expecting EMPTY"}}).

%% <!ELEMENT INSTANCEPATH (NAMESPACEPATH, INSTANCENAME)>

'INSTANCEPATH'(_Elt, [{'NAMESPACEPATH', [NamespacePath]},
                      {'INSTANCENAME', [InstanceName]}]) ->
    {'INSTANCEPATH', [], [parse(NamespacePath), parse(InstanceName)]};

'INSTANCEPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (NAMESPACEPATH, INSTANCENAME)"}}).

%% <!ELEMENT LOCALINSTANCEPATH (LOCALNAMESPACEPATH, INSTANCENAME)>

'LOCALINSTANCEPATH'(_Elt, [{'LOCALNAMESPACEPATH', [LocalNamespacePath]},
                           {'INSTANCENAME', [InstanceName]}]) ->
    {'LOCALINSTANCEPATH', [], [parse(LocalNamespacePath), parse(InstanceName)]};

'LOCALINSTANCEPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (LOCALNAMESPACEPATH, "
                   "INSTANCENAME)"}}).

%% <!ELEMENT INSTANCENAME (KEYBINDING* | KEYVALUE? | VALUE.REFERENCE?)>
%% <!ATTLIST INSTANCENAME
%%	%ClassName;>

'INSTANCENAME'(Elt, []) ->
    ClassName = xml:get_attr(?CLASSNAME, Elt),
    {'INSTANCENAME', 
     [{?CLASSNAME, ClassName#xmlAttribute.value}], 
     []};

'INSTANCENAME'(Elt, [{'KEYBINDING', Keybindings}]) ->
    ClassName = xml:get_attr(?CLASSNAME, Elt),
    {'INSTANCENAME', 
     [{?CLASSNAME, ClassName#xmlAttribute.value}],
     [parse(Keybinding) || Keybinding <- Keybindings]};

'INSTANCENAME'(Elt, [{'KEYVALUE', [KeyValue]}]) ->
    ClassName = xml:get_attr(?CLASSNAME, Elt),
    {'INSTANCENAME', [{?CLASSNAME, ClassName#xmlAttribute.value}],
     [parse(KeyValue)]};

'INSTANCENAME'(Elt, [{'VALUE.REFERENCE', [ValueReference]}]) ->
    ClassName = xml:get_attr(?CLASSNAME, Elt),
    {'INSTANCENAME', [{?CLASSNAME, ClassName#xmlAttribute.value}],
     [parse(ValueReference)]};

'INSTANCENAME'(_Elt, _) ->
    throw({error, {parse_error, 
                   "Expecting (KEYBINDING* | KEYVALUE? | VALUE.REFERENCE?)"}}).

%% <!ELEMENT OBJECTPATH (INSTANCEPATH | CLASSPATH)>

'OBJECTPATH'(_Elt, [{'INSTANCEPATH', [InstancePath]}]) ->
    {'OBJECTPATH', [], [parse(InstancePath)]};

'OBJECTPATH'(_Elt, [{'CLASSPATH', [ClassPath]}]) ->
    {'OBJECTPATH', [], [parse(ClassPath)]};

'OBJECTPATH'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (INSTANCEPATH | CLASSPATH)"}}).

%% <!ELEMENT KEYBINDING (KEYVALUE | VALUE.REFERENCE)>
%% <!ATTLIST KEYBINDING
%%	%CIMName;>

'KEYBINDING'(Elt, [{ChildTag, [Child]}]) 
  when ChildTag == 'KEYVALUE' orelse ChildTag == 'VALUE.REFERENCE' ->
    Name = xml:get_attr(?NAME, Elt),
    {'KEYBINDING', [{?NAME, Name#xmlAttribute.value}], [parse(Child)]};

'KEYBINDING'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (KEYVALUE | VALUE.REFERENCE)"}}).

%% <!ELEMENT KEYVALUE (#PCDATA)>
%% <!ATTLIST KEYVALUE
%%	VALUETYPE (string | boolean | numeric) "string"
%%        %CIMType;              #IMPLIED>

'KEYVALUE'(Elt, []) ->
    ValueType = get_valuetype_attr(Elt),
    Type = get_type_attr(Elt),
    {'KEYVALUE', 
     [{?VALUETYPE, ValueType#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}], 
     [""]};

'KEYVALUE'(Elt, [{'#xmlText', Value}]) ->
    ValueType = get_valuetype_attr(Elt),
    Type = get_type_attr(Elt),
    {'KEYVALUE', 
     [{?VALUETYPE, ValueType#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}], 
     [Value]};

'KEYVALUE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (#PCDATA)"}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Object definition elements

%% <!ELEMENT CLASS (QUALIFIER*, (PROPERTY | PROPERTY.ARRAY | 
%%                  PROPERTY.REFERENCE)*, METHOD*)>
%% <!ATTLIST CLASS
%%	%CIMName; 
%%	%SuperClass;>

'CLASS'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),
    SuperClass = xml:get_attr(?SUPERCLASS, Elt, undefined),
    {_Qualifiers, Rest} =
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    {_Properties, Rest2} =
        lists:splitwith(
          fun({Tag, _}) -> 
                  lists:member(
                    Tag, 
                    ['PROPERTY', 'PROPERTY.ARRAY', 'PROPERTY.REFERENCE']) end,
          Rest),
    {_Methods, Rest3} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'METHOD' end, Rest2),
    case length(Rest3) of
        0 -> ok;
        _ -> throw({error, {parse_error, "Expecting (QUALIFIER*, (PROPERTY | "
                            "PROPERTY.ARRAY | PROPERTY.REFERENCE)*, METHOD*)"}})
    end,
    {'CLASS', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?SUPERCLASS, SuperClass#xmlAttribute.value}], 
     lists:flatten(
       [[parse(E) || E <- ChildElts] || {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT INSTANCE (QUALIFIER*, (PROPERTY | PROPERTY.ARRAY | 
%%                    PROPERTY.REFERENCE)*)>
%% <!ATTLIST INSTANCE
%%	%ClassName;
%%        xml:lang   NMTOKEN      #IMPLIED>

'INSTANCE'(Elt, Children) ->
    ClassName = xml:get_attr(?CLASSNAME, Elt),
    {Qualifiers, Rest} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    {Properties, Rest2} =
        lists:splitwith(
          fun({Tag, _}) -> lists:member(Tag,
                                        ['PROPERTY', 'PROPERTY.ARRAY',
                                         'PROPERTY.REFERENCE']) end, Rest),
    case length(Rest2) of
        0 -> ok;
        _ ->throw({error, 
                   {parse_error, "Expecting (QUALIFIER*, (PROPERTY | "
                    "PROPERTY.ARRAY | PROPERTY.REFERENCE)*)"}})
    end,
    {'INSTANCE', 
     [{?CLASSNAME, ClassName#xmlAttribute.value}],
     [parse(X) || X <- Qualifiers ++ Properties]}.

%% <!ELEMENT QUALIFIER ((VALUE | VALUE.ARRAY)?)>
%% <!ATTLIST QUALIFIER 
%%         %CIMName;
%%         %CIMType;              #REQUIRED
%%         %Propagated;
%%         %QualifierFlavor;
%%         xml:lang   NMTOKEN     #IMPLIED>

'QUALIFIER'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),    
    Type = get_type_attr(Elt),
    Propagated = get_bool_attr(?PROPAGATED, Elt),
    {'QUALIFIER', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     []};

'QUALIFIER'(Elt, [{'VALUE', [Value]}]) ->
    Name = xml:get_attr(?NAME, Elt),    
    Type = get_type_attr(Elt),
    Propagated = get_bool_attr(?PROPAGATED, Elt),
    {'QUALIFIER', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     [parse(Value)]};

'QUALIFIER'(Elt, [{'VALUE.ARRAY', [ValueArray]}]) ->
    Name = xml:get_attr(?NAME, Elt),    
    Type = get_type_attr(Elt),
    Propagated = get_bool_attr(?PROPAGATED, Elt),
    {'QUALIFIER', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     [parse(ValueArray)]};

'QUALIFIER'(_Elt, _) ->
    throw({error, {parse_error, "Expecting ((VALUE | VALUE.ARRAY)?)"}}).

%% <!ELEMENT PROPERTY (QUALIFIER*, VALUE?)>
%% <!ATTLIST PROPERTY 
%%         %CIMName;
%%         %ClassOrigin;
%%         %Propagated;
%%         %CIMType;              #REQUIRED
%%         xml:lang   NMTOKEN     #IMPLIED>

'PROPERTY'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),    
    ClassOrigin = xml:get_attr(?CLASSORIGIN, Elt, undefined),
    Propagated = get_bool_attr(?PROPAGATED, Elt),
    Type = get_type_attr(Elt),
    {_Qualifiers, Value} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    case Value of
        [{'VALUE', _}] -> pass;
        [] -> pass;
        _ -> throw({error, {parse_error, "Expecting (QUALIFIER*, VALUE?)"}})
    end,
    {'PROPERTY', 
     [{?NAME, Name#xmlAttribute.value},
      {?CLASSORIGIN, ClassOrigin#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}],
     lists:flatten(
       [[parse(E) || E <- ChildElts] || {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT PROPERTY.ARRAY (QUALIFIER*, VALUE.ARRAY?)>
%% <!ATTLIST PROPERTY.ARRAY 
%%         %CIMName;
%%         %CIMType;              #REQUIRED
%%         %ArraySize;
%%         %ClassOrigin;
%%         %Propagated;
%%         xml:lang   NMTOKEN     #IMPLIED>

'PROPERTY.ARRAY'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),    
    Type = get_type_attr(Elt),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    ClassOrigin = xml:get_attr(?CLASSORIGIN, Elt, undefined),
    Propagated = get_bool_attr(?PROPAGATED, Elt),
    {_Qualifiers, ValueArray} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    case ValueArray of
        [{'VALUE.ARRAY', _}] -> pass;
        [] -> pass;
        _ -> throw({error, 
                    {parse_error, "Expecting (QUALIFIER*, VALUE.ARRAY?)"}})
    end,
    {'PROPERTY', 
     [{?NAME, Name#xmlAttribute.value},
      {?TYPE, Type#xmlAttribute.value},
      {?ARRAYSIZE, ArraySize#xmlAttribute.value},
      {?CLASSORIGIN, ClassOrigin#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     lists:flatten(
       [[parse(E) || E <- ChildElts] || {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT PROPERTY.REFERENCE (QUALIFIER*, (VALUE.REFERENCE)?)>
%% <!ATTLIST PROPERTY.REFERENCE
%%	%CIMName; 
%%	%ReferenceClass; 
%%	%ClassOrigin; 
%%	%Propagated;>

'PROPERTY.REFERENCE'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),
    ReferenceClass = xml:get_attr(?REFERENCECLASS, Elt, undefined),
    ClassOrigin = xml:get_attr(?CLASSORIGIN, Elt, undefined),
    Propagated = xml:get_attr(?PROPAGATED, Elt, undefined),
    {_Qualifiers, ValueReference} =
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    case ValueReference of
        [{'VALUE.REFERENCE', _}] -> pass;
        [] -> pass;
        _ -> throw({error, {parse_error, 
                            "Expecting (QUALIFIER*, (VALUE.REFERENCE)?)"}})
    end,
    {'PROPERTY.REFERENCE', 
     [{?NAME, Name#xmlAttribute.value},
      {?REFERENCECLASS, ReferenceClass#xmlAttribute.value},
      {?CLASSORIGIN, ClassOrigin#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     lists:flatten(
       [[parse(E) || E <- ChildElts]|| {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT METHOD (QUALIFIER*, (PARAMETER | PARAMETER.REFERENCE | 
%%                   PARAMETER.ARRAY | PARAMETER.REFARRAY)*)>
%% <!ATTLIST METHOD 
%%         %CIMName;
%%         %CIMType;              #IMPLIED
%%         %ClassOrigin;
%%         %Propagated;>

'METHOD'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt, undefined),
    ClassOrigin = xml:get_attr(?CLASSORIGIN, Elt, undefined),
    Propagated = xml:get_attr(?PROPAGATED, Elt, undefined),
    {_Qualifiers, Rest} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'QUALIFIER' end, Children),
    {_Parameters, Rest2} =
        lists:splitwith(
          fun({Tag, _}) -> 
                  lists:member(
                    Tag,
                    ['PARAMETER', 'PARAMETER.REFERENCE', 'PARAMETER.ARRAY',
                     'PARAMETER.REFARRAY']) end, 
          Rest),
    if 
        length(Rest2) > 0 ->
            throw({error, {parse_error, "Expecting (QUALIFIER*, (PARAMETER | "
                           "PARAMETER.REFERENCE | PARAMETER.ARRAY | "
                           "PARAMETER.REFARRAY)*)"}});
        true -> ok
    end,
    {'METHOD', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}, 
      {?CLASSORIGIN, ClassOrigin#xmlAttribute.value},
      {?PROPAGATED, Propagated#xmlAttribute.value}],
     lists:flatten(
       [[parse(E) || E <- ChildElts] || {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT PARAMETER (QUALIFIER*)>
%% <!ATTLIST PARAMETER 
%%          %CIMName;
%%          %CIMType;              #REQUIRED>

'PARAMETER'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt),
    {'PARAMETER', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?TYPE, Type#xmlAttribute.value}], 
     []};

'PARAMETER'(Elt, [{'QUALIFIER', Qualifiers}]) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt),
    {'PARAMETER', 
     [{?NAME, Name#xmlAttribute.value}, {?TYPE, Type#xmlAttribute.value}],
     [parse(Qualifier) || Qualifier <- Qualifiers]};

'PARAMETER'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (QUALIFIER*)"}}).

%% <!ELEMENT PARAMETER.REFERENCE (QUALIFIER*)>
%% <!ATTLIST PARAMETER.REFERENCE
%% 	%CIMName; 
%% 	%ReferenceClass;>

'PARAMETER.REFERENCE'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    ReferenceClass = xml:get_attr(?REFERENCECLASS, Elt, undefined),
    {'PARAMETER.REFERENCE', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?REFERENCECLASS, ReferenceClass#xmlAttribute.value}], 
     []};

'PARAMETER.REFERENCE'(Elt, [{'QUALIFIER', Qualifiers}]) ->
    Name = xml:get_attr(?NAME, Elt),
    ReferenceClass = xml:get_attr(?REFERENCECLASS, Elt, undefined),
    {'PARAMETER.REFERENCE', 
     [{?NAME, Name#xmlAttribute.value},
      {?REFERENCECLASS, ReferenceClass#xmlAttribute.value}], 
     [parse(Qualifier) || Qualifier <- Qualifiers]};

'PARAMETER.REFERENCE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (QUALIFIER*)"}}).

%% <!ELEMENT PARAMETER.ARRAY (QUALIFIER*)>
%% <!ATTLIST PARAMETER.ARRAY 
%%          %CIMName;
%%          %CIMType;              #REQUIRED
%%          %ArraySize;>

'PARAMETER.ARRAY'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt, undefined),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    {'PARAMETER.ARRAY', 
     [{?NAME, Name#xmlAttribute.value},
      {?TYPE, Type#xmlAttribute.value}, 
      {?ARRAYSIZE, ArraySize#xmlAttribute.value}], 
     []};

'PARAMETER.ARRAY'(Elt, [{'QUALIFIER', Qualifiers}]) ->
    Name = xml:get_attr(?NAME, Elt),
    Type = get_type_attr(Elt, undefined),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    {'PARAMETER.ARRAY', 
     [{?NAME, Name#xmlAttribute.value},
      {?TYPE, Type#xmlAttribute.value}, 
      {?ARRAYSIZE, ArraySize#xmlAttribute.value}], 
     [parse(Qualifier) || Qualifier <- Qualifiers]};

'PARAMETER.ARRAY'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (QUALIFIER*)"}}).

%% <!ELEMENT PARAMETER.REFARRAY (QUALIFIER*)>
%% <!ATTLIST PARAMETER.REFARRAY
%% 	%CIMName; 
%% 	%ReferenceClass; 
%% 	%ArraySize;>

'PARAMETER.REFARRAY'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    ReferenceClass = xml:get_attr(?REFERENCECLASS, Elt, undefined),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    {'PARAMETER.REFARRAY', 
     [{?NAME, Name#xmlAttribute.value},
      {?REFERENCECLASS, ReferenceClass#xmlAttribute.value}, 
      {?ARRAYSIZE, ArraySize#xmlAttribute.value}], 
     []};

'PARAMETER.REFARRAY'(Elt, [{'QUALIFIER', Qualifiers}]) ->
    Name = xml:get_attr(?NAME, Elt),
    ReferenceClass = xml:get_attr(?REFERENCECLASS, Elt, undefined),
    ArraySize = xml:get_attr(?ARRAYSIZE, Elt, undefined),
    {'PARAMETER.REFARRAY', 
     [{?NAME, Name#xmlAttribute.value},
      {?REFERENCECLASS, ReferenceClass#xmlAttribute.value}, 
      {?ARRAYSIZE, ArraySize#xmlAttribute.value}], 
     [parse(Qualifier) || Qualifier <- Qualifiers]};

'PARAMETER.REFARRAY'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (QUALIFIER*)"}}).


%% <!ELEMENT TABLECELL.DECLARATION EMPTY> 
%% <!ATTLIST TABLECELL.DECLARATION
%%      %CIMName;
%%      %CIMType;   	#REQUIRED
%%      ISARRAY           (true|false) "false"
%%      %ArraySize;
%%      CELLPOS  		CDATA       #REQUIRED
%%      SORTPOS     	CDATA       #IMPLIED
%%      SORTDIR            (ASC|DESC)  #IMPLIED>

%% <!ELEMENT TABLECELL.REFERENCE EMPTY> 
%% <!ATTLIST TABLECELL.REFERENCE
%%      %CIMName;
%%      %ReferenceClass;	
%%      ISARRAY        (true|false) "false"
%%      %ArraySize; 
%%      CELLPOS          CDATA       #REQUIRED
%%      SORTPOS          CDATA       #IMPLIED
%%      SORTDIR         (ASC|DESC)   #IMPLIED>

%% <!ELEMENT TABLEROW.DECLARATION (TABLECELL.DECLARATION | 
%%                                 TABLECELL.REFERENCE)*>

%% <!ELEMENT TABLE (TABLEROW.DECLARATION,(TABLEROW)*)>

%% <!ELEMENT TABLEROW (VALUE | VALUE.ARRAY | VALUE.REFERENCE | VALUE.REFARRAY |
%%                     VALUE.NULL)*>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Message elements

%% <!ELEMENT MESSAGE (SIMPLEREQ | MULTIREQ | SIMPLERSP | MULTIRSP | 
%%                    SIMPLEEXPREQ | MULTIEXPREQ | SIMPLEEXPRSP | MULTIEXPRSP)>
%% <!ATTLIST MESSAGE
%% 	ID CDATA #REQUIRED
%% 	PROTOCOLVERSION CDATA #REQUIRED>

'MESSAGE'(Elt, [{ChildTag, [Child]}]) 
  when ChildTag == 'SIMPLEREQ' orelse ChildTag == 'MULTIREQ' ->
    Id = xml:get_attr(?ID, Elt),
    ProtocolVersion = xml:get_attr(?PROTOCOLVERSION, Elt),
    {'MESSAGE', 
     [{?ID, Id#xmlAttribute.value},
      {?PROTOCOLVERSION, ProtocolVersion#xmlAttribute.value}],
     [parse(Child)]};

'MESSAGE'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (SIMPLEREQ | MULTIREQ | "
                   "SIMPLEEXPREQ | MULTIEXPREQ)"}}).

%% <!ELEMENT MULTIREQ (SIMPLEREQ, SIMPLEREQ+)>

'MULTIREQ'(_Elt, [{'SIMPLEREQ', SimpleReqs}]) ->
    {'MULTIREQ', [], [parse(SimpleReq) || SimpleReq <- SimpleReqs]}.

%% <!ELEMENT MULTIEXPREQ (SIMPLEEXPREQ, SIMPLEEXPREQ+)>

%% <!ELEMENT SIMPLEREQ (IMETHODCALL | METHODCALL)>

'SIMPLEREQ'(_Elt, [{ChildTag, [Child]}]) 
  when ChildTag == 'IMETHODCALL' orelse ChildTag == 'METHODCALL' ->
    {'SIMPLEREQ', [], [parse(Child)]};

'SIMPLEREQ'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (IMETHODCALL | METHODCALL)"}}).

%% <!ELEMENT SIMPLEEXPREQ (EXPMETHODCALL)>

%% <!ELEMENT IMETHODCALL (LOCALNAMESPACEPATH, IPARAMVALUE*, 
%%                        RESPONSEDESTINATION?)>
%% <!ATTLIST IMETHODCALL
%% 	%CIMName;>

'IMETHODCALL'(Elt, [{'LOCALNAMESPACEPATH', [LocalNSP]} | Children]) ->
    Name = xml:get_attr(?NAME, Elt),
    {_Params, Rest} = 
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'IPARAMVALUE' end, Children),
    {_ResponseDestination, Rest2} =
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'RESPONSEDESTINATION' end, Rest),
    if 
        length(Rest2) > 0 ->
            throw({error, 
                   {parse_error, "Expecting (LOCALNAMESPACEPATH, IPARAMVALUE*, "
                    "RESPONSEDESTINATION?)"}});
        true -> ok
    end,
    {'IMETHODCALL', 
     [{?NAME, Name#xmlAttribute.value}], 
     [parse(LocalNSP)] ++ 
     lists:flatten(
       [[parse(E) || E <- ChildElts] || 
           {_Tag, ChildElts} <- [LocalNSP] ++ Children])};

'IMETHODCALL'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (LOCALNAMESPACEPATH, IPARAMVALUE*, "
                   "RESPONSEDESTINATION?)"}}).

%% <!ELEMENT METHODCALL ((LOCALINSTANCEPATH | LOCALCLASSPATH), PARAMVALUE*, 
%%                       RESPONSEDESTINATION?)>
%% <!ATTLIST METHODCALL
%% 	%CIMName;>

'METHODCALL'(Elt, [{PathTag, [Path]}]) 
  when PathTag == 'LOCALINSTANCEPATH' orelse PathTag == 'LOCALCLASSPATH' ->
    Name = xml:get_attr(?NAME, Elt),
    {'METHODCALL', [{?NAME, Name#xmlAttribute.value}], [parse(Path)]};

'METHODCALL'(Elt, [{PathTag, [Path]} | Children])
  when PathTag == 'LOCALINSTANCEPATH' orelse PathTag == 'LOCALCLASSPATH' ->   
    Name = xml:get_attr(?NAME, Elt),
    {ParamValues, Rest} =
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'PARAMVALUE' end, Children),
    {ResponseDestination, Rest2} =
        lists:splitwith(
          fun({Tag, _}) -> Tag == 'RESPONSEDESTINATION' end, Rest),
    case length(Rest2) of
        0 -> ok;
        _ -> throw({error, {parse_error, "Expecting ((LOCALINSTANCEPATH | "
                            "LOCALCLASSPATH), PARAMVALUE*, "
                            "RESPONSEDESTINATION?)"}})
    end,
    {'METHODCALL', 
     [{?NAME, Name#xmlAttribute.value}],
     [parse(X) || X <- [Path] ++ ParamValues ++ ResponseDestination]};

'METHODCALL'(_Elt, _) ->
    throw({error, 
		   {parse_error, "Expecting ((LOCALINSTANCEPATH | "
		    "LOCALCLASSPATH), PARAMVALUE*, RESPONSEDESTINATION?)"}}).

%% <!ELEMENT EXPMETHODCALL (EXPPARAMVALUE*)>
%% <!ATTLIST EXPMETHODCALL
%% 	%CIMName;>

%% <!ELEMENT PARAMVALUE (VALUE | VALUE.REFERENCE | VALUE.ARRAY | 
%%                       VALUE.REFARRAY)?>
%% <!ATTLIST PARAMVALUE
%% 	%CIMName; 
%%       %ParamType;  #IMPLIED> 

'PARAMVALUE'(Elt, []) ->
    Name = xml:get_attr(?NAME, Elt),
    ParamType = xml:get_attr(?PARAMTYPE, Elt, undefined),
    {'PARAMVALUE', 
     [{?NAME, Name#xmlAttribute.value}, 
      {?PARAMTYPE, ParamType#xmlAttribute.value}], 
     []};

'PARAMVALUE'(Elt, [{ValueTag, ValueElement}])
  when ValueTag == 'VALUE' orelse ValueTag == 'VALUE.REFERENCE' orelse
ValueTag == 'VALUE.ARRAY' orelse ValueTag == 'VALUE.REFARRAY' ->
    Name = xml:get_attr(?NAME, Elt),
    ParamType = xml:get_attr(?PARAMTYPE, Elt, undefined),
    {'PARAMVALUE', 
     [{?NAME, Name#xmlAttribute.value},
      {?PARAMTYPE, ParamType#xmlAttribute.value}],
     [parse(ValueElement)]};

'PARAMVALUE'(_Elt, _) ->
    throw({error, 
           {parse_error, "Expecting (VALUE | VALUE.REFERENCE | VALUE.ARRAY | "
            "VALUE.REFARRAY)?"}}).

%% <!ELEMENT IPARAMVALUE (VALUE | VALUE.ARRAY | VALUE.REFERENCE | 
%%                        INSTANCENAME | CLASSNAME | QUALIFIER.DECLARATION | 
%%                        CLASS | INSTANCE | VALUE.NAMEDINSTANCE)?>
%% <!ATTLIST IPARAMVALUE
%% 	%CIMName;>

'IPARAMVALUE'(Elt, Children) ->
    Name = xml:get_attr(?NAME, Elt),
    {_, Rest} =
        lists:splitwith(
          fun({Tag, _}) ->
                  lists:member(Tag, ['VALUE', 'VALUE.ARRAY', 'VALUE.REFERENCE',
                                     'INSTANCENAME', 'CLASSNAME', 
                                     'QUALIFIER.DECLARATION', 'CLASS',
                                     'INSTANCE', 'VALUE.NAMEDINSTANCE']) end,
          Children),
    case length(Rest) of
        0 -> ok;
        _ -> throw({error, 
                    {parse_error, "Expecting (VALUE | VALUE.ARRAY | "
                     "VALUE.REFERENCE | INSTANCENAME | CLASSNAME | "
                     "QUALIFIER.DECLARATION | CLASS | INSTANCE | "
                     "VALUE.NAMEDINSTANCE)?"}})
    end,
    {'IPARAMVALUE', 
     [{?NAME, Name#xmlAttribute.value}],
     lists:flatten(
       [[parse(E) || E <- ChildElts] || {_Tag, ChildElts} <- Children])}.

%% <!ELEMENT EXPPARAMVALUE (INSTANCE?|VALUE?|METHODRESPONSE?|IMETHODRESPONSE?)>
%% <!ATTLIST EXPPARAMVALUE
%% 	%CIMName;>

%% <!ELEMENT MULTIRSP (SIMPLERSP, SIMPLERSP+)>

%% <!ELEMENT MULTIEXPRSP (SIMPLEEXPRSP, SIMPLEEXPRSP+)>

%% <!ELEMENT SIMPLERSP (METHODRESPONSE | IMETHODRESPONSE | SIMPLEREQACK)>

%% <!ELEMENT SIMPLEEXPRSP (EXPMETHODRESPONSE)>

%% <!ELEMENT METHODRESPONSE (ERROR|(RETURNVALUE?,PARAMVALUE*))>
%% <!ATTLIST METHODRESPONSE
%% 	%CIMName;>

%% <!ELEMENT EXPMETHODRESPONSE (ERROR|IRETURNVALUE?)>
%% <!ATTLIST EXPMETHODRESPONSE
%% 	%CIMName;>

%% <!ELEMENT IMETHODRESPONSE (ERROR|IRETURNVALUE?)>
%% <!ATTLIST IMETHODRESPONSE
%% 	%CIMName;>

%% <!ELEMENT ERROR (INSTANCE*)>
%% <!ATTLIST ERROR
%% 	CODE CDATA #REQUIRED
%% 	DESCRIPTION CDATA #IMPLIED>

%% <!ELEMENT RETURNVALUE (VALUE | VALUE.REFERENCE)>
%% <!ATTLIST RETURNVALUE
%% 	%ParamType;       #IMPLIED>

%% <!ELEMENT IRETURNVALUE (CLASSNAME* | INSTANCENAME* | VALUE* | 
%%                         VALUE.OBJECTWITHPATH* | VALUE.OBJECTWITHLOCALPATH* | 
%%                         VALUE.OBJECT* | OBJECTPATH* | 
%%                         QUALIFIER.DECLARATION* | VALUE.ARRAY? | 
%%                         VALUE.REFERENCE? | CLASS* | INSTANCE* | 
%%                         VALUE.NAMEDINSTANCE* | TABLE )>

%% <!ELEMENT RESPONSEDESTINATION (INSTANCE)> 

'RESPONSEDESTINATION'(_Elt, [{'INSTANCE', [Instance]}]) ->
    {'RESPONSEDESTINATION', [], [parse(Instance)]};

'RESPONSEDESTINATION'(_Elt, _) ->
    throw({error, {parse_error, "Expecting (INSTANCE)"}}).

%% <!ELEMENT SIMPLEREQACK (ERROR?)> 
%% <!ATTLIST SIMPLEREQACK  
%%          INSTANCEID CDATA     #REQUIRED> 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Parse an xmerl document

doc(XML) ->
    case XML#xmlElement.name of
        'CIM' ->
            parse(XML);
        _ ->
            throw({error, {parse_error, "Expecting CIM root element"}})
    end.

%% Parse a string - for unit testing only.

string(Str) ->
    {Elt, _Rest} = xmerl_scan:string(Str),
    case Elt#xmlElement.name of
        'CIM' ->
            parse(Elt);
        _ ->
            throw({error, {parse_error, "Expecting CIM root element"}})
    end.

%% Parse any element in the CIM-XML schema.  For testing only.

any_string(Str) ->
    {Elt, _Rest} = xmerl_scan:string(Str),
    parse(Elt).
