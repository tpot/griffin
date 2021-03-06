-module(repository).
-behaviour(gen_server).

%% client interface

-export([start_link/1, get_subclasses/4, isa/4, get_class/7, stop/1,
         create_class/3, delete_class/3,
         enumerate_class_names/2, enumerate_class_names/3, 
         enumerate_class_names/4]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include_lib("cim.hrl").

-record(state, {
          storage,                              % ets or dets
          table
	 }).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Client interface

get_subclasses(Repository, NameSpace, ClassName, DeepInheritance) ->
    {ok, ClassList} = gen_server:call(
      Repository, {getSubclasses, NameSpace, ClassName, DeepInheritance}),
    ClassList.

get_class(Repository, NameSpace, ClassName, LocalOnly, IncludeQualifiers, 
          IncludeClassOrigin, PropertyList) ->
    gen_server:call(
      Repository, {getClass, NameSpace, ClassName, LocalOnly, 
                   IncludeQualifiers, IncludeClassOrigin, PropertyList}).

create_class(Pid, NameSpace, NewClass) ->
    gen_server:call(Pid, {createClass, NameSpace, NewClass}).

delete_class(Pid, NameSpace, ClassName) ->
    gen_server:call(Pid, {deleteClass, NameSpace, ClassName}).

enumerate_class_names(Repository, NameSpace) ->
    enumerate_class_names(Repository, NameSpace, undefined).

enumerate_class_names(Repository, NameSpace, ClassName) ->
    enumerate_class_names(Repository, NameSpace, ClassName, false).

enumerate_class_names(Repository, NameSpace, ClassName, DeepInheritance) ->
    Result = gen_server:call(
               Repository, 
               {enumerateClassNames, NameSpace, ClassName, DeepInheritance}),
    case Result of
        {ok, ClassNames} ->
            lists:map(fun(Elt) -> Elt#classname.name end, ClassNames);
        _ ->
            Result
    end.

isa(Repository, NameSpace, ClassName, BaseClass) ->
    {ok, Result} = gen_server:call(
                     Repository, {isa, NameSpace, ClassName, BaseClass}),
    Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).    

stop(Pid) ->
    gen_server:call(Pid, stop).

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

cim_error(Code) ->
    {cim_error, {Code, cim_error_string(Code)}}.

cim_error(Code, Msg) ->
    {cim_error, {Code, Msg}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Wrappers to call dets or ets, depending on config

match(State, Pattern) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:match(Table, Pattern);
        ets ->
            ets:match(Table, Pattern)
    end.

lookup(State, Key) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:lookup(Table, Key);
        ets ->
            ets:lookup(Table, Key)
    end.    

delete(State, Key) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:delete(Table, Key);
        ets ->
            ets:delete_object(Table, Key),
            ok
    end.    

insert(State, Objects) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:insert(Table, Objects);
        ets ->
            ets:insert(Table, Objects),
            ok
    end.

foldl(State, Function, Acc0) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:foldl(Function, Acc0, Table);
        ets ->
            ets:foldl(Function, Acc0, Table)
    end.    

close(State) ->
    #state{storage = Storage, table = Table} = State,
    case Storage of
        dets ->
            dets:close(Table);
        ets ->
            ok
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

internal_get_subclasses_deep(_AllClasses, []) ->
    [];

internal_get_subclasses_deep(AllClasses, [H | T]) ->
    SubClasses = 
        lists:map(
          fun([C, _]) -> C end,
          lists:filter(fun([_, SuperClass]) -> H == SuperClass end, 
                       AllClasses)),
    SubClasses ++
        internal_get_subclasses_deep(AllClasses, SubClasses) ++
        internal_get_subclasses_deep(AllClasses, T).

internal_get_subclasses(Table, NameSpace, ClassName, DeepInheritance) ->
    AllClasses = match(Table, {{class, NameSpace, '_'},
                               {'_', '$1', '$2', '_', '_', '_'}}),
    case ClassName of
        undefined ->
            case DeepInheritance of
                true ->
                    %% All classes
                    lists:map(fun([C, _]) -> C end, AllClasses);
                false ->
                    %% All top level classes
                    lists:map(fun([C, _]) -> C end,
                              lists:filter(
                                fun([_, SuperClass]) -> 
                                        SuperClass == undefined 
                                end, AllClasses))
            end;
        _ -> 
            case DeepInheritance of
                true ->
                    %% All subclasses of ClassName
                    internal_get_subclasses_deep(AllClasses, [ClassName]);
                false ->
                    %% Immediate subclasses of ClassName
                    lists:map(fun([C, _]) -> C end,
                              lists:filter(
                                fun([_, SuperClass]) ->
                                        SuperClass == ClassName
                                end, AllClasses))
            end
    end.

%% Get inheritance hierachy for a class

internal_get_superclass(Table, NameSpace, ClassName) ->
    case lookup(Table, class_key(NameSpace, ClassName)) of
        [{_, Class}] ->
            Class#class.superclass;
        _ ->
            undefined
    end.

internal_get_superclasses(Table, NameSpace, ClassName) ->
    case internal_get_superclass(Table, NameSpace, ClassName) of
        undefined ->
            [];
        SuperClass ->
            internal_get_superclasses(Table, NameSpace, SuperClass) ++ 
                [SuperClass]
    end.

%% Apply a function to all CLASSORIGIN attributes in a class definition

map_classorigin(Fun, Class) when is_record(Class, class) ->
    Class#class{
      properties = map_classorigin(Fun, Class#class.properties),
      methods    = map_classorigin(Fun, Class#class.methods)};

map_classorigin(Fun, List) when is_list(List) ->
    lists:map(
      fun(Elt) ->
              case Elt of
                  Prop when is_record(Elt, property) ->
                      Prop#property{
                        classorigin = Fun(Prop#property.classorigin)};
                  PropArray when is_record(Elt, property_array) ->
                      PropArray#property_array{
                        classorigin = 
                          Fun(PropArray#property_array.classorigin)};
                  RefProp when is_record(Elt, property_reference) ->
                      RefProp#property_reference{
                        classorigin = 
                          Fun(RefProp#property_reference.classorigin)};
                  Method when is_record(Elt, method) ->
                      Method#method{
                        classorigin = Fun(Method#method.classorigin)};
                  Other ->
                      Other
              end
      end, List).

%% Apply a function to all PROPAGATED attributes in a class definition

map_propagated(Fun, Class) when is_record(Class, class) ->
    Class#class{
      qualifiers = map_propagated(Fun, Class#class.qualifiers),
      properties = map_propagated(Fun, Class#class.properties),
      methods    = map_propagated(Fun, Class#class.methods)};    

map_propagated(Fun, List) when is_list(List) ->
    lists:map(
      fun(Elt) ->
              case Elt of
                  Prop when is_record(Elt, property) ->
                      Prop#property{
                          propagated = Fun(Prop#property.propagated),
                          qualifiers = 
                            map_propagated(Fun, Prop#property.qualifiers)};
                  PropArray when is_record(Elt, property_array) ->
                      PropArray#property_array{
                          propagated = Fun(PropArray#property_array.propagated),
                          qualifiers = map_propagated(
                            Fun, PropArray#property_array.qualifiers)};
                  RefProp when is_record(Elt, property_reference) ->
                      RefProp#property_reference{
                          propagated = 
                            Fun(RefProp#property_reference.propagated),
                          qualifiers = map_propagated(
                            Fun, RefProp#property_reference.qualifiers)};
                  Qual when is_record(Elt, qualifier) ->
                      Qual#qualifier{
                        propagated = Fun(Qual#qualifier.propagated)};
                  Method when is_record(Elt, method) ->
                      Method#method{
                          propagated = Fun(Method#method.propagated),
                          qualifiers = map_propagated(
                                         Fun, Method#method.qualifiers),
                          parameters = map_propagated(
                                         Fun, Method#method.parameters)};
                  Param when is_record(Elt, parameter) ->
                      Param#parameter{
                        qualifiers = map_propagated(
                           Fun, Param#parameter.qualifiers)};
                  RefParam when is_record(Elt, parameter_reference) ->
                      RefParam#parameter_reference{
                        qualifiers = map_propagated(
                           Fun, RefParam#parameter_reference.qualifiers)};
                  ArrayParam when is_record(Elt, parameter_array) ->
                      ArrayParam#parameter_array{
                        qualifiers = map_propagated(
                           Fun, ArrayParam#parameter_array.qualifiers)};
                  RefArrayParam when is_record(Elt, parameter_refarray) ->
                      RefArrayParam#parameter_refarray{
                        qualifiers = map_propagated(
                           Fun, RefArrayParam#parameter_refarray.qualifiers)};
                  Other ->
                      Other
              end
      end, List).

%% Return keys suitable for use by ETS/DETS

class_key(NameSpace, Class) when is_record(Class, class) ->
    {class, NameSpace, string:to_lower(Class#class.name)};

class_key(NameSpace, ClassName) ->
    {class, NameSpace, string:to_lower(ClassName)}.

qualdecl_key(NameSpace, QualifierDecl) 
  when is_record(QualifierDecl, qualifier_declaration) ->
    {qualifier_declaration, NameSpace, 
     QualifierDecl#qualifier_declaration.name};

qualdecl_key(NameSpace, QualDeclName) ->
    {qualifier_declaration, NameSpace, string:to_lower(QualDeclName)}.

%% Return true/false for the existence of a class

class_exists(Table, NameSpace, ClassOrClassName) ->
    case lookup(Table, class_key(NameSpace, ClassOrClassName)) of
        [{_, _}] ->
            true;
        [] ->
            false;
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Initialise a repository instance.  Options allow persistent vs
%% non-persistent storage of repository data, and reading/writing to a
%% file.

init(Options) ->
    Filename = proplists:get_value(file, Options),
    %% Persistence defaults to true when a filename is specified, but is
    %% false otherwise
    Persistent = proplists:get_value(
                   persistent, Options,
                   case Filename of
                       undefined ->
                           false;
                       _ ->
                           true
                   end),
    case {Filename, Persistent} of
        %% No filename and persistent is an error
        {undefined, true} ->
            {stop, "No filename specified for persistent repository"};
        %% No filename and non-persistent is an ets table
        {undefined, false} ->
            Table = ets:new(?MODULE, [set]),
            {ok, #state{storage = ets, table = Table}};
        %% Filename and non-persistent calls ets:from_dets()
        {_, false} ->
            EtsTable = ets:new(?MODULE, [set]),
            {ok, DetsTable} = 
                dets:open_file(
                  ?MODULE, [{type, set}, {file, Filename}]),
            ets:from_dets(EtsTable, DetsTable),
            dets:close(?MODULE),
            {ok, #state{storage = ets, table = EtsTable}};
        %% Filename and persistent is a dets table
        {_, true} -> 
            case dets:open_file(?MODULE, [{type, set}, {file, Filename}]) of
                {ok, Table} ->
                    {ok, #state{storage = dets, table = Table}};
                {error, Reason} ->
                    {stop, Reason}
            end
    end.

handle_call(stop, _From, State) ->
    %% Table close handled in terminate callback
    {stop, normal, stopped, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GetClass

handle_call({getClass, NameSpace, ClassName, LocalOnly, _IncludeQualifiers,
             _IncludeClassOrigin, _PropertyList}, _From, State) ->

    try

        %% Check class exists

         case class_exists(State, NameSpace, ClassName) of
             true ->
                 ok;
             false ->
                 throw(cim_error(?CIM_ERR_NOT_FOUND));
             {error, _} ->
                 throw(cim_error(?CIM_ERR_FAILED))
         end,

        %% Fetch class from repository
        
        ClassList = case LocalOnly of
                        true ->
                            [];
                        false ->
                            internal_get_superclasses(
                              State, NameSpace, ClassName)
                    end ++ [ClassName],
        
        Result = lists:foldl(
                   fun(SubclassName, Acc) ->
                           Key = class_key(NameSpace, SubclassName),
                           case lookup(State, Key) of
                               [{_, Subclass}] ->
                                   Acc#class{
                                     name = Subclass#class.name,
                                     superclass = Subclass#class.superclass,
                                     properties = 
                                     lists:append(Acc#class.properties,
                                                  Subclass#class.properties),
                                     methods =
                                     lists:append(Acc#class.methods,
                                                  Subclass#class.methods),
                                     qualifiers =
                                     lists:append(Acc#class.qualifiers,
                                                  Subclass#class.qualifiers)};
                               {error, _} ->
                                   throw(cim_error(?CIM_ERR_FAILED))
                           end
                   end, 
                   #class{},
                   ClassList),
        
        {reply, {ok, Result}, State}
        
    catch
        {cim_error, ErrorCode} ->
            {reply, {error, ErrorCode}, State}
    end;

%% DeleteClass

handle_call({deleteClass, NameSpace, ClassName}, _From, State) ->
    
    try

        %% Check class exists

        case class_exists(State, NameSpace, ClassName) of
            true ->
                ok;
            false ->
                throw(cim_error(?CIM_ERR_NOT_FOUND));
            {error, _} ->
                throw(cim_error(?CIM_ERR_FAILED))
        end,

        %% Check class has no children

        case internal_get_subclasses(State, NameSpace, ClassName, false) of
            [] ->
                ok;
            _ ->
                throw(cim_error(?CIM_ERR_CLASS_HAS_CHILDREN))
        end,

        %% Delete class from repository
        
        case delete(State, class_key(NameSpace, ClassName)) of
            ok ->
                ok;
            {error, _} ->
                throw(cim_error(?CIM_ERR_FAILED))
        end,

        {reply, ok, State}

    catch
        {cim_error, ErrorCode} ->
            {reply, {error, ErrorCode}, State}
    end;

%% CreateClass

handle_call({createClass, NameSpace, NewClass}, _From, State) ->

    try
        
        %% Check class does not already exists

        case class_exists(State, NameSpace, NewClass#class.name) of
            true ->
                throw(cim_error(?CIM_ERR_ALREADY_EXISTS));
            false ->
                ok;
            {error, _} ->
                throw(cim_error(?CIM_ERR_FAILED))
        end,

        %% Check superclass exists
                 
        case NewClass#class.superclass of
            undefined ->
                ok;
            _ ->
                case lookup(State, 
                            class_key(NameSpace, NewClass#class.superclass)) of
                    [{_, _Superclass}] ->
                        ok;
                    [] ->
                        throw(cim_error(?CIM_ERR_INVALID_SUPERCLASS));
                    {error, _} ->
                        throw(cim_error(?CIM_ERR_FAILED))
                end
        end,
        
        %% Set classorigin to base class and clear propagated
        %% attribute.
        
        Value = map_classorigin(
                  fun(_ClassOrigin) -> 
                          case NewClass#class.superclass of
                              undefined ->
                                  NewClass#class.name;
                              _ ->
                                  undefined
                          end
                  end,
                  map_propagated(fun(_Propagated) -> undefined end, NewClass)),
        
        %% Add class to repository
        
        case insert(State, {class_key(NameSpace, NewClass), Value}) of
            ok ->
                {reply, ok, State};
            {error, Reason} ->
                throw(cim_error(?CIM_ERR_FAILED, Reason))
        end
        
    catch
        {cim_error, ErrorCode} ->
            {reply, {error, ErrorCode}, State}
    end;

%% ModifyClass

handle_call({modifyClass, NameSpace, ModifiedClass}, _From, State) ->
    Key = class_key(NameSpace, ModifiedClass),
    case lookup(State, Key) of
        [{Key, _}] ->
            case insert(State, {Key, ModifiedClass}) of
                ok ->
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
            end;
        [] ->
            Msg = io_lib:format("Class ~s not found", 
                                [ModifiedClass#class.name]),
            {reply, {error, {?CIM_ERR_NOT_FOUND, Msg}}, State};
        {error, Reason} ->
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% EnumerateClasses

handle_call({enumerateClasses, NameSpace, ClassName, DeepInheritance,
             _LocalOnly, _IncludeQualifiers, _IncludeClassOrigin}, 
            _From, State) ->
    ClassNames = 
        internal_get_subclasses(State, NameSpace, ClassName, DeepInheritance),
    Classes = [case lookup(State, class_key(NameSpace, CN)) of
                   [{_, Class}] ->
                       Class
               end || CN <- ClassNames],
    {reply, {ok, Classes}, State};

%% EnumerateClassNames

handle_call({enumerateClassNames, NameSpace, ClassName, DeepInheritance},
            _From, State) ->
    try

        %% Check basis class exists

        case ClassName of
            undefined ->
                ok;
            _ ->
                case lookup(State, class_key(NameSpace, ClassName)) of
                    [{_, _}] ->
                        ok;
                    [] ->
                        throw(cim_error(?CIM_ERR_INVALID_CLASS));
                    {error, _} ->
                        throw(cim_error(?CIM_ERR_FAILED))
                end
        end,

        %% Return enumeration

        ClassNames = internal_get_subclasses(
                       State, NameSpace, ClassName, DeepInheritance),

        {reply, 
         {ok, lists:map(fun(C) -> #classname{name = C} end, ClassNames)},
         State}

    catch
        {cim_error, ErrorCode} ->
            {reply, {error, ErrorCode}, State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% SetQualifier

handle_call({setQualifier, NameSpace, QualifierDeclaration}, _From, State) ->
    Key = qualdecl_key(NameSpace, QualifierDeclaration),
    case insert(State, {Key, QualifierDeclaration}) of
        ok ->
            {reply, ok, State};
        {error, Reason} -> 
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% EnumerateQualifiers

handle_call({enumerateQualifiers, NameSpace}, _From, State) ->
    Result = foldl(
           State,    
	       fun(Object, Acc) -> 
		       case Object of
			   {{qualifier_declaration, NameSpace, _}, Q} -> [Q] ++ Acc;
			   _ -> Acc
		       end
	       end, []),
    {reply, {ok, Result}, State};

%% GetQualifier

handle_call({getQualifier, NameSpace, Name}, _From, State) ->
    case lookup(State, qualdecl_key(NameSpace, Name)) of
        [{_Key, Value}] -> 
            {reply, {ok, Value}, State};
        [] -> 
            Msg = io_lib:format("Qualifier ~s not found", [Name]),
            {reply, {error, {?CIM_ERR_NOT_FOUND, Msg}}, State};
        {error, Reason} -> 
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% DeleteQualifier

handle_call({deleteQualifier, NameSpace, Name}, _From, State) ->
    Key = qualdecl_key(NameSpace, Name),
    case lookup(State, Key) of
        [{Key, _}] -> 
            case delete(State, Key) of
                ok ->
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
            end;
        [] -> 
            Msg = io_lib:format("Qualifier ~s not found", [Name]),
            {reply, {error, {?CIM_ERR_NOT_FOUND, Msg}}, State};
        {error, Reason} -> 
            Msg = io_lib:format("Qualifier lookup failed: ~s", [Reason]),
            {reply, {error, {?CIM_ERR_FAILED, Msg}}, State}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Return subclasses of a class

handle_call({getSubclasses, NameSpace, ClassName, DeepInheritance}, 
            _From, State) ->
    Result = 
        internal_get_subclasses(State, NameSpace, ClassName, DeepInheritance),
    {reply, {ok, Result}, State};
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Return true if a class is a subclass of a base class

handle_call({isa, NameSpace, ClassName, BaseClass}, _From, State) ->
    SubClasses = [BaseClass] ++
        internal_get_subclasses(State, NameSpace, BaseClass, true),
    {reply, {ok, lists:member(ClassName, SubClasses)}, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Any other call is not supported

handle_call(_Msg, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Do nothing implementation of remaining gen_server callbacks

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
