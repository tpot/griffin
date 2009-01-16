-module(repository).
-behaviour(gen_server).

%% client interface

-export([start_link/1, get_subclasses/4, isa/4, get_class/7, stop/1,
         create_class/3,
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

handle_call({getClass, NameSpace, ClassName, _LocalOnly, _IncludeQualifiers,
             _IncludeClassOrigin, _PropertyList}, _From, State) ->
    error_logger:info_msg("getClass ~s:~s~n", [NameSpace, ClassName]),
    Key = {class, NameSpace, string:to_lower(ClassName)},
    case lookup(State, Key) of
        [{_, Class}] ->
            {reply, {ok, Class}, State};
        [] ->
            Msg = io_lib:format("Class ~s not found", [ClassName]),
            {reply, {error, {?CIM_ERR_NOT_FOUND, Msg}}, State};
        {error, Reason} ->
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% DeleteClass

handle_call({deleteClass, NameSpace, ClassName}, _From, State) ->
    error_logger:info_msg("deleteClass ~s:~s~n", [NameSpace, ClassName]),
    Key = {class, NameSpace, string:to_lower(ClassName)},
    case lookup(State, Key) of
        [{Key, _}] ->
            case delete(State, Key) of
                ok ->
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
            end;
        [] ->
            Msg = io_lib:format("Classname ~s not found", [ClassName]),
            {reply, {error, {?CIM_ERR_NOT_FOUND, Msg}}, State};
        {error, Reason} ->
            Msg = io_lib:format("Classname lookup failed: ~s", [Reason]),
            {reply, {error, {?CIM_ERR_FAILED, Msg}}, State}
    end;

%% CreateClass

handle_call({createClass, NameSpace, NewClass}, _From, State) ->
    error_logger:info_msg("createClass ~s:~s~n", 
                          [NameSpace, NewClass#class.name]),
    Key = {class, NameSpace, string:to_lower(NewClass#class.name)},
    Value = map_classorigin(
              fun(_ClassOrigin) -> undefined end, 
              map_propagated(
                fun(_Propagated) -> undefined end,
                NewClass)),
    case insert(State, {Key, Value}) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% ModifyClass

handle_call({modifyClass, NameSpace, ModifiedClass}, _From, State) ->
    error_logger:info_msg("modifyClass ~s:~s~n",
                          [NameSpace, ModifiedClass#class.name]),
    Key = {class, NameSpace, string:to_lower(ModifiedClass#class.name)},
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
    error_logger:info_msg("enumerateClasses ~s:~s Deep=~s~n", 
                          [NameSpace, ClassName, DeepInheritance]),
    ClassNames = 
        internal_get_subclasses(State, NameSpace, ClassName, DeepInheritance),
    Classes = [case lookup(State, {class, NameSpace, string:to_lower(CN)}) of
                   [{_, Class}] ->
                       Class
               end || CN <- ClassNames],
    {reply, {ok, Classes}, State};

%% EnumerateClassNames

handle_call({enumerateClassNames, NameSpace, ClassName, DeepInheritance},
            _From, State) ->
    error_logger:info_msg("enumerateClassNames ~s:~s:~p~n", 
                          [NameSpace, ClassName, DeepInheritance]),
    ClassNames = 
        internal_get_subclasses(State, NameSpace, ClassName, DeepInheritance),
    {reply, 
     {ok, lists:map(fun(C) -> #classname{name = C} end, ClassNames)},
     State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% SetQualifier

handle_call({setQualifier, NameSpace, QualifierDeclaration}, _From, State) ->
    error_logger:info_msg(
      "setQualifier ~s:~s~n", 
      [NameSpace, QualifierDeclaration#qualifier_declaration.name]),
    Key = {qualifier_declaration, 
           NameSpace, 
           string:to_lower(QualifierDeclaration#qualifier_declaration.name)},
    case insert(State, {Key, QualifierDeclaration}) of
        ok ->
            {reply, ok, State};
        {error, Reason} -> 
            {reply, {error, {?CIM_ERR_FAILED, Reason}}, State}
    end;

%% EnumerateQualifiers

handle_call({enumerateQualifiers, NameSpace}, _From, State) ->
    error_logger:info_msg("enumerateQualifiers ~s~n", [NameSpace]),
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
    error_logger:info_msg("getQualifier ~s:~s~n", [NameSpace, Name]),
    case lookup(
           State, 
           {qualifier_declaration, NameSpace, string:to_lower(Name)}) of
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
    error_logger:info_msg("deleteQualifier ~s:~s~n", [NameSpace, Name]),
    Key = {qualifier_declaration, NameSpace, string:to_lower(Name)},
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
    error_logger:info_msg("getSubclasses ~s:~s~n", [NameSpace, ClassName]),
    Result = 
        internal_get_subclasses(State, NameSpace, ClassName, DeepInheritance),
    {reply, {ok, Result}, State};
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Return true if a class is a subclass of a base class

handle_call({isa, NameSpace, ClassName, BaseClass}, _From, State) ->
    error_logger:info_msg("isa ~s:~s ~s~n", [NameSpace, ClassName, BaseClass]),
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
