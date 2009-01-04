-module(cimomhandle).
-behaviour(gen_server).

-include_lib("cim.hrl").

-export([start_link/1, register_provider/3, unregister_provider/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
          repository,
          providersforclass = []
         }).

-define(SERVER, ?MODULE).

%% Start server

start_link(Options) ->
    Register = proplists:get_value(register, Options, false),
    case Register of
        false ->
            gen_server:start_link(?MODULE, Options, []);
        _ ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, Options, [])
    end.

init(Options) ->
    Pid = case proplists:get_value(repository_pid, Options) of
              undefined ->
                  RepoOptions = proplists:get_value(
                                  repository_options, Options, []),
                  {ok, RepoPid} = repository:start_link(RepoOptions),
                  RepoPid;
              RepoPid ->
                  RepoPid
          end,
    {ok, #state{repository = Pid}}.

stop(Pid) ->
    gen_server:call(Pid, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Client interface

register_provider(NameSpace, ClassName, Module) ->
    gen_server:call(
      cimomhandle, {registerProvider, NameSpace, ClassName, Module}).

unregister_provider(NameSpace, ClassName) ->
    gen_server:call(
      cimomhandle, {unregisterProvider, NameSpace, ClassName}).

%% Look up provider module for namespace:classname

module_for_class(ProvidersForClass, NameSpace, ClassName) ->
    Key = {string:to_lower(NameSpace), string:to_lower(ClassName)},
    proplists:get_value(Key, ProvidersForClass).

module_for_classes(_ProvidersForClass, _NameSpace, []) ->
    [];

module_for_classes(ProvidersForClass, NameSpace, [H|T]) ->
    case module_for_class(ProvidersForClass, NameSpace, H) of
        undefined ->
            module_for_classes(ProvidersForClass, NameSpace, T);
        Module ->
            [{H, Module}] ++ module_for_classes(ProvidersForClass, NameSpace, T)
    end.

%% Return true if a class is an association class

is_assoc_class(Repository, NameSpace, ClassName) ->
    Result = repository:get_class(
               Repository, NameSpace, ClassName, false, true, false, undefined),
    case Result of 
        {error, _} ->
            {error, {?CIM_ERR_INVALID_PARAMETER, undefined}};
        {ok, ClassDef} ->
            ClassQuals = ClassDef#class.qualifiers,
            LowerQuals = lists:map(
                           fun(Qual) -> 
                                   {string:to_lower(Qual#qualifier.name), 
                                    Qual#qualifier.value} end,
                           ClassQuals),
            error_logger:info_msg("Quals = ~p~n", [ClassQuals]),
            case proplists:get_value("association", LowerQuals) of
                undefined ->
                    error_logger:info_msg("~s is NOT assoc~n", [ClassName]),
                    false;
                AssocQual ->
                    X = string:to_lower(AssocQual) == "true",
                    case X of
                        true ->
                            error_logger:info_msg("~s is assoc~n", [ClassName]);
                        false ->
                            error_logger:info_msg("~s is NOT assoc~n", [ClassName])
                    end,
                    X
            end
    end.

%% Return a list of registered association providers

assoc_providers(Repository, ProvidersForClass, NameSpace) ->
    lists:filter(
      fun({{NS, CN}, _}) -> 
              NS == NameSpace andalso is_assoc_class(Repository, NS, CN) end,
      ProvidersForClass).

%% Return list of tuples of {Name, ReferenceClass} for each reference
%% property in a class.

reference_props(Repository, NameSpace, ClassName) ->
    Result = repository:get_class(
               Repository, NameSpace, ClassName, false, true, false, undefined),
    case Result of 
        {error, _} ->
            {error, {?CIM_ERR_INVALID_PARAMETER, undefined}};
        {ok, ClassDef} ->
            Props = ClassDef#class.properties,
            lists:map(
              fun(Prop) ->
                      {Prop#property_reference.name, 
                       Prop#property_reference.referenceclass}
              end,
              lists:filter(
                fun(Prop) -> is_record(Prop, property_reference) end,
                Props))
    end.

%% Map a function across providers registered for any subclasses of
%% ClassName in the given NameSpace.  Return a list of results, or
%% an error tuple for the first error encountered.

map_subclasses(Repository, Fun, ProvidersForClass, NameSpace, ClassName) ->
    %% Get list of classes to fold over
    ClassList = [ClassName] ++ 
        repository:get_subclasses(Repository, NameSpace, ClassName, true),
    %% Get registered providers for class list
    ModuleList = module_for_classes(ProvidersForClass, NameSpace, ClassList),
    %% Call fun on each provider but abort on first error
    {_, Result} = 
        lists:mapfoldr(
          fun({CN, Module}, Acc) ->
                  case Acc of
                      {error, _} ->
                          Acc;
                      _ ->
                          case Fun(NameSpace, CN, Module) of
                              {error, Reason} ->
                                  {dummy, {error, Reason}};
                              {ok, Data} ->
                                  {dummy, Data ++ Acc}
                          end
                  end
          end, 
          [],
          ModuleList),
    case Result of
        {error, _} ->
            Result;
        _ ->
            {ok, Result}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Functions to register/unregister providers.

handle_call({registerProvider, NameSpace, ClassName, Module}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    Pid = providermanager:start(Module, []),
    Value = {{string:to_lower(NameSpace), string:to_lower(ClassName)}, Pid},
    NewProvidersForClass = 
        [Value] ++ proplists:delete(Value, ProvidersForClass),
    {reply, ok, State#state{providersforclass = NewProvidersForClass}};

handle_call({unregisterProvider, NameSpace, ClassName}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    Key = {string:to_lower(NameSpace), string:to_lower(ClassName)},
    NewProvidersForClass = proplists:delete(Key, ProvidersForClass),
    {reply, ok, State#state{providersforclass = NewProvidersForClass}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Basic Read

handle_call({getClass, NameSpace, ClassName, LocalOnly, IncludeQualifiers,
             IncludeClassOrigin, PropertyList}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(
               Repository,
               {getClass, NameSpace, ClassName, LocalOnly, IncludeQualifiers,
                IncludeClassOrigin, PropertyList}),
    {reply, Result, State};

handle_call({enumerateClasses, NameSpace, ClassName, DeepInheritance,
             LocalOnly, IncludeQualifiers, IncludeClassOrigin}, 
            _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(
               Repository,
               {enumerateClasses, NameSpace, ClassName, DeepInheritance,
                LocalOnly, IncludeQualifiers, IncludeClassOrigin}),
    {reply, Result, State};

handle_call({enumerateClassNames, NameSpace, ClassName, DeepInheritance},
            _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(
               Repository,
               {enumerateClassNames, NameSpace, ClassName, DeepInheritance}),
    {reply, Result, State};

handle_call({getInstance, NameSpace, InstanceName, LocalOnly, IncludeQualifiers,
             IncludeClassOrigin, PropertyList}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = InstanceName#instancename.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_FOUND, undefined}};
         ProviderModule ->
             providermanager:call(
               ProviderModule,
               {getInstance, NameSpace, InstanceName, LocalOnly,
                IncludeQualifiers, IncludeClassOrigin, PropertyList})
     end,
     State};

handle_call({enumerateInstances, NameSpace, ClassName, LocalOnly, 
             DeepInheritance, IncludeQualifiers, IncludeClassOrigin, 
             PropertyList}, _From, State) ->
    Repository = State#state.repository,
    ProvidersForClass = State#state.providersforclass,
    Result = map_subclasses(
               Repository,
               fun(NS, CN, Module) ->
                       providermanager:call(
                         Module, {enumerateInstance, NS, CN, LocalOnly,
                                  DeepInheritance, IncludeQualifiers,
                                  IncludeClassOrigin, PropertyList})
               end,
               ProvidersForClass, NameSpace, ClassName),
    {reply, Result, State};

handle_call({enumerateInstanceNames, NameSpace, ClassName}, _From, State) ->
    Repository = State#state.repository,    
    ProvidersForClass = State#state.providersforclass,
    Result = map_subclasses(
               Repository,
               fun(NS, CN, Module) ->
                       providermanager:call(
                         Module, {enumerateInstanceNames, NS, CN}) 
               end,
               ProvidersForClass, NameSpace, ClassName),
    {reply, Result, State};

handle_call({getProperty, _NameSpace, _InstanceName, _PropertyName},
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

%% Pulled Read

%% openEnumerateInstances
%% openEnumerateInstancePaths
%% openReferenceInstances
%% openReferenceInstancePaths
%% openAssociatorInstances
%% openAssociatorPaths
%% pullInstancesWithPath
%% pullInstancePaths
%% closeEnumeration

%% Pulled Read Count

%% enumerationCount

%% Pulled Query Execution

%% openQueryInstances
%% pullInstances

%% Basic Write

handle_call({setProperty, _NameSpace, _InstanceName, _PropertyName, _NewValue},
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

%% Schema Manipulation

handle_call({createClass, NameSpace, NewClass}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(Repository, {createClass, NameSpace, NewClass}),
    {reply, Result, State};

handle_call({modifyClass, NameSpace, ModifiedClass}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call( 
               Repository, {modifyClass, NameSpace, ModifiedClass}),
    {reply, Result, State};

handle_call({deleteClass, NameSpace, ClassName}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(
               Repository, {deleteClass, NameSpace, ClassName}),
    {reply, Result, State};

%% Instance Manipulation

handle_call({createInstance, NameSpace, NewInstance}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = NewInstance#instance.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_SUPPORTED, undefined}};
         ProviderModule ->
             providermanager:call(
               ProviderModule, {createInstance, NameSpace, NewInstance})
     end,
     State};

handle_call({modifyInstance, NameSpace, ModifiedInstance, IncludeQualifiers,
             PropertyList}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = ModifiedInstance#instance.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_SUPPORTED, undefined}};
         ProviderModule ->
             providermanager:call(
               ProviderModule, {modifyInstance, NameSpace, ModifiedInstance,
                                IncludeQualifiers, PropertyList})
     end,
     State};

handle_call({deleteInstance, NameSpace, InstanceName}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = InstanceName#instancename.classname,
    {reply, 
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_SUPPORTED, undefined}};
         ProviderModule ->
             providermanager:call(
               ProviderModule, {deleteInstance, NameSpace, InstanceName})
     end, 
     State};

%% Association Traversal

handle_call({associators, _NameSpace, _ObjectName, _AssocClass, _ResultClass, 
             _Role, _ResultRole, _IncludeQualifiers, _IncludeClassOrigin, 
             _PropertyList}, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};
    
handle_call({associatorNames, NameSpace, ObjectName, _AssocClass, 
             _ResultClass, _Role, _ResultRole}, _From, State) ->
    Repository = State#state.repository,
    ProvidersForClass = State#state.providersforclass,
    %% Return association providers that have a reference property of
    %% ObjectName's class or subclass.
    ClassName = ObjectName#instancename.classname,
    ClassList = 
        [ClassName] ++ repository:get_subclasses(
                         Repository, NameSpace, ClassName, true),
    Providers = 
        lists:filter(
          fun({{NS, CN}, Module}) -> 
                  error_logger:info_msg("checking = ~p~n", [CN]),
                  lists:member(CN, ClassList)
          end,
          assoc_providers(Repository, ProvidersForClass, NameSpace)),
    error_logger:info_msg("providers = ~p~n", [Providers]),
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

handle_call({references, _NameSpace, _ObjectName, _ResultClass, _Role, 
             _IncludeQualifiers, _IncludeClassOrigin, _PropertyList}, 
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

handle_call({referenceNames, _NameSpace, _ObjectName, _ResultClass, _Role},
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

%% Query Execution

handle_call({execQuery, _QueryLanguage, _Query}, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED, undefined}}, State};

%% Qualifier Declaration

handle_call({getQualifier, NameSpace, Name}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(Repository, {getQualifier, NameSpace, Name}),
    {reply, Result, State};

handle_call({setQualifier, NameSpace, QualifierDeclaration}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(
               Repository, {setQualifier, NameSpace, QualifierDeclaration}),
    {reply, Result, State};

handle_call({deleteQualifier, NameSpace, Name}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(Repository, {deleteQualifier, NameSpace, Name}),
    {reply, Result, State};

handle_call({enumerateQualifiers, NameSpace}, _From, State) ->
    Repository = State#state.repository,
    Result = gen_server:call(Repository, {enumerateQualifiers, NameSpace}),
    {reply, Result, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

%% Do nothing implementation of remaining gen_server callbacks

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
