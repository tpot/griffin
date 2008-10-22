-module(cimomhandle).
-behaviour(gen_server).

-include_lib("cim.hrl").

-export([start_link/0, register_provider/3, unregister_provider/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
          providersforclass = []
         }).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).    

init([]) ->
    {ok, #state{}}.

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
    Result = gen_server:call(
               repository,
               {getClass, NameSpace, ClassName, LocalOnly, IncludeQualifiers,
                IncludeClassOrigin, PropertyList}),
    {reply, Result, State};

handle_call({enumerateClasses, NameSpace, ClassName, DeepInheritance,
             LocalOnly, IncludeQualifiers, IncludeClassOrigin}, 
            _From, State) ->
    Result = gen_server:call(
               repository,
               {enumerateClasses, NameSpace, ClassName, DeepInheritance,
                LocalOnly, IncludeQualifiers, IncludeClassOrigin}),
    {reply, Result, State};

handle_call({enumerateClassNames, NameSpace, ClassName, DeepInheritance},
            _From, State) ->
    Result = gen_server:call(
               repository,
               {enumerateClassNames, NameSpace, ClassName, DeepInheritance}),
    {reply, Result, State};

handle_call({getInstance, NameSpace, InstanceName, LocalOnly, IncludeQualifiers,
             IncludeClassOrigin, PropertyList}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = InstanceName#instancename.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_FOUND}};
         ProviderModule ->
             gen_server:call(
               ProviderModule,
               {getInstance, NameSpace, ClassName, LocalOnly,
                IncludeQualifiers, IncludeClassOrigin, PropertyList})
     end,
     State};

handle_call({enumerateInstances, NameSpace, InstanceName, LocalOnly, 
             DeepInheritance, IncludeQualifiers, IncludeClassOrigin, 
             PropertyList}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = InstanceName#instancename.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {ok, []};
         ProviderModule ->
             providermanager:call(
               ProviderModule,
               {enumerateInstances, NameSpace, ClassName, LocalOnly,
                DeepInheritance, IncludeQualifiers, IncludeClassOrigin,
                PropertyList})
     end,
     State};

handle_call({enumerateInstanceNames, NameSpace, ClassName}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    {reply,
     case get_module(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {ok, []};
         ProviderModule ->
             providermanager:call(
               ProviderModule, {enumerateInstanceNames, NameSpace, ClassName})
     end,
     State};

handle_call({getProperty, _NameSpace, _InstanceName, _PropertyName},
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

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
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

%% Schema Manipulation

handle_call({createClass, NameSpace, NewClass}, _From, State) ->
    Result = gen_server:call(repository, {createClass, NameSpace, NewClass}),
    {reply, Result, State};

handle_call({modifyClass, NameSpace, ModifiedClass}, _From, State) ->
    Result = gen_server:call( 
               repository, {modifyClass, NameSpace, ModifiedClass}),
    {reply, Result, State};

handle_call({deleteClass, NameSpace, ClassName}, _From, State) ->
    Result = gen_server:call(
               repository, {deleteClass, NameSpace, ClassName}),
    {reply, Result, State};

%% Instance Manipulation

handle_call({createInstance, NameSpace, NewInstance}, _From, State) ->
    ProvidersForClass = State#state.providersforclass,
    ClassName = NewInstance#instance.classname,
    {reply,
     case module_for_class(ProvidersForClass, NameSpace, ClassName) of
         undefined ->
             {error, {?CIM_ERR_NOT_SUPPORTED}};
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
             {error, {?CIM_ERR_NOT_SUPPORTED}};
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
             {error, {?CIM_ERR_NOT_SUPPORTED}};
         ProviderModule ->
             providermanager:call(
               ProviderModule, {deleteInstance, NameSpace, InstanceName})
     end, 
     State};

%% Association Traversal

handle_call({associators, _NameSpace, _ObjectName, _AssocClass, _ResultClass, 
             _Role, _ResultRole, _IncludeQualifiers, _IncludeClassOrigin, 
             _PropertyList}, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};
    
handle_call({associatorNames, _NameSpace, _ObjectName, _AssocClass, 
             _ResultClass, _Role, _ResultRole}, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

handle_call({references, _NameSpace, _ObjectName, _ResultClass, _Role, 
             _IncludeQualifiers, _IncludeClassOrigin, _PropertyList}, 
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

handle_call({referenceNames, _NameSpace, _ObjectName, _ResultClass, _Role},
            _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

%% Query Execution

handle_call({execQuery, _QueryLanguage, _Query}, _From, State) ->
    {reply, {error, {?CIM_ERR_NOT_SUPPORTED}}, State};

%% Qualifier Declaration

handle_call({getQualifier, NameSpace, Name}, _From, State) ->
    Result = gen_server:call(repository, {getQualifier, NameSpace, Name}),
    {reply, Result, State};

handle_call({setQualifier, NameSpace, QualifierDeclaration}, _From, State) ->
    Result = gen_server:call(
               repository, {setQualifier, NameSpace, QualifierDeclaration}),
    {reply, Result, State};

handle_call({deleteQualifier, NameSpace, Name}, _From, State) ->
    Result = gen_server:call(repository, {deleteQualifier, NameSpace, Name}),
    {reply, Result, State};

handle_call({enumerateQualifiers, NameSpace}, _From, State) ->
    Result = gen_server:call(repository, {enumerateQualifiers, NameSpace}),
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
