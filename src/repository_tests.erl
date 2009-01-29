-module(repository_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("cim.hrl").

%% Temporary version of get_class/3 client function

get_class(Pid, NameSpace, ClassName) ->
    gen_server:call(
      Pid, {getClass, NameSpace, ClassName, true, true, false, []}).

%% Specifying persistence without a filename is an error

no_filename_persistent_test() ->
    ?assertMatch({error, _}, repository:start_link([{persistent, true}])).

%% Test creating multiple repository processes

create_multiple_repositories_test_() ->
    {foreach,
     fun() -> 
             Options = [],
             {ok, Pid1} = repository:start_link(Options),
             {ok, Pid2} = repository:start_link(Options),
             {Pid1, Pid2}
     end,
     fun({Pid1, Pid2}) ->
             repository:stop(Pid1),
             repository:stop(Pid2)
     end,
     [fun({Pid1, Pid2}) -> ?_assert(Pid1 /= Pid2) end]
    }.

%% Test persistence settings

nonpersistence_nofile_test_() ->
    {foreach,
     fun() -> 
             Options = [],
             {ok, Pid1} = repository:start_link(Options),
             {ok, Pid2} = repository:start_link(Options),
             {Pid1, Pid2}
     end,
     fun({Pid1, Pid2}) ->
             repository:stop(Pid1),
             repository:stop(Pid2)
     end,
     [fun({Pid1, Pid2}) ->
              NameSpace = "test",
              ClassName = "CIM_Foo",
              [?_assertEqual(
                  ok, 
                  repository:create_class(
                    Pid1, NameSpace, #class{name = ClassName})),
               ?_assertMatch(
                  {error, _},
                  get_class(Pid2, NameSpace, ClassName))]
      end]}.

nonpersistence_file_test_() ->
    {foreach,
     fun() ->
             Filename = "nonpersistence_file_test.dets",
             Options = [{file, Filename}, {persistent, false}],
             {ok, Pid1} = repository:start_link(Options),
             {ok, Pid2} = repository:start_link(Options),
             {Filename, Pid1, Pid2}
     end,
     fun({Filename, Pid1, Pid2}) ->
             repository:stop(Pid1),
             repository:stop(Pid2),
             file:delete(Filename)
     end,
     [fun({_Filename, Pid1, Pid2}) ->
              NameSpace = "test",
              ClassName = "CIM_Foo",
              [?_assertEqual(
                  ok,
                  repository:create_class(
                    Pid1, NameSpace, #class{name = ClassName})),
               ?_assertMatch(
                  {error, _},
                  get_class(Pid2, NameSpace, ClassName))]
      end]}.
    
persistence_file_test_() ->
    {foreach,
     fun() ->
             Filename = "persistence_file_test.dets",
             Options = [{file, Filename}],
             {ok, Pid1} = repository:start_link(Options),
             {ok, Pid2} = repository:start_link(Options),
             {Filename, Pid1, Pid2}
     end,
     fun({Filename, Pid1, Pid2}) ->
             repository:stop(Pid1),
             repository:stop(Pid2),
             file:delete(Filename)
     end,
     [fun({_Filename, Pid1, Pid2}) ->
              NameSpace = "test",
              ClassName = "CIM_Foo",
              Class = #class{name = ClassName},
              [?_assertEqual(
                  ok,
                  repository:create_class(
                    Pid1, NameSpace, #class{name = ClassName})),
               ?_assertMatch(
                  {ok, Class},
                  get_class(Pid2, NameSpace, ClassName))]
      end]}.

%% Test EnumerateClassNames operation

enumerate_class_names_test_() ->
    NS = "test",
    ClassNameFoo = "CIM_Foo",
    ClassNameBar = "CIM_Bar",
    ClassNameBaz = "CIM_Baz",
    ClassFoo = #class{name = ClassNameFoo},
    ClassBar = #class{name = ClassNameBar, superclass = ClassNameFoo},
    ClassBaz = #class{name = ClassNameBaz, superclass = ClassNameBar},
    {foreach,
     fun() ->
             Options = [],
             {ok, Pid} = repository:start_link(Options),
             ok = repository:create_class(Pid, NS, ClassFoo),
             ok = repository:create_class(Pid, NS, ClassBar),
             ok = repository:create_class(Pid, NS, ClassBaz),
             Pid
     end,
     fun(Pid) ->
             repository:stop(Pid)
     end,
     [fun(Pid) -> 

              %% Test 2-ary function (ClassName = null, DeepInheritance = false)

              [{"enumerate_class_names/2",
                ?_assertEqual(
                   [ClassNameFoo],
                   repository:enumerate_class_names(Pid, NS))},

               %% Test 3-ary function (DeepInheritance = false)

               {"enumerate_class_names/3 - foo",
                ?_assertEqual(
                   [ClassNameBar],
                   repository:enumerate_class_names(Pid, NS, ClassNameFoo))},

               {"enumerate_class_names/3 - bar",
                ?_assertEqual(
                   [ClassNameBaz],
                   repository:enumerate_class_names(Pid, NS, ClassNameBar))},

               %% Test 4-ary function (all combos of ClassName = null/non-null
               %% and DeepInheritance = true/false)

               {"enumerate_class_names/4 - undefined, true",
                ?_assertEqual(
                   lists:sort([ClassNameFoo, ClassNameBar, ClassNameBaz]),
                   lists:sort(
                     repository:enumerate_class_names(
                       Pid, NS, undefined, true)))},

               {"enumerate_class_names/4 - undefined, false",
                ?_assertEqual(
                   [ClassNameFoo], 
                   repository:enumerate_class_names(
                     Pid, NS, undefined, false))},

               {"enumerate_class_names/4 - foo, true",
                ?_assertEqual(
                   [ClassNameBar, ClassNameBaz],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameFoo, true))},

               {"enumerate_class_names/4 - foo, false",
                ?_assertEqual(
                   [ClassNameBar],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameFoo, false))},

               {"enumerate_class_names/4 - bar, true",
                ?_assertEqual(
                   [ClassNameBaz],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameBar, true))},

               {"enumerate_class_names/4 - bar, false",
                ?_assertEqual(
                   [ClassNameBaz],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameBar, false))},

               {"enumerate_class_names/4 - baz, true",
                ?_assertEqual(
                   [],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameBaz, true))},

               {"enumerate_class_names/4 - baz, false",
                ?_assertEqual(
                   [],
                   repository:enumerate_class_names(
                     Pid, NS, ClassNameBaz, true))}]
      end]}.

%% Test CreateClass operation

create_class_test_() ->
    NS = "test",
    {foreach,
     fun() ->
             {ok, Pid} = repository:start_link([]),
           Pid
     end,
     fun(Pid) ->
             repository:stop(Pid)
     end,
     [fun(Pid) ->

              %% Any CLASSORIGIN or PROPAGATED attributes in the new
              %% class must be ignored by the server.
              
              ClassName = "Griffin_Test",

              Qual = #qualifier{name = "Qual", value = "Foo"},

              Prop = #property{name = "Prop", qualifiers = [Qual]},
              PropArray = #property_array{
                name = "ArrayProp", qualifiers = [Qual]},
              PropRef = #property_reference{
                name = "RefProp", qualifiers = [Qual]},

              Param = #parameter{
                name = "Param", qualifiers = [Qual]},
              RefParam = #parameter_reference{
                name = "RefParam", qualifiers = [Qual]},
              ArrayParam = #parameter_array{
                name = "ArrayParam", qualifiers = [Qual]},
              RefArrayParam = #parameter_refarray{
                name = "RefArrayParam", qualifiers = [Qual]},

              Meth = #method{
                name = "Meth", qualifiers = [Qual],
                parameters = [Param, RefParam, ArrayParam, RefArrayParam]},

              {"Ignore CLASSORIGIN and PROPAGATED attributes in create_class",

               ?_assertEqual(
                  {ok, #class{
                     name = ClassName,
                     qualifiers = [Qual],
                     properties = [
                       Prop#property{classorigin = ClassName}, 
                       PropArray#property_array{classorigin = ClassName}, 
                       PropRef#property_reference{classorigin = ClassName}],
                     methods = [
                       Meth#method{
                         classorigin = ClassName,
                         parameters = 
                           [Param, RefParam, ArrayParam, RefArrayParam]}]}},
                  begin

                      %% Create class with classorigin and propagated
                      %% attributes set to invalid values.

                      Class = 
                          #class{
                            name = ClassName,
                            qualifiers = [Qual#qualifier{propagated = "bar"}],
                            properties = 
                              [Prop#property{
                                 classorigin = "foo",
                                 propagated = "bar",
                                 qualifiers = 
                                 [Qual#qualifier{propagated = "bar"}]},
                               PropArray#property_array{
                                 classorigin = "foo",
                                 propagated = "bar",
                                 qualifiers =
                                 [Qual#qualifier{propagated = "bar"}]},
                               PropRef#property_reference{
                                 classorigin = "foo",
                                 propagated = "bar",
                                 qualifiers = 
                                 [Qual#qualifier{propagated = "bar"}]}],
                             methods = 
                               [Meth#method{
                                  classorigin = "foo",
                                  propagated = "bar",
                                  parameters = 
                                    [Param#parameter{qualifiers = 
                                      [Qual#qualifier{propagated = "bar"}]},
                                     RefParam#parameter_reference{qualifiers = 
                                      [Qual#qualifier{propagated = "bar"}]},
                                     ArrayParam#parameter_array{qualifiers =
                                      [Qual#qualifier{propagated = "bar"}]},
                                     RefArrayParam#parameter_refarray{
                                       qualifiers = 
                                         [Qual#qualifier{
                                            propagated = "bar"}]}]}]},
                      repository:create_class(Pid, NS, Class),
                      get_class(Pid, NS, ClassName)
                  end)}
      end,
      
      fun(Pid) ->
              
              %% If class has no superclass, all properties and
              %% methods of the new class have a CLASSORGIN attribute
              %% whose value is the name of the new class.
              
              ClassName = "Griffin_Test",

              Prop = #property{name = "Prop"},
              PropArray = #property_array{name = "ArrayProp"},
              PropRef = #property_reference{name = "RefProp"},
              Meth = #method{name = "Meth"},

              {"CLASSORIGIN attributes set on base class in create_class",
               
               ?_assertEqual(
                  {ok, #class{
                     name = ClassName,
                     properties = 
                     [Prop#property{classorigin = ClassName},
                      PropArray#property_array{classorigin = ClassName},
                      PropRef#property_reference{classorigin = ClassName}],
                     methods = [Meth#method{classorigin = ClassName}]}},
                  begin
                      Class =
                          #class{name = ClassName,
                                 properties = [Prop, PropArray, PropRef],
                                 methods = [Meth]},
                      repository:create_class(Pid, NS, Class),
                      get_class(Pid, NS, ClassName)
                  end)}
      end,
      
      fun(Pid) ->

              %% If new class has a superclass it must exist.

              ClassName = "Griffin_Test",

              {"Superclass must exist",
               
               ?_assertMatch(
                  {error, {?CIM_ERR_INVALID_SUPERCLASS, _}},
                  begin
                      Class = #class{name = ClassName, superclass = "Foo"},
                      repository:create_class(Pid, NS, Class)
                  end)}
      end,
     
     fun(Pid) ->
            
            %% Check class does not already exists

            ClassName = "Griffin_Test",

            {"Class does not already exist",

             ?_assertMatch(
               {error, {?CIM_ERR_ALREADY_EXISTS, _}},
               begin
                  Class = #class{name = ClassName},
                  repository:create_class(Pid, NS, Class),
                  repository:create_class(Pid, NS, Class)
               end)}
     end]}.

%% Test DeleteClass operation

delete_class_test_() ->
    NS = "test",
    {foreach,
     fun() ->
             {ok, Pid} = repository:start_link([]),
           Pid
     end,
     fun(Pid) ->
             repository:stop(Pid)
     end,
     [fun(Pid) ->
              
              %% Test class to be deleted does not exist

              ClassName = "Griffin_Test",

              {"Class must exist",

               ?_assertMatch(
                  {error, {?CIM_ERR_NOT_FOUND, _}},
                  repository:delete_class(Pid, NS, ClassName)
                 )}
      end,
      fun(Pid) ->

              %% Test deleting a class with children fails

              ClassName = "Griffin_Test",
              SubclassName = "Griffin_Test2",

              Class = #class{name = ClassName},
              SubClass = #class{name = SubclassName, superclass = ClassName},

              {"Deleting a class with subclasses fails",

               ?_assertMatch(
                  {error, {?CIM_ERR_CLASS_HAS_CHILDREN, _}},
                  begin
                      repository:create_class(Pid, NS, Class),
                      repository:create_class(Pid, NS, SubClass),
                      repository:delete_class(Pid, NS, ClassName)
                  end)}
      end]}.
                  
                  

              
