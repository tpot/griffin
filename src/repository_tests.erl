-module(repository_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("cim.hrl").

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
                  gen_server:call(
                    Pid2, 
                    {getClass, NameSpace, ClassName, true, true, true, []}))]
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
                  gen_server:call(
                    Pid2, 
                    {getClass, NameSpace, ClassName, true, true, true, []}))]
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
                  gen_server:call(
                    Pid2, 
                    {getClass, NameSpace, ClassName, true, true, true, []}))]
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

get_class(Pid, NameSpace, ClassName) ->
    gen_server:call(
      Pid, {getClass, NameSpace, ClassName, true, true, false, []}).

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
              
              {"Ignore CLASSORIGIN and PROPAGATED attributes in create_class",

               ?_assertEqual(
                  {ok, #class{
                     name = ClassName,
                     properties = [#property{name = "Prop"},
                                   #property_array{name = "ArrayProp"},
                                   #property_reference{name = "RefProp"}],
                     methods = [#method{name = "Meth"}]}},
                  begin
                      Class = 
                          #class{name = ClassName,
                                 properties = [
                                     #property{name = "Prop",
                                               classorigin = "blah",
                                               propagated = "True"},
                                     #property_array{name = "ArrayProp",
                                                     classorigin = "blah",
                                                     propagated = "True"},
                                     #property_reference{name = "RefProp",
                                                         classorigin = "blah",
                                                         propagated = "True"}],
                                methods = [
                                     #method{name = "Meth",
                                             classorigin = "blah",
                                             propagated = "True"}]},

                      repository:create_class(Pid, NS, Class),
                      get_class(Pid, NS, ClassName)
                  end)}
      end]}.
