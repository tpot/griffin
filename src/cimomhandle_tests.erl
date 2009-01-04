-module(cimomhandle_tests).

-include_lib("eunit/include/eunit.hrl").

%% Create a cimomhandle test spec from a function and list of tests.
%% The test creates a cimomhandle and repository process, runs the
%% test, then tears the cimomhandle and repository down.

make_testspec(Setup, Test) ->
    {foreach,
     %% Setup
     fun() -> 
             %% Start repository and cimomhandle 
             {ok, Repository} = repository:start_link([]),
             {ok, CIMOMHandle} = cimomhandle:start_link(Repository),
             %% Execute setup function
             Setup(CIMOMHandle),
             %% Return process IDs
             {Repository, CIMOMHandle}
     end,
     %% Teardown
     fun({Repository, CIMOMHandle}) ->
             cimomhandle:stop(CIMOMHandle),
             repository:stop(Repository)
     end,
     %% Run test
     [fun({_Repository, CIMOMHandle}) -> ?_test(Test(CIMOMHandle)) end]}.

getclass_test_() ->
    ClassName = "CIM_A",
    ClassDef = {class,ClassName,
                undefined,
                [],
                [{property,"A",ClassName,undefined,"string",[],undefined}],
                []},
    make_testspec(
      %% Create a class
      fun(CIMOMHandle) -> 
              gen_server:call(CIMOMHandle, 
                              {createClass, "root/cimv2", ClassDef})
      end,
      %% Check class against definition
      fun(CIMOMHandle) ->
              {ok, Class} = gen_server:call(
                              CIMOMHandle, 
                              {getClass, "root/cimv2", ClassName, 
                               false, true, true, undefined}),
              ?_assertEqual(ClassDef, Class)
      end).
