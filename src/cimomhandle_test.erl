-module(cimomhandle_test).

-include_lib("eunit/include/eunit.hrl").

%% Create a cimomhandle test spec from a function and list of tests.
%% The test creates a cimomhandle and repository process, runs the
%% tests, then tears the cimomhandle and repository down.

make_testspec(Fun, Test) ->
    {foreach,
     %% Setup
     fun() -> 
             %% Start repository and cimomhandle 
             {ok, Repository} = repository:start_link(),
             {ok, CIMOMHandle} = cimomhandle:start_link(Repository),
             %% Execute setup function
             Fun(CIMOMHandle),
             %% Return process IDs
             {Repository, CIMOMHandle}
     end,
     %% Teardown
     fun({Repository, CIMOMHandle}) ->
             cimomhandle:stop(CIMOMHandle),
             repository:stop(Repository)
     end,
     %% Run test
     [fun({_Repository, CIMOMHandle}) -> Test(CIMOMHandle) end]}.

getclass_test_() ->
    ClassName = "CIM_A",
    ClassDef = {class,ClassName,
                undefined,
                [],
                [{property,"A",ClassName,undefined,"string",[],undefined}],
                []},
    make_testspec(
      fun(CIMOMHandle) -> 
              gen_server:call(CIMOMHandle, 
                              {createClass, "root/cimv2", ClassDef})
      end,
      fun(CIMOMHandle) ->
               ?_test(begin
                          {ok, Class} = gen_server:call(
                                          CIMOMHandle, 
                                          {getClass, "root/cimv3", ClassName, false, true, true, undefined}),
                          ?_assertEqual(ClassDef, Class)
                      end)
       end).
