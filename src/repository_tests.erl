-module(repository_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("cim.hrl").

%% Specifying persistence without a filename is an error

no_filename_persistent_test() ->
    ?assertMatch({error, _}, repository:start_link([{persistent, true}])).

%% Test creating multiple repository processes

create_multiple_repositories_test() ->
    {ok, Pid1} = repository:start_link([]),
    {ok, Pid2} = repository:start_link([]),
    ?assert(Pid1 /= Pid2),
    repository:stop(Pid1),
    repository:stop(Pid2).

%% Test persistence settings

nonpersistence_nofile_test() ->
    Options = [],
    {ok, Pid1} = repository:start_link(Options),
    NameSpace = "test",
    ClassName = "CIM_Foo",
    ok = gen_server:call(
           Pid1, {createClass, NameSpace, #class{name = ClassName}}),
    repository:stop(Pid1),
    {ok, Pid2} = repository:start_link(Options),
    {error, _} = 
        gen_server:call(
          Pid2, {getClass, NameSpace, ClassName, true, true, true, []}).
   
nonpersistence_file_test() ->
    Filename = "nonpersistence_file_test.dets",
    Options = [{file, Filename}, {persistent, false}],
    {ok, Pid1} = repository:start_link(Options),
    NameSpace = "test",
    ClassName = "CIM_Foo",
    ok = gen_server:call(
           Pid1, {createClass, NameSpace, #class{name = ClassName}}),
    repository:stop(Pid1),
    {ok, Pid2} = repository:start_link(Options),
    {error, _} = 
        gen_server:call(
          Pid2, {getClass, NameSpace, ClassName, true, true, true, []}),
    file:delete(Filename).
    
persistence_file_test() ->
    Filename = "persistence_file_test.dets",
    Options = [{file, Filename}],
    {ok, Pid1} = repository:start_link(Options),
    NameSpace = "test",
    ClassName = "CIM_Foo",
    Class = #class{name = ClassName},
    ok = gen_server:call(
           Pid1, {createClass, NameSpace, #class{name = ClassName}}),
    repository:stop(Pid1),
    {ok, Pid2} = repository:start_link(Options),
    {ok, Class} = gen_server:call(
                    Pid2, {getClass, NameSpace, ClassName, true, true, 
                           true, []}),
    file:delete(Filename).
    
