-module(repository_tests).

-include_lib("eunit/include/eunit.hrl").

%% Specifying persistence without a filename is an error

no_filename_persistent_test() ->
    ?assertMatch({error, _}, repository:start_link([{persistent, true}])).

%% Test creating multiple repository processes

create_multiple_repositories_test_() ->
    {foreach,
     fun() -> 
             {ok, Pid1} = repository:start_link([]),
             {ok, Pid2} = repository:start_link([]),
             {Pid1, Pid2} 
     end,
     fun({Pid1, Pid2}) ->
             repository:stop(Pid1),
             repository:stop(Pid2)
     end,
     [fun({Pid1, Pid2}) -> ?_assert(Pid1 /= Pid2) end]}.
