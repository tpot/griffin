-module(cimom_test).

-include_lib("eunit/include/eunit.hrl").

demo_test_() ->
    {foreach,
     fun() -> 
             {ok, Repository} = repository:start_link([]),
             {ok, CIMOMHandle} = cimomhandle:start_link(Repository),
             {Repository, CIMOMHandle}
     end,
     fun({Repository, CIMOMHandle}) ->
             cimomhandle:stop(CIMOMHandle),
             repository:stop(Repository)
     end,
     [fun({_Repository, _CIMOMHandle}) ->
              ?_assertEqual(1, 1)
      end]}.
