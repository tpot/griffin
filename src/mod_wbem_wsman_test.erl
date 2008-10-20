-module(mod_wbem_wsman_test).
-include_lib("eunit/include/eunit.hrl").

%% Test empty request body

empty_wsman_request() ->
    {Status, _Headers, _Body} = mod_wbem_wsman:do([], ""),
    ?assertEqual(Status, 500).

%% Test badly formed XML request

badly_formed_wsman_request() ->
    {Status, _Headers, _Body} = mod_wbem_wsman:do([], "foo"),
    ?assertEqual(Status, 500).

%% Test suite

run_test_() ->
    [{"Empty WS-Man request", ?_test(empty_wsman_request())},
     {"Badly formed WS-Man request", ?_test(badly_formed_wsman_request())}].

