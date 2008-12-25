-module(mod_wbem_cimxml_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test empty request body

empty_cimxml_request() ->
    {Status, Headers, _Body} = mod_wbem_cimxml:do([], ""),
    ?assertEqual(Status, 400),
    ?assertEqual(
       proplists:get_value("CIMError", Headers), "request-not-well-formed").

%% Test badly formed XML request

badly_formed_cimxml_request() ->
    {Status, Headers, _Body} = mod_wbem_cimxml:do([], "foo"),
    ?assertEqual(Status, 400),
    ?assertEqual(
       proplists:get_value("CIMError", Headers), "request-not-well-formed").

%% Test well-formed XML but invalid CIM-XML request

invalid_cimxml_request() ->
    {Status, Headers, _Body} = mod_wbem_cimxml:do([], "<CIM/>"),
    ?assertEqual(Status, 400),
    ?assertEqual(proplists:get_value("CIMError", Headers), "request-not-valid"),
    ?_assert(proplists:get_value("GriffinError", Headers) /= undefined).

%% Test suite

run_test_() ->
    [{"Empty CIM-XML request", ?_test(empty_cimxml_request())},
     {"Badly formed CIM-XML request", ?_test(badly_formed_cimxml_request())},
     {"Invalid CIM-XML request", ?_test(invalid_cimxml_request())}].
