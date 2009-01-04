-module(griffin_tests).
-include_lib("eunit/include/eunit.hrl").

griffin_test_() ->
    [{module, repository},
     {module, mod_wbem_cimxml},
     {module, mod_wbem_wsman},
     {module, cimxml_parse},
     {module, cimomhandle}].
