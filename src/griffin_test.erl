-module(griffin_test).
-include_lib("eunit/include/eunit.hrl").

griffin_test_() ->
    [{module, mod_wbem_cimxml_test},
     {module, mod_wbem_wsman_test},
     {module, cimxml_parse_test},
     {module, cimomhandle_test}].
