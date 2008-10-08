-module(griffin_test).
-include_lib("eunit/include/eunit.hrl").

griffin_test_() ->
    [{module, mod_wbem_test},
     {module, cimxml_parse_test}].
