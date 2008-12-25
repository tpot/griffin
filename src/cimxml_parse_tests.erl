-module(cimxml_parse_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test invalid CIM-XML request

invalid_request() ->
    ?assertException(
       throw, {error, {parse_error, _}}, cimxml_parse:string("<CIM/>")).

%% Test valid CIM-XML request

-define(ECN, "<CIM CIMVERSION=\"2.0\" DTDVERSION=\"2.0\"><MESSAGE ID=\"1001\" PROTOCOLVERSION=\"1.0\"><SIMPLEREQ><IMETHODCALL NAME=\"EnumerateClassNames\"><LOCALNAMESPACEPATH><NAMESPACE NAME=\"root\"/><NAMESPACE NAME=\"cimv2\"/></LOCALNAMESPACEPATH></IMETHODCALL></SIMPLEREQ></MESSAGE></CIM>").

valid_request() ->
    cimxml_parse:string(?ECN).

%% Test suite

run_test_() ->
    [{"Invalid request", ?_test(invalid_request())},
     {"Enumerate class names", ?_test(valid_request())}].
