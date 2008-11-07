%% XML namespaces

-define(s, 'http://www.w3.org/2003/05/soap-envelope').
-define(wsman, 'http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd').
-define(wsa, 'http://schemas.xmlsoap.org/ws/2004/08/addressing').

%% Action URIs

-define(ActionGet, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Get').
-define(ActionPut, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Put').
-define(ActionCreate, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Create').
-define(ActionDelete, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Delete').
-define(ActionEnumerate, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Enumerate').
-define(ActionPull, 'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Pull').
-define(ActionRenew, 'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Renew').
-define(ActionGetStatus, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/GetStatus').
-define(ActionRelease, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Release').
-define(ActionSubscribe,
        'http://schemas.xmlsoap.org/ws/2004/08/eventing/Subscribe').
