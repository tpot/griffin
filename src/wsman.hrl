%% XML namespaces

-define(s, 'http://www.w3.org/2003/05/soap-envelope').
-define(wsman, 'http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd').
-define(wsa, 'http://schemas.xmlsoap.org/ws/2004/08/addressing').

%% Action URIs

-define(ActionGet, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Get').
-define(ActionGetResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/transfer/GetResponse').

-define(ActionPut, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Put').
-define(ActionPutResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/transfer/PutResponse').

-define(ActionCreate, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Create').
-define(ActionCreateResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/transfer/CreateResponse').

-define(ActionDelete, 'http://schemas.xmlsoap.org/ws/2004/09/transfer/Delete').
-define(ActionDeleteResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/transfer/DeleteResponse').

-define(ActionEnumerate, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Enumerate').
-define(ActionEnumerateResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/EnumerateResponse').

-define(ActionPull, 'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Pull').
-define(ActionPullResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/PullResponse').

-define(ActionRenew, 'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Renew').
-define(ActionRenewResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Renew').

-define(ActionGetStatus, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/GetStatus').

-define(ActionGetStatusResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/GetStatusResponse').

-define(ActionRelease, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/Release').
-define(ActionReleaseResponse, 
        'http://schemas.xmlsoap.org/ws/2004/09/enumeration/ReleaseResponse').

-define(ActionSubscribe,
        'http://schemas.xmlsoap.org/ws/2004/08/eventing/Subscribe').
-define(ActionSubscribeResponse,
        'http://schemas.xmlsoap.org/ws/2004/08/eventing/SubscribeResponse').

-define(ActionFault, 'http://schemas.xmlsoap.org/ws/2004/08/addressing/fault').
