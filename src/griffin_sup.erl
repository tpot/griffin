-module(griffin_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    CIMOMHandleOptions = [{register, true},
                          {repository, [{file, 'repository.dets'}]}],

    CIMOMHandle = {cimomhandle,
                   {cimomhandle, start_link, [CIMOMHandleOptions]},
                   permanent, 2000, worker, [cimomhandle]},

    HTTPServer = {http_server, 
                  {http_server, start_link, []},
                  permanent, 2000, worker, [http_server]},

    {ok, {{one_for_one, 5, 10}, [CIMOMHandle, HTTPServer]}}.
