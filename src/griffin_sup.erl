-module(griffin_sup).
-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    Repository = {repository, 
                  {repository, start_link, [[{file, 'repository.dets'}]]},
                  permanent, 2000, worker, [repository]},

    CIMOMHandle = {cimomhandle,
                   {cimomhandle, start_link, []},
                   permanent, 2000, worker, [cimomhandle]},

    HTTPServer = {http_server, 
                  {http_server, start_link, []},
                  permanent, 2000, worker, [http_server]},

    {ok, {{one_for_one, 5, 10}, [Repository, CIMOMHandle, HTTPServer]}}.
