-module(cimxml_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-define(WBEM_HTTP, 5988).
-define(WBEM_HTTPS, 5989).

-define(SERVER_ROOT, "server_root").

init([]) ->

    {ok, Host} = application:get_env(listen_host),
    {ok, IP} = inet:getaddr(Host, inet),

    httpd:start2([{bind_address, IP},
                  {port, ?WBEM_HTTP},
                  {server_root, ?SERVER_ROOT},
                  {error_log, "error.log"},
                  {transfer_log, "transfer.log"},
                  {security_log, "security.log"},
                  {modules, [mod_wbem, mod_log]}]),

    httpd:start2([{bind_address, IP},
                  {port, ?WBEM_HTTPS},
                  {com_type, ssl},
                  {server_root, ?SERVER_ROOT},
                  {error_log, "error.log"},
                  {transfer_log, "transfer.log"},
                  {security_log, "security.log"},
                  {ssl_certificate_file, "server.pem"},
                  {modules, [mod_wbem, mod_log]}]),

    {ok, nostate}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
