%%
%% A FSM to manage the lifetime of providers.  A provider is started
%% on demand and remains running while there are outstanding requests
%% and within an idle period.
%%
%% Providers exist in four states:
%%
%% stopped
%%     Provider is not running.
%%
%% starting
%%     Provider is starting up (gen_server init/1 callback running).
%%
%% running
%%     Provider is currently processing requests or idling.
%%
%% stopping
%%     Provider is stopping (gen_server terminate/2 callback running).
%%

-module(providermanager).
-behaviour(gen_fsm).

%% Callbacks

-export([init/1, handle_info/3, handle_event/3, code_change/4, terminate/3,
         handle_sync_event/4]).

%% States

-export([stopped/3, starting/2, starting/3, running/3, stopping/3]).

%% Client interface

-export([start/2, call/2, stop/1, debug/1]).

%% State record

-record(state, {
          module,                               % Name of provider module
          args,                                 % Args to pass to module:init/1
          pid,                                  % Module pid, if running
          pending = [],                         % Pending requests
          timer                                 % Startup/shutdown/idle timer
         }).

%% Client interface

start(Provider, Args) ->
    {ok, Pid} = gen_fsm:start_link(providermanager, [Provider, Args], []),
    Pid.

call(Provider, RequestData) ->
    case catch gen_fsm:sync_send_event(Provider, {request, RequestData}) of
        {'EXIT', Reason} ->
            {error, operation_failed, Reason};
        Result ->
            Result
    end.

stop(Pid) ->
    case catch gen_fsm:sync_send_all_state_event(Pid, stop) of
        _ ->
            ok
    end.            

debug(Pid) ->
    catch gen_fsm:sync_send_all_state_event(Pid, debug).

%% Process a request.  Send request to provider, reply to sender with
%% result and send a completed event to clean up.

handle_request(FsmRef, Provider, From, CallData) ->
    Result = case catch gen_server:call(Provider, CallData) of
                 %% Provider stopped
                 undefined ->
                     {error, operation_failed};
                 %% Provider exited
                 {'EXIT', _Reason} ->
                     %% Fixed in R12B, see gen_server manpage for details
                     FsmRef ! {'EXIT', Provider, fake_exit_message},
                     {error, operation_failed};
                 %% Normal result
                 Term ->
                     Term
             end,
    gen_fsm:reply(From, Result),
    gen_fsm:send_all_state_event(FsmRef, {completed, From, CallData}).

%% Initialise FSM

init([Module, Args]) ->
    StateData = #state{module = Module, args = Args},
    process_flag(trap_exit, true),
    {ok, stopped, StateData}.

%% Terminate FSM

terminate(_Reason, _StateName, _StateData) ->
    normal.

%% Handle process exits

handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
    error_logger:info_msg(
      "providermanager:handle_info('EXIT', ~p, ~p)~n", [Pid, Reason]),
    #state{pid = ProviderPid} = StateData,
    case Pid of
        ProviderPid -> 
           {next_state, stopped, StateData#state{pid = undefined}};
        _ ->
            error_logger:error_msg(
              "Ignoring unexpected EXIT from ~p: ~p~n", [Pid, Reason]),
            {next_state, StateName, StateData}
    end.

%% Code change

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% Shutdown

handle_sync_event(stop, _From, _StateName, StateData) ->
    StopResult = case StateData#state.pid of
                     undefined ->
                         ok;
                     Pid ->
                         catch gen_server:call(Pid, stop)
                 end,
    {reply, StopResult, stopped, StateData#state{pid = undefined}};

%% Return internal state of FSM

handle_sync_event(debug, _From, StateName, StateData) -> 
    {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event({debug, getCurrentState}, _From, StateName, StateData) ->
    {reply, StateName, StateName, StateData};

handle_sync_event({debug, getPendingRequests}, _From, StateName, StateData) ->
    {reply, StateData#state.pending, StateName, StateData}.

%% Handle request completed event

handle_event({completed, From, CallData}, StateName, StateData) ->
    error_logger:info_msg(
      "providermanager:handle_event(completed, ~p)~n", [CallData]),
    NewPending = lists:delete({From, CallData}, StateData#state.pending),
    {next_state, StateName, StateData#state{pending = NewPending}}.

%% Stopped state

stopped({request, CallData}, From, StateData) ->
    error_logger:info_msg("providermanager:stopped(request, ~p)~n", [CallData]),
    #state{module = Module, args = Args} = StateData,
    case gen_server:start_link(Module, Args, []) of
        {ok, Pid} ->
            gen_fsm:send_event(self(), {started, Pid}),
            NewStateData = StateData#state{pending = [{From, CallData}]},
            {next_state, starting, NewStateData};
        {error, Error} ->
            {reply, {error, startup_failed, Error}, stopped, StateData}
    end.

%% Starting state

starting({started, Pid}, StateData) ->
    error_logger:info_msg("providermanager:starting(started, ~p)~n", [Pid]),
    PPid = self(),
    [spawn(
       fun() ->
               handle_request(PPid, Pid, From, CallData) 
       end) || {From, CallData} <- lists:reverse(StateData#state.pending)],
    {next_state, running, StateData#state{pid = Pid}}.

starting({request, CallData}, From, StateData) ->
    NewPending = [{From, CallData}] ++ StateData#state.pending,
    {next_state, starting, StateData#state{pending = NewPending}}.

%% Running state

running({request, CallData}, From, StateData) ->
    PPid = self(),
    spawn(
      fun() ->
              handle_request(PPid, StateData#state.pid, From, CallData)
      end),
    NewPending = [{From, CallData}] ++ StateData#state.pending,
    {next_state, running, StateData#state{pending = NewPending}}.

%% Stopping state

stopping({request, _CallData}, From, StateData) ->
    gen_fsm:reply(From, {error, shutting_down}),
    {next_state, stopping, StateData}.
