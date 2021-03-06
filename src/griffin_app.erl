-module(griffin_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case griffin_sup:start_link() of
        {ok, Pid} ->
            init(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

init() ->
    %% Startup stuff here
    ok.
