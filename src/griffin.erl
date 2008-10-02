-module(griffin).

-export([start/0]).

start() ->
    application:start(?MODULE).
