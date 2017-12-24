-module(ping).
-behavior(gen_server).

-export([start/0, activate/0, deactivate/0]).
-export([init/1, handle_call/3, handle_info/2]).
-define(TIMEOUT, 5000).

start() ->
    gen_server:start_link({local, ping}, ping, [], []).

init(_Args) ->
    {ok, undefined, ?TIMEOUT}.

activate() ->
    gen_server:call(ping, activate).

deactivate() ->
    gen_server:call(ping, deactivate).

handle_call(activate, _From, LoopData) ->
    {reply, active, LoopData, ?TIMEOUT};
handle_call(deactivate, _From, LoopData) ->
    {reply, inactive, LoopData}.

handle_info(timeout, LoopData) ->
    {_Hour, _Min, Sec} = time(),
    io:format("~2.w~n", [Sec]),
    {noreply, LoopData, ?TIMEOUT}.
