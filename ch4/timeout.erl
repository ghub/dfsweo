-module(timeout).
-behavior(gen_server).

-export([start/0, sleep/1]).
-export([init/1, handle_call/3]).

start() ->
    gen_server:start_link({local, timeout}, timeout, [], []).

init(_Args) ->
    {ok, undefined}.

sleep(Ms) ->
    gen_server:call(timeout, {sleep, Ms}).

handle_call({sleep, Ms}, _From, LoopData) ->
    timer:sleep(Ms),
    {reply, ok, LoopData}.
