-module(frequency).
-behavior(gen_server).

-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    gen_server:start_link({local, frequency}, frequency, [], []).

init(_Args) ->
    Frequencies = {get_frequencies(), []},
    {ok, Frequencies}.

get_frequencies() -> [10, 11, 12, 13, 14, 15].

stop() ->
    gen_server:cast(frequency, stop).

terminate(_Reason, _LoopData) ->
    ok.

allocate() ->
    gen_server:call(frequency, {allocate, self()}).

handle_call({allocate, Pid}, _From, Frequencies) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    {reply, Reply, NewFrequencies}.

deallocate(Frequency) ->
    gen_server:cast(frequency, {deallocate, Frequency}).

handle_cast({deallocate, Freq}, Frequencies) ->
    NewFrequencies = deallocate(Frequencies, Freq),
    {noreply, NewFrequencies};
handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_info(_Msg, LoopData) ->
    {noreply, LoopData}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
