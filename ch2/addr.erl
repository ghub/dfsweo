-module(addr).
-export([type/1, type2/1]).

-include_lib("kernel/include/inet.hrl").

type(Addr) ->
    {ok, HostEnt} = inet:gethostbyaddr(Addr),
    HostEnt#hostent.h_addrtype.

type2(Addr) ->
    {ok, #hostent{h_addrtype=AddrType}} = inet:gethostbyaddr(Addr),
    AddrType.
