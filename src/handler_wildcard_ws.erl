-module(handler_wildcard_ws).

-export([connect/1, disconnect/1]).
-export([msg/2]).

-include("global.hrl").

connect(S) ->
    Socket = maps:get(socket, S),
    {ok, {SourceAddr, _}} = transport_peername(Socket),
    ?PRINT({"WS_Unhandled: Connect", SourceAddr}),
    S
    .

disconnect(S) ->
    ?PRINT({"WS_Unhandled: Disconnect"})
    .

msg(Bin, S) ->
    ?PRINT({"WS_Unhandled:", Bin}),
    S
    .