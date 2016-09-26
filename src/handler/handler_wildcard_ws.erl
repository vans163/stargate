-module(handler_wildcard_ws).

-export([connect/1, disconnect/1]).
-export([msg/2]).

-include("../global.hrl").

connect(S) ->
    Socket = maps:get(socket, S),
    {ok, {SourceAddr, _}} = ?TRANSPORT_PEERNAME(Socket),
    ?PRINT({"WS_Unhandled: Connect", SourceAddr, self()}),
    S.
disconnect(S) -> 
    ?PRINT({"WS_Unhandled: Disconnect", self()}).

handle_info(S) -> 
    ?PRINT({"WS_Unhandled: handle_info", self()}),
    S.

msg(Bin, S) ->
    ?PRINT({"WS_Unhandled:", self(), Bin}),
    S
    .