-module(handler_wildcard_ws).

-export([connect/3, disconnect/1]).
-export([msg/2]).
-export([handle_info/2]).

-include("../global.hrl").

connect(_Query, _Headers, S) ->
    Socket = maps:get(socket, S),
    {ok, {SourceAddr, _}} = ?TRANSPORT_PEERNAME(Socket),
    ?PRINT({"WS_Unhandled: Connect", SourceAddr, self()}),
    S.
disconnect(_S) -> 
    ?PRINT({"WS_Unhandled: Disconnect", self()}).

handle_info(_Msg, S) -> 
    ?PRINT({"WS_Unhandled: handle_info", self()}),
    S.

msg(Bin, S) ->
    ?PRINT({"WS_Unhandled:", self(), Bin}),
    S
    .
