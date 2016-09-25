-module(handler_wildcard).

-export([http/6]).

-include("global.hrl").

http(Type, Path, Query, HttpHeaders, Body, SessState) ->
    Socket = maps:get(socket, SessState),
    {ok, {SourceAddr, _}} = transport_peername(Socket),

    %?PRINT({"Unhandled", inet:ntoa(SourceAddr), Type, Path, Query, HttpHeaders, Body}),
    {200, #{}, <<"">>, SessState}
    .
