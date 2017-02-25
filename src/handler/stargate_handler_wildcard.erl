-module(stargate_handler_wildcard).
-export([http/6]).

-import(stargate_transport, [peername/1]).

http(Type, Path, Query, HttpHeaders, Body, SessionState) ->
    Socket = maps:get(socket, SessionState),
    {ok, {SourceAddr, _}} = peername(Socket),

    io:format("~p:~n Unhandled http~n ~p~n ~p~n ~p~n ~p~n ~p~n ~p~n",
        [?MODULE, inet:ntoa(SourceAddr), Type, Path, Query, HttpHeaders, Body]),
    {200, #{}, <<"">>, SessionState}.