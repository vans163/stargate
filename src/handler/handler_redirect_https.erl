-module(handler_redirect_https).

-export([http/6]).

-include("../global.hrl").

http(Type, Path, Query, HttpHeaders, Body, SessState) ->
    Host = maps:get('Host', HttpHeaders, <<"">>),
    SSLPath = <<"https://", Host/binary, Path/binary>>,

    {301, #{<<"Location">>=> SSLPath}, <<"">>, SessState}
    .