-module(handler_redirect_https).

-export([http/6]).

-include("../global.hrl").

http(_Type, Path, _Query, HttpHeaders, _Body, SessState) ->
    Host = maps:get('Host', HttpHeaders, <<"">>),
    SSLPath = <<"https://", Host/binary, Path/binary>>,

    {301, #{<<"Location">>=> SSLPath}, <<"">>, SessState}
    .