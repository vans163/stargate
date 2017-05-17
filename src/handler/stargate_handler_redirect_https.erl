-module(stargate_handler_redirect_https).
-export([http/6]).

http(_Type, Path, _Query, HttpHeaders, _Body, SessionState) ->
    Host = maps:get("host", HttpHeaders, <<"">>),
    SSLPath = <<"https://", Host/binary, Path/binary>>,
    {301, #{<<"Location">>=> SSLPath}, <<"">>, SessionState}.
