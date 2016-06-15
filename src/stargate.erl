-module(stargate).

-export([warpin/0, warpin/1]).
-export([test/0]).

-include("global.hrl").

warpin() ->
    warpin(#{
            port=> 80,
            ip=> {0,0,0,0},
            paths=> #{<<"*">>=> ?HANDLER_WILDCARD}
        }
    ).

warpin(MapArgs) ->
    Port = maps:get(port, MapArgs),
    Ip = maps:get(ip, MapArgs),
    Paths = maps:get(paths, MapArgs),
    Paths2 = ensure_wildcard(Paths),

    tcp_server:start(#{ip=> Ip, port=> Port, params=> Paths2})
    .

ensure_wildcard(Paths) ->
    case maps:get(<<"*">>, Paths, undefined) of
        undefined -> maps:put(<<"*">>, ?HANDLER_WILDCARD, Paths);
        _ -> Paths
    end.