
-define(HANDLER_WILDCARD, handler_wildcard).
-define(HANDLER_WILDCARD_WS, handler_wildcard_ws).

-define(TIMEOUT, 30000).
-define(MAX_TCP_TIMEOUT, 120000).
-define(MAX_TCP_TIMEOUT_SEC, 120).

-define(MAX_HEADER_SIZE, 8000).
-define(MAX_BODY_SIZE, 20000).

-define(MAX_WS_PAYLOAD_SIZE, 16000).
-define(MAX_WS_BUFFER_SIZE, 64000).
-define(WS_PING_INTERVAL, 15000).


-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

unix_time() -> {A,B,_} = os:timestamp(), (A * 1000000) + B.