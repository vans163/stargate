
-define(HANDLER_WILDCARD, handler_wildcard).
-define(HANDLER_WILDCARD_WS, handler_wildcard_ws).

-define(TIMEOUT, 30000).
-define(MAX_TCP_TIMEOUT, 120000).
-define(MAX_TCP_TIMEOUT_SEC, 120).

-define(HTTP_MAX_HEADER_SIZE, 8000).
-define(HTTP_MAX_BODY_SIZE, 64000).

-define(MAX_WS_PAYLOAD_SIZE, 16000).
-define(MAX_WS_BUFFER_SIZE, 64000).
-define(WS_PING_INTERVAL, 15000).


-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.


unix_time() -> {A,B,_} = os:timestamp(), (A * 1000000) + B.


transport_setopts(SSLSocket={sslsocket, _, _}, Opts) -> ssl:setopts(SSLSocket, Opts);
transport_setopts(Socket, Opts) -> inet:setopts(Socket, Opts).

transport_send(SSLSocket={sslsocket, _, _}, Payload) -> ssl:send(SSLSocket, Payload);
transport_send(Socket, Payload) -> gen_tcp:send(Socket, Payload).

transport_recv(SSLSocket={sslsocket, _, _}, M, T) -> ssl:recv(SSLSocket, M, T);
transport_recv(Socket, M, T) -> gen_tcp:recv(Socket, M, T).

transport_close(SSLSocket={sslsocket, _, _}) -> ssl:close(SSLSocket);
transport_close(Socket) -> gen_tcp:close(Socket).

transport_peername(SSLSocket={sslsocket, _, _}) -> ssl:peername(SSLSocket);
transport_peername(Socket) -> inet:peername(Socket).
