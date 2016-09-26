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

-define(TRANSPORT_SETOPTS(Socket, Opts), 
    (fun ({sslsocket, _, _}) -> ssl:setopts(Socket, Opts);
         (_) -> inet:setopts(Socket, Opts)
    end)(Socket) 
).

-define(TRANSPORT_PEERNAME(Socket), 
    (fun ({sslsocket, _, _}) -> ssl:peername(Socket);
         (_) -> inet:peername(Socket)
    end)(Socket) 
).


-define(TRANSPORT_SEND(Socket, Payload), 
    (fun ({sslsocket, _, _}) -> ssl:send(Socket, Payload);
         (_) -> gen_tcp:send(Socket, Payload)
    end)(Socket) 
).

-define(TRANSPORT_RECV(Socket, M, T), 
    (fun ({sslsocket, _, _}) -> ssl:recv(Socket, M, T);
         (_) -> gen_tcp:recv(Socket, M, T)
    end)(Socket) 
).

-define(TRANSPORT_CLOSE(Socket), 
    (fun ({sslsocket, _, _}) -> ssl:close(Socket);
         (_) -> gen_tcp:close(Socket)
    end)(Socket) 
).