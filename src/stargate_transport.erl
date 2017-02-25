-module(stargate_transport).
-compile(export_all).

peername(Socket={sslsocket, _, _}) -> ssl:peername(Socket);
peername(Socket) -> inet:peername(Socket).

setopts(Socket={sslsocket, _, _}, Opts) -> ssl:setopts(Socket, Opts);
setopts(Socket, Opts) -> inet:setopts(Socket, Opts).

send(Socket={sslsocket, _, _}, Payload) -> ssl:send(Socket, Payload);
send(Socket, Payload) -> gen_tcp:send(Socket, Payload).

recv(Socket={sslsocket, _, _}, M, T) -> ssl:recv(Socket, M, T);
recv(Socket, M, T) -> gen_tcp:recv(Socket, M, T).

close(Socket={sslsocket, _, _}) -> ssl:close(Socket);
close(Socket) -> gen_tcp:close(Socket).