# stargate
Erlang customizable webserver

<img src="http://i.imgur.com/8vmU7W4.jpg" width="960" height="600" />

### Status
Work in progress.  

### Current Features
Simple support for HTTP  
Websockets

### Roadmap
hot-loading new paths  
zlib (GZIP)  
HTTPS  
~~Websockets~~  

### Websockets
Keep-alives are sent from server automatically  
Defaults are in global.hrl  
Max sizes protect vs DDOS  

```erlang
-define(MAX_TCP_TIMEOUT, 120000).

-define(MAX_WS_PAYLOAD_SIZE, 16000).
-define(MAX_WS_BUFFER_SIZE, 64000).
-define(WS_PING_INTERVAL, 15000).
```

### Example
```erlang
%Listen on all interfaces for any non-ssl request /w websocket on port 8000
%Dumps all http and websocket requests to default handlers
stargate:warp_in().

%Listen on port 80 on interface "120.1.1.1"
%No ssl
%Host paths to appropriate erlang modules
stargate:start_link(
  #{
      port=> 80,
      ip=> {120,1,1,1},
      ssl=> false,
      hosts=> #{
          {http, <<"adwords.google.com">>}=> {google_adwords, []},
          {http, <<"tracker.google.com">>}=> {google_tracker, []},
          {http, <<"google.com">>}=> {google_website, []},

          {ws, <<"adwords.google.com">>}=> {google_adwords_ws, []}
      }
  }
)

%google_adwords module example
-module(google_adwords).
-export([http/6]).

http('GET', <<"/click">>, #{...}=Query, #{...}=HttpHeaders, <<Body>>, #{...}=S) ->
    Socket = maps:get(socket, S),
    {ok, {SourceAddr, _}} = inet:peername(Socket),
    
    ExtraHeaders = #{},
    ReplyBody = <<>>,
    {200, ExtraHeaders, ReplyBody, S}
    .
    
% Response payload:
%
% HTTP/1.1 200 OK\r\n
% \r\n


%google_adwords_ws module example
-module(google_adwords_ws).

-export([connect/1, disconnect/1]).
-export([msg/2]).

connect(S) -> 
  Socket = maps:get(socket, S),

  Bin1 = proto_ws:encode_frame(<<"hello">>),
  ok = gen_tcp:send(Socket, Bin1),

  Bin2 = proto_ws:encode_frame("hello"),
  ok = gen_tcp:send(Socket, Bin2),

  Bin3 = proto_ws:encode_frame(<<1,2,3,4>>, bin),
  ok = gen_tcp:send(Socket, Bin3),

  Bin4 = proto_ws:encode_frame(<<>, close),
  ok = gen_tcp:send(Socket, Bin4),

  S.

disconnect(S) -> pass.

msg(Bin, S) -> S.
```
