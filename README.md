# stargate
Erlang customizable webserver

<img src="http://i.imgur.com/8vmU7W4.jpg" width="960" height="600" />

### Status
Work in progress.  

### Current Features
Simple support for HTTP  
Websockets  
SSL  

### Roadmap
hot-loading new paths  
zlib (GZIP)  
~~SSL~~  
* SNI  
~~Websockets~~  
* Compression  

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
stargate:warp_in().

%Listen on port 8000 on all interfaces
%Host paths to appropriate erlang modules
stargate:start_link(
  #{
      port=> 8000,
      ip=> {0,0,0,0},
      ssl=> false,
      hosts=> #{
          {http, <<"adwords.google.com">>}=> {google_adwords, []},
          {http, <<"tracker.google.com">>}=> {google_tracker, []},
          {http, <<"google.com">>}=> {google_website, []},

          {ws, <<"adwords.google.com">>}=> {google_adwords_ws, []}
      }
  }
)


%SSL example

%generate key+cert
%openssl req -x509 -newkey rsa:2048 -keyout key.pem \
% -out cert.pem -days 100 -nodes -subj '/CN=localhost'

stargate:start_link(
  #{
      port=> 8443,
      ip=> {0,0,0,0},
      ssl=> true,
      certfile=> "./priv/cert.pem",
      keyfile=> "./priv/key.pem",
      hosts=> #{
          {http, <<"adwords.google.com">>}=> {google_adwords, []},
          {http, <<"tracker.google.com">>}=> {google_tracker, []},
          {http, <<"google.com">>}=> {google_website, []},

          {ws, <<"adwords.google.com">>}=> {google_adwords_ws, []}
      }
  }
)
```

### Example Modules

```erlang
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
% Connection: close\r\n
% \r\n



%google_adwords_ws module example
-module(google_adwords_ws).

-export([connect/1, disconnect/1]).
-export([msg/2]).

connect(S) -> 
  Socket = maps:get(socket, S),

  Bin1 = proto_ws:encode_frame(<<"hello mike">>),
  ok = gen_tcp:send(Socket, Bin1),

  Bin2 = proto_ws:encode_frame("hello joe"),
  ok = gen_tcp:send(Socket, Bin2),

  Bin3 = proto_ws:encode_frame(<<1,2,3,4>>, bin),
  ok = gen_tcp:send(Socket, Bin3),

  Bin4 = proto_ws:encode_frame(close),
  ok = gen_tcp:send(Socket, Bin4),

  S.

disconnect(S) -> pass.

msg(Bin, S) -> S.
```

```javascript

//Chrome WSS example:
var Socket = new WebSocket("wss://localhost:8443");
Socket.send("Hello Mike");
```