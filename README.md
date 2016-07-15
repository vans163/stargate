# stargate
Erlang customizable webserver

<img src="http://i.imgur.com/8vmU7W4.jpg" width="960" height="600" />

### Status
Work in progress.  

### Current Features
- Simple support for HTTP  
- SSL  
- Websockets  
  - Compression  

### Roadmap
- hot-loading new paths  
- zlib (GZIP)  
- ~~SSL~~  
  - SNI  
- ~~Websockets~~  
  - ~~Compression~~  
- HTTP/2  
- QUIC  

### Thinness
```
git ls-files | grep -P ".*(erl|hrl)" | xargs wc -l

   38 src/global.hrl
   12 src/handler_wildcard.erl
   21 src/handler_wildcard_ws.erl
  164 src/proto/proto_http.erl
  172 src/proto/proto_ws.erl
  102 src/stargate.erl
  228 src/vessel.erl
  
  737 total
```

Stargate is currently 737 lines of code.


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
% SSL requests on port 8443  ./priv/cert.pem   ./priv/key.pem  
stargate:warp_in().

%Listen on port 8000 on all interfaces
%Hosts map to appropriate erlang modules
stargate:start_link(
  #{
      port=> 8000,
      ip=> {0,0,0,0},
      hosts=> #{
          {http, <<"adwords.google.com">>}=> {google_adwords, #{}},
          {http, <<"tracker.google.com">>}=> {google_tracker, #{}},
          {http, <<"google.com">>}=> {google_website, #{}},
          {http, <<"www.google.com">>}=> {google_website, #{}},

          {ws, <<"adwords.google.com">>}=> {google_adwords_ws, #{}}
      }
  }
)


%SSL example

%generate key+cert
%openssl req -x509 -newkey rsa:2048 -keyout key.pem \
% -out cert.pem -days 100 -nodes -subj '/CN=localhost'


WSCompress = #{window_bits=> 15, level=>best_speed, mem_level=>8, strategy=>default},
stargate:start_link(
  #{
      port=> 8000, 
      ip=> {0,0,0,0},
      hosts=> #{
          {http, <<"*">>}=> {handler_redirect, #{}},
      }
  }
),
stargate:start_link(
  #{
      port=> 8443,
      ip=> {0,0,0,0},
      ssl_opts=> [
        {certfile, "./priv/cert.pem"},
        {keyfile, "./priv/key.pem"},

        %{cacertfile, "./priv/lets-encrypt-x3-cross-signed.pem"}
      ],
      hosts=> #{
          {http, <<"adwords.google.com">>}=> {google_adwords, #{}},
          {http, <<"tracker.google.com">>}=> {google_tracker, #{}},
          {http, <<"google.com">>}=> {google_website, #{}},
          {http, <<"www.google.com">>}=> {google_website, #{}},

          {ws, <<"adwords.google.com">>}=> {google_adwords_ws, #{compress=> WSCompress}}
      }
  }
)
```

### Example hotloading config

```erlang
Pid = whereis(stargate_https),
Pid:update_params(Pid, %{
  hosts=> #{ {http, <<"new_subdomain.google.com">>}=> {google_new, #{}} 
  ssl_opts=> [
    {certfile, "./priv/new_cert.pem"},
    {keyfile, "./priv/new_key.pem"}
  ]
})
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
```

```erlang

%google_adwords_ws module example
-module(google_adwords_ws).

-export([connect/1, disconnect/1]).
-export([msg/2]).

connect(S) -> 
    Socket = maps:get(socket, S),
    Pid = self(),
  
    vessel:ws_send(Pid, <<"hello mike">>),
    vessel:ws_send(Pid, "hello joe"),
    vessel:ws_send(Pid, <<1,2,3,4>>, bin),

    vessel:ws_send(Pid, <<"hello mike">>, compress),
    vessel:ws_send(Pid, "hello joe", compress),
    vessel:ws_send(Pid, <<1,2,3,4>>, bin_compress),

    vessel:ws_send(Pid, close),

    S.

disconnect(S) -> pass.
msg(Bin, S) -> S.
```

```javascript

//Chrome javascript WSS example:
var Socket = new WebSocket("wss://localhost:8443");
Socket.send("Hello Mike");
```
