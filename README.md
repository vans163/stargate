# stargate
Erlang customizable webserver

<img src="http://i.imgur.com/8vmU7W4.jpg" width="960" height="600" />

### Status
Currently being tested for single page app and simple use cases.  
No planned support for full HTTP spec.  

### Releases
0.1-genserver  
This release is the original stargate, using 1 acceptor and a gen_server model.

master a.k.a. 0.2-gen_statem  
R19.1+ only. This release is more modern featuring a mostly tickless gen_statem with variable acceptor pool (default equal to scheduler count) 
plus OTP supervision trees.  

### Current Features
- Simple support for HTTP  
- hot-loading new paths  
- GZIP
- SSL  
- Simple plugins
- Websockets  
  - Compression  

### Roadmap
- half-closed sockets  
- HTTP/2   ** Postponed until Websockets/other raw streaming is supported    
- QUIC     ** Postponed until Websockets/other raw streaming is supported  

### Benchmarks

### Thinness
```
git ls-files | grep -P ".*(erl|hrl)" | xargs wc -l

   11 src/handler/handler_redirect_https.erl
   12 src/handler/handler_wildcard.erl
   22 src/handler/handler_wildcard_ws.erl

   36 src/logic_chain/http_chain.erl
   37 src/logic_chain/ws_chain.erl

   11 src/plugin/stargate_plugin.erl
   82 src/plugin/stargate_static_file.erl

  161 src/proto/proto_http.erl
  165 src/proto/proto_ws.erl

   48 src/global.hrl

   98 src/stargate.erl
   40 src/stargate_acceptor.erl
   25 src/stargate_acceptor_sup.erl
   65 src/stargate_child.erl
   38 src/stargate_sup.erl
  215 src/vessel.erl

 1074 total

```

Stargate is currently 1074 lines of code.  
 

### Example

Basic demo.  
```erlang

%Listen on all interfaces for any non-ssl request /w websocket on port 8000
% SSL requests on port 8443  ./priv/cert.pem   ./priv/key.pem  

stargate:launch_demo().
```

Live configuration example.   
```erlang


{ok, HttpPid} = stargate:warp_in(
  #{
      port=> 80, 
      ip=> {0,0,0,0},
      hosts=> #{
          {http, <<"hologram_cache.templar-archive.aiur">>}=> {hologram_cache, #{}},
          {http, <<"*">>}=> {handler_redirect_https, #{}},
      }
  }
),

WSCompress = #{window_bits=> 15, level=>best_speed, mem_level=>8, strategy=>default},
{ok, HttpsPid} = stargate:warp_in(
  #{
      port=> 443,
      ip=> {0,0,0,0},
      ssl_opts=> [
        {certfile, "./priv/lets-encrypt-cert.pem"},
        {keyfile, "./priv/lets-encrypt-key.pem"},

        {cacertfile, "./priv/lets-encrypt-x3-cross-signed.pem"}
      ],
      hosts=> #{
          {http, <<"templar-archive.aiur">>}=> {templar_archive, #{}},
          {http, <<"www.templar-archive.aiur">>}=> {templar_archive, #{}},

          {http, <<"research.templar-archive.aiur">>}=> {templar_archive_research, #{}},

          {ws, {<<"ws.templar-archive.aiur">>, <<"/emitter">>}}=> {ws_emitter, #{compress=> WSCompress}},

          {ws, {<<"ws.templar-archive.aiur">>, <<"/transmission">>}}=> {ws_transmission, #{compress=> WSCompress}}
      }
  }
)

-module(hologram_cache).
-compile(export_all).

http('GET', Path, Query, HttpHeaders, Body, S) ->
    stargate_plugin:serve_static(<<"./priv/holograms/">>, Path, Headers, S).


-module(templar_archive).
-compile(export_all).

http('GET', <<"/">>, Query, HttpHeaders, Body, S) ->
    Socket = maps:get(socket, S),
    {ok, {SourceAddr, _}} = ?TRANSPORT_PEERNAME(Socket),

    SourceIp = unicode:characters_to_binary(inet:ntoa(SourceAddr)),
    Resp =  <<"Welcome to the templar archives ", SourceIp/binary>>,
    {200, #{}, Resp, S}
    .


-module(templar_archive_research).
-compile(export_all).

http('GET', Path, Query, #{'Cookie':= <<"power_overwhelming">>}, Body, S) ->
    stargate_plugin:serve_static(<<"./priv/research/">>, Path, Headers, S);

http('GET', Path, Query, HttpHeaders, Body, S) ->
    Resp =  <<"Access Denied">>,
    {200, #{}, Resp, S}.


-module(ws_emitter).
-compile(export_all).

connect(S) -> S.
disconnect(S) -> ok.


handle_info({send, {transmission, Freq}}, S) -> 
    stargate_plugin:ws_send(self(), {bin, Freq},
    S.
handle_info({send, {chat, Text}}, S) -> 
    stargate_plugin:ws_send(self(), {text_compress, Text},
    S.


msg(<<"{transmission: 555}">>, S) ->
    stargate_plugin:ws_message(self(), {send, {transmission, 555}}),
    S;
msg(<<"{chat: 'hello'}">>, S) ->
    stargate_plugin:ws_message(self(), {send, {chat, "goodbye"}}),
    S;
msg(_, S) -> S.

```


### Example hotloading config

```erlang
%Pid gotten from return value of warp_in/[1,2].

stargate:update_params(HttpsPid, #{
  hosts=> #{ 
    {http, <<"new_quarters.templar-archive.aiur">>}=> {new_quarters, #{}}
  }, 
  ssl_opts=> [
    {certfile, "./priv/new_cert.pem"},
    {keyfile, "./priv/new_key.pem"}
  ]
})
```

### Example GZip
```erlang
Headers = #{'Accept-Encoding'=> <<"gzip">>, <<"ETag">>=> <<"12345">>},
S = old_state,
{ReplyCode, ReplyHeaders, ReplyBody, NewState} = 
  stargate_plugin:serve_static(<<"./priv/website/">>, <<"index.html">>, Headers, S),

ReplyCode = 200,
ReplyHeaders = #{<<"Content-Encoding">>=> <<"gzip">>, <<"ETag">>=> <<"54321">>},
```

### Websockets
Keep-alives are sent from server automatically  
Defaults are in global.hrl  
Max sizes protect vs DDOS  
  
Keep inmind that encoding/decoding json + websocket frames produces alot of eheap_allocs; fragmenting the process heap beyond possible GC cleanup. Make sure to do these operations inside the vessel process itself, or a temporary process.  You greatly risk crashing the entire beam VM otherwise due to it not being able to allocate anymore eheap.  
  
Using max_heap_size erl vm arg can somewhat remedy this problem.



```erlang
-module(ws_transmission).

-export([connect/1, disconnect/1]).
-export([msg/2, handle_info/2]).

connect(S) -> 
    Socket = maps:get(socket, S),
    Pid = self(),
  
    stargate_plugin:ws_send(Pid, {text, "hello joe"}),
    stargate_plugin:ws_send(Pid, {text, <<"hello joe">>}),
    stargate_plugin:ws_send(Pid, {bin, <<1,2,3,4>>}),

    stargate_plugin:ws_send(Pid, {text_compress, <<"hello mike">>}),
    stargate_plugin:ws_send(Pid, {bin_compress, <<1,2,3,4>>}),

    stargate_plugin:ws_send(Pid, close),

    S.

disconnect(S) -> ok.
msg(Bin, S) -> S.
handle_info(Msg, S) -> S.
```

```javascript

//Chrome javascript WSS example:
var Socket = new WebSocket("wss://localhost:8443");
Socket.send("Hello Mike");
```
