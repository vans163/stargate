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
R19.1+ only.  
This release is more modern featuring a mostly tickless gen_statem with variable acceptor pool (default equal to scheduler count) 
plus OTP supervision trees.  

### Current Features
- Simple support for HTTP  
- hot-loading new paths  
- GZIP
- SSL  
- Simple plugins
  - Templates
  - Static File Server
- Websockets  
  - Compression  

### Roadmap
- half-closed sockets  
- HTTP/2   ** Postponed until Websockets/other raw streaming is supported    
- QUIC     ** Postponed until Websockets/other raw streaming is supported  

### Benchmarks

### Thinness
<details>
<summary>Stargate is currently 1090 lines of code</summary>  
```
git ls-files | grep -P ".*(erl|hrl)" | xargs wc -l

    8 src/app/stargate_app.erl

   11 src/handler/handler_redirect_https.erl
   12 src/handler/handler_wildcard.erl
   23 src/handler/handler_wildcard_ws.erl

   11 src/plugin/stargate_plugin.erl
   82 src/plugin/stargate_static_file.erl

  161 src/proto/proto_http.erl
  165 src/proto/proto_ws.erl

   36 src/logic_chain/http_chain.erl
   37 src/logic_chain/ws_chain.erl

   49 src/global.hrl

  113 src/stargate.erl
   40 src/stargate_acceptor.erl
   25 src/stargate_acceptor_sup.erl
   65 src/stargate_child.erl
   37 src/stargate_sup.erl
  215 src/stargate_vessel.erl

 1090 total

```
</details> 
 

### Example
<details>
<summary>Basic example</summary>
```erlang

%Listen on all interfaces for any non-ssl request /w websocket on port 8000
% SSL requests on port 8443  ./priv/cert.pem   ./priv/key.pem  

stargate:launch_demo().
```
</details>

<details>
<summary>Live configuration example</summary>
   
```erlang

{ok, _} = application:ensure_all_started(stargate),

{ok, HttpPid} = stargate:warp_in(
  #{
      port=> 80, 
      ip=> {0,0,0,0},
      hosts=> #{
          {http, "public.templar-archive.aiur"}=> {templar_archive_public, #{}},
          {http, "*"}=> {handler_redirect_https, #{}},
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
          {http, "templar-archive.aiur"}=> {templar_archive, #{}},
          {http, "www.templar-archive.aiur"}=> {templar_archive, #{}},

          {http, "research.templar-archive.aiur"}=> {templar_archive_research, #{}},

          {ws, {"ws.templar-archive.aiur", "/emitter"}}=> 
              {ws_emitter, #{compress=> WSCompress}},
          {ws, {"ws.templar-archive.aiur", "/transmission"}}=> 
              {ws_transmission, #{compress=> WSCompress}}
      }
  }
).

-module(templar_archive_public).
-compile(export_all).

http('GET', Path, Query, Headers, Body, S) ->
    stargate_plugin:serve_static(<<"./priv/public/">>, Path, Headers, S).


-module(templar_archive).
-compile(export_all).

http('GET', <<"/">>, Query, Headers, Body, S) ->
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

http('GET', Path, Query, Headers, Body, S) ->
    Resp =  <<"Access Denied">>,
    {200, #{}, Resp, S}.


-module(ws_emitter).
-compile(export_all).

connect(_Headers, S) -> S.
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
</details>  
  
<details>
<summary>Hotloading example</summary>

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
</details>  
  
<details>
<summary>Gzip example</summary>

```erlang
Headers = #{'Accept-Encoding'=> <<"gzip">>, <<"ETag">>=> <<"12345">>},
S = old_state,
{ReplyCode, ReplyHeaders, ReplyBody, NewState} = 
    stargate_plugin:serve_static(<<"./priv/website/">>, <<"index.html">>, Headers, S),

ReplyCode = 200,
ReplyHeaders = #{<<"Content-Encoding">>=> <<"gzip">>, <<"ETag">>=> <<"54321">>},
```
</details>

<details>
<summary>Websockets example</summary>  
  
Keep-alives are sent from server automatically  
Defaults are in global.hrl  
Max sizes protect vs DDOS  
  
Keep in mind that encoding/decoding json + websocket frames produces alot of eheap_allocs; fragmenting the process heap beyond possible GC cleanup. Make sure to do these operations inside the stargate_vessel process itself or a temporary process.  You greatly risk crashing the entire beam VM otherwise due to it not being able to allocate anymore eheap.  
  
Using max_heap_size erl vm arg can somewhat remedy this problem.



```erlang
-module(ws_transmission).

-export([connect/2, disconnect/1]).
-export([msg/2, handle_info/2]).

connect(Headers, S) -> 
    Socket = maps:get(socket, S),
    Pid = self(),
  
    Cookies = maps:get('Cookie', Headers, undefined),

    stargate_plugin:ws_send(Pid, {text, "hello joe"}),
    stargate_plugin:ws_send(Pid, {text, <<"hello joe">>}),
    stargate_plugin:ws_send(Pid, {bin, <<1,2,3,4>>}),

    stargate_plugin:ws_send(Pid, {text_compress, <<"hello mike">>}),
    stargate_plugin:ws_send(Pid, {bin_compress, <<1,2,3,4>>}),

    stargate_plugin:ws_send(Pid, close),

    case Cookies of
      <<"token=mysecret">> -> S;
      _ -> reject
    end.

disconnect(S) -> ok.
msg(Bin, S) -> S.
handle_info(Msg, S) -> S.
```

```javascript

//Chrome javascript WS example:
var socket = new WebSocket("ws://127.0.0.1:8000");
socket.send("Hello Mike");
```
</details>


<details>
<summary>Templating example</summary>  
  
Basic templating system uses the default regex of "<%=(.*?)%>" to pull out captures from a binary.

For example writing html like:

```html
<li class='my-nav-list <%= case :category of <<\"index\">>-> 'my-nav-list-active'; _-> '' end. %>'>
  <a href='/' class='link'>
    <span class='act'>Home</span>
    <span class='hov'>Home</span>
  </a>
</li>
```

You can now do:

```erlang
KeyValue = #{category=> <<"index">>},
TransformedBin = stargate_plugin:template(HtmlBin, KeyValue).
```

The return is the evaluation of the expressions between the match with the :terms substituted.

You may pass your own regex to match against using stargate_plugin:template/3:

```erlang
stargate_plugin:template("{{(.*?)}}", HtmlBin, KeyValue).
```
  </details>
