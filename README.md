# stargate
Elixir fast and featured webserver

<img src="https://i.imgur.com/UKKzOYr.jpg" width="960" height="600" />

### Status
Very fast and customizable.  
No support for HTTP2.  

### Releases
These releases are breaking changes.  
  
0.1-genserver  
  - 1 acceptor 
  - Ticking gen_server

0.2-gen_statem
  - R19.1+ only
  - OTP supervision trees
  - Tickless gen_statem
  - Multiple acceptors
  - Query and Headers on websocket connection

0.3-proc_lib
  - R19.2+ only
  - websocket connection becomes a gen_server process
  - keep_alive http connection currently under question
  - all headers normalized to lowercase binary
    - preparation for http/2

0.4-elixir
  - requires Elixir now
  - performance upgrades
  - headers as lists now
  - benchmarks
  - streaming bodies

### Current Features
- Simple support for HTTP  
- hot-loading new paths  
- GZIP
- SSL  
- Streams API (Binary streaming)
- Simple plugins
  - Templates
  - Static File Server
- Websockets  
  - Compression  

### Roadmap
- ~half-closed sockets~  
- HTTP/2   ** Postponed until Websockets/other raw streaming is supported    
- QUIC     ** Postponed until Websockets/other raw streaming is supported  

### Benchmarks

```elixir
git clone https://github.com/vans163/stargate

#Intel(R) Xeon(R) CPU E5-2670 v2 @ 2.50GHz
#10 physical cores and 10 hyperthreads to erlang

iex --erl "+S 10 +sbt ts +sct L10-19C10-19P1N1" -S mix
:erlang.system_info(:cpu_topology)

#1 core per autocannon instance, which produces 45k reqs per second
rm bench
taskset -c 0 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 1 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 2 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 3 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 4 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 5 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 6 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 7 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 8 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &
taskset -c 9 autocannon -c 10 -d 10 -S /tmp/star.sock http://test.com 2>> bench &

grep "k requests in " bench | cut -c 1-3 | python -c"import sys; print(sum(map(int, sys.stdin)))"
grep "Latency" bench

3299k requests per 11 seconds
300k requests per second

13ms average latency


Cowboy:
110k
30ms average latency

webserver = %{
    ip: {:local, "/tmp/star.sock"},
    port: 0,
}
_pid = Stargate.warp_in(webserver)
```

### Thinness
<details>
<summary>Stargate is currently 1144 lines of code</summary>  
```
git ls-files | grep -P ".*(erl|hrl)" | xargs wc -l

   43 src/app/acceptor/stargate_acceptor_gen.erl
   25 src/app/acceptor/stargate_acceptor_sup.erl
    8 src/app/stargate_app.erl
   69 src/app/stargate_child_gen.erl
   25 src/app/stargate_sup.erl
    6 src/handler/stargate_handler_redirect_https.erl
   11 src/handler/stargate_handler_wildcard.erl
   39 src/handler/stargate_handler_wildcard_ws.erl
   21 src/plugin/stargate_plugin.erl
   88 src/plugin/stargate_static_file.erl
   96 src/plugin/stargate_template.erl
  172 src/proto/stargate_proto_http.erl
  162 src/proto/stargate_proto_ws.erl
  103 src/stargate.erl
   16 src/stargate_transport.erl
  260 src/stargate_vessel.erl

 1144 total

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
      listen_args=> [{nodelay, false}],
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
      listen_args=> [{nodelay, false}],
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
-behavior(gen_server).
-compile(export_all).

handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

start_link(Params) -> gen_server:start_link(?MODULE, Params, []).

init({ParentPid, Query, Headers, State}) ->
    %If we dont trap_exit plus catch 'EXIT' we cant have terminate called, up to you
    process_flag(trap_exit, true),

    {ok, State#{parent=> ParentPid}}.

terminate(Reason, _S) -> 
    io:format("~p:~n disconnect~n ~p~n", [?MODULE, Reason]).

handle_info({'EXIT', _, _Reason}, D) ->
    {stop, {shutdown, got_exit_signal}, D};



handle_info({text, Bin}, S=#{parent:= ParentPid}) ->
    ParentPid ! {ws_send, {bin, <<"hello">>}},
    ParentPid ! {ws_send, {bin_compress, <<"hello compressed">>}},
    {noreply, S};

handle_info({bin, Bin}, S) ->
    io:format("~p:~n Got bin~n ~p~n", [?MODULE, Bin]),
    ParentPid ! {ws_send, {text, <<"a websocket text msg">>}},
    ParentPid ! {ws_send, {text_compress, <<"a websocket text msg compressed">>}},
    {noreply, S};

handle_info(Message, S) -> 
    io:format("~p:~n Unhandled handle_info~n ~p~n ~p~n", [?MODULE, Message, S]),
    {noreply, S}.

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
-behavior(gen_server).
-compile(export_all).

handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

start_link(Params) -> gen_server:start_link(?MODULE, Params, []).

init({ParentPid, Query, Headers, State}) ->
    %If we dont trap_exit plus catch 'EXIT' we cant have terminate called, up to you
    process_flag(trap_exit, true),

    Cookies = maps:get(<<"cookie">>, Headers, undefined),
    case Cookies of
        <<"token=mysecret">> -> {ok, State#{parent=> ParentPid}};
        _ -> ignore
    end.

terminate(Reason, _S) -> 
    io:format("~p:~n disconnect~n ~p~n", [?MODULE, Reason]).

handle_info({'EXIT', _, _Reason}, D) ->
    {stop, {shutdown, got_exit_signal}, D};



handle_info({text, Bin}, S=#{parent:= ParentPid}) ->
    ParentPid ! {ws_send, {bin, <<"hello">>}},
    ParentPid ! {ws_send, {bin_compress, <<"hello compressed">>}},
    {noreply, S};

handle_info({bin, Bin}, S) ->
    io:format("~p:~n Got bin~n ~p~n", [?MODULE, Bin]),
    ParentPid ! {ws_send, {text, "a websocket text list"}},
    ParentPid ! {ws_send, {text, <<"a websocket text bin">>}},
    ParentPid ! {ws_send, {text_compress, <<"a websocket text msg compressed">>}},
    {noreply, S};

handle_info(Message, S) -> 
    io:format("~p:~n Unhandled handle_info~n ~p~n ~p~n", [?MODULE, Message, S]),
    {noreply, S}.
```

```javascript

//Chrome javascript WS example:
var socket = new WebSocket("ws://127.0.0.1:8000");
socket.send("Hello Mike");
```
</details>

<details>
<summary>Websockets inject_headers</summary>  
  
Sometimes we need to send back custom headers in the
handshake. We can now add an inject_headers param (which
is a map) to the site definition.

```erlang
NoVNCServer = #{
    port=> 5600, ip=> {0,0,0,0},
    hosts=> #{
        {ws, {"localhost:5000", "/websockify"}}=> {handler_panel_vnc, #{
            inject_headers=> #{<<"Sec-WebSocket-Protocol">>=> <<"binary">>}
        }}
    }
}
```
</details>

<details>
<summary>Cookie Parser example</summary>  
```erlang
Map = stargate_plugin:cookie_parse(<<"token=mysecret; other_stuff=some_other_thing">>)
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

<details>
<summary>Streams API (binary streaming)</summary>  

Binary streaming for non-chunked encoding responses.

```erlang

-module(http_handler_stream).
-compile(export_all).

close_stream(Pid) ->
    Pid ! close_connection.

ticker(Pid) ->
      timer:sleep(1000),
      Pid ! {send_chunk, <<"hi">>},
      ticker(Pid).

http('GET', <<"/stream">>, _Query, _Headers, _Body, S) ->
      io:format("Streaming.. ~p ~p ~n", [S, self()]),
      spawn_link(http_handler_stream, ticker, [self()]),
      {200, #{}, stream, S}.
```
</details>
