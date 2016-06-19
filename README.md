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
% Content-Length: 0\r\n
% \r\n

```
