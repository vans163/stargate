# stargate
Erlang high performance webserver

<img src="http://i.imgur.com/8vmU7W4.jpg" width="960" height="600" />

### Status
Work in progress.  

### Current Features
Simple support for HTTP  

### Roadmap
hot-loading new paths  
zlib (GZIP)  
HTTPS  
Websockets  

### Example
```erlang
%Listen on all interfaces for any request on port 80
stargate:warpin().

%Listen on port 80 on interface "120.1.1.1"
%Route Host paths to appropriate erlang modules
stargate:warpin(
  #{
      port=> 80,
      ip=> {120,1,1,1},
      paths=> #{
          <<"adwords.google.com">>=> google_adwords,
          <<"tracker.google.com">>=> google_tracker,
          <<"google.com">>=> google_website
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
