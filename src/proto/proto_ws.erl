-module(proto_ws).

-export([check_version/1]).
-export([handshake/1]).

check_version(<<Ver/binary>>) -> check_version(binary_to_integer(Ver));
check_version(Ver) -> lists:member(Ver, [7,8,13]).

handshake(WSKey) ->
    UselessHash = crypto:hash(sha, 
        <<WSKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    UselessHashBase64 = base64:encode(UselessHash),

    Headers = #{
        <<"Upgrade">>=> <<"websocket">>,
        <<"Connection">>=> <<"Upgrade">>,
        <<"Sec-WebSocket-Accept">>=> UselessHashBase64
    },
    proto_http:response(101, Headers, <<"">>)
    .