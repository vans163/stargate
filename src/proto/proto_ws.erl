-module(proto_ws).

-export([check_version/1]).
-export([handshake/1]).
-export([decode_frame/1]).

-export([encode_frame/1, encode_frame/2]).

-include("../global.hrl").

%TODO: is 7 and 8 backwards compat with 13?
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




xor_payload(Payload, Mask) -> xor_payload(Payload, Mask, <<>>).

xor_payload(<<>>, Mask, Acc) -> Acc;
xor_payload(<<Chunk:32, Rest/binary>>, M= <<Mask:32>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:32>>)
    ;
xor_payload(<<Chunk:24, Rest/binary>>, M= <<Mask:24, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:24>>)
    ;
xor_payload(<<Chunk:16, Rest/binary>>, M= <<Mask:16, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:16>>)
    ;
xor_payload(<<Chunk:8, Rest/binary>>, M= <<Mask:8, _/binary>>, Acc) ->
    XorChunk = Chunk bxor Mask,
    xor_payload(Rest, M, <<Acc/binary, XorChunk:8>>)
    .


decode_frame(<<Fin:1, RSV1:1, RSV2:1, RSV3:1, 
        Opcode:4, Rest/binary>>) ->
    
    %unimplemented. Only firefox splits frames after 32kb
    %http://lucumr.pocoo.org/2012/9/24/websockets-101/
    true = 0 /= Opcode,
    %?PRINT({Opcode}),

    case Rest of
        <<0:1, 127:7, PLen:64, P:PLen/binary, R/binary>> ->
            {ok, Opcode, P, R};

        <<0:1, 126:7, PLen:16, P:PLen/binary, R/binary>> ->
            {ok, Opcode, P, R};

        <<0:1, PLen:7, P:PLen/binary, R/binary>> when PLen < 126 ->
            {ok, Opcode, P, R};


        <<1:1, 127:7, PLen:64, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, PayloadXored, R};

        <<1:1, 126:7, PLen:16, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, PayloadXored, R};

        <<1:1, PLen:7, M:4/binary, P:PLen/binary, R/binary>> when PLen < 126 ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, PayloadXored, R};


        <<R/binary>> -> 
            {incomplete, R}
    end
.

encode_frame(ping) -> encode_frame(<<>>, ping);
encode_frame(close) -> encode_frame(<<>>, close);
encode_frame(Bin) -> encode_frame(Bin, text).

encode_frame(Bin, T) when is_list(Bin) -> encode_frame(list_to_binary(Bin), T);
encode_frame(Bin, text) -> encode_frame(Bin, 1);
encode_frame(Bin, bin) -> encode_frame(Bin, 2);
encode_frame(Bin, close) -> encode_frame(Bin, 8);
encode_frame(Bin, ping) -> encode_frame(Bin, 9);

encode_frame(Bin, Type) when byte_size(Bin) =< 125 ->
    <<1:1, 0:1, 0:1, 0:1, Type:4, 0:1, (byte_size(Bin)):7, Bin/binary>>
;

encode_frame(Bin, Type) when byte_size(Bin) =< 65536 ->
    <<1:1, 0:1, 0:1, 0:1, Type:4, 0:1, 126:7, (byte_size(Bin)):16, Bin/binary>>
;

encode_frame(Bin, Type) ->
    <<1:1, 0:1, 0:1, 0:1, Type:4, 0:1, 127:7, (byte_size(Bin)):64, Bin/binary>>
.