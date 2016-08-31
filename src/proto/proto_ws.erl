-module(proto_ws).

-export([check_version/1]).
-export([handshake/3]).
-export([decode_frame/1]).

-export([encode_frame/1, encode_frame/2, encode_frame/3]).
-export([deflate/2]).

-include("../global.hrl").

%TODO: is 7 and 8 backwards compat with 13?
check_version(<<Ver/binary>>) -> check_version(binary_to_integer(Ver));
check_version(Ver) -> lists:member(Ver, [7,8,13]). 

parse_extensions(WSExtensions) ->
    WSExtStripped = binary:replace(WSExtensions, <<" ">>, <<>>, [global]),
    WSExtensionValues = binary:split(WSExtStripped, <<";">>, [global, trim_all]),
    lists:foldr(fun(Exten, Acc) ->
            case binary:split(Exten, <<"=">>, [trim_all]) of
                [K] -> Acc#{K=><<>>};
                [K,V] -> Acc#{K=>V}
            end
        end, #{}, WSExtensionValues
    )
    .

useless_hash(WSKey) ->
    UselessHash = crypto:hash(sha, 
        <<WSKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    UselessHashBase64 = base64:encode(UselessHash)
    .

inflateInit() ->
    Z = zlib:open(),
    zlib:inflateInit(Z, -15),
    Z
    .

deflateInit(CompressOpts) ->
    Level = maps:get(level, CompressOpts, 1),
    MemLevel = maps:get(mem_level, CompressOpts, 8),
    WindowBits = maps:get(window_bits, CompressOpts, 15),
    Strategy = maps:get(strategy, CompressOpts, default),

    Z = zlib:open(),
    zlib:deflateInit(Z, Level, deflated, -WindowBits, MemLevel, Strategy),
    Z
    .

deflate(ZDeflate, Payload) ->
    Bin = iolist_to_binary(zlib:deflate(ZDeflate, Payload, sync)),
    Len = byte_size(Bin) - 4,
    case Bin of
        <<Body:Len/binary, 0:8, 0:8, 255:8, 255:8>> -> Body;
        _ -> Bin
    end
    .

handshake(WSKey, WSExtensions, WSOptions) ->
    Extensions = parse_extensions(WSExtensions),
    Compress = maps:get(compress, WSOptions, undefined),

    Headers = #{
        <<"Upgrade">>=> <<"websocket">>,
        <<"Connection">>=> <<"Upgrade">>,
        <<"Sec-WebSocket-Accept">>=> useless_hash(WSKey)
    },
    ExtraHeaders = case Compress of
        undefined -> #{};
        CompressOpts ->
            case maps:get(<<"permessage-deflate">>, Extensions, undefined) of
                undefined -> #{};
                <<>> ->
                    #{<<"Sec-WebSocket-Extensions">>=> <<"permessage-deflate">>}
            end
    end,
    Headers2 = maps:merge(Headers, ExtraHeaders),
    RespBin = proto_http:response(101, Headers2, <<"">>),

    case map_size(ExtraHeaders) of
        0 -> {ok, RespBin};
        _ -> {compress, RespBin, inflateInit(), deflateInit(Compress)}
    end.




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


decode_frame(<<>>) -> {incomplete, <<>>};
decode_frame(Chunk= <<Fin:1, RSV1:1, RSV2:1, RSV3:1, 
        Opcode:4, Rest/binary>>) ->
    
    %unimplemented. Only firefox splits frames after 32kb
    %http://lucumr.pocoo.org/2012/9/24/websockets-101/
    true = 0 /= Opcode,

    case Rest of
        <<0:1, 127:7, PLen:64, P:PLen/binary, R/binary>> ->
            {ok, Opcode, RSV1, P, R};

        <<0:1, 126:7, PLen:16, P:PLen/binary, R/binary>> ->
            {ok, Opcode, RSV1, P, R};

        <<0:1, PLen:7, P:PLen/binary, R/binary>> when PLen < 126 ->
            {ok, Opcode, RSV1, P, R};


        <<1:1, 127:7, PLen:64, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};

        <<1:1, 126:7, PLen:16, M:4/binary, P:PLen/binary, R/binary>> ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};

        <<1:1, PLen:7, M:4/binary, P:PLen/binary, R/binary>> when PLen < 126 ->
            PayloadXored = xor_payload(P, M),
            {ok, Opcode, RSV1, PayloadXored, R};


        <<_/binary>> -> 
            {incomplete, Chunk}
    end
.




encode_frame(ping) -> encode_frame(<<>>, ping);
encode_frame(close) -> encode_frame(<<>>, close);
encode_frame(Bin) -> encode_frame(Bin, text).

encode_frame(Bin, T) when is_list(Bin) -> encode_frame(list_to_binary(Bin), T);
encode_frame(Bin, text) -> encode_frame(Bin, 0, 1);
encode_frame(Bin, bin) -> encode_frame(Bin, 0, 2);
encode_frame(Bin, close) -> encode_frame(Bin, 0, 8);
encode_frame(Bin, ping) -> encode_frame(Bin, 0, 9);

encode_frame(Bin, compress) -> encode_frame(Bin, 1, 1);
encode_frame(Bin, bin_compress) -> encode_frame(Bin, 1, 2).


encode_frame(Bin, RSV1, Type) when byte_size(Bin) =< 125 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, (byte_size(Bin)):7, Bin/binary>>
;

encode_frame(Bin, RSV1, Type) when byte_size(Bin) =< 65536 ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, 126:7, (byte_size(Bin)):16, Bin/binary>>
;

encode_frame(Bin, RSV1, Type) ->
    <<1:1, RSV1:1, 0:1, 0:1, Type:4, 0:1, 127:7, (byte_size(Bin)):64, Bin/binary>>
.