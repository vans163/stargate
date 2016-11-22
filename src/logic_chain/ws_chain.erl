-module(ws_chain).
-compile(export_all).

get_ws_handler(Host, Path, Hosts) ->
    WildCardWSAtom = maps:get({ws, <<"*">>}, Hosts),
    {WSHandlerAtom, WSHandlerOptions} = 
        maps:get({ws, {Host, Path}}, Hosts, WildCardWSAtom),

    {WSHandlerAtom, WSHandlerOptions}.


proc(Type, Path, Query, Headers, Body, Data) ->
    Socket = maps:get(socket, Data),
    Params = maps:get(params, Data),
    Hosts = maps:get(hosts, Params),

    Host = maps:get('Host', Headers, <<"*">>),

    {WSHandlerAtom, WSHandlerOptions} = get_ws_handler(Host, Path, Hosts),

    WSVersion = maps:get(<<"Sec-Websocket-Version">>, Headers),
    WSKey = maps:get(<<"Sec-Websocket-Key">>, Headers),
    WSExtensions = maps:get(<<"Sec-Websocket-Extensions">>, Headers, <<"">>),

    true = proto_ws:check_version(WSVersion),

    {Data2, WSResponseBin} = case proto_ws:handshake(WSKey, WSExtensions, WSHandlerOptions) of
        {ok, Bin} -> { Data, Bin };
        {compress, Bin, ZInflate, ZDeflate} -> 
            { Data#{zinflate=> ZInflate, zdeflate=> ZDeflate}, Bin }
    end,

    TempState = maps:get(temp_state, Data2),
    TempState2 = TempState#{socket=> Socket},
    case apply(WSHandlerAtom, connect, [Headers, TempState2]) of
        reject -> {websocket_reject, proto_http:response(<<"404">>, #{}, <<>>), Data2};

        TempState3 ->
            {websocket_upgrade, WSResponseBin, 
                Data2#{ws_handler=> WSHandlerAtom, ws_buf=> <<>>, temp_state=> TempState3}}
    end.

