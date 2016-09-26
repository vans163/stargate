-module(http_chain).
-compile(export_all).

proc(Type, Path, Query, Headers, Body, Data) ->
    case maps:get('Upgrade', Headers, undefined) of
        <<"websocket">> -> ws_chain:proc(Type, Path, Query, Headers, Body, Data);
        _ -> proc_1(Type, Path, Query, Headers, Body, Data)
    end.

proc_1(Type, Path, Query, Headers, Body, Data) ->
    Socket = maps:get(socket, Data),
    TempState = maps:get(temp_state, Data),
    Params = maps:get(params, Data),
    Hosts = maps:get(hosts, Params),

    Host = maps:get('Host', Headers, <<"*">>),
    WCardAtom = maps:get({http, <<"*">>}, Hosts),
    {HandlerAtom, HandlerOpts} = maps:get({http, Host}, Hosts, WCardAtom),

    {RCode, RHeaders, RBody, TempState2} = 
        apply(HandlerAtom, http, 
            [Type, Path, Query, Headers, Body, 
                TempState#{socket=> Socket, host=> Host}
            ]
    ),

    case maps:get('Connection', Headers, <<"close">>) of
        <<"keep-alive">> -> 
            RBin = proto_http:response(RCode, 
                RHeaders#{<<"Connection">>=> <<"keep-alive">>}, RBody),
            {http_keepalive, RBin, Data#{temp_state=> TempState2}};

        _ -> 
            RBin = proto_http:response(RCode, 
                RHeaders#{<<"Connection">>=> <<"close">>}, RBody),
            {http_close, RBin, Data#{temp_state=> TempState2}}
    end.