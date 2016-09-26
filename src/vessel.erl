-module(vessel).
-behaviour(gen_statem).

-compile(export_all).

-include("global.hrl").

%callback_mode() ->  state_functions.
callback_mode() ->  handle_event_function.

start(Params) -> gen_statem:start(?MODULE, Params, []).
start_link(Params) -> gen_statem:start_link(?MODULE, Params, []).

init(Params) ->
    process_flag(trap_exit, true),

    {ok, 
        waiting_socket,
        #{params=> Params, temp_state=> #{}},
        ?MAX_TCP_TIMEOUT
        %{next_event, internal, init}
    }.


%waiting_socket - SSL
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, 
    D=#{params:= #{ssl_opts:= SSLOpts, error_logger:= ELogger}}) 
->
    case ssl:ssl_accept(ClientSocket, SSLOpts, 30000) of
        {ok, SSLSocket} ->
            ok = ?TRANSPORT_SETOPTS(SSLSocket, [{active, once}, {packet, http_bin}]),
            {next_state, https, D#{socket=> SSLSocket}};

        {error, Error} ->
            ELogger(ClientSocket, <<"ssl_accept">>, Error),
            {stop, {shutdown, tcp_closed}, D}
    end;
%waiting_socket - Regular
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, D) ->
    ok = ?TRANSPORT_SETOPTS(ClientSocket, [{active, once}, {packet, http_bin}]),
    {next_state, http, D#{socket=> ClientSocket}};



handle_event(timeout, _EvContent, _S, D) 
-> {stop, {shutdown, tcp_closed}, D};

handle_event(info, {tcp_closed, _Socket}, _S, D) 
-> {stop, {shutdown, tcp_closed}, D};
handle_event(info, {ssl_closed, _Socket}, _S, D) 
-> {stop, {shutdown, tcp_closed}, D};




%We got an error parsing the request line, netscanner or malformed
handle_event(info, {_T, _Socket, {http_error, _}}, _S, D) 
->
    {stop, {shutdown, tcp_closed}, D};

%We dont support this, netscanners make this request? Never a browser?
handle_event(info, 
    {_T, _Socket, {http_request, _Type, {absoluteURI, _,_,_,_Path}, _HttpVer}}, 
    _S, D) 
-> 
    {stop, {shutdown, tcp_closed}, D};

%http_request http, ssl + handle websocket
handle_event(info, {T, Socket, {http_request, Type, {abs_path, RawPath}, _HttpVer}}, 
    S, D) when T == http; T == ssl 
->
    %Type = 'GET'/'POST'/'ETC'

    {Path, Query} = proto_http:path_and_query(RawPath),
    {Headers, Body} = proto_http:recv(Socket),

    case http_chain:proc(Type, Path, Query, Headers, Body, D) of
        {http_close, RespBin, D2} ->
            ?TRANSPORT_SEND(Socket, RespBin),
            ?TRANSPORT_CLOSE(Socket),
            {stop, {shutdown, tcp_closed}, D2};

        {http_keepalive, RespBin, D2} ->
            case ?TRANSPORT_SEND(Socket, RespBin) of
                {error, _Error} -> 
                    %ELogger(Socket, <<"transport_send">>, Error),
                    {stop, {shutdown, tcp_closed}, D2};

                ok ->
                    ok = ?TRANSPORT_SETOPTS(Socket, [{active, once}, {packet, http_bin}]),
                    {next_state, S, D2, ?MAX_TCP_TIMEOUT}
            end;

        {websocket_upgrade, RespBin, D2} ->
            case ?TRANSPORT_SEND(Socket, RespBin) of
                {error, _Error} -> 
                    %ELogger(Socket, <<"transport_send">>, Error),
                    {stop, {shutdown, tcp_closed}, D2};

                ok ->
                    ok = ?TRANSPORT_SETOPTS(Socket, [{active, once}, {packet, raw}, binary]),
                    erlang:send_after(?WS_PING_INTERVAL, self(), ws_ping),

                    {next_state, websocket, D2, ?MAX_TCP_TIMEOUT}
            end
    end;



%%% Websocket stuff
handle_event(info, {T, Socket, Bin}, websocket, 
    D=#{ws_handler:= WSHandler, ws_buf:= WSBuf}) 
when T == tcp; T == ssl ->
    ok = ?TRANSPORT_SETOPTS(Socket, [{active, once}, binary]),
    
            %close
    (fun Decode({ok, 8, _, _, _Buffer}, _D) -> 
                ?TRANSPORT_CLOSE(Socket),
                {stop, {shutdown, tcp_closed}, _D};

            %pong
            Decode({ok, 10, _, _, Buffer}, _D) ->
                Decode(proto_ws:decode_frame(Buffer), _D);

            %regular and compressed frame
            Decode({ok, _Opcode, Compressed, Payload, Buffer}, _D) ->
                Payload2 = case Compressed of
                    0 -> Payload;
                    1 ->
                        iolist_to_binary(
                            zlib:inflate(maps:get(zinflate, _D), 
                                <<Payload/binary,0,0,255,255>>)
                        )
                end,
                TempState = maps:get(temp_state, _D),
                TempState2 = apply(WSHandler, msg, [Payload2, TempState]),
                Decode(proto_ws:decode_frame(Buffer), _D#{temp_state=> TempState2});

            %incomplete
            Decode({incomplete, Buffer}, _D) ->
                {next_state, websocket, _D#{ws_buf=> Buffer}, ?MAX_TCP_TIMEOUT}

    end)(proto_ws:decode_frame(<<WSBuf/binary, Bin/binary>>), D);

handle_event(info, ws_ping, websocket, D=#{socket:= Socket}) ->
    erlang:send_after(?WS_PING_INTERVAL, self(), ws_ping),
    p_send(Socket, proto_ws:encode_frame(ping), websocket, D);


handle_event(info, {ws_send, {text, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = proto_ws:encode_frame(P, text),
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {bin, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = proto_ws:encode_frame(P, bin),
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {text_compress, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, D, undefined) of
        undefined -> proto_ws:encode_frame(P, text);
        ZDeflate ->
            P2 = proto_ws:deflate(ZDeflate, P),
            proto_ws:encode_frame(P2, text_compress)
    end,
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {bin_compress, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, D, undefined) of
        undefined -> proto_ws:encode_frame(P, bin);
        ZDeflate ->
            P2 = proto_ws:deflate(ZDeflate, P),
            proto_ws:encode_frame(P2, bin_compress)
    end,
    p_send(Socket, Bin, websocket, D);

%forward messages to ws_handler
handle_event(info, {ws_message, Msg}, websocket, D=#{ws_handler:= WSHandler}) ->
    TempState = maps:get(temp_state, D),
    try
        apply(WSHandler, handle_info, [Msg, TempState])
    catch
        E:R -> ?PRINT({"info apply failed", Msg, E, R})
    end,
    {next_state, websocket, D}.



%handle_event(info, Msg, S, D) ->
    %?PRINT({"Unhandled msg", Msg, S, D}),
%    {next_state, S, D}.


terminate({shutdown, tcp_closed}, _S, D) -> forward_ws_closed(D);
terminate(R, S, D) ->
    forward_ws_closed(D),
    ?PRINT({"Terminated", R, S, D}),
    ok.


code_change(_V, S, D, _E) -> {ok, S, D}.



forward_ws_closed(#{ws_handler:= WSHandler, temp_state:= TempState}) ->
    apply(WSHandler, disconnect, [TempState]);
forward_ws_closed(_) -> ok.

p_send(Socket, Bin, S, D=#{params:= #{error_logger:= _ELogger}}) ->
    case ?TRANSPORT_SEND(Socket, Bin) of
        ok -> 
            {next_state, S, D, ?MAX_TCP_TIMEOUT};

        {error, _Error} -> 
            %ELogger(Socket, <<"transport_send">>, Error),
            {stop, {shutdown, tcp_closed}, D}
    end.


