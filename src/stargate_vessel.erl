-module(stargate_vessel).
-behaviour(gen_statem).
-compile(export_all).

-import(stargate_transport, [setopts/2, send/2, close/1]).
-import(stargate_proto_http, [path_and_query/1, recv/1, response/3]).
-import(stargate_proto_ws, [decode_frame/1, encode_frame/1, encode_frame/2, deflate/2]).

-define(TIMEOUT_BASIC, 2 * 60000).
-define(WS_PING_INTERVAL, 15000).


%callback_mode() ->  state_functions.
callback_mode() ->  handle_event_function.

code_change(_V, S, D, _E) -> {ok, S, D}.

start(Params) -> gen_statem:start(?MODULE, Params, []).
start_link(Params) -> gen_statem:start_link(?MODULE, Params, []).

init(Params) ->
    process_flag(trap_exit, true),

    {ok, 
        waiting_socket,
        #{params=> Params, temp_state=> #{}},
        ?TIMEOUT_BASIC
    }.

terminate({shutdown, tcp_closed}, _S, _D) -> ok;
terminate(R, S, D) ->
    Trace = erlang:get_stacktrace(),
    io:format("~p:~n Terminated~n ~p~n ~p~n ~p~n ~p~n", [?MODULE, S, D, R, Trace]).




%private funcs
get_http_handler(Host, Data) ->
    Params = maps:get(params, Data),
    Hosts = maps:get(hosts, Params),

    WCardAtom = maps:get({http, <<"*">>}, Hosts),
    {_Atom, _Opts} = maps:get({http, Host}, Hosts, WCardAtom).

get_ws_handler(Host, Path, Data) ->
    Params = maps:get(params, Data),
    Hosts = maps:get(hosts, Params),

    WCardAtom = maps:get({ws, <<"*">>}, Hosts),
    WCardAtomPath = maps:get({ws, {<<"*">>, Path}}, Hosts, WCardAtom),
    {_Atom, _Opts} = maps:get({ws, {Host, Path}}, Hosts, WCardAtomPath).




%waiting_socket - SSL
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, 
    D=#{params:= #{ssl_opts:= SSLOpts, error_logger:= ELogger}}) 
->
    case ssl:ssl_accept(ClientSocket, SSLOpts, ?TIMEOUT_BASIC) of
        {ok, SSLSocket} ->
            ok = setopts(SSLSocket, [{active, once}, {packet, http_bin}]),
            {next_state, https, D#{socket=> SSLSocket}};

        {error, Error} ->
            ELogger(ClientSocket, <<"ssl_accept">>, Error),
            {stop, {shutdown, tcp_closed}, D}
    end;
%waiting_socket - Regular
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, D) ->
    ok = setopts(ClientSocket, [{active, once}, {packet, http_bin}]),
    {next_state, http, D#{socket=> ClientSocket}};


%exit handling Maybe in the future we restart
handle_event(info, {'EXIT', WSPid, _Reason}, _S, D=#{ws_pid:= WSPid}) ->
    {stop, {shutdown, tcp_closed}, D};
handle_event(info, {'EXIT', _, _Reason}, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};
    

handle_event(timeout, _EvContent, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};

handle_event(info, {tcp_closed, _Socket}, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};
handle_event(info, {ssl_closed, _Socket}, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};

handle_event(info, {tcp_error, _Socket, Error}, _S, D) ->
    io:format("~p:~n tcp_error~n ~p~n", [?MODULE, Error]),
    {stop, {shutdown, tcp_closed}, D};
handle_event(info, {ssl_error, _Socket, Error}, _S, D) ->
    io:format("~p:~n ssl_error~n ~p~n", [?MODULE, Error]),
    {stop, {shutdown, tcp_closed}, D};

%We got an error parsing the request line, netscanner or malformed
handle_event(info, {_T, _Socket, {http_error, _}}, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};

%We dont support this, netscanners make this request? Never a browser?
handle_event(info, 
    {_T, _Socket, {http_request, _Type, {absoluteURI, _,_,_,_Path}, _HttpVer}}, 
    _S, D) 
-> 
    {stop, {shutdown, tcp_closed}, D};

%% Streaming and chunking
handle_event(info, {send_chunk, Bin}, S, D) ->
    TempState = maps:get(temp_state, D),
    Socket = maps:get(socket, TempState),
    case send(Socket, Bin) of
        {error, _Error} -> 
            %ELogger(Socket, <<"transport_send">>, Error),
            {stop, {shutdown, tcp_closed}, D};

        ok ->
            {next_state, S, D, ?TIMEOUT_BASIC}
    end;
handle_event(info, close_connection, _S, D) ->
    {stop, {shutdown, tcp_closed}, D};


%http_request http, ssl + handle websocket
handle_event(info, {T, Socket, {http_request, Type, {abs_path, RawPath}, _HttpVer}}, 
    S, D) when T == http; T == ssl 
->
    %Type = 'GET'/'POST'/'ETC'
    TempState = maps:get(temp_state, D),

    {Path, Query} = stargate_proto_http:path_and_query(RawPath),
    {Headers, Body} = stargate_proto_http:recv(Socket),
    Host = maps:get(<<"host">>, Headers, <<"*">>),
    Upgrade_ = maps:get(<<"upgrade">>, Headers, <<>>),
    Upgrade = unicode:characters_to_binary(string:to_lower(unicode:characters_to_list(Upgrade_))),

    case Upgrade of
        <<>> ->
            {Atom, _} = get_http_handler(Host, D),
            case apply(Atom, http, [Type, Path, Query, Headers, Body, TempState#{socket=> Socket, host=> Host}]) of
                {RCode, RHeaders, stream, TempState2} ->
                    RHeaders2 = RHeaders#{<<"Connection">>=> <<"close">>},
                    RespBin = stargate_proto_http:response(RCode, RHeaders2, stream),
                    send(Socket, RespBin),
                    % Because we will eventually get another http req if we keep-alive
                    ok = setopts(Socket, [{active, once}, {packet, http_bin}]),
                    {next_state, S, D#{temp_state=> TempState2}, ?TIMEOUT_BASIC};

                {RCode, RHeaders, RBody, TempState2} ->
                    case maps:get(<<"connection">>, Headers, <<"close">>) of
                        <<"keep-alive">> -> 
                            RHeaders2 = RHeaders#{<<"Connection">>=> <<"keep-alive">>},
                            RespBin = stargate_proto_http:response(RCode, RHeaders2, RBody),
                            case send(Socket, RespBin) of
                                {error, _Error} -> 
                                    %ELogger(Socket, <<"transport_send">>, Error),
                                    {stop, {shutdown, tcp_closed}, D#{temp_state=> TempState2}};

                                ok ->
                                    ok = setopts(Socket, [{active, once}, {packet, http_bin}]),
                                    {next_state, S, D#{temp_state=> TempState2}, ?TIMEOUT_BASIC}
                            end;

                        _ -> 
                            RHeaders2 = RHeaders#{<<"Connection">>=> <<"close">>},
                            RespBin = stargate_proto_http:response(RCode, RHeaders2, RBody),
                            send(Socket, RespBin),
                            close(Socket),
                            {stop, {shutdown, tcp_closed}, D#{temp_state=> TempState2}}
                    end
            end;

        <<"websocket">> ->
            {WSAtom, WSOpts} = get_ws_handler(maps:get(<<"host">>, Headers, <<"*">>), Path, D),

            case apply(WSAtom, start_link, 
                [{self(), Query, Headers, TempState#{socket=> Socket}}])
            of
                ignore ->
                    send(Socket, stargate_proto_http:response(<<"404">>, #{}, <<>>)),
                    close(Socket),
                    {stop, {shutdown, tcp_closed}, D};

                {error, Err} ->
                    io:format("WS Init Error: ~p~n", [Err]),
                    send(Socket, stargate_proto_http:response(<<"404">>, #{}, <<>>)),
                    close(Socket),
                    {stop, {shutdown, tcp_closed}, D};

                {ok, WSPid} ->
                    %WSVersion = maps:get(<<"sec-websocket-version">>, Headers),
                    WSKey = maps:get(<<"sec-websocket-key">>, Headers),
                    WSExtensions = maps:get(<<"sec-websocket-extensions">>, Headers, <<"">>),
                    {D2, WSResponseBin} = case stargate_proto_ws:handshake(WSKey, WSExtensions, WSOpts) of
                        {ok, Bin} -> {D, Bin};
                        {compress, Bin, ZInflate, ZDeflate} -> 
                            {D#{zinflate=> ZInflate, zdeflate=> ZDeflate}, Bin}
                    end,
                    send(Socket, WSResponseBin),

                    ok = setopts(Socket, [{active, once}, {packet, raw}, binary]),
                    erlang:send_after(?WS_PING_INTERVAL, self(), ws_ping),
                    D3 = D2#{ws_pid=> WSPid, ws_buf=> <<>>},
                    {next_state, websocket, D3, ?TIMEOUT_BASIC}

            end

    end;



%%% Websocket stuff
handle_event(info, {T, Socket, Bin}, websocket, D=#{ws_pid:= WSPid, ws_buf:= WSBuf}) 
    when T == tcp; T == ssl 
->
    ok = setopts(Socket, [{active, once}, binary]),
    
            %close
    (fun Decode({ok, 8, _, _, _Buffer}, DD) -> 
                close(Socket),
                {stop, {shutdown, tcp_closed}, DD};

            %pong
            Decode({ok, 10, _, _, Buffer}, DD) ->
                Decode(stargate_proto_ws:decode_frame(Buffer), DD);

            %regular and compressed frame
            Decode({ok, Opcode, Compressed, Payload, Buffer}, DD) ->
                Payload2 = case Compressed of
                    0 -> Payload;
                    1 ->
                        iolist_to_binary(
                            zlib:inflate(maps:get(zinflate, DD), 
                                <<Payload/binary,0,0,255,255>>))
                end,
                FrameType = case Opcode of
                    1 -> text;
                    2 -> bin
                end,
                WSPid ! {FrameType, Payload2},
                Decode(stargate_proto_ws:decode_frame(Buffer), DD);

            %incomplete
            Decode({incomplete, Buffer}, DD) ->
                {next_state, websocket, DD#{ws_buf=> Buffer}, ?TIMEOUT_BASIC}

    end)(stargate_proto_ws:decode_frame(<<WSBuf/binary, Bin/binary>>), D);

handle_event(info, ws_ping, websocket, D=#{socket:= Socket}) ->
    erlang:send_after(?WS_PING_INTERVAL, self(), ws_ping),
    p_send(Socket, stargate_proto_ws:encode_frame(ping), websocket, D);


handle_event(info, {ws_send, {text, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = stargate_proto_ws:encode_frame(P, text),
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {bin, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = stargate_proto_ws:encode_frame(P, bin),
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {text_compress, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, D, undefined) of
        undefined -> stargate_proto_ws:encode_frame(P, text);
        ZDeflate ->
            P2 = stargate_proto_ws:deflate(ZDeflate, P),
            stargate_proto_ws:encode_frame(P2, text_compress)
    end,
    p_send(Socket, Bin, websocket, D);
handle_event(info, {ws_send, {bin_compress, P}}, websocket, D=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, D, undefined) of
        undefined -> stargate_proto_ws:encode_frame(P, bin);
        ZDeflate ->
            P2 = stargate_proto_ws:deflate(ZDeflate, P),
            stargate_proto_ws:encode_frame(P2, bin_compress)
    end,
    p_send(Socket, Bin, websocket, D).



p_send(Socket, Bin, S, D=#{params:= #{error_logger:= _ELogger}}) ->
    case send(Socket, Bin) of
        ok -> 
            {next_state, S, D, ?TIMEOUT_BASIC};

        {error, _Error} -> 
            %ELogger(Socket, <<"transport_send">>, Error),
            {stop, {shutdown, tcp_closed}, D}
    end.


