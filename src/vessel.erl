-module(vessel).
-behaviour(gen_server).

-export([start/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([cast/3, ws_send/2, ws_send/3]).


-include("global.hrl").

cast(Pid, Name, Params) ->
    gen_server:cast(Pid, {cast, Name, Params}).

%%%%% Websocket Stuff %%%%

ws_send(Pid, ping) -> 
    Bin = proto_ws:encode_frame(ping),
    gen_server:cast(Pid, {ws_send, Bin});
ws_send(Pid, close) -> 
    Bin = proto_ws:encode_frame(close),
    gen_server:cast(Pid, {ws_send, Bin});
ws_send(Pid, Payload) -> 
    Bin = proto_ws:encode_frame(Payload),
    gen_server:cast(Pid, {ws_send, Bin}).

ws_send(Pid, Payload, bin) ->
    Bin = proto_ws:encode_frame(Payload, bin),
    gen_server:cast(Pid, {ws_send, Bin});

ws_send(Pid, Payload, compress) ->
    gen_server:cast(Pid, {ws_send_compress, Payload});
ws_send(Pid, Payload, bin_compress) ->
    gen_server:cast(Pid, {ws_send_bin_compress, Payload}).



p_send(Socket, Bin, S=#{params:= #{error_logger:= ELogger}}) ->
    case transport_send(Socket, Bin) of
        ok -> 
            {noreply, S};
        {error, Error} -> 
            %ELogger(Socket, <<"transport_send">>, Error),
            {stop, {shutdown, tcp_closed}, S}
    end
    .

start(Params) -> {ok, Pid} = start_link(Params), Pid.
start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({Params, Socket}) ->
    process_flag(trap_exit, true),
    timer:send_after(1000, stalled),

    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    {ok, #{params=> Params, session_state=> #{}, nextDc=> NextDc}}.

%%%% %%%%
handle_cast({pass_socket, ClientSocket}, S=#{params:= #{ssl_opts:= SSLOpts, error_logger:= ELogger}}) ->
    case ssl:ssl_accept(ClientSocket, SSLOpts, 30000) of
        {ok, SSLSocket} ->
            ok = transport_setopts(SSLSocket, [{active, once}, {packet, http_bin}]),
            {noreply, S#{socket=> SSLSocket}};

        {error, Error} ->
            ELogger(ClientSocket, <<"ssl_accept">>, Error),
            {stop, {shutdown, tcp_closed}, S}
    end;

handle_cast({pass_socket, ClientSocket}, S) ->
    ok = transport_setopts(ClientSocket, [{active, once}, {packet, http_bin}]),
    {noreply, S#{socket=> ClientSocket}};
%%%%% %%%%%


handle_cast({ws_send, Payload}, S=#{socket:= Socket}) ->
    Bin = proto_ws:encode_frame(Payload),
    p_send(Socket, Bin, S);

handle_cast({ws_send_compress, Payload}, S=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, S, undefined) of
        undefined -> proto_ws:encode_frame(Payload);
        ZDeflate ->
            Payload_ = proto_ws:deflate(ZDeflate, Payload),
            proto_ws:encode_frame(Payload_, compress)
    end,
    p_send(Socket, Bin, S);
handle_cast({ws_send_bin_compress, Payload}, S=#{socket:= Socket}) ->
    Bin = case maps:get(zdeflate, S, undefined) of
        undefined -> proto_ws:encode_frame(Payload, bin);
        ZDeflate ->
            Payload_ = proto_ws:deflate(ZDeflate, Payload),
            proto_ws:encode_frame(Payload_, bin_compress)
    end,
    p_send(Socket, Bin, S);

handle_cast({cast, Name, Params}, S=#{socket:= Socket, ws_handler:= WSHandler}) ->
    try
        apply(WSHandler, handle_cast, [Name, Params, S])
    catch
        E:R -> ?PRINT({"cast apply failed", Name, Params, E, R})
    end,
    {noreply, S};

handle_cast(Message, S) -> {noreply, S}.
handle_call(Message, From, S) -> {reply, ok, S}.


%% WS Upgrade
handle_http(Headers=#{'Upgrade':= <<"websocket">>}, Body, S=#{
    socket:=Socket, params:= #{hosts:=Hosts}
}) ->
    Host = maps:get('Host', Headers, <<"*">>),
    WildCardWSAtom = maps:get({ws, <<"*">>}, Hosts),
    {WSHandlerAtom, WSHandlerOptions} = maps:get({ws, Host}, Hosts, WildCardWSAtom),

    WSVersion = maps:get(<<"Sec-Websocket-Version">>, Headers),
    true = proto_ws:check_version(WSVersion),

    WSKey = maps:get(<<"Sec-Websocket-Key">>, Headers),
    WSExtensions = maps:get(<<"Sec-Websocket-Extensions">>, Headers, <<"">>),

    {S_, WSResponseBin_} = case proto_ws:handshake(WSKey, WSExtensions, WSHandlerOptions) of
        {ok, WSResponseBin} -> { S, WSResponseBin };
        {compress, WSResponseBin, ZInflate, ZDeflate} -> 
            { S#{zinflate=> ZInflate, zdeflate=> ZDeflate}, WSResponseBin }
    end,

    ok = transport_send(Socket, WSResponseBin_),
    S__ = apply(WSHandlerAtom, connect, [S_]),

    ok = transport_setopts(Socket, [{active, once}, {packet, raw}, binary]),
    timer:send_after(?WS_PING_INTERVAL, ws_ping),
    S__#{ws_handler=> WSHandlerAtom, ws_buf=> <<>>}
    ;

%Regular HTTP request
handle_http(Headers, Body, S=#{
    socket:=Socket, params:= #{hosts:=Hosts}, session_state:= SS=#{path:=Path, type:=Type}
}) ->
    Host = maps:get('Host', Headers, <<"*">>),
    WCardAtom = maps:get({http, <<"*">>}, Hosts),
    {HandlerAtom, HandlerOpts} = maps:get({http, Host}, Hosts, WCardAtom),

    {CleanPath, Query} = proto_http:path_and_query(Path),

    {RCode, RHeaders, RBody, SS2} = 
        apply(HandlerAtom, http, 
            [Type, CleanPath, Query, Headers, Body, 
                SS#{socket=> Socket, host=> Host}
            ]
    ),

    ConnectionHeader = case maps:get('Connection', Headers, <<"close">>) of
        <<"keep-alive">> -> <<"keep-alive">>;
        _ -> <<"close">>
    end,

    RBin = proto_http:response(RCode, RHeaders#{<<"Connection">>=> ConnectionHeader}, RBody),
    ok = transport_send(Socket, RBin),

    case ConnectionHeader of
        <<"keep-alive">> -> ok = transport_setopts(Socket, [{active, once}, {packet, http_bin}]);
        _ -> transport_close(Socket)
    end,
    S#{session_state=> SS2}
    .


%%% HTTP / Negotiate Websockets

%Ignore invalid http_request
handle_info({T, Socket, {http_request, Type, {absoluteURI, _,_,_,Path}, HttpVer}}, S) 
when T == http; T == ssl 
->
    {noreply, S#{nextDc=> 1}}
;
handle_info({T, Socket, {http_request, Type, {abs_path, Path}, HttpVer}}, S=#{
    session_state:= SessState
}) when T == http; T == ssl ->
    %Type = 'GET'/'POST'/'ETC'
    {HttpHeaders, Body} = proto_http:recv(Socket),
    S2 = handle_http(HttpHeaders, Body, S#{
        session_state=> SessState#{path=> Path, type=> Type}
    }),
    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    {noreply, S2#{nextDc=> NextDc}}
;

%%% HTTP Error
handle_info({T, Socket, {http_error, _}}, S=#{
    session_state:= SessState
}) when T == http; T == ssl ->
    %disconnect
    {noreply, S#{nextDc=> 1}}
;

%%% Websockets
handle_info({T, Socket, Bin}, S=#{ws_handler:= WSHandler, ws_buf:= WSBuf}) 
when T == tcp; T == ssl->
    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    S3 = case proto_ws:decode_frame(<<WSBuf/binary, Bin/binary>>) of
        %close
        {ok, 8, _, _, Buffer} ->
            transport_close(Socket),
            {noreply, S#{nextDc=> NextDc, ws_buf=> Buffer}};

        %pong
        {ok, 10, _, _, Buffer} ->
            ok = transport_setopts(Socket, [{active, once}, binary]),
            {noreply, S#{nextDc=> NextDc, ws_buf=> Buffer}};

        {ok, Opcode, 1, Payload, Buffer} ->
            Payload_ = zlib:inflate(maps:get(zinflate, S), <<Payload/binary,0,0,255,255>>),
            S2 = apply(WSHandler, msg, [iolist_to_binary(Payload_), S]),

            ok = transport_setopts(Socket, [{active, once}, binary]),
            {noreply, S2#{nextDc=> NextDc, ws_buf=> Buffer}};

        {ok, Opcode, 0, Payload, Buffer} ->
            S2 = apply(WSHandler, msg, [Payload, S]),

            ok = transport_setopts(Socket, [{active, once}, binary]),
            {noreply, S2#{nextDc=> NextDc, ws_buf=> Buffer}};

        {incomplete, Buffer} ->
            ok = transport_setopts(Socket, [{active, once}, binary]),
            {noreply, S#{nextDc=> NextDc, ws_buf=> Buffer}}
    end;

handle_info(ws_ping, S=#{socket:= Socket}) ->
    timer:send_after(?WS_PING_INTERVAL, ws_ping),
    p_send(Socket, proto_ws:encode_frame(ping), S);
%%%


%%%timeout check
handle_info(stalled, S=#{nextDc:= NextDc}) ->
    timer:send_after(1000, stalled),
    Now = unix_time(),
    if
        Now > NextDc ->
            case maps:get(socket, S, undefined) of
                undefined -> pass;
                Socket -> transport_close(Socket)
            end,
            {stop, {shutdown, tcp_closed}, S};

        true -> {noreply, S}
    end;

handle_info({T, Socket}, S) when T == tcp_closed; T == ssl_closed ->
    %WS / WSS
    {stop, {shutdown, tcp_closed}, S};
%%%


handle_info(Message, S) ->
    ?PRINT({"INFO", Message}),
    {noreply, S}.


forward_ws_closed(S) ->
    case maps:get(ws_handler, S, undefined) of
        undefined -> pass;
        WSHandler -> apply(WSHandler, disconnect, [S])
    end
    .

terminate({shutdown, tcp_closed}, S) ->
    forward_ws_closed(S),
    ok;
terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S}),
    forward_ws_closed(S),
    ok
    .

code_change(_OldVersion, S, _Extra) -> {ok, S}. 
