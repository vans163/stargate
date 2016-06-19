-module(vessel).
-behaviour(gen_server).

-export([start/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("global.hrl").



start(Params) -> {ok, Pid} = start_link(Params), Pid.
start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({Params, Socket}) ->
    process_flag(trap_exit, true),
    timer:send_after(1000, stalled),

    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    {ok, #{params=> Params, session_state=> #{}, nextDc=> NextDc}}.

handle_cast({pass_socket, ClientSocket}, S) ->
    inet:setopts(ClientSocket, [{active, once}, {packet, http_bin}]),
    {noreply, S#{socket=> ClientSocket}};

handle_cast(Message, S) -> {noreply, S}.
handle_call(Message, From, S) -> {reply, ok, S}.

%HTTP / WS
handle_info({http, Socket, {http_request, Type, {abs_path, Path}, HttpVer}}, S=#{
    session_state:= SessState, params:= #{ssl:=false, hosts:=Hosts}
}) ->
    %Type = 'GET'/'POST'/'ETC'
    {HttpHeaders, Body} = proto_http:recv(Socket),
    Host = maps:get('Host', HttpHeaders, <<"*">>),

    Upgrade = maps:get('Upgrade', HttpHeaders, undefined),
    {S2, SessState3} = case Upgrade of
        <<"websocket">> ->
            WildCardWSAtom = maps:get({ws, <<"*">>}, Hosts),
            {WSHandlerAtom, WSHandlerOptions} = maps:get({ws, Host}, Hosts, WildCardWSAtom),

            WSVersion = maps:get(<<"Sec-Websocket-Version">>, HttpHeaders),
            true = proto_ws:check_version(WSVersion),
            WSKey = maps:get(<<"Sec-Websocket-Key">>, HttpHeaders),
            WSExtensions = maps:get(<<"Sec-Websocket-Extensions">>, HttpHeaders, <<"">>),
            WSResponseBin = proto_ws:handshake(WSKey),
            ok = gen_tcp:send(Socket, WSResponseBin),
            S_ = apply(WSHandlerAtom, connect, [S]),
            inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
            timer:send_after(?WS_PING_INTERVAL, ws_ping),
            {S_#{ws_handler=> WSHandlerAtom, ws_buf=> <<>>}, SessState};

        undefined ->
            WildCardHTTPAtom = maps:get({http, <<"*">>}, Hosts),
            {HTTPHandlerAtom, HTTPHandlerOptions} = 
                maps:get({http, Host}, Hosts, WildCardHTTPAtom),

            {CleanPath, Query} = proto_http:path_and_query(Path),
            {ResponseCode, ResponseHeaders, ResponseBody, SessState2} = 
                apply(HTTPHandlerAtom, http, 
                    [Type, CleanPath, Query, HttpHeaders, Body, 
                        SessState#{socket=> Socket, host=> Host}
                    ]
            ),
            ResponseBin = proto_http:response(
                ResponseCode, ResponseHeaders, ResponseBody),

            ok = gen_tcp:send(Socket, ResponseBin),
            gen_tcp:close(Socket),
            {S, SessState2}
    end,

    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    {noreply, S2#{socket=> Socket, session_state=> SessState3, nextDc=> NextDc}}
;

%HTTPS / WSS
handle_info({http, Socket, {http_request, Type, {abs_path, Path}, HttpVer}}, S=#{
    session_state:= SessState, params:= Params=#{ssl:=true, hosts:=Hosts}
}) ->
    ssl_to_do
;

%%%WS / WSS
handle_info({tcp, Socket, Bin}, S=#{ws_handler:= WSHandler, ws_buf:= WSBuf}) ->

    S3 = case proto_ws:decode_frame(<<WSBuf/binary, Bin/binary>>) of
        %pong
        {ok, 10, _, Buffer} ->
            S#{ws_buf=> Buffer};

        {ok, Opcode, Payload, Buffer} ->
            S2 = apply(WSHandler, msg, [Payload, S]),
            S2#{ws_buf=> Buffer};

        {incomplete, Buffer} -> 
            S#{ws_buf=> Buffer}
    end,

    inet:setopts(Socket, [{active, once}, binary]),

    NextDc = unix_time() + ?MAX_TCP_TIMEOUT_SEC,
    {noreply, S3#{nextDc=> NextDc}}
;

handle_info(ws_ping, S=#{socket:= Socket}) ->
    timer:send_after(?WS_PING_INTERVAL, ws_ping),
    Bin = proto_ws:encode_frame(<<>>, ping),
    ok = gen_tcp:send(Socket, Bin),
    {noreply, S};
%%%


%%%timeout check
handle_info(stalled, S=#{nextDc:= NextDc}) ->
    timer:send_after(1000, stalled),
    Now = unix_time(),
    if
        Now > NextDc ->
            case maps:get(socket, S, undefined) of
                undefined -> pass;
                Socket -> gen_tcp:close(Socket)
            end,
            {stop, {shutdown, tcp_closed}, S};

        true -> {noreply, S}
    end;

handle_info({tcp_closed, Socket}, S) ->
    %WS / WSS
    case maps:get(ws_handler, S, undefined) of
        undefined -> pass;
        WSHandler -> apply(WSHandler, disconnect, [S])
    end,
    {stop, {shutdown, tcp_closed}, S};
%%%


handle_info(Message, S) ->
    ?PRINT({"INFO", Message, S}),
    {noreply, S}.


terminate({shutdown, tcp_closed}, S) -> ok;
terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S})
    .

code_change(_OldVersion, S, _Extra) -> {ok, S}. 
